{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AMQP.Client where

import AMQP.Serialisation
import AMQP.Serialisation.Base
import AMQP.Serialisation.Frame
import AMQP.Serialisation.Generated.Methods
import Control.Monad
import qualified Data.Attoparsec.ByteString as Attoparsec
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.ByteString.Builder as ByteString (Builder)
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import Data.List
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import qualified Network.Connection as Network
import qualified Network.Socket as Socket
import UnliftIO

data ConnectionSettings = ConnectionSettings
  { connectionSettingHostName :: !Socket.HostName,
    connectionSettingPort :: !Socket.PortNumber,
    connectionSettingSASLMechanism :: !SASLMechanism
  }
  deriving (Show, Eq, Generic)

data SASLMechanism
  = PLAINMechanism
      !Text
      -- ^ Username
      !Text
      -- ^ Password
  deriving (Show, Eq, Generic)

data Connection = Connection
  { connectionNetworkConnection :: !Network.Connection,
    -- The leftover bytes that were already fetched but not yet parsed.
    -- This matters in case the server pipelines multiple frames, for example.
    --
    -- Full whenever the connection is ready to parse the next piece
    connectionLeftoversVar :: !(MVar ByteString)
  }
  deriving (Generic)

-- | Set up (and clean up) a connection to the AMQP server.
--
-- If you don't know what 'MonadUnliftIO' is, you can just pretent it is 'IO'.
--
-- We do not expose an 'openConnection' and 'closeConnection' function just so
-- you can't get the 'bracket' wrong.
withConnection :: MonadUnliftIO m => ConnectionSettings -> (Connection -> m a) -> m a
withConnection ConnectionSettings {..} callback = do
  connectionContext <- liftIO Network.initConnectionContext
  let connectionParams =
        Network.ConnectionParams
          { Network.connectionHostname = connectionSettingHostName,
            Network.connectionPort = connectionSettingPort,
            Network.connectionUseSecure = Nothing,
            Network.connectionUseSocks = Nothing
          }
  bracket
    (liftIO $ Network.connectTo connectionContext connectionParams)
    (liftIO . Network.connectionClose)
    $ \networkConnection -> do
      connectionPutBuilder networkConnection (buildProtocolHeader protocolHeader)
      leftoversVar <- newMVar SB.empty
      frame <- connectionParse networkConnection leftoversVar parseProtocolNegotiationResponse
      f <- case frame of
        Left err -> throwIO $ ProtocolNegotiationFailed err
        Right (ProtocolRejected ph) -> throwIO $ ProtocolNegotiationRejected ph
        Right (ProtocolProposed f) -> pure f
      liftIO $ print f
      let locales = SB.split 0x20 (longStringBytes (connectionStartLocales f))
          ourLocaleSupported = ourLocale `elem` locales
      when (not ourLocaleSupported) $ throwIO LocaleNotSupported
      let mechanisms = SB.split 0x20 (longStringBytes (connectionStartMechanisms f))

          ourMechanismName = saslMechanismName connectionSettingSASLMechanism
          ourMechanismSupported = ourMechanismName `elem` mechanisms
      when (not ourMechanismSupported) $ throwIO $ SASLMechanismNotSupported ourMechanismName
      let connectionOk =
            ConnectionStartOk
              { connectionStartOkClientProperties = FieldTable M.empty,
                connectionStartOkMechanism = ShortString ourMechanismName,
                connectionStartOkResponse = saslMechanismResponse connectionSettingSASLMechanism,
                connectionStartOkLocale = ShortString ourLocale
              }
      liftIO $ putStrLn "Answering: "
      liftIO $ print connectionOk
      connectionPutBuilder networkConnection (buildGivenMethodFrame 0 connectionOk)
      errOrRes <- connectionParseMethod networkConnection leftoversVar
      liftIO $ print (errOrRes :: Either String Method)

      let amqpConnection =
            Connection
              { connectionNetworkConnection = networkConnection,
                connectionLeftoversVar = leftoversVar
              }
      callback amqpConnection

saslMechanismName :: SASLMechanism -> ByteString
saslMechanismName = \case
  PLAINMechanism {} -> plainSASLName

saslMechanismResponse :: SASLMechanism -> LongString
saslMechanismResponse = \case
  PLAINMechanism username password -> plainSASLResponse username password

plainSASLName :: ByteString
plainSASLName = "PLAIN"

-- | The @PLAIN@ SASL mechanism. See <http://tools.ietf.org/html/rfc4616 RFC4616>
plainSASLResponse :: Text -> Text -> LongString
plainSASLResponse username password =
  LongString $
    LB.toStrict $
      SBB.toLazyByteString $
        mconcat
          [ SBB.word8 0,
            SBB.byteString $ TE.encodeUtf8 username,
            SBB.word8 0,
            SBB.byteString $ TE.encodeUtf8 password
          ]

connectionPutBuilder :: MonadIO m => Network.Connection -> ByteString.Builder -> m ()
connectionPutBuilder conn b = liftIO $ mapM_ (Network.connectionPut conn) (LB.toChunks (SBB.toLazyByteString b))

connectionParseMethod :: (MonadUnliftIO m) => Network.Connection -> MVar ByteString -> m (Either String Method)
connectionParseMethod conn leftoversVar = connectionParse conn leftoversVar parseMethodFrame

-- TODO keep track of the last bit of bytestring that we already got.
connectionParse :: MonadUnliftIO m => Network.Connection -> MVar ByteString -> Attoparsec.Parser a -> m (Either String a)
connectionParse conn leftoversVar parser = modifyMVar leftoversVar $ \leftovers -> do
  result <- liftIO $ Attoparsec.parseWith (Network.connectionGet conn chunkSize) parser leftovers
  pure $ case result of
    (Attoparsec.Done newLeftovers r) -> (newLeftovers, Right r)
    (Attoparsec.Fail newLeftovers [] msg) -> (newLeftovers, Left msg)
    (Attoparsec.Fail newLeftovers ctxs msg) -> (newLeftovers, Left (intercalate " > " ctxs ++ ": " ++ msg))
    (Attoparsec.Partial _) -> (leftovers, Left "Result: incomplete input. This should not happen because we use 'parseWith'")

-- | How many bytes to read at a time, at most
chunkSize :: Int
chunkSize = 4098

ourLocale :: ByteString
ourLocale = "en_US"

data ClientException
  = -- | The protocol negotiation was rejected because the server sent back a protocol header instead of a Connection.start method frame.
    ProtocolNegotiationRejected ProtocolHeader
  | -- | The protocol negotion failed because the server just did not follow the protocol and sent over something other than a protocol header during negotiation
    ProtocolNegotiationFailed String
  | -- | The server does not support our locale
    LocaleNotSupported
  | -- | The server does not support our SASL mechanism
    SASLMechanismNotSupported ByteString
  deriving (Show, Eq, Generic)

instance Exception ClientException
