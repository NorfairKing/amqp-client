{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module AMQP.Client where

import AMQP.Serialisation
import qualified Data.Attoparsec.ByteString as Attoparsec
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.ByteString.Builder as ByteString (Builder)
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import Data.List
import GHC.Generics (Generic)
import qualified Network.Connection as Network
import qualified Network.Socket as Socket
import UnliftIO

data ConnectionSettings = ConnectionSettings
  { connectionSettingHostName :: Socket.HostName,
    connectionSettingPort :: Socket.PortNumber
  }
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
      liftIO $ print (frame :: Either String ProtocolNegotiationResponse)
      let amqpConnection =
            Connection
              { connectionNetworkConnection = networkConnection,
                connectionLeftoversVar = leftoversVar
              }
      callback amqpConnection

connectionPutBuilder :: MonadIO m => Network.Connection -> ByteString.Builder -> m ()
connectionPutBuilder conn b = liftIO $ mapM_ (Network.connectionPut conn) (LB.toChunks (SBB.toLazyByteString b))

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
