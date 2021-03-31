{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module AMQP.Client where

import AMQP.Serialisation
import AMQP.Serialisation.Argument
import AMQP.Serialisation.Base
import AMQP.Serialisation.Frame
import AMQP.Serialisation.Generated.DomainTypes
import AMQP.Serialisation.Generated.Methods
import Control.Monad
import qualified Data.Attoparsec.ByteString as Attoparsec
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.ByteString.Builder as ByteString (Builder)
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
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
    -- | The leftover bytes that were already fetched but not yet parsed.
    --
    -- This matters in case the server pipelines multiple frames, for example.
    --
    -- Full whenever the connection is ready to parse the next piece
    connectionLeftoversVar :: !(MVar ByteString),
    -- | An mvar to wrap lock around synchronous methods.
    --
    -- When a synchronous request is sent, this mvar is always taken before the 'connectionLeftoversVar'.
    -- In this way, there can never be a deadlock because of these two @MVar@s alone.
    connectionSynchronousVar :: !(MVar ()),
    connectionMaximumNumberOfChannels :: !ShortUInt,
    connectionMaximumFrameSize :: !LongUInt,
    connectionHeartbeatInterval :: !ShortUInt,
    connectionMessageQueue :: !(TQueue ConnectionMessage),
    connectionChannels :: !(TVar (IntMap Channel))
  }
  deriving (Generic)

data ConnectionMessage
  = ConnectionMethod Method
  deriving (Show, Eq, Generic)

data ChannelMessage
  = ChannelMethod Method
  deriving (Show, Eq, Generic)

data Channel = Channel
  { channelConnection :: !Connection,
    channelNumber :: !ChannelNumber,
    channelMessageQueue :: !(TQueue ChannelMessage) -- TODO we want to be able to consume other things than just methods
  }
  deriving (Generic)

data HandleResult = NotHandled | HandledButNotDone | HandledAndDone
  deriving (Generic)

channelOpen :: MonadUnliftIO m => Connection -> m Channel
channelOpen conn@Connection {..} = do
  let number = 1 -- TODO choose an open index for a new open channel.
  ChannelOpenOk {..} <- synchronouslyRequest connectionNetworkConnection connectionLeftoversVar connectionSynchronousVar number ChannelOpen {channelOpenReserved1 = ""}
  messageQueue <- newTQueueIO
  pure $
    Channel
      { channelConnection = conn,
        channelNumber = number,
        channelMessageQueue = messageQueue
      }

chanHasMessage :: MonadIO m => Channel -> m Bool
chanHasMessage Channel {..} = atomically $ not <$> isEmptyTQueue channelMessageQueue

defaultQueueSettings :: QueueSettings
defaultQueueSettings = QueueSettings

data QueueSettings = QueueSettings
  deriving (Show, Eq, Generic)

data Queue = Queue {queueName :: ShortString}
  deriving (Show, Eq, Generic)

queueDeclare :: MonadUnliftIO m => Channel -> QueueName -> QueueSettings -> m Queue
queueDeclare Channel {..} name QueueSettings = do
  let Connection {..} = channelConnection
  QueueDeclareOk {..} <-
    synchronouslyRequest
      connectionNetworkConnection
      connectionLeftoversVar
      connectionSynchronousVar
      channelNumber
      QueueDeclare
        { queueDeclareReserved1 = 0,
          queueDeclareQueue = name,
          queueDeclarePassive = False,
          queueDeclareDurable = True,
          queueDeclareExclusive = False,
          queueDeclareAutoDelete = False,
          queueDeclareNoWait = False,
          queueDeclareArguments = emptyFieldTable
        }
  pure Queue {queueName = queueDeclareOkQueue}

queueBind :: MonadUnliftIO m => Channel -> QueueName -> ExchangeName -> RoutingKey -> m ()
queueBind Channel {..} queueName exchangeName routingKey = do
  let Connection {..} = channelConnection
  QueueBindOk <-
    synchronouslyRequest
      connectionNetworkConnection
      connectionLeftoversVar
      connectionSynchronousVar
      channelNumber
      QueueBind
        { queueBindReserved1 = 0,
          queueBindQueue = queueName,
          queueBindExchange = exchangeName,
          queueBindRoutingKey = routingKey,
          queueBindNoWait = False,
          queueBindArguments = emptyFieldTable
        }
  pure ()

type RoutingKey = ShortString

defaultExchangeSettings :: ExchangeSettings
defaultExchangeSettings = ExchangeSettings

data ExchangeSettings = ExchangeSettings
  deriving (Show, Eq, Generic)

data Exchange = Exchange
  deriving (Show, Eq, Generic)

exchangeDeclare :: MonadUnliftIO m => Channel -> ExchangeName -> ExchangeSettings -> m Exchange
exchangeDeclare Channel {..} name ExchangeSettings = do
  let Connection {..} = channelConnection
  ExchangeDeclareOk <-
    synchronouslyRequest
      connectionNetworkConnection
      connectionLeftoversVar
      connectionSynchronousVar
      channelNumber
      ExchangeDeclare
        { exchangeDeclareReserved1 = 0,
          exchangeDeclareExchange = name,
          exchangeDeclareType = "direct",
          exchangeDeclarePassive = False,
          exchangeDeclareDurable = True,
          exchangeDeclareReserved2 = False,
          exchangeDeclareReserved3 = False,
          exchangeDeclareNoWait = False,
          exchangeDeclareArguments = emptyFieldTable
        }
  pure Exchange

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
      -- C: protocol-header
      connectionPutBuilder networkConnection (buildProtocolHeader protocolHeader)
      leftoversVar <- newMVar SB.empty
      synchronousVar <- newMVar ()
      -- S: START
      frame <- connectionParse networkConnection leftoversVar parseProtocolNegotiationResponse
      f <- case frame of
        Left err -> throwIO $ ProtocolNegotiationFailed err
        Right (ProtocolRejected ph) -> throwIO $ ProtocolNegotiationRejected ph
        Right (ProtocolProposed f) -> pure f
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
      -- C: START-OK
      connectionPutBuilder networkConnection (buildGivenMethodFrame 0 connectionOk)
      -- TODO: Implement the challenge part of the protocol
      -- S: TUNE
      ConnectionTune {..} <- waitForSynchronousMessage networkConnection leftoversVar
      -- QUESTION: do we want to negotiate down any of these?
      --
      -- NOTE: We can only negotiate down, so if we ever make these
      -- configurable then we want to make sure of that.
      let maxChannels = connectionTuneChannelMax
          maxFrameSize = connectionTuneFrameMax
          heartbeatInterval = connectionTuneHeartbeat
      let connectionTuneOk =
            ConnectionTuneOk
              { connectionTuneOkChannelMax = maxChannels,
                connectionTuneOkFrameMax = maxFrameSize,
                connectionTuneOkHeartbeat = heartbeatInterval
              }
      -- C: TUNE-OK
      connectionPutBuilder networkConnection (buildGivenMethodFrame 0 connectionTuneOk)

      let connectionOpen =
            ConnectionOpen
              { connectionOpenVirtualHost = "/",
                connectionOpenReserved1 = "",
                connectionOpenReserved2 = False
              }
      -- C: OPEN
      -- S: OPEN-OK
      ConnectionOpenOk {..} <- synchronouslyRequest networkConnection leftoversVar synchronousVar 0 connectionOpen

      messageQueue <- newTQueueIO
      channelVar <- newTVarIO IM.empty
      let amqpConnection =
            Connection
              { connectionNetworkConnection = networkConnection,
                connectionLeftoversVar = leftoversVar,
                connectionSynchronousVar = synchronousVar,
                connectionMaximumNumberOfChannels = maxChannels,
                connectionMaximumFrameSize = maxFrameSize,
                connectionHeartbeatInterval = heartbeatInterval,
                connectionMessageQueue = messageQueue,
                connectionChannels = channelVar
              }
      callback amqpConnection

-- TODO generate these.
instance SynchronousRequest ConnectionOpen where
  type SynchronousResponse ConnectionOpen = ConnectionOpenOk

instance SynchronousRequest ChannelOpen where
  type SynchronousResponse ChannelOpen = ChannelOpenOk

instance SynchronousRequest QueueDeclare where
  type SynchronousResponse QueueDeclare = QueueDeclareOk

instance SynchronousRequest QueueBind where
  type SynchronousResponse QueueBind = QueueBindOk

instance SynchronousRequest ExchangeDeclare where
  type SynchronousResponse ExchangeDeclare = ExchangeDeclareOk

-- | Send a synchronous method to the server and wait for a synchronous answer.
--
-- This implements the pseudo logic in section 2.2.2: Mapping AMQP to a middleware API
synchronouslyRequest :: (MonadUnliftIO m, IsMethod a, SynchronousResponse a ~ b, FromMethod b) => Network.Connection -> MVar ByteString -> MVar () -> ChannelNumber -> a -> m b
synchronouslyRequest conn leftoversVar synchronousVar cn a = withMVar synchronousVar $ \() -> do
  connectionPutGivenMethod conn cn a
  waitForSynchronousMessage conn leftoversVar

waitForSynchronousMessage :: (MonadUnliftIO m, FromMethod b) => Network.Connection -> MVar ByteString -> m b
waitForSynchronousMessage conn leftoversVar = go
  where
    go = do
      errOrRes <- connectionParseMethod conn leftoversVar
      case errOrRes of
        Left err -> throwIO $ ProtocolViolation err
        Right r ->
          if methodIsSynchronous r
            then case fromMethod r of
              Nothing -> case fromMethod r of
                Nothing ->
                  throwIO $
                    ProtocolViolation $
                      unwords
                        [ "Expected a response method of type",
                          "TODO generated name",
                          "but got this method instead:",
                          show r
                        ]
                Just closed -> throwIO $ ServerClosedAMQPConnectionUnexpectedly closed
              Just m -> pure m
            else do
              -- TODO do something useful with this method, we probably don't just want to drop it.
              go

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

connectionPutGivenMethod :: (IsMethod a, MonadIO m) => Network.Connection -> ChannelNumber -> a -> m ()
connectionPutGivenMethod conn chan m = connectionPutBuilder conn (buildGivenMethodFrame chan m)

connectionPutBuilder :: MonadIO m => Network.Connection -> ByteString.Builder -> m ()
connectionPutBuilder conn b = liftIO $ mapM_ (Network.connectionPut conn) (LB.toChunks (SBB.toLazyByteString b))

connectionParseMethod :: (MonadUnliftIO m) => Network.Connection -> MVar ByteString -> m (Either String Method)
connectionParseMethod conn leftoversVar = connectionParse conn leftoversVar parseMethodFrame

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
  | -- | The protocol negotion failed because the server just did not follow the protocol and sent over something other than a protocol header during negotiation.
    ProtocolNegotiationFailed String
  | -- | The server does not support our locale.
    LocaleNotSupported
  | -- | The server does not support our SASL mechanism.
    SASLMechanismNotSupported ByteString
  | -- | The server closed the AMQP connection (not the socket) unexpectedly
    ServerClosedAMQPConnectionUnexpectedly !ConnectionClose
  | -- | The server comitted a protocol violation.
    ProtocolViolation String
  deriving (Show, Eq, Generic)

instance Exception ClientException
