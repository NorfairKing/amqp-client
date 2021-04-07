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
import AMQP.Serialisation.Generated.Content
import AMQP.Serialisation.Generated.DomainTypes
import AMQP.Serialisation.Generated.Methods
import Control.Monad
import qualified Data.Attoparsec.ByteString as Attoparsec
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.ByteString.Builder as ByteString (Builder)
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import Data.List
import Data.Map (Map)
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
    connectionSettingSASLMechanism :: !SASLMechanism,
    -- | The proposed maximum size of AMQP frames sent between the server and the client.
    --
    -- Note that the protocol specifies that the server (not the client) first
    -- proposes a maximum frame size.  The client can lower this maximum size
    -- but not raise it. That means that this setting specifies the maximum
    -- maximum frame size, but the actual maximum frame size could be lower,
    -- depending on the server.
    --
    -- Also note that the server may still reject very large frames if it
    -- cannot allocate the resources for it.
    --
    -- If 'Nothing', the client will take whatever the server proposed as the
    -- maximum frame size.
    connectionSettingMaximumFrameSize :: !(Maybe LongUInt)
  }
  deriving (Show, Eq, Generic)

-- | Make the simplest 'ConnectionSettings'.
--
-- Note that these settings use default values that may not be appropriate for your application.
-- For example, it uses 'PLAINMechanism "guest" "guest"' as its SASL Mechanism.
mkConnectionSettings :: Socket.HostName -> Socket.PortNumber -> ConnectionSettings
mkConnectionSettings connectionSettingHostName connectionSettingPort =
  let connectionSettingMaximumFrameSize = Nothing
      connectionSettingSASLMechanism = PLAINMechanism "guest" "guest"
   in ConnectionSettings {..}

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
    -- | An mvar to wrap lock around sending contiguous data
    --
    -- When a synchronous request is sent, this mvar is always taken before connectionLeftoversVar and connectionSynchronousVar.
    -- In this way, there can never be a deadlock because of these three @MVar@s alone.
    --
    -- Full whenever the connection is ready to send the next piece
    connectionSenderVar :: !(MVar ()),
    connectionMaximumNumberOfChannels :: !ShortUInt,
    connectionMaximumFrameSize :: !LongUInt,
    connectionHeartbeatInterval :: !ShortUInt,
    connectionMessageQueue :: !(TQueue ConnectionMessage),
    connectionChannels :: !(TVar (Map ChannelNumber Channel))
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
  let number = 1 :: ChannelNumber -- TODO choose an open index for a new open channel.
  ChannelOpenOk {} <- synchronouslyRequestByItself conn number ChannelOpen {channelOpenReserved1 = ""}
  messageQueue <- newTQueueIO
  let c =
        Channel
          { channelConnection = conn,
            channelNumber = number,
            channelMessageQueue = messageQueue
          }
  atomically $ modifyTVar' connectionChannels (M.insert number c)
  pure c

chanHasMessage :: MonadIO m => Channel -> m Bool
chanHasMessage Channel {..} = atomically $ not <$> isEmptyTQueue channelMessageQueue

data QueueSettings = QueueSettings
  { queueSetPassive :: !Bool,
    queueSetDurable :: !Bool,
    queueSetExclusive :: !Bool,
    queueSetAutoDelete :: !Bool,
    queueSetWait :: !Wait,
    queueSetArguments :: !FieldTable
  }
  deriving (Show, Eq, Generic)

defaultQueueSettings :: QueueSettings
defaultQueueSettings =
  QueueSettings
    { queueSetPassive = False,
      queueSetDurable = True,
      queueSetExclusive = False,
      queueSetAutoDelete = False,
      queueSetWait = DoWait,
      queueSetArguments = emptyFieldTable
    }

data Queue = Queue {queueName :: QueueName}
  deriving (Show, Eq, Generic)

queueDeclare :: MonadUnliftIO m => Channel -> QueueName -> QueueSettings -> m Queue
queueDeclare Channel {..} name QueueSettings {..} = do
  QueueDeclareOk {..} <-
    synchronouslyRequestByItself
      channelConnection
      channelNumber
      QueueDeclare
        { queueDeclareReserved1 = 0,
          queueDeclareQueue = name,
          queueDeclarePassive = queueSetPassive,
          queueDeclareDurable = queueSetDurable,
          queueDeclareExclusive = queueSetExclusive,
          queueDeclareAutoDelete = queueSetAutoDelete,
          queueDeclareNoWait = waitToNoWait queueSetWait,
          queueDeclareArguments = emptyFieldTable
        }
  pure Queue {queueName = queueDeclareOkQueue}

queueBind :: MonadUnliftIO m => Channel -> Queue -> Exchange -> RoutingKey -> m ()
queueBind Channel {..} Queue {..} Exchange {..} routingKey = do
  QueueBindOk <-
    synchronouslyRequestByItself
      channelConnection
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

data Exchange = Exchange {exchangeName :: !ExchangeName}
  deriving (Show, Eq, Generic)

exchangeDeclare :: MonadUnliftIO m => Channel -> ExchangeName -> ExchangeSettings -> m Exchange
exchangeDeclare Channel {..} name ExchangeSettings = do
  ExchangeDeclareOk <-
    synchronouslyRequestByItself
      channelConnection
      channelNumber
      ExchangeDeclare
        { exchangeDeclareReserved1 = 0,
          exchangeDeclareExchange = name,
          exchangeDeclareType = "direct",
          exchangeDeclarePassive = False,
          exchangeDeclareDurable = True,
          exchangeDeclareAutoDelete = False,
          exchangeDeclareInternal = False,
          exchangeDeclareNoWait = False,
          exchangeDeclareArguments = emptyFieldTable
        }
  pure Exchange {exchangeName = name}

basicGet :: MonadUnliftIO m => Channel -> Queue -> Ack -> m (Maybe Message)
basicGet Channel {..} Queue {..} ack = withMVar (connectionSynchronousVar channelConnection) $ \() -> do
  let bg = BasicGet {basicGetReserved1 = 0, basicGetQueue = queueName, basicGetNoAck = ackToNoAck ack}
  connectionSendGivenMethodByItself channelConnection channelNumber bg
  bgr <- waitForSynchronousMethod channelConnection
  case bgr of
    BasicGetResponseGetEmpty BasicGetEmpty {} -> pure Nothing
    BasicGetResponseGetOk BasicGetOk {} -> do
      frame <- connectionGetFrame channelConnection
      case frame of
        -- TODO deal with multiple channels
        -- TODO deal with multi-body content
        ContentHeaderPayload _ (ContentHeaderFrame _ _) -> do
          frame2 <- connectionGetFrame channelConnection
          case frame2 of
            ContentBodyPayload _ ContentBody {..} -> pure $ Just $ Message {messageBody = contentBodyPayload}
            _ -> throwIO $ ProtocolViolation $ "Expected a content body, got this instead: " <> show frame
        _ -> throwIO $ ProtocolViolation $ "Expected a content header, got this instead: " <> show frame

basicPublish :: MonadUnliftIO m => Channel -> Exchange -> RoutingKey -> Message -> m ()
basicPublish Channel {..} Exchange {..} routingKey msg@Message {..} = do
  let bp =
        BasicPublish
          { basicPublishReserved1 = 0,
            basicPublishExchange = exchangeName,
            basicPublishRoutingKey = routingKey,
            basicPublishMandatory = True,
            basicPublishImmediate = False
          }
      ContentFrames chf cbs = messageToContentFrames (connectionMaximumFrameSize channelConnection) msg
  connectionSendBuilder channelConnection $
    mconcat $
      buildFrame (MethodPayload channelNumber (MethodBasicPublish bp)) :
      buildFrame (ContentHeaderPayload channelNumber chf) :
      map (buildFrame . ContentBodyPayload channelNumber) cbs

-- | An intermediate type to represent a 'Messag' as the frames that it will be rendered as.
data ContentFrames
  = ContentFrames
      (ContentHeaderFrame ContentHeader)
      [ContentBody]
  deriving (Show, Eq, Generic)

-- | Split an opaque message into 'ContentFrames'
--
-- This function takes the connection's maximum frame size as a first argument.
--
-- There is a 8 byte overhead for every frame that we send:
--
-- - 1 byte for the type
-- - 2 bytes for the channel
-- - 4 bytes for the size
-- - 1 byte for the frame end
--
-- This function will therefore only work if the maximum frame size is greater
-- than 8.  In reality this is not a problem because there are method frames
-- much larger than 8 bytes that will already have been sent by the time this
-- function can be used.
messageToContentFrames :: LongUInt -> Message -> ContentFrames
messageToContentFrames maximumFrameSize Message {..} =
  let chf =
        ContentHeaderFrame
          { contentHeaderFrameBodySize = intToWord64 $ SB.length messageBody,
            contentHeaderFrameProperties =
              ContentHeaderBasic $
                BasicContentHeader -- TODO allow the user to set these somehow
                  { basicContentHeaderContentType = Nothing,
                    basicContentHeaderContentEncoding = Nothing,
                    basicContentHeaderHeaders = Nothing,
                    basicContentHeaderDeliveryMode = Nothing,
                    basicContentHeaderPriority = Nothing,
                    basicContentHeaderCorrelationId = Nothing,
                    basicContentHeaderReplyTo = Nothing,
                    basicContentHeaderExpiration = Nothing,
                    basicContentHeaderMessageId = Nothing,
                    basicContentHeaderTimestamp = Nothing,
                    basicContentHeaderType = Nothing,
                    basicContentHeaderUserId = Nothing,
                    basicContentHeaderAppId = Nothing,
                    basicContentHeaderReserved = Nothing
                  }
          }
   in ContentFrames chf $ go messageBody
  where
    maximumBodySize :: Int
    maximumBodySize = word32ToInt $ maximumFrameSize - 8
    go :: ByteString -> [ContentBody]
    go sb
      | SB.null sb = []
      | SB.length sb <= maximumBodySize = [ContentBody {contentBodyPayload = sb}]
      | otherwise = ContentBody {contentBodyPayload = SB.take maximumBodySize sb} : go (SB.drop maximumBodySize sb)

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
      connectionSendBuilderWithoutLock networkConnection (buildProtocolHeader protocolHeader)
      leftoversVar <- newMVar SB.empty
      synchronousVar <- newMVar ()
      senderVar <- newMVar ()
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
      connectionSendBuilderWithoutLock networkConnection (buildGivenMethodFrame 0 connectionOk)
      -- TODO: Implement the challenge part of the protocol
      -- S: TUNE
      errOrTune <- connectionParse networkConnection leftoversVar $ snd <$> parseGivenMethodFrame
      ConnectionTune {..} <- case errOrTune of
        Left err -> throwIO $ ProtocolViolation $ "The server did not send a connection.tune frame: " <> err
        Right ct -> pure ct

      -- QUESTION: do we want to negotiate down any of these?
      --
      -- NOTE: We can only negotiate down, so if we ever make these
      -- configurable then we want to make sure of that.
      let maxChannels = connectionTuneChannelMax
          maxFrameSize = case connectionSettingMaximumFrameSize of
            Nothing -> connectionTuneFrameMax
            Just clientSuggestedMaxFrameSize -> min connectionTuneFrameMax clientSuggestedMaxFrameSize
          heartbeatInterval = connectionTuneHeartbeat
      let connectionTuneOk =
            ConnectionTuneOk
              { connectionTuneOkChannelMax = maxChannels,
                connectionTuneOkFrameMax = maxFrameSize,
                connectionTuneOkHeartbeat = heartbeatInterval
              }
      -- C: TUNE-OK
      connectionSendBuilderWithoutLock networkConnection (buildGivenMethodFrame 0 connectionTuneOk)

      let connectionOpen =
            ConnectionOpen
              { connectionOpenVirtualHost = "/",
                connectionOpenReserved1 = "",
                connectionOpenReserved2 = False
              }
      -- C: OPEN
      connectionSendBuilderWithoutLock networkConnection (buildGivenMethodFrame 0 connectionOpen)
      -- S: OPEN-OK
      errOrOpenOk <- connectionParse networkConnection leftoversVar $ snd <$> parseGivenMethodFrame
      ConnectionOpenOk {} <- case errOrOpenOk of
        Left err -> throwIO $ ProtocolViolation $ "The server did not send a connection.open-ok frame: " <> err
        Right coo -> pure coo

      messageQueue <- newTQueueIO
      channelVar <- newTVarIO M.empty
      let amqpConnection =
            Connection
              { connectionNetworkConnection = networkConnection,
                connectionLeftoversVar = leftoversVar,
                connectionSynchronousVar = synchronousVar,
                connectionSenderVar = senderVar,
                connectionMaximumNumberOfChannels = maxChannels,
                connectionMaximumFrameSize = maxFrameSize,
                connectionHeartbeatInterval = heartbeatInterval,
                connectionMessageQueue = messageQueue,
                connectionChannels = channelVar
              }
      callback amqpConnection

-- | Send a synchronous method to the server and wait for a synchronous answer.
--
-- This implements the pseudo logic in section 2.2.2: Mapping AMQP to a middleware API
--
-- This method requires that the response grammar does not require any subsequent frames to be sent contiguously.
synchronouslyRequestByItself :: (MonadUnliftIO m, IsMethod a, SynchronousResponse a ~ b, FromMethod b) => Connection -> ChannelNumber -> a -> m b
synchronouslyRequestByItself conn@Connection {..} cn req = withMVar connectionSynchronousVar $ \() -> do
  connectionSendGivenMethodByItself conn cn req
  waitForSynchronousMethod conn

waitForSynchronousMethod :: (MonadUnliftIO m, FromMethod b) => Connection -> m b
waitForSynchronousMethod Connection {..} = go
  where
    go = do
      errOrRes <- connectionParseFrame connectionNetworkConnection connectionLeftoversVar
      case errOrRes of
        Left err -> throwIO $ ProtocolViolation err
        Right f -> case f of
          MethodPayload _ m ->
            -- TODO we need to do something with the channel number
            if methodIsSynchronous m
              then case fromMethod m of
                Nothing -> case fromMethod m of
                  Nothing ->
                    throwIO $
                      ProtocolViolation $
                        unwords
                          [ "Expected a response method of type",
                            "TODO generated name",
                            "but got this method instead:",
                            show m
                          ]
                  Just closed -> throwIO $ ServerClosedAMQPConnectionUnexpectedly closed
                Just responseMethod -> pure responseMethod
              else do
                -- TODO do something useful with this method, we probably don't just want to drop it.
                go
          HeartbeatPayload -> go -- TODO we probably don't just want to ignore heartbeats
          _ -> undefined -- TODO we need to do something with other frames.

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

-- | Send a method frame for the given method
--
-- Note that the method grammar must not involve any frames that need to be sent immediately afterwards.
connectionSendGivenMethodByItself :: (IsMethod a, MonadUnliftIO m) => Connection -> ChannelNumber -> a -> m ()
connectionSendGivenMethodByItself conn chan m = connectionSendBuilder conn $ buildGivenMethodFrame chan m

-- | Send raw 'ByteString.Builder's to the server
--
-- This function uses the 'connectionSenderVar' lock
connectionSendBuilder :: MonadUnliftIO m => Connection -> ByteString.Builder -> m ()
connectionSendBuilder Connection {..} b = withMVar connectionSenderVar $ \() -> do
  connectionSendBuilderWithoutLock connectionNetworkConnection b

connectionSendBuilderWithoutLock :: MonadIO m => Network.Connection -> ByteString.Builder -> m ()
connectionSendBuilderWithoutLock conn b =
  liftIO $ mapM_ (Network.connectionPut conn) (LB.toChunks (SBB.toLazyByteString b))

connectionParseMethod :: (MonadUnliftIO m) => Network.Connection -> MVar ByteString -> m (Either String Method)
connectionParseMethod conn leftoversVar = connectionParse conn leftoversVar parseMethodFrame

connectionGetFrame :: MonadUnliftIO m => Connection -> m Frame
connectionGetFrame Connection {..} = do
  errOrFrame <- connectionParseFrame connectionNetworkConnection connectionLeftoversVar
  case errOrFrame of
    Left err -> throwIO $ ProtocolViolation err
    Right f -> pure f

connectionParseFrame :: (MonadUnliftIO m) => Network.Connection -> MVar ByteString -> m (Either String Frame)
connectionParseFrame conn leftoversVar = connectionParse conn leftoversVar parseFrame

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

data Wait = DoWait | Don'tWait
  deriving (Show, Eq, Generic)

waitToNoWait :: Wait -> NoWait
waitToNoWait = \case
  -- Yes, it's backwards, I know.
  DoWait -> False
  Don'tWait -> True

data Ack = Ack | NoAck

ackToNoAck :: Ack -> NoAck
ackToNoAck = \case
  -- Yes, it's backwards, I know.
  Ack -> False
  NoAck -> True

data Message = Message
  { messageBody :: ByteString
  }
  deriving (Show, Eq, Generic)

mkMessage :: ByteString -> Message
mkMessage body = Message {messageBody = body}
