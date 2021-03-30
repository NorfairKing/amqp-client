{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module AMQP.Serialisation.Generated.Methods where

import AMQP.Serialisation.Argument
import AMQP.Serialisation.Base
import AMQP.Serialisation.Frame
import AMQP.Serialisation.Generated.DomainTypes
import Data.Attoparsec.ByteString as Parse
import Data.ByteString.Builder as ByteString (Builder)
import Data.Proxy
import Data.Validity
import GHC.Generics (Generic)

-- * The @connection@ class

-- The connection class provides methods for a client to establish a network connection to
-- a server, and for both peers to operate the connection thereafter.
--
-- Grammar:
--
-- >
-- >       connection          = open-connection *use-connection close-connection
-- >       open-connection     = C:protocol-header
-- >                             S:START C:START-OK
-- >                             *challenge
-- >                             S:TUNE C:TUNE-OK
-- >                             C:OPEN S:OPEN-OK
-- >       challenge           = S:SECURE C:SECURE-OK
-- >       use-connection      = *channel
-- >       close-connection    = C:CLOSE S:CLOSE-OK
-- >                           / S:CLOSE C:CLOSE-OK
-- >

-- | The @start@ method: start connection negotiation
--
-- This method starts the connection negotiation process by telling the client the
-- protocol version that the server proposes, along with a list of security mechanisms
-- which the client can use for authentication.
data ConnectionStart = ConnectionStart
  { connectionStartVersionMajor :: !Octet,
    connectionStartVersionMinor :: !Octet,
    connectionStartServerProperties :: !PeerProperties,
    connectionStartMechanisms :: !LongString,
    connectionStartLocales :: !LongString
  }
  deriving (Show, Eq, Generic)

instance Validity ConnectionStart

instance IsMethod ConnectionStart where
  methodClassId (Proxy) = 10
  methodMethodId (Proxy) = 10
  methodSynchronous (Proxy) = True

instance FromMethod ConnectionStart where
  fromMethod = \case
    MethodConnectionStart m -> Just m
    _ -> Nothing

-- | The @start-ok@ method: select security mechanism and locale
--
-- This method selects a SASL security mechanism.
data ConnectionStartOk = ConnectionStartOk
  { connectionStartOkClientProperties :: !PeerProperties,
    connectionStartOkMechanism :: !ShortString,
    connectionStartOkResponse :: !LongString,
    connectionStartOkLocale :: !ShortString
  }
  deriving (Show, Eq, Generic)

instance Validity ConnectionStartOk

instance IsMethod ConnectionStartOk where
  methodClassId (Proxy) = 10
  methodMethodId (Proxy) = 11
  methodSynchronous (Proxy) = True

instance FromMethod ConnectionStartOk where
  fromMethod = \case
    MethodConnectionStartOk m -> Just m
    _ -> Nothing

-- | The @secure@ method: security mechanism challenge
--
-- The SASL protocol works by exchanging challenges and responses until both peers have
-- received sufficient information to authenticate each other. This method challenges
-- the client to provide more information.
data ConnectionSecure = ConnectionSecure {connectionSecureChallenge :: !LongString}
  deriving (Show, Eq, Generic)

instance Validity ConnectionSecure

instance IsMethod ConnectionSecure where
  methodClassId (Proxy) = 10
  methodMethodId (Proxy) = 20
  methodSynchronous (Proxy) = True

instance FromMethod ConnectionSecure where
  fromMethod = \case
    MethodConnectionSecure m -> Just m
    _ -> Nothing

-- | The @secure-ok@ method: security mechanism response
--
-- This method attempts to authenticate, passing a block of SASL data for the security
-- mechanism at the server side.
data ConnectionSecureOk = ConnectionSecureOk {connectionSecureOkResponse :: !LongString}
  deriving (Show, Eq, Generic)

instance Validity ConnectionSecureOk

instance IsMethod ConnectionSecureOk where
  methodClassId (Proxy) = 10
  methodMethodId (Proxy) = 21
  methodSynchronous (Proxy) = True

instance FromMethod ConnectionSecureOk where
  fromMethod = \case
    MethodConnectionSecureOk m -> Just m
    _ -> Nothing

-- | The @tune@ method: propose connection tuning parameters
--
-- This method proposes a set of connection configuration values to the client. The
-- client can accept and/or adjust these.
data ConnectionTune = ConnectionTune
  { connectionTuneChannelMax :: !ShortUInt,
    connectionTuneFrameMax :: !LongUInt,
    connectionTuneHeartbeat :: !ShortUInt
  }
  deriving (Show, Eq, Generic)

instance Validity ConnectionTune

instance IsMethod ConnectionTune where
  methodClassId (Proxy) = 10
  methodMethodId (Proxy) = 30
  methodSynchronous (Proxy) = True

instance FromMethod ConnectionTune where
  fromMethod = \case
    MethodConnectionTune m -> Just m
    _ -> Nothing

-- | The @tune-ok@ method: negotiate connection tuning parameters
--
-- This method sends the client's connection tuning parameters to the server.
-- Certain fields are negotiated, others provide capability information.
data ConnectionTuneOk = ConnectionTuneOk
  { connectionTuneOkChannelMax :: !ShortUInt,
    connectionTuneOkFrameMax :: !LongUInt,
    connectionTuneOkHeartbeat :: !ShortUInt
  }
  deriving (Show, Eq, Generic)

instance Validity ConnectionTuneOk

instance IsMethod ConnectionTuneOk where
  methodClassId (Proxy) = 10
  methodMethodId (Proxy) = 31
  methodSynchronous (Proxy) = True

instance FromMethod ConnectionTuneOk where
  fromMethod = \case
    MethodConnectionTuneOk m -> Just m
    _ -> Nothing

-- | The @open@ method: open connection to virtual host
--
-- This method opens a connection to a virtual host, which is a collection of
-- resources, and acts to separate multiple application domains within a server.
-- The server may apply arbitrary limits per virtual host, such as the number
-- of each type of entity that may be used, per connection and/or in total.
data ConnectionOpen = ConnectionOpen
  { connectionOpenVirtualHost :: !Path,
    connectionOpenReserved1 :: !ShortString,
    connectionOpenReserved2 :: !Bit
  }
  deriving (Show, Eq, Generic)

instance Validity ConnectionOpen

instance IsMethod ConnectionOpen where
  methodClassId (Proxy) = 10
  methodMethodId (Proxy) = 40
  methodSynchronous (Proxy) = True

instance FromMethod ConnectionOpen where
  fromMethod = \case
    MethodConnectionOpen m -> Just m
    _ -> Nothing

-- | The @open-ok@ method: signal that connection is ready
--
-- This method signals to the client that the connection is ready for use.
data ConnectionOpenOk = ConnectionOpenOk {connectionOpenOkReserved1 :: !ShortString}
  deriving (Show, Eq, Generic)

instance Validity ConnectionOpenOk

instance IsMethod ConnectionOpenOk where
  methodClassId (Proxy) = 10
  methodMethodId (Proxy) = 41
  methodSynchronous (Proxy) = True

instance FromMethod ConnectionOpenOk where
  fromMethod = \case
    MethodConnectionOpenOk m -> Just m
    _ -> Nothing

-- | The @close@ method: request a connection close
--
-- This method indicates that the sender wants to close the connection. This may be
-- due to internal conditions (e.g. a forced shut-down) or due to an error handling
-- a specific method, i.e. an exception. When a close is due to an exception, the
-- sender provides the class and method id of the method which caused the exception.
data ConnectionClose = ConnectionClose
  { connectionCloseReplyCode :: !ReplyCode,
    connectionCloseReplyText :: !ReplyText,
    connectionCloseClassId :: !ClassId,
    connectionCloseMethodId :: !MethodId
  }
  deriving (Show, Eq, Generic)

instance Validity ConnectionClose

instance IsMethod ConnectionClose where
  methodClassId (Proxy) = 10
  methodMethodId (Proxy) = 50
  methodSynchronous (Proxy) = True

instance FromMethod ConnectionClose where
  fromMethod = \case
    MethodConnectionClose m -> Just m
    _ -> Nothing

-- | The @close-ok@ method: confirm a connection close
--
-- This method confirms a Connection.Close method and tells the recipient that it is
-- safe to release resources for the connection and close the socket.
data ConnectionCloseOk
  = ConnectionCloseOk
  deriving (Show, Eq, Generic)

instance Validity ConnectionCloseOk

instance IsMethod ConnectionCloseOk where
  methodClassId (Proxy) = 10
  methodMethodId (Proxy) = 51
  methodSynchronous (Proxy) = True

instance FromMethod ConnectionCloseOk where
  fromMethod = \case
    MethodConnectionCloseOk m -> Just m
    _ -> Nothing

-- * The @channel@ class

-- The channel class provides methods for a client to establish a channel to a
-- server and for both peers to operate the channel thereafter.
--
-- Grammar:
--
-- >
-- >       channel             = open-channel *use-channel close-channel
-- >       open-channel        = C:OPEN S:OPEN-OK
-- >       use-channel         = C:FLOW S:FLOW-OK
-- >                           / S:FLOW C:FLOW-OK
-- >                           / functional-class
-- >       close-channel       = C:CLOSE S:CLOSE-OK
-- >                           / S:CLOSE C:CLOSE-OK
-- >

-- | The @open@ method: open a channel for use
--
-- This method opens a channel to the server.
data ChannelOpen = ChannelOpen {channelOpenReserved1 :: !ShortString}
  deriving (Show, Eq, Generic)

instance Validity ChannelOpen

instance IsMethod ChannelOpen where
  methodClassId (Proxy) = 20
  methodMethodId (Proxy) = 10
  methodSynchronous (Proxy) = True

instance FromMethod ChannelOpen where
  fromMethod = \case
    MethodChannelOpen m -> Just m
    _ -> Nothing

-- | The @open-ok@ method: signal that the channel is ready
--
-- This method signals to the client that the channel is ready for use.
data ChannelOpenOk = ChannelOpenOk {channelOpenOkReserved1 :: !LongString}
  deriving (Show, Eq, Generic)

instance Validity ChannelOpenOk

instance IsMethod ChannelOpenOk where
  methodClassId (Proxy) = 20
  methodMethodId (Proxy) = 11
  methodSynchronous (Proxy) = True

instance FromMethod ChannelOpenOk where
  fromMethod = \case
    MethodChannelOpenOk m -> Just m
    _ -> Nothing

-- | The @flow@ method: enable/disable flow from peer
--
-- This method asks the peer to pause or restart the flow of content data sent by
-- a consumer. This is a simple flow-control mechanism that a peer can use to avoid
-- overflowing its queues or otherwise finding itself receiving more messages than
-- it can process. Note that this method is not intended for window control. It does
-- not affect contents returned by Basic.Get-Ok methods.
data ChannelFlow = ChannelFlow {channelFlowActive :: !Bit}
  deriving (Show, Eq, Generic)

instance Validity ChannelFlow

instance IsMethod ChannelFlow where
  methodClassId (Proxy) = 20
  methodMethodId (Proxy) = 20
  methodSynchronous (Proxy) = True

instance FromMethod ChannelFlow where
  fromMethod = \case
    MethodChannelFlow m -> Just m
    _ -> Nothing

-- | The @flow-ok@ method: confirm a flow method
--
-- Confirms to the peer that a flow command was received and processed.
data ChannelFlowOk = ChannelFlowOk {channelFlowOkActive :: !Bit}
  deriving (Show, Eq, Generic)

instance Validity ChannelFlowOk

instance IsMethod ChannelFlowOk where
  methodClassId (Proxy) = 20
  methodMethodId (Proxy) = 21
  methodSynchronous (Proxy) = False

instance FromMethod ChannelFlowOk where
  fromMethod = \case
    MethodChannelFlowOk m -> Just m
    _ -> Nothing

-- | The @close@ method: request a channel close
--
-- This method indicates that the sender wants to close the channel. This may be due to
-- internal conditions (e.g. a forced shut-down) or due to an error handling a specific
-- method, i.e. an exception. When a close is due to an exception, the sender provides
-- the class and method id of the method which caused the exception.
data ChannelClose = ChannelClose
  { channelCloseReplyCode :: !ReplyCode,
    channelCloseReplyText :: !ReplyText,
    channelCloseClassId :: !ClassId,
    channelCloseMethodId :: !MethodId
  }
  deriving (Show, Eq, Generic)

instance Validity ChannelClose

instance IsMethod ChannelClose where
  methodClassId (Proxy) = 20
  methodMethodId (Proxy) = 40
  methodSynchronous (Proxy) = True

instance FromMethod ChannelClose where
  fromMethod = \case
    MethodChannelClose m -> Just m
    _ -> Nothing

-- | The @close-ok@ method: confirm a channel close
--
-- This method confirms a Channel.Close method and tells the recipient that it is safe
-- to release resources for the channel.
data ChannelCloseOk = ChannelCloseOk deriving (Show, Eq, Generic)

instance Validity ChannelCloseOk

instance IsMethod ChannelCloseOk where
  methodClassId (Proxy) = 20
  methodMethodId (Proxy) = 41
  methodSynchronous (Proxy) = True

instance FromMethod ChannelCloseOk where
  fromMethod = \case
    MethodChannelCloseOk m -> Just m
    _ -> Nothing

-- * The @exchange@ class

-- Exchanges match and distribute messages across queues. Exchanges can be configured in
-- the server or declared at runtime.
--
-- Grammar:
--
-- >
-- >       exchange            = C:DECLARE  S:DECLARE-OK
-- >                           / C:DELETE   S:DELETE-OK
-- >

-- | The @declare@ method: verify exchange exists, create if needed
--
-- This method creates an exchange if it does not already exist, and if the exchange
-- exists, verifies that it is of the correct and expected class.
data ExchangeDeclare = ExchangeDeclare
  { exchangeDeclareReserved1 :: !ShortUInt,
    exchangeDeclareExchange :: !ExchangeName,
    exchangeDeclareType :: !ShortString,
    exchangeDeclarePassive :: !Bit,
    exchangeDeclareDurable :: !Bit,
    exchangeDeclareReserved2 :: !Bit,
    exchangeDeclareReserved3 :: !Bit,
    exchangeDeclareNoWait :: !NoWait,
    exchangeDeclareArguments :: !FieldTable
  }
  deriving (Show, Eq, Generic)

instance Validity ExchangeDeclare

instance IsMethod ExchangeDeclare where
  methodClassId (Proxy) = 40
  methodMethodId (Proxy) = 10
  methodSynchronous (Proxy) = True

instance FromMethod ExchangeDeclare where
  fromMethod = \case
    MethodExchangeDeclare m -> Just m
    _ -> Nothing

-- | The @declare-ok@ method: confirm exchange declaration
--
-- This method confirms a Declare method and confirms the name of the exchange,
-- essential for automatically-named exchanges.
data ExchangeDeclareOk
  = ExchangeDeclareOk
  deriving (Show, Eq, Generic)

instance Validity ExchangeDeclareOk

instance IsMethod ExchangeDeclareOk where
  methodClassId (Proxy) = 40
  methodMethodId (Proxy) = 11
  methodSynchronous (Proxy) = True

instance FromMethod ExchangeDeclareOk where
  fromMethod = \case
    MethodExchangeDeclareOk m -> Just m
    _ -> Nothing

-- | The @delete@ method: delete an exchange
--
-- This method deletes an exchange. When an exchange is deleted all queue bindings on
-- the exchange are cancelled.
data ExchangeDelete = ExchangeDelete
  { exchangeDeleteReserved1 :: !ShortUInt,
    exchangeDeleteExchange :: !ExchangeName,
    exchangeDeleteIfUnused :: !Bit,
    exchangeDeleteNoWait :: !NoWait
  }
  deriving (Show, Eq, Generic)

instance Validity ExchangeDelete

instance IsMethod ExchangeDelete where
  methodClassId (Proxy) = 40
  methodMethodId (Proxy) = 20
  methodSynchronous (Proxy) = True

instance FromMethod ExchangeDelete where
  fromMethod = \case
    MethodExchangeDelete m -> Just m
    _ -> Nothing

-- | The @delete-ok@ method: confirm deletion of an exchange
--
-- This method confirms the deletion of an exchange.
data ExchangeDeleteOk
  = ExchangeDeleteOk
  deriving (Show, Eq, Generic)

instance Validity ExchangeDeleteOk

instance IsMethod ExchangeDeleteOk where
  methodClassId (Proxy) = 40
  methodMethodId (Proxy) = 21
  methodSynchronous (Proxy) = True

instance FromMethod ExchangeDeleteOk where
  fromMethod = \case
    MethodExchangeDeleteOk m -> Just m
    _ -> Nothing

-- * The @queue@ class

-- Queues store and forward messages. Queues can be configured in the server or created at
-- runtime. Queues must be attached to at least one exchange in order to receive messages
-- from publishers.
--
-- Grammar:
--
-- >
-- >       queue               = C:DECLARE  S:DECLARE-OK
-- >                           / C:BIND     S:BIND-OK
-- >                           / C:UNBIND   S:UNBIND-OK
-- >                           / C:PURGE    S:PURGE-OK
-- >                           / C:DELETE   S:DELETE-OK
-- >

-- | The @declare@ method: declare queue, create if needed
--
-- This method creates or checks a queue. When creating a new queue the client can
-- specify various properties that control the durability of the queue and its
-- contents, and the level of sharing for the queue.
data QueueDeclare = QueueDeclare
  { queueDeclareReserved1 :: !ShortUInt,
    queueDeclareQueue :: !QueueName,
    queueDeclarePassive :: !Bit,
    queueDeclareDurable :: !Bit,
    queueDeclareExclusive :: !Bit,
    queueDeclareAutoDelete :: !Bit,
    queueDeclareNoWait :: !NoWait,
    queueDeclareArguments :: !FieldTable
  }
  deriving (Show, Eq, Generic)

instance Validity QueueDeclare

instance IsMethod QueueDeclare where
  methodClassId (Proxy) = 50
  methodMethodId (Proxy) = 10
  methodSynchronous (Proxy) = True

instance FromMethod QueueDeclare where
  fromMethod = \case
    MethodQueueDeclare m -> Just m
    _ -> Nothing

-- | The @declare-ok@ method: confirms a queue definition
--
-- This method confirms a Declare method and confirms the name of the queue, essential
-- for automatically-named queues.
data QueueDeclareOk = QueueDeclareOk
  { queueDeclareOkQueue :: !QueueName,
    queueDeclareOkMessageCount :: !MessageCount,
    queueDeclareOkConsumerCount :: !LongUInt
  }
  deriving (Show, Eq, Generic)

instance Validity QueueDeclareOk

instance IsMethod QueueDeclareOk where
  methodClassId (Proxy) = 50
  methodMethodId (Proxy) = 11
  methodSynchronous (Proxy) = True

instance FromMethod QueueDeclareOk where
  fromMethod = \case
    MethodQueueDeclareOk m -> Just m
    _ -> Nothing

-- | The @bind@ method: bind queue to an exchange
--
-- This method binds a queue to an exchange. Until a queue is bound it will not
-- receive any messages. In a classic messaging model, store-and-forward queues
-- are bound to a direct exchange and subscription queues are bound to a topic
-- exchange.
data QueueBind = QueueBind
  { queueBindReserved1 :: !ShortUInt,
    queueBindQueue :: !QueueName,
    queueBindExchange :: !ExchangeName,
    queueBindRoutingKey :: !ShortString,
    queueBindNoWait :: !NoWait,
    queueBindArguments :: !FieldTable
  }
  deriving (Show, Eq, Generic)

instance Validity QueueBind

instance IsMethod QueueBind where
  methodClassId (Proxy) = 50
  methodMethodId (Proxy) = 20
  methodSynchronous (Proxy) = True

instance FromMethod QueueBind where
  fromMethod = \case
    MethodQueueBind m -> Just m
    _ -> Nothing

-- | The @bind-ok@ method: confirm bind successful
--
-- This method confirms that the bind was successful.
data QueueBindOk = QueueBindOk deriving (Show, Eq, Generic)

instance Validity QueueBindOk

instance IsMethod QueueBindOk where
  methodClassId (Proxy) = 50
  methodMethodId (Proxy) = 21
  methodSynchronous (Proxy) = True

instance FromMethod QueueBindOk where
  fromMethod = \case
    MethodQueueBindOk m -> Just m
    _ -> Nothing

-- | The @unbind@ method: unbind a queue from an exchange
--
-- This method unbinds a queue from an exchange.
data QueueUnbind = QueueUnbind
  { queueUnbindReserved1 :: !ShortUInt,
    queueUnbindQueue :: !QueueName,
    queueUnbindExchange :: !ExchangeName,
    queueUnbindRoutingKey :: !ShortString,
    queueUnbindArguments :: !FieldTable
  }
  deriving (Show, Eq, Generic)

instance Validity QueueUnbind

instance IsMethod QueueUnbind where
  methodClassId (Proxy) = 50
  methodMethodId (Proxy) = 50
  methodSynchronous (Proxy) = True

instance FromMethod QueueUnbind where
  fromMethod = \case
    MethodQueueUnbind m -> Just m
    _ -> Nothing

-- | The @unbind-ok@ method: confirm unbind successful
--
-- This method confirms that the unbind was successful.
data QueueUnbindOk = QueueUnbindOk deriving (Show, Eq, Generic)

instance Validity QueueUnbindOk

instance IsMethod QueueUnbindOk where
  methodClassId (Proxy) = 50
  methodMethodId (Proxy) = 51
  methodSynchronous (Proxy) = True

instance FromMethod QueueUnbindOk where
  fromMethod = \case
    MethodQueueUnbindOk m -> Just m
    _ -> Nothing

-- | The @purge@ method: purge a queue
--
-- This method removes all messages from a queue which are not awaiting
-- acknowledgment.
data QueuePurge = QueuePurge
  { queuePurgeReserved1 :: !ShortUInt,
    queuePurgeQueue :: !QueueName,
    queuePurgeNoWait :: !NoWait
  }
  deriving (Show, Eq, Generic)

instance Validity QueuePurge

instance IsMethod QueuePurge where
  methodClassId (Proxy) = 50
  methodMethodId (Proxy) = 30
  methodSynchronous (Proxy) = True

instance FromMethod QueuePurge where
  fromMethod = \case
    MethodQueuePurge m -> Just m
    _ -> Nothing

-- | The @purge-ok@ method: confirms a queue purge
--
-- This method confirms the purge of a queue.
data QueuePurgeOk = QueuePurgeOk {queuePurgeOkMessageCount :: !MessageCount}
  deriving (Show, Eq, Generic)

instance Validity QueuePurgeOk

instance IsMethod QueuePurgeOk where
  methodClassId (Proxy) = 50
  methodMethodId (Proxy) = 31
  methodSynchronous (Proxy) = True

instance FromMethod QueuePurgeOk where
  fromMethod = \case
    MethodQueuePurgeOk m -> Just m
    _ -> Nothing

-- | The @delete@ method: delete a queue
--
-- This method deletes a queue. When a queue is deleted any pending messages are sent
-- to a dead-letter queue if this is defined in the server configuration, and all
-- consumers on the queue are cancelled.
data QueueDelete = QueueDelete
  { queueDeleteReserved1 :: !ShortUInt,
    queueDeleteQueue :: !QueueName,
    queueDeleteIfUnused :: !Bit,
    queueDeleteIfEmpty :: !Bit,
    queueDeleteNoWait :: !NoWait
  }
  deriving (Show, Eq, Generic)

instance Validity QueueDelete

instance IsMethod QueueDelete where
  methodClassId (Proxy) = 50
  methodMethodId (Proxy) = 40
  methodSynchronous (Proxy) = True

instance FromMethod QueueDelete where
  fromMethod = \case
    MethodQueueDelete m -> Just m
    _ -> Nothing

-- | The @delete-ok@ method: confirm deletion of a queue
--
-- This method confirms the deletion of a queue.
data QueueDeleteOk = QueueDeleteOk {queueDeleteOkMessageCount :: !MessageCount}
  deriving (Show, Eq, Generic)

instance Validity QueueDeleteOk

instance IsMethod QueueDeleteOk where
  methodClassId (Proxy) = 50
  methodMethodId (Proxy) = 41
  methodSynchronous (Proxy) = True

instance FromMethod QueueDeleteOk where
  fromMethod = \case
    MethodQueueDeleteOk m -> Just m
    _ -> Nothing

-- * The @basic@ class

-- The Basic class provides methods that support an industry-standard messaging model.
--
-- Grammar:
--
-- >
-- >       basic               = C:QOS S:QOS-OK
-- >                           / C:CONSUME S:CONSUME-OK
-- >                           / C:CANCEL S:CANCEL-OK
-- >                           / C:PUBLISH content
-- >                           / S:RETURN content
-- >                           / S:DELIVER content
-- >                           / C:GET ( S:GET-OK content / S:GET-EMPTY )
-- >                           / C:ACK
-- >                           / C:REJECT
-- >                           / C:RECOVER-ASYNC
-- >                           / C:RECOVER S:RECOVER-OK
-- >

-- | The @qos@ method: specify quality of service
--
-- This method requests a specific quality of service. The QoS can be specified for the
-- current channel or for all channels on the connection. The particular properties and
-- semantics of a qos method always depend on the content class semantics. Though the
-- qos method could in principle apply to both peers, it is currently meaningful only
-- for the server.
data BasicQos = BasicQos
  { basicQosPrefetchSize :: !LongUInt,
    basicQosPrefetchCount :: !ShortUInt,
    basicQosGlobal :: !Bit
  }
  deriving (Show, Eq, Generic)

instance Validity BasicQos

instance IsMethod BasicQos where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 10
  methodSynchronous (Proxy) = True

instance FromMethod BasicQos where
  fromMethod = \case
    MethodBasicQos m -> Just m
    _ -> Nothing

-- | The @qos-ok@ method: confirm the requested qos
--
-- This method tells the client that the requested QoS levels could be handled by the
-- server. The requested QoS applies to all active consumers until a new QoS is
-- defined.
data BasicQosOk = BasicQosOk deriving (Show, Eq, Generic)

instance Validity BasicQosOk

instance IsMethod BasicQosOk where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 11
  methodSynchronous (Proxy) = True

instance FromMethod BasicQosOk where
  fromMethod = \case
    MethodBasicQosOk m -> Just m
    _ -> Nothing

-- | The @consume@ method: start a queue consumer
--
-- This method asks the server to start a "consumer", which is a transient request for
-- messages from a specific queue. Consumers last as long as the channel they were
-- declared on, or until the client cancels them.
data BasicConsume = BasicConsume
  { basicConsumeReserved1 :: !ShortUInt,
    basicConsumeQueue :: !QueueName,
    basicConsumeConsumerTag :: !ConsumerTag,
    basicConsumeNoLocal :: !NoLocal,
    basicConsumeNoAck :: !NoAck,
    basicConsumeExclusive :: !Bit,
    basicConsumeNoWait :: !NoWait,
    basicConsumeArguments :: !FieldTable
  }
  deriving (Show, Eq, Generic)

instance Validity BasicConsume

instance IsMethod BasicConsume where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 20
  methodSynchronous (Proxy) = True

instance FromMethod BasicConsume where
  fromMethod = \case
    MethodBasicConsume m -> Just m
    _ -> Nothing

-- | The @consume-ok@ method: confirm a new consumer
--
-- The server provides the client with a consumer tag, which is used by the client
-- for methods called on the consumer at a later stage.
data BasicConsumeOk = BasicConsumeOk {basicConsumeOkConsumerTag :: !ConsumerTag}
  deriving (Show, Eq, Generic)

instance Validity BasicConsumeOk

instance IsMethod BasicConsumeOk where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 21
  methodSynchronous (Proxy) = True

instance FromMethod BasicConsumeOk where
  fromMethod = \case
    MethodBasicConsumeOk m -> Just m
    _ -> Nothing

-- | The @cancel@ method: end a queue consumer
--
-- This method cancels a consumer. This does not affect already delivered
-- messages, but it does mean the server will not send any more messages for
-- that consumer. The client may receive an arbitrary number of messages in
-- between sending the cancel method and receiving the cancel-ok reply.
data BasicCancel = BasicCancel
  { basicCancelConsumerTag :: !ConsumerTag,
    basicCancelNoWait :: !NoWait
  }
  deriving (Show, Eq, Generic)

instance Validity BasicCancel

instance IsMethod BasicCancel where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 30
  methodSynchronous (Proxy) = True

instance FromMethod BasicCancel where
  fromMethod = \case
    MethodBasicCancel m -> Just m
    _ -> Nothing

-- | The @cancel-ok@ method: confirm a cancelled consumer
--
-- This method confirms that the cancellation was completed.
data BasicCancelOk = BasicCancelOk {basicCancelOkConsumerTag :: !ConsumerTag}
  deriving (Show, Eq, Generic)

instance Validity BasicCancelOk

instance IsMethod BasicCancelOk where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 31
  methodSynchronous (Proxy) = True

instance FromMethod BasicCancelOk where
  fromMethod = \case
    MethodBasicCancelOk m -> Just m
    _ -> Nothing

-- | The @publish@ method: publish a message
--
-- This method publishes a message to a specific exchange. The message will be routed
-- to queues as defined by the exchange configuration and distributed to any active
-- consumers when the transaction, if any, is committed.
data BasicPublish = BasicPublish
  { basicPublishReserved1 :: !ShortUInt,
    basicPublishExchange :: !ExchangeName,
    basicPublishRoutingKey :: !ShortString,
    basicPublishMandatory :: !Bit,
    basicPublishImmediate :: !Bit
  }
  deriving (Show, Eq, Generic)

instance Validity BasicPublish

instance IsMethod BasicPublish where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 40
  methodSynchronous (Proxy) = False

instance FromMethod BasicPublish where
  fromMethod = \case
    MethodBasicPublish m -> Just m
    _ -> Nothing

-- | The @return@ method: return a failed message
--
-- This method returns an undeliverable message that was published with the "immediate"
-- flag set, or an unroutable message published with the "mandatory" flag set. The
-- reply code and text provide information about the reason that the message was
-- undeliverable.
data BasicReturn = BasicReturn
  { basicReturnReplyCode :: !ReplyCode,
    basicReturnReplyText :: !ReplyText,
    basicReturnExchange :: !ExchangeName,
    basicReturnRoutingKey :: !ShortString
  }
  deriving (Show, Eq, Generic)

instance Validity BasicReturn

instance IsMethod BasicReturn where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 50
  methodSynchronous (Proxy) = False

instance FromMethod BasicReturn where
  fromMethod = \case
    MethodBasicReturn m -> Just m
    _ -> Nothing

-- | The @deliver@ method: notify the client of a consumer message
--
-- This method delivers a message to the client, via a consumer. In the asynchronous
-- message delivery model, the client starts a consumer using the Consume method, then
-- the server responds with Deliver methods as and when messages arrive for that
-- consumer.
data BasicDeliver = BasicDeliver
  { basicDeliverConsumerTag :: !ConsumerTag,
    basicDeliverDeliveryTag :: !DeliveryTag,
    basicDeliverRedelivered :: !Redelivered,
    basicDeliverExchange :: !ExchangeName,
    basicDeliverRoutingKey :: !ShortString
  }
  deriving (Show, Eq, Generic)

instance Validity BasicDeliver

instance IsMethod BasicDeliver where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 60
  methodSynchronous (Proxy) = False

instance FromMethod BasicDeliver where
  fromMethod = \case
    MethodBasicDeliver m -> Just m
    _ -> Nothing

-- | The @get@ method: direct access to a queue
--
-- This method provides a direct access to the messages in a queue using a synchronous
-- dialogue that is designed for specific types of application where synchronous
-- functionality is more important than performance.
data BasicGet = BasicGet
  { basicGetReserved1 :: !ShortUInt,
    basicGetQueue :: !QueueName,
    basicGetNoAck :: !NoAck
  }
  deriving (Show, Eq, Generic)

instance Validity BasicGet

instance IsMethod BasicGet where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 70
  methodSynchronous (Proxy) = True

instance FromMethod BasicGet where
  fromMethod = \case
    MethodBasicGet m -> Just m
    _ -> Nothing

-- | The @get-ok@ method: provide client with a message
--
-- This method delivers a message to the client following a get method. A message
-- delivered by 'get-ok' must be acknowledged unless the no-ack option was set in the
-- get method.
data BasicGetOk = BasicGetOk
  { basicGetOkDeliveryTag :: !DeliveryTag,
    basicGetOkRedelivered :: !Redelivered,
    basicGetOkExchange :: !ExchangeName,
    basicGetOkRoutingKey :: !ShortString,
    basicGetOkMessageCount :: !MessageCount
  }
  deriving (Show, Eq, Generic)

instance Validity BasicGetOk

instance IsMethod BasicGetOk where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 71
  methodSynchronous (Proxy) = True

instance FromMethod BasicGetOk where
  fromMethod = \case
    MethodBasicGetOk m -> Just m
    _ -> Nothing

-- | The @get-empty@ method: indicate no messages available
--
-- This method tells the client that the queue has no messages available for the
-- client.
data BasicGetEmpty = BasicGetEmpty {basicGetEmptyReserved1 :: !ShortString}
  deriving (Show, Eq, Generic)

instance Validity BasicGetEmpty

instance IsMethod BasicGetEmpty where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 72
  methodSynchronous (Proxy) = True

instance FromMethod BasicGetEmpty where
  fromMethod = \case
    MethodBasicGetEmpty m -> Just m
    _ -> Nothing

-- | The @ack@ method: acknowledge one or more messages
--
-- This method acknowledges one or more messages delivered via the Deliver or Get-Ok
-- methods. The client can ask to confirm a single message or a set of messages up to
-- and including a specific message.
data BasicAck = BasicAck
  { basicAckDeliveryTag :: !DeliveryTag,
    basicAckMultiple :: !Bit
  }
  deriving (Show, Eq, Generic)

instance Validity BasicAck

instance IsMethod BasicAck where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 80
  methodSynchronous (Proxy) = False

instance FromMethod BasicAck where
  fromMethod = \case
    MethodBasicAck m -> Just m
    _ -> Nothing

-- | The @reject@ method: reject an incoming message
--
-- This method allows a client to reject a message. It can be used to interrupt and
-- cancel large incoming messages, or return untreatable messages to their original
-- queue.
data BasicReject = BasicReject
  { basicRejectDeliveryTag :: !DeliveryTag,
    basicRejectRequeue :: !Bit
  }
  deriving (Show, Eq, Generic)

instance Validity BasicReject

instance IsMethod BasicReject where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 90
  methodSynchronous (Proxy) = False

instance FromMethod BasicReject where
  fromMethod = \case
    MethodBasicReject m -> Just m
    _ -> Nothing

-- | The @recover-async@ method: redeliver unacknowledged messages
--
-- This method asks the server to redeliver all unacknowledged messages on a
-- specified channel. Zero or more messages may be redelivered.  This method
-- is deprecated in favour of the synchronous Recover/Recover-Ok.
data BasicRecoverAsync = BasicRecoverAsync {basicRecoverAsyncRequeue :: !Bit}
  deriving (Show, Eq, Generic)

instance Validity BasicRecoverAsync

instance IsMethod BasicRecoverAsync where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 100
  methodSynchronous (Proxy) = False

instance FromMethod BasicRecoverAsync where
  fromMethod = \case
    MethodBasicRecoverAsync m -> Just m
    _ -> Nothing

-- | The @recover@ method: redeliver unacknowledged messages
--
-- This method asks the server to redeliver all unacknowledged messages on a
-- specified channel. Zero or more messages may be redelivered.  This method
-- replaces the asynchronous Recover.
data BasicRecover = BasicRecover {basicRecoverRequeue :: !Bit}
  deriving (Show, Eq, Generic)

instance Validity BasicRecover

instance IsMethod BasicRecover where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 110
  methodSynchronous (Proxy) = False

instance FromMethod BasicRecover where
  fromMethod = \case
    MethodBasicRecover m -> Just m
    _ -> Nothing

-- | The @recover-ok@ method: confirm recovery
--
-- This method acknowledges a Basic.Recover method.
data BasicRecoverOk = BasicRecoverOk deriving (Show, Eq, Generic)

instance Validity BasicRecoverOk

instance IsMethod BasicRecoverOk where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 111
  methodSynchronous (Proxy) = True

instance FromMethod BasicRecoverOk where
  fromMethod = \case
    MethodBasicRecoverOk m -> Just m
    _ -> Nothing

-- * The @tx@ class

-- The Tx class allows publish and ack operations to be batched into atomic
-- units of work.  The intention is that all publish and ack requests issued
-- within a transaction will complete successfully or none of them will.
-- Servers SHOULD implement atomic transactions at least where all publish
-- or ack requests affect a single queue.  Transactions that cover multiple
-- queues may be non-atomic, given that queues can be created and destroyed
-- asynchronously, and such events do not form part of any transaction.
-- Further, the behaviour of transactions with respect to the immediate and
-- mandatory flags on Basic.Publish methods is not defined.
--
-- Grammar:
--
-- >
-- >       tx                  = C:SELECT S:SELECT-OK
-- >                           / C:COMMIT S:COMMIT-OK
-- >                           / C:ROLLBACK S:ROLLBACK-OK
-- >

-- | The @select@ method: select standard transaction mode
--
-- This method sets the channel to use standard transactions. The client must use this
-- method at least once on a channel before using the Commit or Rollback methods.
data TxSelect = TxSelect deriving (Show, Eq, Generic)

instance Validity TxSelect

instance IsMethod TxSelect where
  methodClassId (Proxy) = 90
  methodMethodId (Proxy) = 10
  methodSynchronous (Proxy) = True

instance FromMethod TxSelect where
  fromMethod = \case
    MethodTxSelect m -> Just m
    _ -> Nothing

-- | The @select-ok@ method: confirm transaction mode
--
-- This method confirms to the client that the channel was successfully set to use
-- standard transactions.
data TxSelectOk = TxSelectOk deriving (Show, Eq, Generic)

instance Validity TxSelectOk

instance IsMethod TxSelectOk where
  methodClassId (Proxy) = 90
  methodMethodId (Proxy) = 11
  methodSynchronous (Proxy) = True

instance FromMethod TxSelectOk where
  fromMethod = \case
    MethodTxSelectOk m -> Just m
    _ -> Nothing

-- | The @commit@ method: commit the current transaction
--
-- This method commits all message publications and acknowledgments performed in
-- the current transaction.  A new transaction starts immediately after a commit.
data TxCommit = TxCommit deriving (Show, Eq, Generic)

instance Validity TxCommit

instance IsMethod TxCommit where
  methodClassId (Proxy) = 90
  methodMethodId (Proxy) = 20
  methodSynchronous (Proxy) = True

instance FromMethod TxCommit where
  fromMethod = \case
    MethodTxCommit m -> Just m
    _ -> Nothing

-- | The @commit-ok@ method: confirm a successful commit
--
-- This method confirms to the client that the commit succeeded. Note that if a commit
-- fails, the server raises a channel exception.
data TxCommitOk = TxCommitOk deriving (Show, Eq, Generic)

instance Validity TxCommitOk

instance IsMethod TxCommitOk where
  methodClassId (Proxy) = 90
  methodMethodId (Proxy) = 21
  methodSynchronous (Proxy) = True

instance FromMethod TxCommitOk where
  fromMethod = \case
    MethodTxCommitOk m -> Just m
    _ -> Nothing

-- | The @rollback@ method: abandon the current transaction
--
-- This method abandons all message publications and acknowledgments performed in
-- the current transaction. A new transaction starts immediately after a rollback.
-- Note that unacked messages will not be automatically redelivered by rollback;
-- if that is required an explicit recover call should be issued.
data TxRollback = TxRollback deriving (Show, Eq, Generic)

instance Validity TxRollback

instance IsMethod TxRollback where
  methodClassId (Proxy) = 90
  methodMethodId (Proxy) = 30
  methodSynchronous (Proxy) = True

instance FromMethod TxRollback where
  fromMethod = \case
    MethodTxRollback m -> Just m
    _ -> Nothing

-- | The @rollback-ok@ method: confirm successful rollback
--
-- This method confirms to the client that the rollback succeeded. Note that if an
-- rollback fails, the server raises a channel exception.
data TxRollbackOk = TxRollbackOk deriving (Show, Eq, Generic)

instance Validity TxRollbackOk

instance IsMethod TxRollbackOk where
  methodClassId (Proxy) = 90
  methodMethodId (Proxy) = 31
  methodSynchronous (Proxy) = True

instance FromMethod TxRollbackOk where
  fromMethod = \case
    MethodTxRollbackOk m -> Just m
    _ -> Nothing

-- | A sum type of all the methods
data Method
  = MethodConnectionStart !ConnectionStart
  | MethodConnectionStartOk !ConnectionStartOk
  | MethodConnectionSecure !ConnectionSecure
  | MethodConnectionSecureOk !ConnectionSecureOk
  | MethodConnectionTune !ConnectionTune
  | MethodConnectionTuneOk !ConnectionTuneOk
  | MethodConnectionOpen !ConnectionOpen
  | MethodConnectionOpenOk !ConnectionOpenOk
  | MethodConnectionClose !ConnectionClose
  | MethodConnectionCloseOk !ConnectionCloseOk
  | MethodChannelOpen !ChannelOpen
  | MethodChannelOpenOk !ChannelOpenOk
  | MethodChannelFlow !ChannelFlow
  | MethodChannelFlowOk !ChannelFlowOk
  | MethodChannelClose !ChannelClose
  | MethodChannelCloseOk !ChannelCloseOk
  | MethodExchangeDeclare !ExchangeDeclare
  | MethodExchangeDeclareOk !ExchangeDeclareOk
  | MethodExchangeDelete !ExchangeDelete
  | MethodExchangeDeleteOk !ExchangeDeleteOk
  | MethodQueueDeclare !QueueDeclare
  | MethodQueueDeclareOk !QueueDeclareOk
  | MethodQueueBind !QueueBind
  | MethodQueueBindOk !QueueBindOk
  | MethodQueueUnbind !QueueUnbind
  | MethodQueueUnbindOk !QueueUnbindOk
  | MethodQueuePurge !QueuePurge
  | MethodQueuePurgeOk !QueuePurgeOk
  | MethodQueueDelete !QueueDelete
  | MethodQueueDeleteOk !QueueDeleteOk
  | MethodBasicQos !BasicQos
  | MethodBasicQosOk !BasicQosOk
  | MethodBasicConsume !BasicConsume
  | MethodBasicConsumeOk !BasicConsumeOk
  | MethodBasicCancel !BasicCancel
  | MethodBasicCancelOk !BasicCancelOk
  | MethodBasicPublish !BasicPublish
  | MethodBasicReturn !BasicReturn
  | MethodBasicDeliver !BasicDeliver
  | MethodBasicGet !BasicGet
  | MethodBasicGetOk !BasicGetOk
  | MethodBasicGetEmpty !BasicGetEmpty
  | MethodBasicAck !BasicAck
  | MethodBasicReject !BasicReject
  | MethodBasicRecoverAsync !BasicRecoverAsync
  | MethodBasicRecover !BasicRecover
  | MethodBasicRecoverOk !BasicRecoverOk
  | MethodTxSelect !TxSelect
  | MethodTxSelectOk !TxSelectOk
  | MethodTxCommit !TxCommit
  | MethodTxCommitOk !TxCommitOk
  | MethodTxRollback !TxRollback
  | MethodTxRollbackOk !TxRollbackOk
  deriving (Show, Eq, Generic)

instance Validity Method

-- | A type class of things that could be in a method frame
class FromMethod a where
  fromMethod :: Method -> Maybe a

-- | Turn a 'Method' into a 'ByteString.Builder'.
buildMethodFramePayload :: Method -> ByteString.Builder
buildMethodFramePayload = \case
  MethodConnectionStart m -> buildGivenMethodFramePayload m
  MethodConnectionStartOk m -> buildGivenMethodFramePayload m
  MethodConnectionSecure m -> buildGivenMethodFramePayload m
  MethodConnectionSecureOk m -> buildGivenMethodFramePayload m
  MethodConnectionTune m -> buildGivenMethodFramePayload m
  MethodConnectionTuneOk m -> buildGivenMethodFramePayload m
  MethodConnectionOpen m -> buildGivenMethodFramePayload m
  MethodConnectionOpenOk m -> buildGivenMethodFramePayload m
  MethodConnectionClose m -> buildGivenMethodFramePayload m
  MethodConnectionCloseOk m -> buildGivenMethodFramePayload m
  MethodChannelOpen m -> buildGivenMethodFramePayload m
  MethodChannelOpenOk m -> buildGivenMethodFramePayload m
  MethodChannelFlow m -> buildGivenMethodFramePayload m
  MethodChannelFlowOk m -> buildGivenMethodFramePayload m
  MethodChannelClose m -> buildGivenMethodFramePayload m
  MethodChannelCloseOk m -> buildGivenMethodFramePayload m
  MethodExchangeDeclare m -> buildGivenMethodFramePayload m
  MethodExchangeDeclareOk m -> buildGivenMethodFramePayload m
  MethodExchangeDelete m -> buildGivenMethodFramePayload m
  MethodExchangeDeleteOk m -> buildGivenMethodFramePayload m
  MethodQueueDeclare m -> buildGivenMethodFramePayload m
  MethodQueueDeclareOk m -> buildGivenMethodFramePayload m
  MethodQueueBind m -> buildGivenMethodFramePayload m
  MethodQueueBindOk m -> buildGivenMethodFramePayload m
  MethodQueueUnbind m -> buildGivenMethodFramePayload m
  MethodQueueUnbindOk m -> buildGivenMethodFramePayload m
  MethodQueuePurge m -> buildGivenMethodFramePayload m
  MethodQueuePurgeOk m -> buildGivenMethodFramePayload m
  MethodQueueDelete m -> buildGivenMethodFramePayload m
  MethodQueueDeleteOk m -> buildGivenMethodFramePayload m
  MethodBasicQos m -> buildGivenMethodFramePayload m
  MethodBasicQosOk m -> buildGivenMethodFramePayload m
  MethodBasicConsume m -> buildGivenMethodFramePayload m
  MethodBasicConsumeOk m -> buildGivenMethodFramePayload m
  MethodBasicCancel m -> buildGivenMethodFramePayload m
  MethodBasicCancelOk m -> buildGivenMethodFramePayload m
  MethodBasicPublish m -> buildGivenMethodFramePayload m
  MethodBasicReturn m -> buildGivenMethodFramePayload m
  MethodBasicDeliver m -> buildGivenMethodFramePayload m
  MethodBasicGet m -> buildGivenMethodFramePayload m
  MethodBasicGetOk m -> buildGivenMethodFramePayload m
  MethodBasicGetEmpty m -> buildGivenMethodFramePayload m
  MethodBasicAck m -> buildGivenMethodFramePayload m
  MethodBasicReject m -> buildGivenMethodFramePayload m
  MethodBasicRecoverAsync m -> buildGivenMethodFramePayload m
  MethodBasicRecover m -> buildGivenMethodFramePayload m
  MethodBasicRecoverOk m -> buildGivenMethodFramePayload m
  MethodTxSelect m -> buildGivenMethodFramePayload m
  MethodTxSelectOk m -> buildGivenMethodFramePayload m
  MethodTxCommit m -> buildGivenMethodFramePayload m
  MethodTxCommitOk m -> buildGivenMethodFramePayload m
  MethodTxRollback m -> buildGivenMethodFramePayload m
  MethodTxRollbackOk m -> buildGivenMethodFramePayload m

-- | Parse a 'Method' frame payload.
parseMethodFramePayload :: Parser Method
parseMethodFramePayload =
  parseMethodFramePayloadHelper
    ( \cid mid -> case cid of
        10 -> case mid of
          10 -> MethodConnectionStart <$> methodArgumentsParser
          11 -> MethodConnectionStartOk <$> methodArgumentsParser
          20 -> MethodConnectionSecure <$> methodArgumentsParser
          21 -> MethodConnectionSecureOk <$> methodArgumentsParser
          30 -> MethodConnectionTune <$> methodArgumentsParser
          31 -> MethodConnectionTuneOk <$> methodArgumentsParser
          40 -> MethodConnectionOpen <$> methodArgumentsParser
          41 -> MethodConnectionOpenOk <$> methodArgumentsParser
          50 -> MethodConnectionClose <$> methodArgumentsParser
          51 -> MethodConnectionCloseOk <$> methodArgumentsParser
          _ -> ParseFail ("Unknown method id for class connection (10)" ++ show mid)
        20 -> case mid of
          10 -> MethodChannelOpen <$> methodArgumentsParser
          11 -> MethodChannelOpenOk <$> methodArgumentsParser
          20 -> MethodChannelFlow <$> methodArgumentsParser
          21 -> MethodChannelFlowOk <$> methodArgumentsParser
          40 -> MethodChannelClose <$> methodArgumentsParser
          41 -> MethodChannelCloseOk <$> methodArgumentsParser
          _ -> ParseFail ("Unknown method id for class channel (20)" ++ show mid)
        40 -> case mid of
          10 -> MethodExchangeDeclare <$> methodArgumentsParser
          11 -> MethodExchangeDeclareOk <$> methodArgumentsParser
          20 -> MethodExchangeDelete <$> methodArgumentsParser
          21 -> MethodExchangeDeleteOk <$> methodArgumentsParser
          _ -> ParseFail ("Unknown method id for class exchange (40)" ++ show mid)
        50 -> case mid of
          10 -> MethodQueueDeclare <$> methodArgumentsParser
          11 -> MethodQueueDeclareOk <$> methodArgumentsParser
          20 -> MethodQueueBind <$> methodArgumentsParser
          21 -> MethodQueueBindOk <$> methodArgumentsParser
          50 -> MethodQueueUnbind <$> methodArgumentsParser
          51 -> MethodQueueUnbindOk <$> methodArgumentsParser
          30 -> MethodQueuePurge <$> methodArgumentsParser
          31 -> MethodQueuePurgeOk <$> methodArgumentsParser
          40 -> MethodQueueDelete <$> methodArgumentsParser
          41 -> MethodQueueDeleteOk <$> methodArgumentsParser
          _ -> ParseFail ("Unknown method id for class queue (50)" ++ show mid)
        60 -> case mid of
          10 -> MethodBasicQos <$> methodArgumentsParser
          11 -> MethodBasicQosOk <$> methodArgumentsParser
          20 -> MethodBasicConsume <$> methodArgumentsParser
          21 -> MethodBasicConsumeOk <$> methodArgumentsParser
          30 -> MethodBasicCancel <$> methodArgumentsParser
          31 -> MethodBasicCancelOk <$> methodArgumentsParser
          40 -> MethodBasicPublish <$> methodArgumentsParser
          50 -> MethodBasicReturn <$> methodArgumentsParser
          60 -> MethodBasicDeliver <$> methodArgumentsParser
          70 -> MethodBasicGet <$> methodArgumentsParser
          71 -> MethodBasicGetOk <$> methodArgumentsParser
          72 -> MethodBasicGetEmpty <$> methodArgumentsParser
          80 -> MethodBasicAck <$> methodArgumentsParser
          90 -> MethodBasicReject <$> methodArgumentsParser
          100 -> MethodBasicRecoverAsync <$> methodArgumentsParser
          110 -> MethodBasicRecover <$> methodArgumentsParser
          111 -> MethodBasicRecoverOk <$> methodArgumentsParser
          _ -> ParseFail ("Unknown method id for class basic (60)" ++ show mid)
        90 -> case mid of
          10 -> MethodTxSelect <$> methodArgumentsParser
          11 -> MethodTxSelectOk <$> methodArgumentsParser
          20 -> MethodTxCommit <$> methodArgumentsParser
          21 -> MethodTxCommitOk <$> methodArgumentsParser
          30 -> MethodTxRollback <$> methodArgumentsParser
          31 -> MethodTxRollbackOk <$> methodArgumentsParser
          _ -> ParseFail ("Unknown method id for class tx (90)" ++ show mid)
        _ -> ParseFail ("Unknown class id" ++ show cid)
    )

-- | Check if a 'Method' is synchronous.
methodIsSynchronous :: Method -> Bool
methodIsSynchronous = \case
  MethodConnectionStart _ -> methodSynchronous (Proxy :: Proxy ConnectionStart)
  MethodConnectionStartOk _ -> methodSynchronous (Proxy :: Proxy ConnectionStartOk)
  MethodConnectionSecure _ -> methodSynchronous (Proxy :: Proxy ConnectionSecure)
  MethodConnectionSecureOk _ -> methodSynchronous (Proxy :: Proxy ConnectionSecureOk)
  MethodConnectionTune _ -> methodSynchronous (Proxy :: Proxy ConnectionTune)
  MethodConnectionTuneOk _ -> methodSynchronous (Proxy :: Proxy ConnectionTuneOk)
  MethodConnectionOpen _ -> methodSynchronous (Proxy :: Proxy ConnectionOpen)
  MethodConnectionOpenOk _ -> methodSynchronous (Proxy :: Proxy ConnectionOpenOk)
  MethodConnectionClose _ -> methodSynchronous (Proxy :: Proxy ConnectionClose)
  MethodConnectionCloseOk _ -> methodSynchronous (Proxy :: Proxy ConnectionCloseOk)
  MethodChannelOpen _ -> methodSynchronous (Proxy :: Proxy ChannelOpen)
  MethodChannelOpenOk _ -> methodSynchronous (Proxy :: Proxy ChannelOpenOk)
  MethodChannelFlow _ -> methodSynchronous (Proxy :: Proxy ChannelFlow)
  MethodChannelFlowOk _ -> methodSynchronous (Proxy :: Proxy ChannelFlowOk)
  MethodChannelClose _ -> methodSynchronous (Proxy :: Proxy ChannelClose)
  MethodChannelCloseOk _ -> methodSynchronous (Proxy :: Proxy ChannelCloseOk)
  MethodExchangeDeclare _ -> methodSynchronous (Proxy :: Proxy ExchangeDeclare)
  MethodExchangeDeclareOk _ -> methodSynchronous (Proxy :: Proxy ExchangeDeclareOk)
  MethodExchangeDelete _ -> methodSynchronous (Proxy :: Proxy ExchangeDelete)
  MethodExchangeDeleteOk _ -> methodSynchronous (Proxy :: Proxy ExchangeDeleteOk)
  MethodQueueDeclare _ -> methodSynchronous (Proxy :: Proxy QueueDeclare)
  MethodQueueDeclareOk _ -> methodSynchronous (Proxy :: Proxy QueueDeclareOk)
  MethodQueueBind _ -> methodSynchronous (Proxy :: Proxy QueueBind)
  MethodQueueBindOk _ -> methodSynchronous (Proxy :: Proxy QueueBindOk)
  MethodQueueUnbind _ -> methodSynchronous (Proxy :: Proxy QueueUnbind)
  MethodQueueUnbindOk _ -> methodSynchronous (Proxy :: Proxy QueueUnbindOk)
  MethodQueuePurge _ -> methodSynchronous (Proxy :: Proxy QueuePurge)
  MethodQueuePurgeOk _ -> methodSynchronous (Proxy :: Proxy QueuePurgeOk)
  MethodQueueDelete _ -> methodSynchronous (Proxy :: Proxy QueueDelete)
  MethodQueueDeleteOk _ -> methodSynchronous (Proxy :: Proxy QueueDeleteOk)
  MethodBasicQos _ -> methodSynchronous (Proxy :: Proxy BasicQos)
  MethodBasicQosOk _ -> methodSynchronous (Proxy :: Proxy BasicQosOk)
  MethodBasicConsume _ -> methodSynchronous (Proxy :: Proxy BasicConsume)
  MethodBasicConsumeOk _ -> methodSynchronous (Proxy :: Proxy BasicConsumeOk)
  MethodBasicCancel _ -> methodSynchronous (Proxy :: Proxy BasicCancel)
  MethodBasicCancelOk _ -> methodSynchronous (Proxy :: Proxy BasicCancelOk)
  MethodBasicPublish _ -> methodSynchronous (Proxy :: Proxy BasicPublish)
  MethodBasicReturn _ -> methodSynchronous (Proxy :: Proxy BasicReturn)
  MethodBasicDeliver _ -> methodSynchronous (Proxy :: Proxy BasicDeliver)
  MethodBasicGet _ -> methodSynchronous (Proxy :: Proxy BasicGet)
  MethodBasicGetOk _ -> methodSynchronous (Proxy :: Proxy BasicGetOk)
  MethodBasicGetEmpty _ -> methodSynchronous (Proxy :: Proxy BasicGetEmpty)
  MethodBasicAck _ -> methodSynchronous (Proxy :: Proxy BasicAck)
  MethodBasicReject _ -> methodSynchronous (Proxy :: Proxy BasicReject)
  MethodBasicRecoverAsync _ -> methodSynchronous (Proxy :: Proxy BasicRecoverAsync)
  MethodBasicRecover _ -> methodSynchronous (Proxy :: Proxy BasicRecover)
  MethodBasicRecoverOk _ -> methodSynchronous (Proxy :: Proxy BasicRecoverOk)
  MethodTxSelect _ -> methodSynchronous (Proxy :: Proxy TxSelect)
  MethodTxSelectOk _ -> methodSynchronous (Proxy :: Proxy TxSelectOk)
  MethodTxCommit _ -> methodSynchronous (Proxy :: Proxy TxCommit)
  MethodTxCommitOk _ -> methodSynchronous (Proxy :: Proxy TxCommitOk)
  MethodTxRollback _ -> methodSynchronous (Proxy :: Proxy TxRollback)
  MethodTxRollbackOk _ -> methodSynchronous (Proxy :: Proxy TxRollbackOk)
