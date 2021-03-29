{-# LANGUAGE DeriveGeneric #-}

module AMQP.Serialisation.Generated where

import AMQP.Serialisation.Argument
import AMQP.Serialisation.Base
import Data.Proxy
import Data.Validity
import Data.Word
import GHC.Generics (Generic)

-- | frame-method
frameMethod :: Word8
frameMethod = 1

-- | frame-header
frameHeader :: Word8
frameHeader = 2

-- | frame-body
frameBody :: Word8
frameBody = 3

-- | frame-heartbeat
frameHeartbeat :: Word8
frameHeartbeat = 8

-- | frame-min-size
frameMinSize :: Word
frameMinSize = 4096

-- | frame-end
frameEnd :: Word8
frameEnd = 206

-- | reply-success
--
-- Indicates that the method completed successfully. This reply code is
-- reserved for future use - the current protocol design does not use positive
-- confirmation and reply codes are sent only in case of an error.
replySuccess :: Word8
replySuccess = 200

-- | content-too-large
--
-- The client attempted to transfer content larger than the server could accept
-- at the present time. The client may retry at a later time.
contentTooLarge :: Word
contentTooLarge = 311

-- | no-consumers
--
-- When the exchange cannot deliver to a consumer when the immediate flag is
-- set. As a result of pending data on the queue or the absence of any
-- consumers of the queue.
noConsumers :: Word
noConsumers = 313

-- | connection-forced
--
-- An operator intervened to close the connection for some reason. The client
-- may retry at some later date.
connectionForced :: Word
connectionForced = 320

-- | invalid-path
--
-- The client tried to work with an unknown virtual host.
invalidPath :: Word
invalidPath = 402

-- | access-refused
--
-- The client attempted to work with a server entity to which it has no
-- access due to security settings.
accessRefused :: Word
accessRefused = 403

-- | not-found
--
-- The client attempted to work with a server entity that does not exist.
notFound :: Word
notFound = 404

-- | resource-locked
--
-- The client attempted to work with a server entity to which it has no
-- access because another client is working with it.
resourceLocked :: Word
resourceLocked = 405

-- | precondition-failed
--
-- The client requested a method that was not allowed because some precondition
-- failed.
preconditionFailed :: Word
preconditionFailed = 406

-- | frame-error
--
-- The sender sent a malformed frame that the recipient could not decode.
-- This strongly implies a programming error in the sending peer.
frameError :: Word
frameError = 501

-- | syntax-error
--
-- The sender sent a frame that contained illegal values for one or more
-- fields. This strongly implies a programming error in the sending peer.
syntaxError :: Word
syntaxError = 502

-- | command-invalid
--
-- The client sent an invalid sequence of frames, attempting to perform an
-- operation that was considered invalid by the server. This usually implies
-- a programming error in the client.
commandInvalid :: Word
commandInvalid = 503

-- | channel-error
--
-- The client attempted to work with a channel that had not been correctly
-- opened. This most likely indicates a fault in the client layer.
channelError :: Word
channelError = 504

-- | unexpected-frame
--
-- The peer sent a frame that was not expected, usually in the context of
-- a content header and body.  This strongly indicates a fault in the peer's
-- content processing.
unexpectedFrame :: Word
unexpectedFrame = 505

-- | resource-error
--
-- The server could not complete the method because it lacked sufficient
-- resources. This may be due to the client creating too many of some type
-- of entity.
resourceError :: Word
resourceError = 506

-- | not-allowed
--
-- The client tried to work with some entity in a manner that is prohibited
-- by the server, due to security settings or by some other criteria.
notAllowed :: Word
notAllowed = 530

-- | not-implemented
--
-- The client tried to use functionality that is not implemented in the
-- server.
notImplemented :: Word
notImplemented = 540

-- | internal-error
--
-- The server could not complete the method because of an internal error.
-- The server may require intervention by an operator in order to resume
-- normal operations.
internalError :: Word
internalError = 541

-- | class-id
type ClassId = ShortUInt

-- | consumer-tag: consumer tag
--
-- Identifier for the consumer, valid within the current channel.
type ConsumerTag = ShortString

-- | delivery-tag: server-assigned delivery tag
--
-- The server-assigned and channel-specific delivery tag
type DeliveryTag = LongLongUInt

-- | exchange-name: exchange name
--
-- The exchange name is a client-selected string that identifies the exchange for
-- publish methods.
type ExchangeName = ShortString

-- | method-id
type MethodId = ShortUInt

-- | no-ack: no acknowledgement needed
--
-- If this field is set the server does not expect acknowledgements for
-- messages. That is, when a message is delivered to the client the server
-- assumes the delivery will succeed and immediately dequeues it. This
-- functionality may increase performance but at the cost of reliability.
-- Messages can get lost if a client dies before they are delivered to the
-- application.
type NoAck = Bit

-- | no-local: do not deliver own messages
--
-- If the no-local field is set the server will not send messages to the connection that
-- published them.
type NoLocal = Bit

-- | no-wait: do not send reply method
--
-- If set, the server will not respond to the method. The client should not wait
-- for a reply method. If the server could not complete the method it will raise a
-- channel or connection exception.
type NoWait = Bit

-- | path
--
-- Unconstrained.
type Path = ShortString

-- | peer-properties
--
-- This table provides a set of peer properties, used for identification, debugging,
-- and general information.
type PeerProperties = FieldTable

-- | queue-name: queue name
--
-- The queue name identifies the queue within the vhost.  In methods where the queue
-- name may be blank, and that has no specific significance, this refers to the
-- 'current' queue for the channel, meaning the last queue that the client declared
-- on the channel.  If the client did not declare a queue, and the method needs a
-- queue name, this will result in a 502 (syntax error) channel exception.
type QueueName = ShortString

-- | redelivered: message is being redelivered
--
-- This indicates that the message has been previously delivered to this or
-- another client.
type Redelivered = Bit

-- | message-count: number of messages in queue
--
-- The number of messages in the queue, which will be zero for newly-declared
-- queues. This is the number of messages present in the queue, and committed
-- if the channel on which they were published is transacted, that are not
-- waiting acknowledgement.
type MessageCount = LongUInt

-- | reply-code: reply code from server
--
-- The reply code. The AMQ reply codes are defined as constants at the start
-- of this formal specification.
type ReplyCode = ShortUInt

-- | reply-text: localised reply text
--
-- The localised reply text. This text can be logged as an aid to resolving
-- issues.
type ReplyText = ShortString

-- | The @start@ method: start connection negotiation
--
-- This method starts the connection negotiation process by telling the client the
-- protocol version that the server proposes, along with a list of security mechanisms
-- which the client can use for authentication.
data ConnectionStart = ConnectionStart
  { connectionStartVersionMajor :: {-# UNPACK #-} !Octet,
    connectionStartVersionMinor :: {-# UNPACK #-} !Octet,
    connectionStartServerProperties :: {-# UNPACK #-} !PeerProperties,
    connectionStartMechanisms :: {-# UNPACK #-} !LongString,
    connectionStartLocales :: {-# UNPACK #-} !LongString
  }
  deriving (Show, Eq, Generic)

instance Validity ConnectionStart

instance Method ConnectionStart where
  methodClassId (Proxy) = 10
  methodMethodId (Proxy) = 10

-- | The @start-ok@ method: select security mechanism and locale
--
-- This method selects a SASL security mechanism.
data ConnectionStartOk = ConnectionStartOk
  { connectionStartOkClientProperties :: {-# UNPACK #-} !PeerProperties,
    connectionStartOkMechanism :: {-# UNPACK #-} !ShortString,
    connectionStartOkResponse :: {-# UNPACK #-} !LongString,
    connectionStartOkLocale :: {-# UNPACK #-} !ShortString
  }
  deriving (Show, Eq, Generic)

instance Validity ConnectionStartOk

instance Method ConnectionStartOk where
  methodClassId (Proxy) = 10
  methodMethodId (Proxy) = 11

-- | The @secure@ method: security mechanism challenge
--
-- The SASL protocol works by exchanging challenges and responses until both peers have
-- received sufficient information to authenticate each other. This method challenges
-- the client to provide more information.
data ConnectionSecure = ConnectionSecure {connectionSecureChallenge :: {-# UNPACK #-} !LongString}
  deriving (Show, Eq, Generic)

instance Validity ConnectionSecure

instance Method ConnectionSecure where
  methodClassId (Proxy) = 10
  methodMethodId (Proxy) = 20

-- | The @secure-ok@ method: security mechanism response
--
-- This method attempts to authenticate, passing a block of SASL data for the security
-- mechanism at the server side.
data ConnectionSecureOk = ConnectionSecureOk {connectionSecureOkResponse :: {-# UNPACK #-} !LongString}
  deriving (Show, Eq, Generic)

instance Validity ConnectionSecureOk

instance Method ConnectionSecureOk where
  methodClassId (Proxy) = 10
  methodMethodId (Proxy) = 21

-- | The @tune@ method: propose connection tuning parameters
--
-- This method proposes a set of connection configuration values to the client. The
-- client can accept and/or adjust these.
data ConnectionTune = ConnectionTune
  { connectionTuneChannelMax :: {-# UNPACK #-} !ShortUInt,
    connectionTuneFrameMax :: {-# UNPACK #-} !LongUInt,
    connectionTuneHeartbeat :: {-# UNPACK #-} !ShortUInt
  }
  deriving (Show, Eq, Generic)

instance Validity ConnectionTune

instance Method ConnectionTune where
  methodClassId (Proxy) = 10
  methodMethodId (Proxy) = 30

-- | The @tune-ok@ method: negotiate connection tuning parameters
--
-- This method sends the client's connection tuning parameters to the server.
-- Certain fields are negotiated, others provide capability information.
data ConnectionTuneOk = ConnectionTuneOk
  { connectionTuneOkChannelMax :: {-# UNPACK #-} !ShortUInt,
    connectionTuneOkFrameMax :: {-# UNPACK #-} !LongUInt,
    connectionTuneOkHeartbeat :: {-# UNPACK #-} !ShortUInt
  }
  deriving (Show, Eq, Generic)

instance Validity ConnectionTuneOk

instance Method ConnectionTuneOk where
  methodClassId (Proxy) = 10
  methodMethodId (Proxy) = 31

-- | The @open@ method: open connection to virtual host
--
-- This method opens a connection to a virtual host, which is a collection of
-- resources, and acts to separate multiple application domains within a server.
-- The server may apply arbitrary limits per virtual host, such as the number
-- of each type of entity that may be used, per connection and/or in total.
data ConnectionOpen = ConnectionOpen
  { connectionOpenVirtualHost :: {-# UNPACK #-} !Path,
    connectionOpenReserved1 :: {-# UNPACK #-} !ShortString,
    connectionOpenReserved2 :: {-# UNPACK #-} !Bit
  }
  deriving (Show, Eq, Generic)

instance Validity ConnectionOpen

instance Method ConnectionOpen where
  methodClassId (Proxy) = 10
  methodMethodId (Proxy) = 40

-- | The @open-ok@ method: signal that connection is ready
--
-- This method signals to the client that the connection is ready for use.
data ConnectionOpenOk = ConnectionOpenOk {connectionOpenOkReserved1 :: {-# UNPACK #-} !ShortString}
  deriving (Show, Eq, Generic)

instance Validity ConnectionOpenOk

instance Method ConnectionOpenOk where
  methodClassId (Proxy) = 10
  methodMethodId (Proxy) = 41

-- | The @close@ method: request a connection close
--
-- This method indicates that the sender wants to close the connection. This may be
-- due to internal conditions (e.g. a forced shut-down) or due to an error handling
-- a specific method, i.e. an exception. When a close is due to an exception, the
-- sender provides the class and method id of the method which caused the exception.
data ConnectionClose = ConnectionClose
  { connectionCloseReplyCode :: {-# UNPACK #-} !ReplyCode,
    connectionCloseReplyText :: {-# UNPACK #-} !ReplyText,
    connectionCloseClassId :: {-# UNPACK #-} !ClassId,
    connectionCloseMethodId :: {-# UNPACK #-} !MethodId
  }
  deriving (Show, Eq, Generic)

instance Validity ConnectionClose

instance Method ConnectionClose where
  methodClassId (Proxy) = 10
  methodMethodId (Proxy) = 50

-- | The @close-ok@ method: confirm a connection close
--
-- This method confirms a Connection.Close method and tells the recipient that it is
-- safe to release resources for the connection and close the socket.
data ConnectionCloseOk
  = ConnectionCloseOk
  deriving (Show, Eq, Generic)

instance Validity ConnectionCloseOk

instance Method ConnectionCloseOk where
  methodClassId (Proxy) = 10
  methodMethodId (Proxy) = 51

-- | The @open@ method: open a channel for use
--
-- This method opens a channel to the server.
data ChannelOpen = ChannelOpen {channelOpenReserved1 :: {-# UNPACK #-} !ShortString}
  deriving (Show, Eq, Generic)

instance Validity ChannelOpen

instance Method ChannelOpen where
  methodClassId (Proxy) = 20
  methodMethodId (Proxy) = 10

-- | The @open-ok@ method: signal that the channel is ready
--
-- This method signals to the client that the channel is ready for use.
data ChannelOpenOk = ChannelOpenOk {channelOpenOkReserved1 :: {-# UNPACK #-} !LongString}
  deriving (Show, Eq, Generic)

instance Validity ChannelOpenOk

instance Method ChannelOpenOk where
  methodClassId (Proxy) = 20
  methodMethodId (Proxy) = 11

-- | The @flow@ method: enable/disable flow from peer
--
-- This method asks the peer to pause or restart the flow of content data sent by
-- a consumer. This is a simple flow-control mechanism that a peer can use to avoid
-- overflowing its queues or otherwise finding itself receiving more messages than
-- it can process. Note that this method is not intended for window control. It does
-- not affect contents returned by Basic.Get-Ok methods.
data ChannelFlow = ChannelFlow {channelFlowActive :: {-# UNPACK #-} !Bit}
  deriving (Show, Eq, Generic)

instance Validity ChannelFlow

instance Method ChannelFlow where
  methodClassId (Proxy) = 20
  methodMethodId (Proxy) = 20

-- | The @flow-ok@ method: confirm a flow method
--
-- Confirms to the peer that a flow command was received and processed.
data ChannelFlowOk = ChannelFlowOk {channelFlowOkActive :: {-# UNPACK #-} !Bit}
  deriving (Show, Eq, Generic)

instance Validity ChannelFlowOk

instance Method ChannelFlowOk where
  methodClassId (Proxy) = 20
  methodMethodId (Proxy) = 21

-- | The @close@ method: request a channel close
--
-- This method indicates that the sender wants to close the channel. This may be due to
-- internal conditions (e.g. a forced shut-down) or due to an error handling a specific
-- method, i.e. an exception. When a close is due to an exception, the sender provides
-- the class and method id of the method which caused the exception.
data ChannelClose = ChannelClose
  { channelCloseReplyCode :: {-# UNPACK #-} !ReplyCode,
    channelCloseReplyText :: {-# UNPACK #-} !ReplyText,
    channelCloseClassId :: {-# UNPACK #-} !ClassId,
    channelCloseMethodId :: {-# UNPACK #-} !MethodId
  }
  deriving (Show, Eq, Generic)

instance Validity ChannelClose

instance Method ChannelClose where
  methodClassId (Proxy) = 20
  methodMethodId (Proxy) = 40

-- | The @close-ok@ method: confirm a channel close
--
-- This method confirms a Channel.Close method and tells the recipient that it is safe
-- to release resources for the channel.
data ChannelCloseOk = ChannelCloseOk deriving (Show, Eq, Generic)

instance Validity ChannelCloseOk

instance Method ChannelCloseOk where
  methodClassId (Proxy) = 20
  methodMethodId (Proxy) = 41

-- | The @declare@ method: verify exchange exists, create if needed
--
-- This method creates an exchange if it does not already exist, and if the exchange
-- exists, verifies that it is of the correct and expected class.
data ExchangeDeclare = ExchangeDeclare
  { exchangeDeclareReserved1 :: {-# UNPACK #-} !ShortUInt,
    exchangeDeclareExchange :: {-# UNPACK #-} !ExchangeName,
    exchangeDeclareType :: {-# UNPACK #-} !ShortString,
    exchangeDeclarePassive :: {-# UNPACK #-} !Bit,
    exchangeDeclareDurable :: {-# UNPACK #-} !Bit,
    exchangeDeclareReserved2 :: {-# UNPACK #-} !Bit,
    exchangeDeclareReserved3 :: {-# UNPACK #-} !Bit,
    exchangeDeclareNoWait :: {-# UNPACK #-} !NoWait,
    exchangeDeclareArguments :: {-# UNPACK #-} !FieldTable
  }
  deriving (Show, Eq, Generic)

instance Validity ExchangeDeclare

instance Method ExchangeDeclare where
  methodClassId (Proxy) = 40
  methodMethodId (Proxy) = 10

-- | The @declare-ok@ method: confirm exchange declaration
--
-- This method confirms a Declare method and confirms the name of the exchange,
-- essential for automatically-named exchanges.
data ExchangeDeclareOk
  = ExchangeDeclareOk
  deriving (Show, Eq, Generic)

instance Validity ExchangeDeclareOk

instance Method ExchangeDeclareOk where
  methodClassId (Proxy) = 40
  methodMethodId (Proxy) = 11

-- | The @delete@ method: delete an exchange
--
-- This method deletes an exchange. When an exchange is deleted all queue bindings on
-- the exchange are cancelled.
data ExchangeDelete = ExchangeDelete
  { exchangeDeleteReserved1 :: {-# UNPACK #-} !ShortUInt,
    exchangeDeleteExchange :: {-# UNPACK #-} !ExchangeName,
    exchangeDeleteIfUnused :: {-# UNPACK #-} !Bit,
    exchangeDeleteNoWait :: {-# UNPACK #-} !NoWait
  }
  deriving (Show, Eq, Generic)

instance Validity ExchangeDelete

instance Method ExchangeDelete where
  methodClassId (Proxy) = 40
  methodMethodId (Proxy) = 20

-- | The @delete-ok@ method: confirm deletion of an exchange
--
-- This method confirms the deletion of an exchange.
data ExchangeDeleteOk
  = ExchangeDeleteOk
  deriving (Show, Eq, Generic)

instance Validity ExchangeDeleteOk

instance Method ExchangeDeleteOk where
  methodClassId (Proxy) = 40
  methodMethodId (Proxy) = 21

-- | The @declare@ method: declare queue, create if needed
--
-- This method creates or checks a queue. When creating a new queue the client can
-- specify various properties that control the durability of the queue and its
-- contents, and the level of sharing for the queue.
data QueueDeclare = QueueDeclare
  { queueDeclareReserved1 :: {-# UNPACK #-} !ShortUInt,
    queueDeclareQueue :: {-# UNPACK #-} !QueueName,
    queueDeclarePassive :: {-# UNPACK #-} !Bit,
    queueDeclareDurable :: {-# UNPACK #-} !Bit,
    queueDeclareExclusive :: {-# UNPACK #-} !Bit,
    queueDeclareAutoDelete :: {-# UNPACK #-} !Bit,
    queueDeclareNoWait :: {-# UNPACK #-} !NoWait,
    queueDeclareArguments :: {-# UNPACK #-} !FieldTable
  }
  deriving (Show, Eq, Generic)

instance Validity QueueDeclare

instance Method QueueDeclare where
  methodClassId (Proxy) = 50
  methodMethodId (Proxy) = 10

-- | The @declare-ok@ method: confirms a queue definition
--
-- This method confirms a Declare method and confirms the name of the queue, essential
-- for automatically-named queues.
data QueueDeclareOk = QueueDeclareOk
  { queueDeclareOkQueue :: {-# UNPACK #-} !QueueName,
    queueDeclareOkMessageCount :: {-# UNPACK #-} !MessageCount,
    queueDeclareOkConsumerCount :: {-# UNPACK #-} !LongUInt
  }
  deriving (Show, Eq, Generic)

instance Validity QueueDeclareOk

instance Method QueueDeclareOk where
  methodClassId (Proxy) = 50
  methodMethodId (Proxy) = 11

-- | The @bind@ method: bind queue to an exchange
--
-- This method binds a queue to an exchange. Until a queue is bound it will not
-- receive any messages. In a classic messaging model, store-and-forward queues
-- are bound to a direct exchange and subscription queues are bound to a topic
-- exchange.
data QueueBind = QueueBind
  { queueBindReserved1 :: {-# UNPACK #-} !ShortUInt,
    queueBindQueue :: {-# UNPACK #-} !QueueName,
    queueBindExchange :: {-# UNPACK #-} !ExchangeName,
    queueBindRoutingKey :: {-# UNPACK #-} !ShortString,
    queueBindNoWait :: {-# UNPACK #-} !NoWait,
    queueBindArguments :: {-# UNPACK #-} !FieldTable
  }
  deriving (Show, Eq, Generic)

instance Validity QueueBind

instance Method QueueBind where
  methodClassId (Proxy) = 50
  methodMethodId (Proxy) = 20

-- | The @bind-ok@ method: confirm bind successful
--
-- This method confirms that the bind was successful.
data QueueBindOk = QueueBindOk deriving (Show, Eq, Generic)

instance Validity QueueBindOk

instance Method QueueBindOk where
  methodClassId (Proxy) = 50
  methodMethodId (Proxy) = 21

-- | The @unbind@ method: unbind a queue from an exchange
--
-- This method unbinds a queue from an exchange.
data QueueUnbind = QueueUnbind
  { queueUnbindReserved1 :: {-# UNPACK #-} !ShortUInt,
    queueUnbindQueue :: {-# UNPACK #-} !QueueName,
    queueUnbindExchange :: {-# UNPACK #-} !ExchangeName,
    queueUnbindRoutingKey :: {-# UNPACK #-} !ShortString,
    queueUnbindArguments :: {-# UNPACK #-} !FieldTable
  }
  deriving (Show, Eq, Generic)

instance Validity QueueUnbind

instance Method QueueUnbind where
  methodClassId (Proxy) = 50
  methodMethodId (Proxy) = 50

-- | The @unbind-ok@ method: confirm unbind successful
--
-- This method confirms that the unbind was successful.
data QueueUnbindOk = QueueUnbindOk deriving (Show, Eq, Generic)

instance Validity QueueUnbindOk

instance Method QueueUnbindOk where
  methodClassId (Proxy) = 50
  methodMethodId (Proxy) = 51

-- | The @purge@ method: purge a queue
--
-- This method removes all messages from a queue which are not awaiting
-- acknowledgment.
data QueuePurge = QueuePurge
  { queuePurgeReserved1 :: {-# UNPACK #-} !ShortUInt,
    queuePurgeQueue :: {-# UNPACK #-} !QueueName,
    queuePurgeNoWait :: {-# UNPACK #-} !NoWait
  }
  deriving (Show, Eq, Generic)

instance Validity QueuePurge

instance Method QueuePurge where
  methodClassId (Proxy) = 50
  methodMethodId (Proxy) = 30

-- | The @purge-ok@ method: confirms a queue purge
--
-- This method confirms the purge of a queue.
data QueuePurgeOk = QueuePurgeOk {queuePurgeOkMessageCount :: {-# UNPACK #-} !MessageCount}
  deriving (Show, Eq, Generic)

instance Validity QueuePurgeOk

instance Method QueuePurgeOk where
  methodClassId (Proxy) = 50
  methodMethodId (Proxy) = 31

-- | The @delete@ method: delete a queue
--
-- This method deletes a queue. When a queue is deleted any pending messages are sent
-- to a dead-letter queue if this is defined in the server configuration, and all
-- consumers on the queue are cancelled.
data QueueDelete = QueueDelete
  { queueDeleteReserved1 :: {-# UNPACK #-} !ShortUInt,
    queueDeleteQueue :: {-# UNPACK #-} !QueueName,
    queueDeleteIfUnused :: {-# UNPACK #-} !Bit,
    queueDeleteIfEmpty :: {-# UNPACK #-} !Bit,
    queueDeleteNoWait :: {-# UNPACK #-} !NoWait
  }
  deriving (Show, Eq, Generic)

instance Validity QueueDelete

instance Method QueueDelete where
  methodClassId (Proxy) = 50
  methodMethodId (Proxy) = 40

-- | The @delete-ok@ method: confirm deletion of a queue
--
-- This method confirms the deletion of a queue.
data QueueDeleteOk = QueueDeleteOk {queueDeleteOkMessageCount :: {-# UNPACK #-} !MessageCount}
  deriving (Show, Eq, Generic)

instance Validity QueueDeleteOk

instance Method QueueDeleteOk where
  methodClassId (Proxy) = 50
  methodMethodId (Proxy) = 41

-- | The @qos@ method: specify quality of service
--
-- This method requests a specific quality of service. The QoS can be specified for the
-- current channel or for all channels on the connection. The particular properties and
-- semantics of a qos method always depend on the content class semantics. Though the
-- qos method could in principle apply to both peers, it is currently meaningful only
-- for the server.
data BasicQos = BasicQos
  { basicQosPrefetchSize :: {-# UNPACK #-} !LongUInt,
    basicQosPrefetchCount :: {-# UNPACK #-} !ShortUInt,
    basicQosGlobal :: {-# UNPACK #-} !Bit
  }
  deriving (Show, Eq, Generic)

instance Validity BasicQos

instance Method BasicQos where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 10

-- | The @qos-ok@ method: confirm the requested qos
--
-- This method tells the client that the requested QoS levels could be handled by the
-- server. The requested QoS applies to all active consumers until a new QoS is
-- defined.
data BasicQosOk = BasicQosOk deriving (Show, Eq, Generic)

instance Validity BasicQosOk

instance Method BasicQosOk where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 11

-- | The @consume@ method: start a queue consumer
--
-- This method asks the server to start a "consumer", which is a transient request for
-- messages from a specific queue. Consumers last as long as the channel they were
-- declared on, or until the client cancels them.
data BasicConsume = BasicConsume
  { basicConsumeReserved1 :: {-# UNPACK #-} !ShortUInt,
    basicConsumeQueue :: {-# UNPACK #-} !QueueName,
    basicConsumeConsumerTag :: {-# UNPACK #-} !ConsumerTag,
    basicConsumeNoLocal :: {-# UNPACK #-} !NoLocal,
    basicConsumeNoAck :: {-# UNPACK #-} !NoAck,
    basicConsumeExclusive :: {-# UNPACK #-} !Bit,
    basicConsumeNoWait :: {-# UNPACK #-} !NoWait,
    basicConsumeArguments :: {-# UNPACK #-} !FieldTable
  }
  deriving (Show, Eq, Generic)

instance Validity BasicConsume

instance Method BasicConsume where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 20

-- | The @consume-ok@ method: confirm a new consumer
--
-- The server provides the client with a consumer tag, which is used by the client
-- for methods called on the consumer at a later stage.
data BasicConsumeOk = BasicConsumeOk {basicConsumeOkConsumerTag :: {-# UNPACK #-} !ConsumerTag}
  deriving (Show, Eq, Generic)

instance Validity BasicConsumeOk

instance Method BasicConsumeOk where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 21

-- | The @cancel@ method: end a queue consumer
--
-- This method cancels a consumer. This does not affect already delivered
-- messages, but it does mean the server will not send any more messages for
-- that consumer. The client may receive an arbitrary number of messages in
-- between sending the cancel method and receiving the cancel-ok reply.
data BasicCancel = BasicCancel
  { basicCancelConsumerTag :: {-# UNPACK #-} !ConsumerTag,
    basicCancelNoWait :: {-# UNPACK #-} !NoWait
  }
  deriving (Show, Eq, Generic)

instance Validity BasicCancel

instance Method BasicCancel where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 30

-- | The @cancel-ok@ method: confirm a cancelled consumer
--
-- This method confirms that the cancellation was completed.
data BasicCancelOk = BasicCancelOk {basicCancelOkConsumerTag :: {-# UNPACK #-} !ConsumerTag}
  deriving (Show, Eq, Generic)

instance Validity BasicCancelOk

instance Method BasicCancelOk where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 31

-- | The @publish@ method: publish a message
--
-- This method publishes a message to a specific exchange. The message will be routed
-- to queues as defined by the exchange configuration and distributed to any active
-- consumers when the transaction, if any, is committed.
data BasicPublish = BasicPublish
  { basicPublishReserved1 :: {-# UNPACK #-} !ShortUInt,
    basicPublishExchange :: {-# UNPACK #-} !ExchangeName,
    basicPublishRoutingKey :: {-# UNPACK #-} !ShortString,
    basicPublishMandatory :: {-# UNPACK #-} !Bit,
    basicPublishImmediate :: {-# UNPACK #-} !Bit
  }
  deriving (Show, Eq, Generic)

instance Validity BasicPublish

instance Method BasicPublish where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 40

-- | The @return@ method: return a failed message
--
-- This method returns an undeliverable message that was published with the "immediate"
-- flag set, or an unroutable message published with the "mandatory" flag set. The
-- reply code and text provide information about the reason that the message was
-- undeliverable.
data BasicReturn = BasicReturn
  { basicReturnReplyCode :: {-# UNPACK #-} !ReplyCode,
    basicReturnReplyText :: {-# UNPACK #-} !ReplyText,
    basicReturnExchange :: {-# UNPACK #-} !ExchangeName,
    basicReturnRoutingKey :: {-# UNPACK #-} !ShortString
  }
  deriving (Show, Eq, Generic)

instance Validity BasicReturn

instance Method BasicReturn where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 50

-- | The @deliver@ method: notify the client of a consumer message
--
-- This method delivers a message to the client, via a consumer. In the asynchronous
-- message delivery model, the client starts a consumer using the Consume method, then
-- the server responds with Deliver methods as and when messages arrive for that
-- consumer.
data BasicDeliver = BasicDeliver
  { basicDeliverConsumerTag :: {-# UNPACK #-} !ConsumerTag,
    basicDeliverDeliveryTag :: {-# UNPACK #-} !DeliveryTag,
    basicDeliverRedelivered :: {-# UNPACK #-} !Redelivered,
    basicDeliverExchange :: {-# UNPACK #-} !ExchangeName,
    basicDeliverRoutingKey :: {-# UNPACK #-} !ShortString
  }
  deriving (Show, Eq, Generic)

instance Validity BasicDeliver

instance Method BasicDeliver where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 60

-- | The @get@ method: direct access to a queue
--
-- This method provides a direct access to the messages in a queue using a synchronous
-- dialogue that is designed for specific types of application where synchronous
-- functionality is more important than performance.
data BasicGet = BasicGet
  { basicGetReserved1 :: {-# UNPACK #-} !ShortUInt,
    basicGetQueue :: {-# UNPACK #-} !QueueName,
    basicGetNoAck :: {-# UNPACK #-} !NoAck
  }
  deriving (Show, Eq, Generic)

instance Validity BasicGet

instance Method BasicGet where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 70

-- | The @get-ok@ method: provide client with a message
--
-- This method delivers a message to the client following a get method. A message
-- delivered by 'get-ok' must be acknowledged unless the no-ack option was set in the
-- get method.
data BasicGetOk = BasicGetOk
  { basicGetOkDeliveryTag :: {-# UNPACK #-} !DeliveryTag,
    basicGetOkRedelivered :: {-# UNPACK #-} !Redelivered,
    basicGetOkExchange :: {-# UNPACK #-} !ExchangeName,
    basicGetOkRoutingKey :: {-# UNPACK #-} !ShortString,
    basicGetOkMessageCount :: {-# UNPACK #-} !MessageCount
  }
  deriving (Show, Eq, Generic)

instance Validity BasicGetOk

instance Method BasicGetOk where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 71

-- | The @get-empty@ method: indicate no messages available
--
-- This method tells the client that the queue has no messages available for the
-- client.
data BasicGetEmpty = BasicGetEmpty {basicGetEmptyReserved1 :: {-# UNPACK #-} !ShortString}
  deriving (Show, Eq, Generic)

instance Validity BasicGetEmpty

instance Method BasicGetEmpty where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 72

-- | The @ack@ method: acknowledge one or more messages
--
-- This method acknowledges one or more messages delivered via the Deliver or Get-Ok
-- methods. The client can ask to confirm a single message or a set of messages up to
-- and including a specific message.
data BasicAck = BasicAck
  { basicAckDeliveryTag :: {-# UNPACK #-} !DeliveryTag,
    basicAckMultiple :: {-# UNPACK #-} !Bit
  }
  deriving (Show, Eq, Generic)

instance Validity BasicAck

instance Method BasicAck where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 80

-- | The @reject@ method: reject an incoming message
--
-- This method allows a client to reject a message. It can be used to interrupt and
-- cancel large incoming messages, or return untreatable messages to their original
-- queue.
data BasicReject = BasicReject
  { basicRejectDeliveryTag :: {-# UNPACK #-} !DeliveryTag,
    basicRejectRequeue :: {-# UNPACK #-} !Bit
  }
  deriving (Show, Eq, Generic)

instance Validity BasicReject

instance Method BasicReject where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 90

-- | The @recover-async@ method: redeliver unacknowledged messages
--
-- This method asks the server to redeliver all unacknowledged messages on a
-- specified channel. Zero or more messages may be redelivered.  This method
-- is deprecated in favour of the synchronous Recover/Recover-Ok.
data BasicRecoverAsync = BasicRecoverAsync {basicRecoverAsyncRequeue :: {-# UNPACK #-} !Bit}
  deriving (Show, Eq, Generic)

instance Validity BasicRecoverAsync

instance Method BasicRecoverAsync where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 100

-- | The @recover@ method: redeliver unacknowledged messages
--
-- This method asks the server to redeliver all unacknowledged messages on a
-- specified channel. Zero or more messages may be redelivered.  This method
-- replaces the asynchronous Recover.
data BasicRecover = BasicRecover {basicRecoverRequeue :: {-# UNPACK #-} !Bit}
  deriving (Show, Eq, Generic)

instance Validity BasicRecover

instance Method BasicRecover where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 110

-- | The @recover-ok@ method: confirm recovery
--
-- This method acknowledges a Basic.Recover method.
data BasicRecoverOk = BasicRecoverOk deriving (Show, Eq, Generic)

instance Validity BasicRecoverOk

instance Method BasicRecoverOk where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 111

-- | The @select@ method: select standard transaction mode
--
-- This method sets the channel to use standard transactions. The client must use this
-- method at least once on a channel before using the Commit or Rollback methods.
data TxSelect = TxSelect deriving (Show, Eq, Generic)

instance Validity TxSelect

instance Method TxSelect where
  methodClassId (Proxy) = 90
  methodMethodId (Proxy) = 10

-- | The @select-ok@ method: confirm transaction mode
--
-- This method confirms to the client that the channel was successfully set to use
-- standard transactions.
data TxSelectOk = TxSelectOk deriving (Show, Eq, Generic)

instance Validity TxSelectOk

instance Method TxSelectOk where
  methodClassId (Proxy) = 90
  methodMethodId (Proxy) = 11

-- | The @commit@ method: commit the current transaction
--
-- This method commits all message publications and acknowledgments performed in
-- the current transaction.  A new transaction starts immediately after a commit.
data TxCommit = TxCommit deriving (Show, Eq, Generic)

instance Validity TxCommit

instance Method TxCommit where
  methodClassId (Proxy) = 90
  methodMethodId (Proxy) = 20

-- | The @commit-ok@ method: confirm a successful commit
--
-- This method confirms to the client that the commit succeeded. Note that if a commit
-- fails, the server raises a channel exception.
data TxCommitOk = TxCommitOk deriving (Show, Eq, Generic)

instance Validity TxCommitOk

instance Method TxCommitOk where
  methodClassId (Proxy) = 90
  methodMethodId (Proxy) = 21

-- | The @rollback@ method: abandon the current transaction
--
-- This method abandons all message publications and acknowledgments performed in
-- the current transaction. A new transaction starts immediately after a rollback.
-- Note that unacked messages will not be automatically redelivered by rollback;
-- if that is required an explicit recover call should be issued.
data TxRollback = TxRollback deriving (Show, Eq, Generic)

instance Validity TxRollback

instance Method TxRollback where
  methodClassId (Proxy) = 90
  methodMethodId (Proxy) = 30

-- | The @rollback-ok@ method: confirm successful rollback
--
-- This method confirms to the client that the rollback succeeded. Note that if an
-- rollback fails, the server raises a channel exception.
data TxRollbackOk = TxRollbackOk deriving (Show, Eq, Generic)

instance Validity TxRollbackOk

instance Method TxRollbackOk where
  methodClassId (Proxy) = 90
  methodMethodId (Proxy) = 31
