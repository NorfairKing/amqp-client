{-# LANGUAGE DeriveGeneric #-}

module AMQP.Serialisation.Generated.Methods where

import AMQP.Serialisation.Argument
import AMQP.Serialisation.Base
import AMQP.Serialisation.Generated.DomainTypes
import Data.Proxy
import Data.Validity
import GHC.Generics (Generic)

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

-- | The @open-ok@ method: signal that connection is ready
--
-- This method signals to the client that the connection is ready for use.
data ConnectionOpenOk = ConnectionOpenOk {connectionOpenOkReserved1 :: !ShortString}
  deriving (Show, Eq, Generic)

instance Validity ConnectionOpenOk

instance IsMethod ConnectionOpenOk where
  methodClassId (Proxy) = 10
  methodMethodId (Proxy) = 41

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

-- | The @open@ method: open a channel for use
--
-- This method opens a channel to the server.
data ChannelOpen = ChannelOpen {channelOpenReserved1 :: !ShortString}
  deriving (Show, Eq, Generic)

instance Validity ChannelOpen

instance IsMethod ChannelOpen where
  methodClassId (Proxy) = 20
  methodMethodId (Proxy) = 10

-- | The @open-ok@ method: signal that the channel is ready
--
-- This method signals to the client that the channel is ready for use.
data ChannelOpenOk = ChannelOpenOk {channelOpenOkReserved1 :: !LongString}
  deriving (Show, Eq, Generic)

instance Validity ChannelOpenOk

instance IsMethod ChannelOpenOk where
  methodClassId (Proxy) = 20
  methodMethodId (Proxy) = 11

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

-- | The @flow-ok@ method: confirm a flow method
--
-- Confirms to the peer that a flow command was received and processed.
data ChannelFlowOk = ChannelFlowOk {channelFlowOkActive :: !Bit}
  deriving (Show, Eq, Generic)

instance Validity ChannelFlowOk

instance IsMethod ChannelFlowOk where
  methodClassId (Proxy) = 20
  methodMethodId (Proxy) = 21

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

-- | The @close-ok@ method: confirm a channel close
--
-- This method confirms a Channel.Close method and tells the recipient that it is safe
-- to release resources for the channel.
data ChannelCloseOk = ChannelCloseOk deriving (Show, Eq, Generic)

instance Validity ChannelCloseOk

instance IsMethod ChannelCloseOk where
  methodClassId (Proxy) = 20
  methodMethodId (Proxy) = 41

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

-- | The @bind-ok@ method: confirm bind successful
--
-- This method confirms that the bind was successful.
data QueueBindOk = QueueBindOk deriving (Show, Eq, Generic)

instance Validity QueueBindOk

instance IsMethod QueueBindOk where
  methodClassId (Proxy) = 50
  methodMethodId (Proxy) = 21

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

-- | The @unbind-ok@ method: confirm unbind successful
--
-- This method confirms that the unbind was successful.
data QueueUnbindOk = QueueUnbindOk deriving (Show, Eq, Generic)

instance Validity QueueUnbindOk

instance IsMethod QueueUnbindOk where
  methodClassId (Proxy) = 50
  methodMethodId (Proxy) = 51

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

-- | The @purge-ok@ method: confirms a queue purge
--
-- This method confirms the purge of a queue.
data QueuePurgeOk = QueuePurgeOk {queuePurgeOkMessageCount :: !MessageCount}
  deriving (Show, Eq, Generic)

instance Validity QueuePurgeOk

instance IsMethod QueuePurgeOk where
  methodClassId (Proxy) = 50
  methodMethodId (Proxy) = 31

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

-- | The @delete-ok@ method: confirm deletion of a queue
--
-- This method confirms the deletion of a queue.
data QueueDeleteOk = QueueDeleteOk {queueDeleteOkMessageCount :: !MessageCount}
  deriving (Show, Eq, Generic)

instance Validity QueueDeleteOk

instance IsMethod QueueDeleteOk where
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
  { basicQosPrefetchSize :: !LongUInt,
    basicQosPrefetchCount :: !ShortUInt,
    basicQosGlobal :: !Bit
  }
  deriving (Show, Eq, Generic)

instance Validity BasicQos

instance IsMethod BasicQos where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 10

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

-- | The @cancel-ok@ method: confirm a cancelled consumer
--
-- This method confirms that the cancellation was completed.
data BasicCancelOk = BasicCancelOk {basicCancelOkConsumerTag :: !ConsumerTag}
  deriving (Show, Eq, Generic)

instance Validity BasicCancelOk

instance IsMethod BasicCancelOk where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 31

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

-- | The @recover-ok@ method: confirm recovery
--
-- This method acknowledges a Basic.Recover method.
data BasicRecoverOk = BasicRecoverOk deriving (Show, Eq, Generic)

instance Validity BasicRecoverOk

instance IsMethod BasicRecoverOk where
  methodClassId (Proxy) = 60
  methodMethodId (Proxy) = 111

-- | The @select@ method: select standard transaction mode
--
-- This method sets the channel to use standard transactions. The client must use this
-- method at least once on a channel before using the Commit or Rollback methods.
data TxSelect = TxSelect deriving (Show, Eq, Generic)

instance Validity TxSelect

instance IsMethod TxSelect where
  methodClassId (Proxy) = 90
  methodMethodId (Proxy) = 10

-- | The @select-ok@ method: confirm transaction mode
--
-- This method confirms to the client that the channel was successfully set to use
-- standard transactions.
data TxSelectOk = TxSelectOk deriving (Show, Eq, Generic)

instance Validity TxSelectOk

instance IsMethod TxSelectOk where
  methodClassId (Proxy) = 90
  methodMethodId (Proxy) = 11

-- | The @commit@ method: commit the current transaction
--
-- This method commits all message publications and acknowledgments performed in
-- the current transaction.  A new transaction starts immediately after a commit.
data TxCommit = TxCommit deriving (Show, Eq, Generic)

instance Validity TxCommit

instance IsMethod TxCommit where
  methodClassId (Proxy) = 90
  methodMethodId (Proxy) = 20

-- | The @commit-ok@ method: confirm a successful commit
--
-- This method confirms to the client that the commit succeeded. Note that if a commit
-- fails, the server raises a channel exception.
data TxCommitOk = TxCommitOk deriving (Show, Eq, Generic)

instance Validity TxCommitOk

instance IsMethod TxCommitOk where
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

instance IsMethod TxRollback where
  methodClassId (Proxy) = 90
  methodMethodId (Proxy) = 30

-- | The @rollback-ok@ method: confirm successful rollback
--
-- This method confirms to the client that the rollback succeeded. Note that if an
-- rollback fails, the server raises a channel exception.
data TxRollbackOk = TxRollbackOk deriving (Show, Eq, Generic)

instance Validity TxRollbackOk

instance IsMethod TxRollbackOk where
  methodClassId (Proxy) = 90
  methodMethodId (Proxy) = 31

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
