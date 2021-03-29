module AMQP.Serialisation.Generated.DomainTypes where

import AMQP.Serialisation.Base

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
