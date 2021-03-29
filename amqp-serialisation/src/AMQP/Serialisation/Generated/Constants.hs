module AMQP.Serialisation.Generated.Constants where

import Data.Word

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
