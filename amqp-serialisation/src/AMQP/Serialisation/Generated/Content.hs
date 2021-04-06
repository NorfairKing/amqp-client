{-# LANGUAGE DeriveGeneric #-}

module AMQP.Serialisation.Generated.Content where

import AMQP.Serialisation.Argument
import AMQP.Serialisation.Base
import Data.Proxy
import Data.Validity
import GHC.Generics (Generic)

data ConnectionContentHeader
  = ConnectionContentHeader
  deriving (Show, Eq, Generic)

instance Validity ConnectionContentHeader

instance IsContentHeader ConnectionContentHeader where
  contentHeaderClassId (Proxy) = 10
  parseContentHeaderArguments = do pure ConnectionContentHeader

data ChannelContentHeader
  = ChannelContentHeader
  deriving (Show, Eq, Generic)

instance Validity ChannelContentHeader

instance IsContentHeader ChannelContentHeader where
  contentHeaderClassId (Proxy) = 20
  parseContentHeaderArguments = do pure ChannelContentHeader

data ExchangeContentHeader
  = ExchangeContentHeader
  deriving (Show, Eq, Generic)

instance Validity ExchangeContentHeader

instance IsContentHeader ExchangeContentHeader where
  contentHeaderClassId (Proxy) = 40
  parseContentHeaderArguments = do pure ExchangeContentHeader

data QueueContentHeader
  = QueueContentHeader
  deriving (Show, Eq, Generic)

instance Validity QueueContentHeader

instance IsContentHeader QueueContentHeader where
  contentHeaderClassId (Proxy) = 50
  parseContentHeaderArguments = do pure QueueContentHeader

data BasicContentHeader = BasicContentHeader
  { basicContentHeaderContentType :: !(Maybe ShortString),
    basicContentHeaderContentEncoding :: !(Maybe ShortString),
    basicContentHeaderHeaders :: !(Maybe FieldTable),
    basicContentHeaderDeliveryMode :: !(Maybe Octet),
    basicContentHeaderPriority :: !(Maybe Octet),
    basicContentHeaderCorrelationId :: !(Maybe ShortString),
    basicContentHeaderReplyTo :: !(Maybe ShortString),
    basicContentHeaderExpiration :: !(Maybe ShortString),
    basicContentHeaderMessageId :: !(Maybe ShortString),
    basicContentHeaderTimestamp :: !(Maybe Timestamp),
    basicContentHeaderType :: !(Maybe ShortString),
    basicContentHeaderUserId :: !(Maybe ShortString),
    basicContentHeaderAppId :: !(Maybe ShortString),
    basicContentHeaderReserved :: !(Maybe ShortString)
  }
  deriving (Show, Eq, Generic)

instance Validity BasicContentHeader

instance IsContentHeader BasicContentHeader where
  contentHeaderClassId (Proxy) = 60
  parseContentHeaderArguments = do
    ( basicContentHeaderContentTypeBit,
      basicContentHeaderContentEncodingBit,
      basicContentHeaderHeadersBit,
      basicContentHeaderDeliveryModeBit,
      basicContentHeaderPriorityBit,
      basicContentHeaderCorrelationIdBit,
      basicContentHeaderReplyToBit,
      basicContentHeaderExpirationBit,
      basicContentHeaderMessageIdBit,
      basicContentHeaderTimestampBit,
      basicContentHeaderTypeBit,
      basicContentHeaderUserIdBit,
      basicContentHeaderAppIdBit,
      basicContentHeaderReservedBit
      ) <-
      parse14PropBits
    basicContentHeaderContentTypeParsed <- parsePropArgument basicContentHeaderContentTypeBit
    basicContentHeaderContentEncodingParsed <- parsePropArgument basicContentHeaderContentEncodingBit
    basicContentHeaderHeadersParsed <- parsePropArgument basicContentHeaderHeadersBit
    basicContentHeaderDeliveryModeParsed <- parsePropArgument basicContentHeaderDeliveryModeBit
    basicContentHeaderPriorityParsed <- parsePropArgument basicContentHeaderPriorityBit
    basicContentHeaderCorrelationIdParsed <- parsePropArgument basicContentHeaderCorrelationIdBit
    basicContentHeaderReplyToParsed <- parsePropArgument basicContentHeaderReplyToBit
    basicContentHeaderExpirationParsed <- parsePropArgument basicContentHeaderExpirationBit
    basicContentHeaderMessageIdParsed <- parsePropArgument basicContentHeaderMessageIdBit
    basicContentHeaderTimestampParsed <- parsePropArgument basicContentHeaderTimestampBit
    basicContentHeaderTypeParsed <- parsePropArgument basicContentHeaderTypeBit
    basicContentHeaderUserIdParsed <- parsePropArgument basicContentHeaderUserIdBit
    basicContentHeaderAppIdParsed <- parsePropArgument basicContentHeaderAppIdBit
    basicContentHeaderReservedParsed <- parsePropArgument basicContentHeaderReservedBit
    pure
      BasicContentHeader
        { basicContentHeaderContentType = basicContentHeaderContentTypeParsed,
          basicContentHeaderContentEncoding = basicContentHeaderContentEncodingParsed,
          basicContentHeaderHeaders = basicContentHeaderHeadersParsed,
          basicContentHeaderDeliveryMode = basicContentHeaderDeliveryModeParsed,
          basicContentHeaderPriority = basicContentHeaderPriorityParsed,
          basicContentHeaderCorrelationId = basicContentHeaderCorrelationIdParsed,
          basicContentHeaderReplyTo = basicContentHeaderReplyToParsed,
          basicContentHeaderExpiration = basicContentHeaderExpirationParsed,
          basicContentHeaderMessageId = basicContentHeaderMessageIdParsed,
          basicContentHeaderTimestamp = basicContentHeaderTimestampParsed,
          basicContentHeaderType = basicContentHeaderTypeParsed,
          basicContentHeaderUserId = basicContentHeaderUserIdParsed,
          basicContentHeaderAppId = basicContentHeaderAppIdParsed,
          basicContentHeaderReserved = basicContentHeaderReservedParsed
        }

data TxContentHeader = TxContentHeader deriving (Show, Eq, Generic)

instance Validity TxContentHeader

instance IsContentHeader TxContentHeader where
  contentHeaderClassId (Proxy) = 90
  parseContentHeaderArguments = do pure TxContentHeader

data ConfirmContentHeader
  = ConfirmContentHeader
  deriving (Show, Eq, Generic)

instance Validity ConfirmContentHeader

instance IsContentHeader ConfirmContentHeader where
  contentHeaderClassId (Proxy) = 85
  parseContentHeaderArguments = do pure ConfirmContentHeader
