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
  parseContentHeaderArguments = undefined

data ChannelContentHeader
  = ChannelContentHeader
  deriving (Show, Eq, Generic)

instance Validity ChannelContentHeader

instance IsContentHeader ChannelContentHeader where
  contentHeaderClassId (Proxy) = 20
  parseContentHeaderArguments = undefined

data ExchangeContentHeader
  = ExchangeContentHeader
  deriving (Show, Eq, Generic)

instance Validity ExchangeContentHeader

instance IsContentHeader ExchangeContentHeader where
  contentHeaderClassId (Proxy) = 40
  parseContentHeaderArguments = undefined

data QueueContentHeader
  = QueueContentHeader
  deriving (Show, Eq, Generic)

instance Validity QueueContentHeader

instance IsContentHeader QueueContentHeader where
  contentHeaderClassId (Proxy) = 50
  parseContentHeaderArguments = undefined

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
  parseContentHeaderArguments = undefined

data TxContentHeader = TxContentHeader deriving (Show, Eq, Generic)

instance Validity TxContentHeader

instance IsContentHeader TxContentHeader where
  contentHeaderClassId (Proxy) = 90
  parseContentHeaderArguments = undefined
