module AMQP.Serialisation.Generated.Content where

data ConnectionContentHeader
  = ConnectionContentHeader
  deriving (Show, Eq, Generic)

instance Validity ConnectionContentHeader

instance IsContentHeaderContent ConnectionContentHeader where
  methodClassId (Proxy) = 10

data ChannelContentHeader
  = ChannelContentHeader
  deriving (Show, Eq, Generic)

instance Validity ChannelContentHeader

instance IsContentHeaderContent ChannelContentHeader where
  methodClassId (Proxy) = 20

data ExchangeContentHeader
  = ExchangeContentHeader
  deriving (Show, Eq, Generic)

instance Validity ExchangeContentHeader

instance IsContentHeaderContent ExchangeContentHeader where
  methodClassId (Proxy) = 40

data QueueContentHeader
  = QueueContentHeader
  deriving (Show, Eq, Generic)

instance Validity QueueContentHeader

instance IsContentHeaderContent QueueContentHeader where
  methodClassId (Proxy) = 50

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

instance IsContentHeaderContent BasicContentHeader where
  methodClassId (Proxy) = 60

data TxContentHeader = TxContentHeader deriving (Show, Eq, Generic)

instance Validity TxContentHeader

instance IsContentHeaderContent TxContentHeader where
  methodClassId (Proxy) = 90
