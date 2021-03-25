{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AMQP.Serialisation where

import Control.Monad
import Data.Attoparsec.Binary as Parse
import Data.Attoparsec.ByteString as Parse
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.ByteString.Builder as ByteString (Builder)
import qualified Data.ByteString.Builder as SBB
import Data.Int
import Data.Map (Map)
import qualified Data.ReinterpretCast as Cast
import Data.Validity
import Data.Validity.ByteString ()
import Data.Word
import GHC.Generics (Generic)

data ProtocolHeader = ProtocolHeader
  { protocolHeaderMajor :: !Word8,
    protocolHeaderMinor :: !Word8,
    protocolHeaderRevision :: !Word8
  }
  deriving (Show, Eq, Generic)

instance Validity ProtocolHeader

-- TODO get this from the spec.
protocolHeader :: ProtocolHeader
protocolHeader =
  ProtocolHeader
    { protocolHeaderMajor = 0,
      protocolHeaderMinor = 9,
      protocolHeaderRevision = 1
    }

buildProtocolHeader :: ProtocolHeader -> ByteString.Builder
buildProtocolHeader ProtocolHeader {..} =
  mconcat
    [ SBB.byteString "AMQP",
      SBB.word8 0,
      SBB.word8 protocolHeaderMajor,
      SBB.word8 protocolHeaderMinor,
      SBB.word8 protocolHeaderRevision
    ]

parseProtocolHeader :: Parser ProtocolHeader
parseProtocolHeader = do
  void $ Parse.string "AMQP"
  void $ Parse.word8 0
  protocolHeaderMajor <- Parse.anyWord8
  protocolHeaderMinor <- Parse.anyWord8
  protocolHeaderRevision <- Parse.anyWord8
  pure ProtocolHeader {..}

data FrameType
  = MethodFrame
  | HeaderFrame
  | BodyFrame
  | HeartbeatFrame
  deriving (Show, Eq, Generic, Enum, Bounded)

instance Validity FrameType

-- TODO get these from the spec
buildFrameType :: FrameType -> ByteString.Builder
buildFrameType = \case
  MethodFrame -> SBB.word8 1
  HeaderFrame -> SBB.word8 2
  BodyFrame -> SBB.word8 3
  -- QUESTION: The pdf says 4 but the spec says 8, which is it?
  -- ANSWER: We'll go with what the spec says.
  HeartbeatFrame -> SBB.word8 8

parseFrameType :: Parser FrameType
parseFrameType = do
  w <- Parse.anyWord8
  -- TODO get these from the spec
  case w of
    1 -> pure MethodFrame
    2 -> pure HeaderFrame
    3 -> pure BodyFrame
    8 -> pure HeartbeatFrame
    _ -> fail $ "Unknown frame type: " <> show w

data RawFrame = RawFrame
  { rawFrameType :: !FrameType,
    rawFrameChannel :: !Word16,
    rawFramePayload :: !ByteString
  }
  deriving (Show, Eq, Generic)

instance Validity RawFrame

buildRawFrame :: RawFrame -> ByteString.Builder
buildRawFrame RawFrame {..} =
  mconcat
    [ buildFrameType rawFrameType,
      -- QUESTION: Should this be little-endian?
      -- ANSWER: No, the spec says that it must be network byte order, or big-endian.
      SBB.word16BE rawFrameChannel,
      -- QUESTION: Should this be little-endian?
      -- ANSWER: the spec says that it must be network byte order, or big-endian.
      --
      -- The 'fromIntegral' should be safe because we cast from Int to Word32.
      SBB.word32BE (fromIntegral (SB.length rawFramePayload)),
      SBB.byteString rawFramePayload,
      SBB.word8 rawFrameEnd
    ]

rawFrameEnd :: Word8
rawFrameEnd = 206 -- TODO get this from the spec

parseRawFrame :: Parser RawFrame
parseRawFrame = do
  rawFrameType <- parseFrameType
  rawFrameChannel <- anyWord16be
  rawFrameLength <- anyWord32be
  rawFramePayload <- Parse.take (fromIntegral rawFrameLength)
  void $ Parse.word8 rawFrameEnd
  pure RawFrame {..}

data ProtocolNegotiationResponse
  = ProtocolRejected ProtocolHeader
  | -- TODO replace this frame with the more specific Connection.Start method
    -- frame once we generate code for that.
    ProtocolProposed RawFrame
  deriving (Show, Eq, Generic)

parseProtocolNegotiationResponse :: Parser ProtocolNegotiationResponse
parseProtocolNegotiationResponse =
  choice
    [ ProtocolRejected <$> parseProtocolHeader,
      ProtocolProposed <$> parseRawFrame
    ]

data ConnectionStartMethodFrame = ConnectionStartMethodFrame
  { connectionStartMethodFrameVersionMajor :: !Octet,
    connectionStartMethodFrameVersionMinor :: !Octet,
    connectionStartMethodFrameServerProperties :: !PeerProperties,
    connectionStartMethodFrameMechanism :: !LongString,
    connectionStartMethodFrameLocales :: !LongString
  }
  deriving (Show, Eq, Generic)

type PeerProperties = FieldTable

type FieldTable = Map ShortString FieldTableValue

data FieldTableValue
  = FieldTableBit !Bit
  | FieldTableShortShortInt !ShortShortInt
  | FieldTableShortShortUInt !ShortShortUInt
  | FieldTableShortInt !ShortInt
  | FieldTableShortUInt !ShortUInt
  | FieldTableLongInt !LongInt
  | FieldTableLongUInt !LongUInt
  | FieldTableLongLongInt !LongLongInt
  | FieldTableLongLongUInt !LongLongUInt
  | FieldTableFloat !Float
  | FieldTableDouble !Double
  | FieldTableDecimal !DecimalValue
  | FieldTableShortString !ShortString
  | FieldTableLongString !LongString
  | FieldTableArray ![FieldTableValue]
  | FieldTableTimestamp !Timestamp
  | FieldTableVoid
  deriving (Show, Eq, Generic)

type Bit = Bool

parseBit :: Parser Bit
parseBit = do
  w <- Parse.anyWord8
  case w of
    0 -> pure False
    _ -> pure True

buildBit :: Bit -> ByteString.Builder
buildBit b =
  SBB.word8 $
    if b
      then 1
      else 0

type Octet = Word8

parseOctet :: Parser Octet
parseOctet = Parse.anyWord8

buildOctet :: Octet -> ByteString.Builder
buildOctet = SBB.word8

type ShortShortInt = Int8

parseShortShortInt :: Parser ShortShortInt
parseShortShortInt = fromIntegral <$> Parse.anyWord8 -- Safe fromintegral Word8 -> Int8

buildShortShortInt :: ShortShortInt -> ByteString.Builder
buildShortShortInt = SBB.int8

type ShortShortUInt = Word8

parseShortShortUInt :: Parser ShortShortUInt
parseShortShortUInt = Parse.anyWord8

buildShortShortUInt :: ShortShortUInt -> ByteString.Builder
buildShortShortUInt = SBB.word8

type ShortInt = Int16

parseShortInt :: Parser ShortInt
parseShortInt = fromIntegral <$> Parse.anyWord16be -- Safe fromintegral Word16 -> Int16

buildShortInt :: ShortInt -> ByteString.Builder
buildShortInt = SBB.int16BE

type ShortUInt = Word16

parseShortUInt :: Parser ShortUInt
parseShortUInt = Parse.anyWord16be

buildShortUInt :: ShortUInt -> ByteString.Builder
buildShortUInt = SBB.word16BE

type LongInt = Int32

parseLongInt :: Parser LongInt
parseLongInt = fromIntegral <$> Parse.anyWord32be -- Safe fromintegral Word32 -> Int32

buildLongInt :: LongInt -> ByteString.Builder
buildLongInt = SBB.int32BE

type LongUInt = Word32

parseLongUInt :: Parser LongUInt
parseLongUInt = Parse.anyWord32be

buildLongUInt :: LongUInt -> ByteString.Builder
buildLongUInt = SBB.word32BE

type LongLongInt = Int64

parseLongLongInt :: Parser LongLongInt
parseLongLongInt = fromIntegral <$> Parse.anyWord64be -- Safe fromintegral Word64 -> Int64

buildLongLongInt :: LongLongInt -> ByteString.Builder
buildLongLongInt = SBB.int64BE

type LongLongUInt = Word64

parseLongLongUInt :: Parser LongLongUInt
parseLongLongUInt = Parse.anyWord64be

buildLongLongUInt :: LongLongUInt -> ByteString.Builder
buildLongLongUInt = SBB.word64BE

parseFloat :: Parser Float
parseFloat = Cast.wordToFloat <$> parseLongUInt

buildFloat :: Float -> ByteString.Builder
buildFloat = SBB.floatBE

parseDouble :: Parser Double
parseDouble = Cast.wordToDouble <$> parseLongLongUInt

buildDouble :: Double -> ByteString.Builder
buildDouble = SBB.doubleBE

data DecimalValue
  = DecimalValue
      !Octet
      !LongUInt
  deriving (Show, Eq, Generic)

instance Validity DecimalValue

parseDecimalValue :: Parser DecimalValue
parseDecimalValue = do
  scale <- parseOctet
  mantissa <- parseLongUInt
  pure $ DecimalValue scale mantissa

buildDecimalValue :: DecimalValue -> ByteString.Builder
buildDecimalValue (DecimalValue scale mantissa) =
  mconcat
    [ buildOctet scale,
      buildLongUInt mantissa
    ]

-- TODO newtype with validity constraint.
type ShortString = ByteString

parseShortString :: Parser ShortString
parseShortString = do
  o <- parseOctet
  -- Safe fromintegral because it's a Octet -> Int
  Parse.take (fromIntegral o)

buildShortString :: ShortString -> ByteString.Builder
buildShortString sb =
  mconcat
    [ buildOctet (fromIntegral (SB.length sb)), -- TODO not safe, use a newtype with validity constraint instead.
      SBB.byteString sb
    ]

-- TODO newtype with validity constraint.
type LongString = ByteString

parseLongString :: Parser LongString
parseLongString = do
  o <- parseLongUInt
  -- Safe fromintegral because it's a Octet -> Int
  Parse.take (fromIntegral o)

buildLongString :: ShortString -> ByteString.Builder
buildLongString sb =
  mconcat
    [ buildLongUInt (fromIntegral (SB.length sb)), -- TODO not safe, use a newtype with validity constraint instead.
      SBB.byteString sb
    ]

type Timestamp = Word64

parseTimestamp :: Parser Timestamp
parseTimestamp = parseLongLongUInt

buildTimestamp :: Timestamp -> ByteString.Builder
buildTimestamp = buildLongLongUInt
