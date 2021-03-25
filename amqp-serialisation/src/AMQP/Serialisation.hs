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
import Data.Char
import Data.Int
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.ReinterpretCast as Cast
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Containers ()
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
  = MethodFrameType
  | HeaderFrameType
  | BodyFrameType
  | HeartbeatFrameType
  deriving (Show, Eq, Generic, Enum, Bounded)

instance Validity FrameType

-- TODO get these from the spec
buildFrameType :: FrameType -> ByteString.Builder
buildFrameType = \case
  MethodFrameType -> SBB.word8 1
  HeaderFrameType -> SBB.word8 2
  BodyFrameType -> SBB.word8 3
  -- QUESTION: The pdf says 4 but the spec says 8, which is it?
  -- ANSWER: We'll go with what the spec says.
  HeartbeatFrameType -> SBB.word8 8

parseFrameType :: Parser FrameType
parseFrameType = do
  w <- Parse.anyWord8
  -- TODO get these from the spec
  case w of
    1 -> pure MethodFrameType
    2 -> pure HeaderFrameType
    3 -> pure BodyFrameType
    8 -> pure HeartbeatFrameType
    _ -> fail $ "Unknown frame type: " <> show w

type ChannelNumber = Word16

parseChannelNumber :: Parser Word16
parseChannelNumber = anyWord16be

buildChannelNumber :: Word16 -> ByteString.Builder
buildChannelNumber = SBB.word16BE

data RawFrame = RawFrame
  { rawFrameType :: !FrameType,
    rawFrameChannel :: !ChannelNumber,
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
      buildChannelNumber rawFrameChannel,
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
  rawFrameChannel <- parseChannelNumber
  rawFrameLength <- anyWord32be
  rawFramePayload <- Parse.take (fromIntegral rawFrameLength)
  void $ Parse.word8 rawFrameEnd
  pure RawFrame {..}

data ProtocolNegotiationResponse
  = ProtocolRejected ProtocolHeader
  | -- TODO replace this frame with the more specific Connection.Start method
    -- frame once we generate code for that.
    ProtocolProposed !RawFrame
  deriving (Show, Eq, Generic)

parseProtocolNegotiationResponse :: Parser ProtocolNegotiationResponse
parseProtocolNegotiationResponse =
  choice
    [ ProtocolRejected <$> parseProtocolHeader,
      ProtocolProposed <$> parseRawFrame
    ]

data MethodFrame = MethodFrame
  { methodFrameChannel :: !ChannelNumber,
    methodFramePayload :: !MethodFramePayload
  }
  deriving (Show, Eq, Generic)

data MethodFramePayload
  = ConnectionStartMethodFramePayload !ConnectionStartMethodFrame
  deriving (Show, Eq, Generic)

data ConnectionStartMethodFrame = ConnectionStartMethodFrame
  { connectionStartMethodFrameVersionMajor :: !Octet,
    connectionStartMethodFrameVersionMinor :: !Octet,
    connectionStartMethodFrameServerProperties :: !PeerProperties,
    connectionStartMethodFrameMechanism :: !LongString,
    connectionStartMethodFrameLocales :: !LongString
  }
  deriving (Show, Eq, Generic)

parseConnectionStartMethodFrame :: Parser ConnectionStartMethodFrame
parseConnectionStartMethodFrame = parseMethodFrame 10 10 parseConnectionStartMethodFramePayload

parseConnectionStartMethodFramePayload :: Parser ConnectionStartMethodFrame
parseConnectionStartMethodFramePayload = do
  connectionStartMethodFrameVersionMajor <- parseOctet
  connectionStartMethodFrameVersionMinor <- parseOctet
  connectionStartMethodFrameServerProperties <- parseFieldTable
  connectionStartMethodFrameMechanism <- parseLongString
  connectionStartMethodFrameLocales <- parseLongString
  pure ConnectionStartMethodFrame {..}

parseMethodFrame :: ClassId -> MethodId -> Parser a -> Parser a
parseMethodFrame cid mid p = do
  RawFrame {..} <- parseRawFrame
  case rawFrameType of
    MethodFrameType -> case parseOnly (parseMethodPayload cid mid p) rawFramePayload of
      Left err -> fail err
      Right r -> pure r
    ft -> fail $ unwords ["Got a frame of type", show ft, "instead of a method frame."]

parseMethodPayload :: ClassId -> MethodId -> Parser a -> Parser a
parseMethodPayload cid mid p = do
  void $ Parse.word16be cid
  void $ Parse.word16be mid
  p

type PeerProperties = FieldTable

buildMethodPayload :: ClassId -> MethodId -> [Argument] -> ByteString.Builder
buildMethodPayload cid mid as =
  mconcat $
    buildShortUInt cid :
    buildShortUInt mid :
    map buildFieldTableValue as

type ClassId = ShortUInt

type MethodId = ShortUInt

type Argument = FieldTableValue

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
  | FieldTableTimestamp !Timestamp
  | FieldTableArray ![FieldTableValue]
  | FieldTableFieldTable !FieldTable
  | FieldTableVoid
  deriving (Show, Eq, Generic)

instance Validity FieldTableValue

parseFieldTable :: Parser FieldTable
parseFieldTable = do
  lu <- parseLongUInt
  -- fromIntegral is safe because it's Word32 -> Int
  fmap M.fromList $
    replicateM (fromIntegral lu) $ do
      ss <- parseShortString
      ftv <- parseFieldTableValue
      pure (ss, ftv)

buildFieldTable :: FieldTable -> ByteString.Builder
buildFieldTable m =
  let tups = M.toList m
      buildFieldTablePair ss ftv = mconcat [buildShortString ss, buildFieldTableValue ftv]
   in -- TODO This fromIntegral is not safe, use a newtype with a validity constraint
      mconcat $ buildLongUInt (fromIntegral (length tups)) : map (uncurry buildFieldTablePair) tups

parseFieldTableValue :: Parser FieldTableValue
parseFieldTableValue = do
  w <- anyWord8
  case chr (fromIntegral w) of -- Safe because it is Word8 -> Int
    't' -> FieldTableBit <$> parseBit
    'b' -> FieldTableShortShortInt <$> parseShortShortInt
    'B' -> FieldTableShortShortUInt <$> parseShortShortUInt
    'U' -> FieldTableShortInt <$> parseShortInt
    'u' -> FieldTableShortUInt <$> parseShortUInt
    'I' -> FieldTableLongInt <$> parseLongInt
    'i' -> FieldTableLongUInt <$> parseLongUInt
    'L' -> FieldTableLongLongInt <$> parseLongLongInt
    'l' -> FieldTableLongLongUInt <$> parseLongLongUInt
    'f' -> FieldTableFloat <$> parseFloat
    'd' -> FieldTableDouble <$> parseDouble
    'D' -> FieldTableDecimal <$> parseDecimalValue
    's' -> FieldTableShortString <$> parseShortString
    'S' -> FieldTableLongString <$> parseLongString
    'T' -> FieldTableTimestamp <$> parseTimestamp
    'A' -> FieldTableArray <$> parseFieldTableArray
    'F' -> FieldTableFieldTable <$> parseFieldTable
    'V' -> pure FieldTableVoid
    -- fromIntegral is safe because it's Word8 -> Int
    _ -> fail $ "Unknown field table value type: " <> show (chr (fromIntegral w))

buildFieldTableValue :: FieldTableValue -> ByteString.Builder
buildFieldTableValue =
  let p c b =
        mconcat
          [ SBB.char8 c,
            b
          ]
   in \case
        FieldTableBit b -> p 't' (buildBit b)
        FieldTableShortShortInt ssi -> p 'b' (buildShortShortInt ssi)
        FieldTableShortShortUInt ssu -> p 'B' (buildShortShortUInt ssu)
        FieldTableShortInt si -> p 'U' (buildShortInt si)
        FieldTableShortUInt su -> p 'u' (buildShortUInt su)
        FieldTableLongInt li -> p 'I' (buildLongInt li)
        FieldTableLongUInt lu -> p 'i' (buildLongUInt lu)
        FieldTableLongLongInt lli -> p 'L' (buildLongLongInt lli)
        FieldTableLongLongUInt llu -> p 'l' (buildLongLongUInt llu)
        FieldTableFloat f -> p 'f' (buildFloat f)
        FieldTableDouble d -> p 'd' (buildDouble d)
        FieldTableDecimal dv -> p 'D' (buildDecimalValue dv)
        FieldTableShortString ss -> p 's' (buildShortString ss)
        FieldTableLongString ls -> p 'S' (buildLongString ls)
        FieldTableTimestamp ts -> p 'T' (buildTimestamp ts)
        FieldTableArray a -> p 'A' (buildFieldTableArray a)
        FieldTableFieldTable ft -> p 'F' (buildFieldTable ft)
        FieldTableVoid -> p 'V' mempty

parseFieldTableArray :: Parser [FieldTableValue]
parseFieldTableArray = do
  i <- parseLongInt
  -- Safe because it is Int32 -> Int
  replicateM (fromIntegral i) parseFieldTableValue

buildFieldTableArray :: [FieldTableValue] -> ByteString.Builder
buildFieldTableArray vs =
  mconcat $
    -- TODO not safe, use a newtype instead
    buildLongInt (fromIntegral (length vs)) : map buildFieldTableValue vs

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
