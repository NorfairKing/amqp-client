{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AMQP.Serialisation.Base where

import Control.Monad
import Data.Attoparsec.Binary as Parse
import Data.Attoparsec.ByteString as Parse
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.ByteString.Builder as ByteString (Builder)
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import Data.Char
import Data.Int
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.ReinterpretCast as Cast
import Data.String
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Containers ()
import Data.Word
import GHC.Generics (Generic)

type ChannelNumber = Word16

parseChannelNumber :: Parser ChannelNumber
parseChannelNumber = label "ChannelNumber" anyWord16be

buildChannelNumber :: ChannelNumber -> ByteString.Builder
buildChannelNumber = SBB.word16BE

data Argument
  = ArgumentBit !Bit
  | ArgumentOctet !Octet
  | ArgumentShortUInt !ShortUInt
  | ArgumentLongUInt !LongUInt
  | ArgumentLongLongUInt !LongLongUInt
  | ArgumentShortString !ShortString
  | ArgumentLongString !LongString
  | ArgumentTimestamp !Timestamp
  | ArgumentFieldTable !FieldTable
  deriving (Show, Eq, Generic)

instance Validity Argument

buildArgument :: Argument -> ByteString.Builder
buildArgument = \case
  ArgumentBit b -> buildBit b
  ArgumentOctet o -> buildOctet o
  ArgumentShortUInt su -> buildShortUInt su
  ArgumentLongUInt lu -> buildLongUInt lu
  ArgumentLongLongUInt llu -> buildLongLongUInt llu
  ArgumentShortString ss -> buildShortString ss
  ArgumentLongString ls -> buildLongString ls
  ArgumentTimestamp ts -> buildTimestamp ts
  ArgumentFieldTable ft -> buildFieldTable ft

buildArguments :: [Argument] -> ByteString.Builder
buildArguments = go
  where
    go :: [Argument] -> ByteString.Builder
    go [] = mempty
    go (a : as) = case a of
      ArgumentBit b -> goBits [b] as
      _ -> buildArgument a <> go as

    goBits :: [Bit] -> [Argument] -> ByteString.Builder
    goBits acc [] = buildBits $ reverse acc
    goBits acc (a : as) = case a of
      ArgumentBit b -> goBits (b : acc) as
      _ -> buildBits (reverse acc) <> go as

data ArgumentParser a where
  ParseBit :: ArgumentParser Bit
  ParseOctet :: ArgumentParser Octet
  ParseShortUInt :: ArgumentParser ShortUInt
  ParseLongUInt :: ArgumentParser LongUInt
  ParseLongLongUInt :: ArgumentParser LongLongUInt
  ParseShortString :: ArgumentParser ShortString
  ParseLongString :: ArgumentParser LongString
  ParseTimestamp :: ArgumentParser Timestamp
  ParseFieldTable :: ArgumentParser FieldTable
  ParseFail :: String -> ArgumentParser a
  ParsePure :: a -> ArgumentParser a
  ParseFmap :: (a -> b) -> ArgumentParser a -> ArgumentParser b
  ParseApp :: ArgumentParser (a -> b) -> ArgumentParser a -> ArgumentParser b

instance Functor ArgumentParser where
  fmap = ParseFmap

instance Applicative ArgumentParser where
  pure = ParsePure
  (<*>) = ParseApp

runArgumentParser :: ArgumentParser a -> Parser a
runArgumentParser = \case
  ParseBit -> parseBit
  ParseOctet -> parseOctet
  ParseShortUInt -> parseShortUInt
  ParseLongUInt -> parseLongUInt
  ParseLongLongUInt -> parseLongLongUInt
  ParseShortString -> parseShortString
  ParseLongString -> parseLongString
  ParseTimestamp -> parseTimestamp
  ParseFieldTable -> parseFieldTable
  ParsePure a -> pure a
  ParseFmap f p -> f <$> runArgumentParser p
  ParseApp fp p -> runArgumentParser fp <*> runArgumentParser p

newtype FieldTable = FieldTable {fieldTableMap :: Map FieldTableKey FieldTableValue}
  deriving (Show, Eq, Generic)

instance Validity FieldTable where
  validate ft@FieldTable {..} =
    mconcat
      [ genericValidate ft,
        declare "there are fewer than a long uint worth of values in the field table" $
          M.size fieldTableMap <= word32ToInt (maxBound :: LongUInt)
      ]

emptyFieldTable :: FieldTable
emptyFieldTable = FieldTable M.empty

parseFieldTable :: Parser FieldTable
parseFieldTable = label "FieldTable" $ do
  lu <- parseLongUInt -- This is the number of bytes that the fields take up, not the number of fields.
  fieldTableBytes <- Parse.take (word32ToInt lu)
  case parseOnly parseManyFieldTablePairs fieldTableBytes of
    Left err -> fail err
    Right r -> pure $ FieldTable $ M.fromList r

-- This parser assumes that the entire input is filled with field table pairs.
-- The list will be ordered backwards.
parseManyFieldTablePairs :: Parser [(FieldTableKey, FieldTableValue)]
parseManyFieldTablePairs = go []
  where
    go acc = do
      e <- Parse.atEnd
      if e
        then pure acc
        else do
          pair <- parseFieldTablePair
          go (pair : acc)

parseFieldTablePair :: Parser (FieldTableKey, FieldTableValue)
parseFieldTablePair = do
  ss <- label "value name" parseFieldTableKey
  ftv <- label (unwords ["value with name", show ss]) parseFieldTableValue
  pure (ss, ftv)

buildFieldTable :: FieldTable -> ByteString.Builder
buildFieldTable (FieldTable m) =
  let tups = M.toList m
      buildFieldTablePair ss ftv = mconcat [buildFieldTableKey ss, buildFieldTableValue ftv]
      buildFieldTablePairBytes = SBB.toLazyByteString $ mconcat $ map (uncurry buildFieldTablePair) tups
   in -- This fromIntegral is safe because of the validity constraint on FieldTable.
      mconcat
        [ buildLongUInt (fromIntegral (LB.length buildFieldTablePairBytes)),
          SBB.lazyByteString buildFieldTablePairBytes
        ]

newtype FieldTableKey = FieldTableKey {fieldTableKeyString :: ShortString}
  deriving (Show, Eq, Ord, Generic)

instance Validity FieldTableKey where
  validate ftk =
    mconcat
      [ genericValidate ftk,
        declare "The field table key has a valid name" $ do
          -- TODO add the name constraint:
          -- Field names MUST start with a letter, '$' or '#' and may continue
          -- with letters, '$' or '#', digits, or underlines, to a maximum
          -- length of 128 characters.
          True
      ]

-- | Not safe, you could invalidate the constraints
instance IsString FieldTableKey where
  fromString = FieldTableKey . fromString

parseFieldTableKey :: Parser FieldTableKey
parseFieldTableKey = label "FieldTableKey" $ FieldTableKey <$> parseShortString

buildFieldTableKey :: FieldTableKey -> ByteString.Builder
buildFieldTableKey = buildShortString . fieldTableKeyString

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

instance IsString FieldTableValue where
  fromString = FieldTableLongString . fromString

parseFieldTableValue :: Parser FieldTableValue
parseFieldTableValue = label "FieldTableValue" $ do
  w8 <- Parse.anyWord8
  let c = chr (fromIntegral w8)
  case c of
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
    _ -> fail $ "Unknown field table value type: " <> show c

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
parseFieldTableArray = label "FieldTableArray" $ do
  numberOfFields <- parseLongInt
  -- Safe because it is Int32 -> Int
  forM [1 .. int32ToInt numberOfFields] $ \ix ->
    label (unwords ["value", show ix, "of", show numberOfFields]) parseFieldTableValue

buildFieldTableArray :: [FieldTableValue] -> ByteString.Builder
buildFieldTableArray vs =
  mconcat $
    -- TODO not safe, use a newtype instead
    buildLongInt (fromIntegral (length vs)) : map buildFieldTableValue vs

type Bit = Bool

parseBit :: Parser Bit
parseBit = label "Bit" $ do
  w <- Parse.anyWord8
  case w of
    0 -> pure False
    _ -> pure True

buildBit :: Bit -> ByteString.Builder
buildBit = SBB.word8 . bitToWord8

-- | Parse 'n' bits
--
-- This function only works for input 8 or fewer
parseBits :: Word8 -> Parser [Bit]
parseBits n = do
  w <- anyWord8
  pure $ go n w
  where
    go :: Word8 -> Word8 -> [Bit]
    go bitsLeft w
      | bitsLeft <= 0 = []
      | otherwise = odd w : go (pred bitsLeft) (w `div` 2)

-- | Build bits, packed into octets
--
-- This function only works for input list sizes 8 or smaller
buildBits :: [Bit] -> ByteString.Builder
buildBits = SBB.word8 . go
  where
    go :: [Bit] -> Word8
    go [] = 0
    go (b : bs) = 2 * go bs + bitToWord8 b

bitToWord8 :: Bit -> Word8
bitToWord8 = \case
  True -> 1
  False -> 0

type Octet = Word8

parseOctet :: Parser Octet
parseOctet = label "Octet" Parse.anyWord8

buildOctet :: Octet -> ByteString.Builder
buildOctet = SBB.word8

type ShortShortInt = Int8

parseShortShortInt :: Parser ShortShortInt
parseShortShortInt = label "ShortShortInt" $ fromIntegral <$> Parse.anyWord8 -- Safe fromintegral Word8 -> Int8

buildShortShortInt :: ShortShortInt -> ByteString.Builder
buildShortShortInt = SBB.int8

type ShortShortUInt = Word8

parseShortShortUInt :: Parser ShortShortUInt
parseShortShortUInt = label "ShortShortUInt" Parse.anyWord8

buildShortShortUInt :: ShortShortUInt -> ByteString.Builder
buildShortShortUInt = SBB.word8

type ShortInt = Int16

parseShortInt :: Parser ShortInt
parseShortInt = label "ShortInt" $ fromIntegral <$> Parse.anyWord16be -- Safe fromintegral Word16 -> Int16

buildShortInt :: ShortInt -> ByteString.Builder
buildShortInt = SBB.int16BE

type ShortUInt = Word16

parseShortUInt :: Parser ShortUInt
parseShortUInt = label "ShortUInt" Parse.anyWord16be

buildShortUInt :: ShortUInt -> ByteString.Builder
buildShortUInt = SBB.word16BE

type LongInt = Int32

parseLongInt :: Parser LongInt
parseLongInt = label "LongUInt" $ fromIntegral <$> Parse.anyWord32be -- Safe fromintegral Word32 -> Int32

buildLongInt :: LongInt -> ByteString.Builder
buildLongInt = SBB.int32BE

type LongUInt = Word32

parseLongUInt :: Parser LongUInt
parseLongUInt = label "LongUInt" Parse.anyWord32be

buildLongUInt :: LongUInt -> ByteString.Builder
buildLongUInt = SBB.word32BE

type LongLongInt = Int64

parseLongLongInt :: Parser LongLongInt
parseLongLongInt = label "LongLongInt" $ fromIntegral <$> Parse.anyWord64be -- Safe fromintegral Word64 -> Int64

buildLongLongInt :: LongLongInt -> ByteString.Builder
buildLongLongInt = SBB.int64BE

type LongLongUInt = Word64

parseLongLongUInt :: Parser LongLongUInt
parseLongLongUInt = label "LongLongUInt" Parse.anyWord64be

buildLongLongUInt :: LongLongUInt -> ByteString.Builder
buildLongLongUInt = SBB.word64BE

parseFloat :: Parser Float
parseFloat = label "Float" $ Cast.wordToFloat <$> parseLongUInt

buildFloat :: Float -> ByteString.Builder
buildFloat = SBB.floatBE

parseDouble :: Parser Double
parseDouble = label "Double" $ Cast.wordToDouble <$> parseLongLongUInt

buildDouble :: Double -> ByteString.Builder
buildDouble = SBB.doubleBE

data DecimalValue
  = DecimalValue
      !Octet
      !LongUInt
  deriving (Show, Eq, Generic)

instance Validity DecimalValue

parseDecimalValue :: Parser DecimalValue
parseDecimalValue = label "DecimalValue" $ do
  scale <- parseOctet
  mantissa <- parseLongUInt
  pure $ DecimalValue scale mantissa

buildDecimalValue :: DecimalValue -> ByteString.Builder
buildDecimalValue (DecimalValue scale mantissa) =
  mconcat
    [ buildOctet scale,
      buildLongUInt mantissa
    ]

newtype ShortString = ShortString {shortStringBytes :: ByteString}
  deriving (Show, Eq, Ord, Generic)

instance Validity ShortString where
  validate ss@ShortString {..} =
    mconcat
      [ genericValidate ss,
        declare "The short string is shorter than 256 bytes" $ SB.length shortStringBytes <= word8ToInt (maxBound :: Octet),
        declare "The short string does not contain zero bytes" $ SB.all (/= 0) shortStringBytes
      ]

-- | Not safe, you could invalidate the constraints.
instance IsString ShortString where
  fromString = ShortString . fromString

parseShortString :: Parser ShortString
parseShortString = label "ShortString" $ do
  o <- parseOctet
  shortStringBytes <- Parse.take (word8ToInt o)
  guard $ SB.all (/= 0) shortStringBytes
  pure ShortString {..}

buildShortString :: ShortString -> ByteString.Builder
buildShortString ShortString {..} =
  mconcat
    [ buildOctet (fromIntegral (SB.length shortStringBytes)), -- Safe because of the validity constraint on 'ShortString'.
      SBB.byteString shortStringBytes
    ]

newtype LongString = LongString {longStringBytes :: ByteString}
  deriving (Show, Eq, Generic)

instance Validity LongString where
  validate ls@LongString {..} =
    mconcat
      [ genericValidate ls,
        declare "The long string contains fewer than a long uint's worth of bytes" $
          SB.length longStringBytes <= word32ToInt (maxBound :: LongUInt)
      ]

-- | Unsafe, you could make a 'LongString' that is not valid, but good luck with that.
instance IsString LongString where
  fromString = LongString . fromString

parseLongString :: Parser LongString
parseLongString = label "LongString" $ do
  o <- parseLongUInt
  longStringBytes <- Parse.take (word32ToInt o)
  pure LongString {..}

buildLongString :: LongString -> ByteString.Builder
buildLongString LongString {..} =
  mconcat
    [ buildLongUInt (fromIntegral (SB.length longStringBytes)), -- safe because of the validity constraint on 'LongString'.
      SBB.byteString longStringBytes
    ]

newtype Timestamp = Timestamp {timestampWord :: Word64}
  deriving (Show, Eq, Generic)

instance Validity Timestamp

parseTimestamp :: Parser Timestamp
parseTimestamp = label "Timestamp" $ Timestamp <$> parseLongLongUInt

buildTimestamp :: Timestamp -> ByteString.Builder
buildTimestamp = buildLongLongUInt . timestampWord

parseValid :: (Show a, Validity a) => Parser a -> Parser a
parseValid func = do
  r <- func
  case prettyValidate r of
    Left err -> fail $ unlines ["Value was invalid: " <> show r, err]
    Right res -> pure res

label :: String -> Parser a -> Parser a
label = flip (Parse.<?>)

-- TODO while these are safe, they might be slow.
-- look into this if we ever have performance issues

-- Safe
word32ToInt :: Word32 -> Int
word32ToInt = fromIntegral

-- Safe
int32ToInt :: Int32 -> Int
int32ToInt = fromIntegral

-- Safe
word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral
