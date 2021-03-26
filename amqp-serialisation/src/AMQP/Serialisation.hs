{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module AMQP.Serialisation where

import AMQP.Serialisation.Base
import Control.Monad
import Data.Attoparsec.Binary as Parse
import Data.Attoparsec.ByteString as Parse
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.ByteString.Builder as ByteString (Builder)
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import Data.Proxy
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Containers ()
import Data.Word
import GHC.Generics (Generic)

data ProtocolHeader = ProtocolHeader
  { protocolHeaderMajor :: !Octet,
    protocolHeaderMinor :: !Octet,
    protocolHeaderRevision :: !Octet
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
      buildOctet 0,
      buildOctet protocolHeaderMajor,
      buildOctet protocolHeaderMinor,
      buildOctet protocolHeaderRevision
    ]

parseProtocolHeader :: Parser ProtocolHeader
parseProtocolHeader = label "ProtocolHeader" $ do
  void $ Parse.string "AMQP"
  void $ Parse.word8 0
  protocolHeaderMajor <- parseOctet
  protocolHeaderMinor <- parseOctet
  protocolHeaderRevision <- parseOctet
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
parseFrameType = label "FrameType" $ do
  w <- Parse.anyWord8
  -- TODO get these from the spec
  case w of
    1 -> pure MethodFrameType
    2 -> pure HeaderFrameType
    3 -> pure BodyFrameType
    8 -> pure HeartbeatFrameType
    _ -> fail $ "Unknown frame type: " <> show w

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
parseRawFrame = label "RawFrame" $ do
  rawFrameType <- parseFrameType
  rawFrameChannel <- parseChannelNumber
  rawFrameLength <- anyWord32be
  rawFramePayload <- Parse.take (fromIntegral rawFrameLength)
  void $ Parse.word8 rawFrameEnd
  pure RawFrame {..}

data ProtocolNegotiationResponse
  = ProtocolRejected ProtocolHeader
  | ProtocolProposed !ConnectionStart
  deriving (Show, Eq, Generic)

parseProtocolNegotiationResponse :: Parser ProtocolNegotiationResponse
parseProtocolNegotiationResponse =
  label "ProtocolNegotiationResponse" $
    choice
      [ ProtocolRejected <$> parseProtocolHeader,
        ProtocolProposed <$> parseMethodFrame
      ]

data MethodFrame = MethodFrame
  { methodFrameChannel :: !ChannelNumber,
    methodFramePayload :: !MethodFramePayload
  }
  deriving (Show, Eq, Generic)

data MethodFramePayload
  = ConnectionStartFrame !ConnectionStart
  deriving (Show, Eq, Generic)

parseMethodFrame :: Method a => Parser a
parseMethodFrame = label "Method Frame" $ do
  RawFrame {..} <- parseRawFrame
  case rawFrameType of
    MethodFrameType -> case parseOnly parseMethodFramePayload rawFramePayload of
      Left err -> fail err
      Right r -> pure r
    ft -> fail $ unwords ["Got a frame of type", show ft, "instead of a method frame."]

parseMethodFramePayload :: forall a. Method a => Parser a
parseMethodFramePayload = label "Method Payload" $ do
  label "class" $ void $ Parse.word16be $ methodClassId (Proxy :: Proxy a)
  label "method" $ void $ Parse.word16be $ methodMethodId (Proxy :: Proxy a)
  label "arguments" parseMethodArguments

buildMethodFrame :: ChannelNumber -> ClassId -> MethodId -> [Argument] -> ByteString.Builder
buildMethodFrame chan cid mid as =
  buildRawFrame $
    RawFrame
      { rawFrameType = MethodFrameType,
        rawFrameChannel = chan,
        rawFramePayload = LB.toStrict $ SBB.toLazyByteString $ buildMethodFramePayload cid mid as
      }

buildMethodFramePayload :: ClassId -> MethodId -> [Argument] -> ByteString.Builder
buildMethodFramePayload cid mid as =
  mconcat $
    buildShortUInt cid :
    buildShortUInt mid :
    map buildArgument as

class Method a where
  methodClassId :: Proxy a -> ClassId
  methodMethodId :: Proxy a -> MethodId
  buildMethodArguments :: a -> [Argument]
  parseMethodArguments :: Parser a

class IsArgument a where
  toArgument :: a -> Argument
  parseArgument :: Parser a

instance IsArgument Bit where
  toArgument = ArgumentBit
  parseArgument = parseBit

instance IsArgument Octet where
  toArgument = ArgumentOctet
  parseArgument = parseOctet

instance IsArgument LongUInt where
  toArgument = ArgumentLongUInt
  parseArgument = parseLongUInt

instance IsArgument LongLongUInt where
  toArgument = ArgumentLongLongUInt
  parseArgument = parseLongLongUInt

instance IsArgument ShortString where
  toArgument = ArgumentShortString
  parseArgument = parseShortString

instance IsArgument LongString where
  toArgument = ArgumentLongString
  parseArgument = parseLongString

instance IsArgument Timestamp where
  toArgument = ArgumentTimestamp
  parseArgument = parseTimestamp

instance IsArgument FieldTable where
  toArgument = ArgumentFieldTable
  parseArgument = parseFieldTable

data ConnectionStart = ConnectionStart
  { connectionStartVersionMajor :: !Octet,
    connectionStartVersionMinor :: !Octet,
    connectionStartServerProperties :: !PeerProperties,
    connectionStartMechanism :: !LongString,
    connectionStartLocales :: !LongString
  }
  deriving (Show, Eq, Generic)

instance Method ConnectionStart where
  methodClassId Proxy = 10
  methodMethodId Proxy = 10
  buildMethodArguments ConnectionStart {..} =
    [toArgument connectionStartVersionMajor, toArgument connectionStartVersionMinor, toArgument connectionStartServerProperties, toArgument connectionStartMechanism, toArgument connectionStartLocales]
  parseMethodArguments =
    ConnectionStart
      <$> parseArgument
      <*> parseArgument
      <*> parseArgument
      <*> parseArgument
      <*> parseArgument

data ConnectionStartOk = ConnectionStartOk
  { connectionStartOkClientProperties :: !PeerProperties,
    connectionStartOkMechanism :: !ShortString,
    connectionStartOkResponse :: !LongString,
    connectionStartOkLocale :: !ShortString
  }
  deriving (Show, Eq, Generic)

-- These should be easy to generate
instance Method ConnectionStartOk where
  methodClassId Proxy = 10
  methodMethodId Proxy = 11
  buildMethodArguments ConnectionStartOk {..} =
    [ toArgument connectionStartOkClientProperties,
      toArgument connectionStartOkMechanism,
      toArgument connectionStartOkResponse,
      toArgument connectionStartOkLocale
    ]
  parseMethodArguments =
    ConnectionStartOk
      <$> parseArgument
      <*> parseArgument
      <*> parseArgument
      <*> parseArgument
