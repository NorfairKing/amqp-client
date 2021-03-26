{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
  | -- TODO replace this frame with the more specific Connection.Start method
    -- frame once we generate code for that.
    ProtocolProposed !ConnectionStartMethodFrame
  deriving (Show, Eq, Generic)

parseProtocolNegotiationResponse :: Parser ProtocolNegotiationResponse
parseProtocolNegotiationResponse =
  label "ProtocolNegotiationResponse" $
    choice
      [ ProtocolRejected <$> parseProtocolHeader,
        ProtocolProposed <$> parseConnectionStartMethodFrame
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
parseConnectionStartMethodFrame = parseMethodFrame 10 10 parseConnectionStartMethodFramePayload'

parseConnectionStartMethodFramePayload :: Parser ConnectionStartMethodFrame
parseConnectionStartMethodFramePayload = parseMethodFramePayload 10 10 parseConnectionStartMethodFramePayload'

parseConnectionStartMethodFramePayload' :: Parser ConnectionStartMethodFrame
parseConnectionStartMethodFramePayload' = label "ConnectionStartMethodFrame" $ do
  connectionStartMethodFrameVersionMajor <- parseOctet
  connectionStartMethodFrameVersionMinor <- parseOctet
  connectionStartMethodFrameServerProperties <- parseFieldTable
  connectionStartMethodFrameMechanism <- parseLongString
  connectionStartMethodFrameLocales <- parseLongString
  pure ConnectionStartMethodFrame {..}

data ConnectionStartOkMethodFrame = ConnectionStartOkMethodFrame
  { connectionStartOkMethodFrameClientProperties :: !PeerProperties,
    connectionStartOkMethodFrameMechanism :: !ShortString,
    connectionStartOkMethodFrameResponse :: !LongString,
    connectionStartOkMethodFrameLocale :: !ShortString
  }
  deriving (Show, Eq, Generic)

parseConnectionStartOkMethodFrame :: Parser ConnectionStartOkMethodFrame
parseConnectionStartOkMethodFrame = parseMethodFrame 10 11 parseConnectionStartOkMethodFrameArguments

parseConnectionStartOkMethodFramePayload :: Parser ConnectionStartOkMethodFrame
parseConnectionStartOkMethodFramePayload = parseMethodFramePayload 10 11 parseConnectionStartOkMethodFrameArguments

parseConnectionStartOkMethodFrameArguments :: Parser ConnectionStartOkMethodFrame
parseConnectionStartOkMethodFrameArguments = label "ConnectionStartOkMethodFrame" $ do
  connectionStartOkMethodFrameClientProperties <- parseFieldTable
  connectionStartOkMethodFrameMechanism <- parseShortString
  connectionStartOkMethodFrameResponse <- parseLongString
  connectionStartOkMethodFrameLocale <- parseShortString
  pure ConnectionStartOkMethodFrame {..}

buildConnectionStartOkMethodFrame :: ConnectionStartOkMethodFrame -> ByteString.Builder
buildConnectionStartOkMethodFrame = buildMethodFrame 0 10 11 . connectionStartOkMethodFrameArguments

buildConnectionStartOkMethodFramePayload :: ConnectionStartOkMethodFrame -> ByteString.Builder
buildConnectionStartOkMethodFramePayload = buildMethodFramePayload 10 11 . connectionStartOkMethodFrameArguments

connectionStartOkMethodFrameArguments :: ConnectionStartOkMethodFrame -> [Argument]
connectionStartOkMethodFrameArguments ConnectionStartOkMethodFrame {..} =
  [ FieldTableFieldTable connectionStartOkMethodFrameClientProperties,
    FieldTableShortString connectionStartOkMethodFrameMechanism,
    FieldTableLongString connectionStartOkMethodFrameResponse,
    FieldTableShortString connectionStartOkMethodFrameLocale
  ]

parseMethodFrame :: ClassId -> MethodId -> Parser a -> Parser a
parseMethodFrame cid mid p = label "Method Frame" $ do
  RawFrame {..} <- parseRawFrame
  case rawFrameType of
    MethodFrameType -> case parseOnly (parseMethodFramePayload cid mid p) rawFramePayload of
      Left err -> fail err
      Right r -> pure r
    ft -> fail $ unwords ["Got a frame of type", show ft, "instead of a method frame."]

parseMethodFramePayload :: ClassId -> MethodId -> Parser a -> Parser a
parseMethodFramePayload cid mid p = label "Method Payload" $ do
  label "class" $ void $ Parse.word16be cid
  label "method" $ void $ Parse.word16be mid
  label "arguments" p

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
    map buildFieldTableValue as
