{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module AMQP.Serialisation where

import AMQP.Serialisation.Argument
import AMQP.Serialisation.Base
import AMQP.Serialisation.Generated
import Control.Monad
import Data.Attoparsec.Binary as Parse
import Data.Attoparsec.ByteString as Parse
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.ByteString.Builder as ByteString (Builder)
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import Data.Proxy
import Data.Validity
import Data.Validity.ByteString ()
import Data.Validity.Containers ()
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

buildFrameType :: FrameType -> ByteString.Builder
buildFrameType = \case
  MethodFrameType -> SBB.word8 frameMethod
  HeaderFrameType -> SBB.word8 frameHeader
  BodyFrameType -> SBB.word8 frameBody
  -- QUESTION: The pdf says 4 but the spec says 8, which is it?
  -- ANSWER: We'll go with what the spec says.
  HeartbeatFrameType -> SBB.word8 frameHeartbeat

parseFrameType :: Parser FrameType
parseFrameType = label "FrameType" $ do
  w <- Parse.anyWord8
  tableCaseMatch
    w
    (fail ("Unknown frame type: " <> show w))
    [ (frameMethod, pure MethodFrameType),
      (frameHeader, pure HeaderFrameType),
      (frameBody, pure BodyFrameType),
      (frameHeartbeat, pure HeartbeatFrameType)
    ]

tableCaseMatch :: Eq a => a -> b -> [(a, b)] -> b
tableCaseMatch a def vals = fromMaybe def $ lookup a vals

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
      SBB.word8 frameEnd
    ]

parseRawFrame :: Parser RawFrame
parseRawFrame = label "RawFrame" $ do
  rawFrameType <- parseFrameType
  rawFrameChannel <- parseChannelNumber
  rawFrameLength <- anyWord32be
  rawFramePayload <- Parse.take (fromIntegral rawFrameLength)
  void $ Parse.word8 frameEnd
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

buildMethodFrame :: forall a. Method a => ChannelNumber -> a -> ByteString.Builder
buildMethodFrame chan a =
  buildRawFrame $
    RawFrame
      { rawFrameType = MethodFrameType,
        rawFrameChannel = chan,
        rawFramePayload = LB.toStrict $ SBB.toLazyByteString $ buildMethodFramePayload a
      }

buildMethodFramePayload :: forall a. Method a => a -> ByteString.Builder
buildMethodFramePayload a =
  mconcat $
    buildShortUInt (methodClassId (Proxy :: Proxy a)) :
    buildShortUInt (methodMethodId (Proxy :: Proxy a)) :
    map buildArgument (buildMethodArguments a)
