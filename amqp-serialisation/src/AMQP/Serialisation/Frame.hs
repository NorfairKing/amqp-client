{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module AMQP.Serialisation.Frame where

import AMQP.Serialisation.Argument
import AMQP.Serialisation.Base
import AMQP.Serialisation.Generated.Constants
import AMQP.Serialisation.Generated.DomainTypes
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

data FrameType
  = MethodFrameType
  | ContentHeaderFrameType
  | ContentBodyFrameType
  | HeartbeatFrameType
  deriving (Show, Eq, Generic, Enum, Bounded)

instance Validity FrameType

buildFrameType :: FrameType -> ByteString.Builder
buildFrameType = \case
  MethodFrameType -> SBB.word8 frameMethod
  ContentHeaderFrameType -> SBB.word8 frameHeader
  ContentBodyFrameType -> SBB.word8 frameBody
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
      (frameHeader, pure ContentHeaderFrameType),
      (frameBody, pure ContentBodyFrameType),
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

buildGivenMethodFramePayload :: forall a. IsMethod a => a -> ByteString.Builder
buildGivenMethodFramePayload a =
  mconcat
    [ buildShortUInt (methodClassId (Proxy :: Proxy a)),
      buildShortUInt (methodMethodId (Proxy :: Proxy a)),
      buildArguments (buildMethodArguments a)
    ]

givenMethodFrameToRawFrame :: forall a. IsMethod a => ChannelNumber -> a -> RawFrame
givenMethodFrameToRawFrame chan a =
  RawFrame
    { rawFrameType = MethodFrameType,
      rawFrameChannel = chan,
      rawFramePayload = LB.toStrict $ SBB.toLazyByteString $ buildGivenMethodFramePayload a
    }

buildGivenMethodFrame :: forall a. IsMethod a => ChannelNumber -> a -> ByteString.Builder
buildGivenMethodFrame chan a =
  buildRawFrame $ givenMethodFrameToRawFrame chan a

parseGivenMethodFrame :: IsMethod a => Parser (ChannelNumber, a)
parseGivenMethodFrame = label "Method Frame" $ do
  RawFrame {..} <- parseRawFrame
  case rawFrameType of
    MethodFrameType -> case parseOnly parseGivenMethodFramePayload rawFramePayload of
      Left err -> fail err
      Right r -> pure (rawFrameChannel, r)
    ft -> fail $ unwords ["Got a frame of type", show ft, "instead of a method frame."]

parseGivenMethodFramePayload :: forall a. IsMethod a => Parser a
parseGivenMethodFramePayload = label "Method Payload" $ do
  label "class" $ void $ Parse.word16be $ methodClassId (Proxy :: Proxy a)
  label "method" $ void $ Parse.word16be $ methodMethodId (Proxy :: Proxy a)
  label "arguments" parseMethodArguments

parseMethodFramePayloadHelper :: (ClassId -> MethodId -> Parser a) -> Parser a
parseMethodFramePayloadHelper func = label "Method Payload" $ do
  cid <- label "ClassId" Parse.anyWord16be
  mid <- label "MethodId" Parse.anyWord16be
  label "Arguments" $ func cid mid

data ContentHeaderFrame a = ContentHeaderFrame
  { -- | Unused, must be zero
    -- contentHeaderFrameWeight :: !ShortUInt,
    contentHeaderFrameBodySize :: !LongLongUInt,
    contentHeaderFrameProperties :: !a
  }
  deriving (Show, Eq, Generic, Functor)

instance Validity a => Validity (ContentHeaderFrame a)

buildGivenContentHeaderFramePayload :: forall a. IsContentHeader a => ContentHeaderFrame a -> ByteString.Builder
buildGivenContentHeaderFramePayload ContentHeaderFrame {..} =
  mconcat
    [ buildShortUInt (contentHeaderClassId (Proxy :: Proxy a)),
      buildShortUInt 0, -- Weight, must be zero
      buildLongLongUInt contentHeaderFrameBodySize,
      buildPropertyArguments $ buildContentHeaderArguments contentHeaderFrameProperties
    ]

parseGivenContentHeaderFramePayload :: forall a. IsContentHeader a => Parser (ContentHeaderFrame a)
parseGivenContentHeaderFramePayload = label "Content Header" $ do
  label "ClassId" $ void $ Parse.word16be $ contentHeaderClassId (Proxy :: Proxy a)
  label "Weight" $ void $ Parse.word16be 0
  contentHeaderFrameBodySize <- label "Body Size" parseLongLongUInt
  contentHeaderFrameProperties <- label "Properties" parseContentHeaderArguments
  pure ContentHeaderFrame {..}

parseContentHeaderFramePayloadHelper :: (ClassId -> Parser a) -> Parser (ContentHeaderFrame a)
parseContentHeaderFramePayloadHelper func = label "Content Header Payload" $ do
  cid <- label "ClassId" Parse.anyWord16be
  label "Weight" $ void $ Parse.word16be 0
  contentHeaderFrameBodySize <- label "Body Size" parseLongLongUInt
  contentHeaderFrameProperties <- label "Properties" $ func cid
  pure ContentHeaderFrame {..}

givenContentHeaderFrameToRawFrame :: forall a. IsContentHeader a => ChannelNumber -> ContentHeaderFrame a -> RawFrame
givenContentHeaderFrameToRawFrame chan a =
  RawFrame
    { rawFrameType = ContentHeaderFrameType,
      rawFrameChannel = chan,
      rawFramePayload = LB.toStrict $ SBB.toLazyByteString $ buildGivenContentHeaderFramePayload a
    }

buildGivenContentHeaderFrame :: forall a. IsContentHeader a => ChannelNumber -> ContentHeaderFrame a -> ByteString.Builder
buildGivenContentHeaderFrame chan a =
  buildRawFrame $ givenContentHeaderFrameToRawFrame chan a

parseGivenContentHeaderFrame :: IsContentHeader a => Parser (ChannelNumber, ContentHeaderFrame a)
parseGivenContentHeaderFrame = label "Content Header Frame" $ do
  RawFrame {..} <- parseRawFrame
  case rawFrameType of
    ContentHeaderFrameType -> case parseOnly parseGivenContentHeaderFramePayload rawFramePayload of
      Left err -> fail err
      Right r -> pure (rawFrameChannel, r)
    ft -> fail $ unwords ["Got a frame of type", show ft, "instead of a content header frame."]

newtype ContentBody = ContentBody
  { contentBodyPayload :: ByteString
  }
  deriving (Show, Eq, Generic)

instance Validity ContentBody

parseContentBodyFrame :: Parser (ChannelNumber, ContentBody)
parseContentBodyFrame = label "Content Body Frame" $ do
  RawFrame {..} <- parseRawFrame
  case rawFrameType of
    ContentBodyFrameType -> pure (rawFrameChannel, ContentBody {contentBodyPayload = rawFramePayload})
    ft -> fail $ unwords ["Got a frame of type", show ft, "instead of a content body frame."]

contentBodyToRawFrame :: ChannelNumber -> ContentBody -> RawFrame
contentBodyToRawFrame chan ContentBody {..} =
  RawFrame
    { rawFrameType = ContentBodyFrameType,
      rawFrameChannel = chan,
      rawFramePayload = contentBodyPayload
    }

buildContentBodyFrame :: ChannelNumber -> ContentBody -> ByteString.Builder
buildContentBodyFrame chan cb = buildRawFrame $ contentBodyToRawFrame chan cb

parseHeartbeatFrame :: Parser ()
parseHeartbeatFrame = do
  RawFrame {..} <- parseRawFrame
  case rawFrameType of
    HeartbeatFrameType -> do
      when (rawFrameChannel /= 0) $ fail $ unwords ["Supposed heartbeat frame had a channel of", show rawFrameChannel, "instead of 0"]
      pure ()
    ft -> fail $ unwords ["Got a frame of type", show ft, "instead of a heartbeat frame."]

heartbeatFrame :: RawFrame
heartbeatFrame =
  RawFrame
    { rawFrameType = HeartbeatFrameType,
      rawFrameChannel = 0,
      rawFramePayload = SB.empty
    }

buildHeartbeatFrame :: ByteString.Builder
buildHeartbeatFrame = buildRawFrame heartbeatFrame
