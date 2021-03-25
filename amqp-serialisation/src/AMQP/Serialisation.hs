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

data Frame = Frame
  { frameType :: !FrameType,
    frameChannel :: !Word16,
    framePayload :: !ByteString
  }
  deriving (Show, Eq, Generic)

instance Validity Frame

buildFrame :: Frame -> ByteString.Builder
buildFrame Frame {..} =
  mconcat
    [ buildFrameType frameType,
      -- QUESTION: Should this be little-endian?
      -- ANSWER: No, the spec says that it must be network byte order, or big-endian.
      SBB.word16BE frameChannel,
      -- QUESTION: Should this be little-endian?
      -- ANSWER: the spec says that it must be network byte order, or big-endian.
      --
      -- The 'fromIntegral' should be safe because we cast from Int to Word32.
      SBB.word32BE (fromIntegral (SB.length framePayload)),
      SBB.byteString framePayload,
      SBB.word8 frameEnd
    ]

frameEnd :: Word8
frameEnd = 206 -- TODO get this from the spec

parseFrame :: Parser Frame
parseFrame = do
  frameType <- parseFrameType
  frameChannel <- anyWord16be
  frameLength <- anyWord32be
  framePayload <- Parse.take (fromIntegral frameLength)
  void $ Parse.word8 frameEnd
  pure Frame {..}

data ProtocolNegotiationResponse
  = ProtocolRejected ProtocolHeader
  | -- TODO replace this frame with the more specific Connection.Start method
    -- frame once we generate code for that.
    ProtocolProposed Frame
  deriving (Show, Eq, Generic)

parseProtocolNegotiationResponse :: Parser ProtocolNegotiationResponse
parseProtocolNegotiationResponse =
  choice
    [ ProtocolRejected <$> parseProtocolHeader,
      ProtocolProposed <$> parseFrame
    ]
