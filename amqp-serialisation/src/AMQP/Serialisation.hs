{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AMQP.Serialisation where

import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.ByteString.Builder as ByteString (Builder)
import qualified Data.ByteString.Builder as SBB
import Data.Validity
import Data.Validity.ByteString
import Data.Word
import GHC.Generics (Generic)

protocolHeader :: ByteString.Builder
protocolHeader = "AMQP0091"

data FrameType
  = MethodFrame
  | HeaderFrame
  | BodyFrame
  | HeartbeatFrame
  deriving (Show, Eq, Generic, Enum, Bounded)

-- TODO get these from the spec
buildFrameType :: FrameType -> ByteString.Builder
buildFrameType = \case
  MethodFrame -> SBB.word8 1
  HeaderFrame -> SBB.word8 2
  BodyFrame -> SBB.word8 3
  -- QUESTION: The pdf says 4 but the spec says 8, which is it?
  HeartbeatFrame -> SBB.word8 8

instance Validity FrameType

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
      SBB.word16LE frameChannel, -- QUESTION: Should this be little-endian?
      -- The 'fromIntegral' should be safe because we cast from Int to Word32.
      SBB.word32LE (fromIntegral (SB.length framePayload)), -- QUESTION: Should this be little-endian?
      SBB.byteString framePayload,
      SBB.word8 frameEnd
    ]

frameEnd :: Word8
frameEnd = 206 -- TODO get this from the spec
