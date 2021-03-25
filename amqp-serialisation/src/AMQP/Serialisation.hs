{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AMQP.Serialisation where

import Data.ByteString (ByteString)
import Data.ByteString.Builder as ByteString (Builder)
import qualified Data.ByteString.Builder as SBB
import Data.Word
import GHC.Generics (Generic)

protocolHeader :: ByteString.Builder
protocolHeader = "AMQP0091"

data Frame = Frame
  { frameType :: !Word8,
    frameChannel :: !Word16,
    frameSize :: !Word32,
    framePayload :: !ByteString
  }
  deriving (Show, Eq, Generic)

buildFrame :: Frame -> ByteString.Builder
buildFrame Frame {..} =
  mconcat
    [ SBB.word8 frameType,
      SBB.word16LE frameChannel, -- QUESTION: Should this be little-endian?
      SBB.word32LE frameSize, -- QUESTION: Should this be little-endian?
      SBB.byteString framePayload,
      SBB.word8 frameEnd
    ]

frameEnd :: Word8
frameEnd = 206 -- TODO get this from the spec
