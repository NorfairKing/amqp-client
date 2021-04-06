{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module AMQP.Serialisation where

import AMQP.Serialisation.Base
import AMQP.Serialisation.Frame
import AMQP.Serialisation.Generated.Content
import AMQP.Serialisation.Generated.Methods
import Control.Monad
import Data.Attoparsec.ByteString as Parse
import Data.ByteString.Builder as ByteString (Builder)
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
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

data ProtocolNegotiationResponse
  = ProtocolRejected ProtocolHeader
  | ProtocolProposed !ConnectionStart
  deriving (Show, Eq, Generic)

parseProtocolNegotiationResponse :: Parser ProtocolNegotiationResponse
parseProtocolNegotiationResponse =
  label "ProtocolNegotiationResponse" $
    choice
      [ ProtocolRejected <$> parseProtocolHeader,
        ProtocolProposed . snd <$> parseGivenMethodFrame
      ]

parseMethodFrame :: Parser Method
parseMethodFrame = label "Method Frame" $ do
  rf@RawFrame {..} <- parseRawFrame
  case rawFrameType of
    MethodFrameType -> case parseOnly parseMethodFramePayload rawFramePayload of
      Left err -> fail err
      Right r -> pure r
    ft ->
      fail $
        unlines
          [ unwords ["Unable to parse method because the frame type was not method but ", show ft],
            show rf
          ]

buildMethodFrame :: ChannelNumber -> Method -> ByteString.Builder
buildMethodFrame chan m =
  buildRawFrame $
    RawFrame
      { rawFrameType = MethodFrameType,
        rawFrameChannel = chan,
        rawFramePayload = LB.toStrict $ SBB.toLazyByteString $ buildMethodFramePayload m
      }

parseContentHeaderFrame :: Parser (ContentHeaderFrame ContentHeader)
parseContentHeaderFrame = label "Content Header Frame" $ do
  rf@RawFrame {..} <- parseRawFrame
  case rawFrameType of
    ContentHeaderFrameType -> case parseOnly parseContentHeaderFramePayload rawFramePayload of
      Left err -> fail err
      Right r -> pure r
    ft ->
      fail $
        unlines
          [ unwords ["Unable to parse content header because the frame type was not content header but ", show ft],
            show rf
          ]

buildContentHeaderFrame :: ChannelNumber -> ContentHeaderFrame ContentHeader -> ByteString.Builder
buildContentHeaderFrame chan m =
  buildRawFrame $
    RawFrame
      { rawFrameType = ContentHeaderFrameType,
        rawFrameChannel = chan,
        rawFramePayload = LB.toStrict $ SBB.toLazyByteString $ buildContentHeaderFramePayload m
      }

data FramePayload
  = MethodPayload !Method
  | ContentHeaderPayload !(ContentHeaderFrame ContentHeader)
  | HeartbeatPayload
  deriving (Show, Eq, Generic)
