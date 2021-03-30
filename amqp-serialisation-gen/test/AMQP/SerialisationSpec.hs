{-# LANGUAGE OverloadedStrings #-}

module AMQP.SerialisationSpec (spec) where

import AMQP.Serialisation
import AMQP.Serialisation.Base
import AMQP.Serialisation.Frame
import AMQP.Serialisation.Gen ()
import AMQP.Serialisation.Generated.Methods
import AMQP.Serialisation.Methods.Gen ()
import AMQP.Serialisation.TestUtils
import Control.Monad
import Data.Attoparsec.ByteString
import qualified Data.ByteString as SB
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import Data.Char as Char
import qualified Data.Map as M
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "buildProtocolHeader" $ do
    it "is 8 octets long" $ LB.length (SBB.toLazyByteString (buildProtocolHeader protocolHeader)) `shouldBe` 8
    it "stays the same" $ pureGoldenByteStringBuilderFile "test_resources/protocol-header.dat" (buildProtocolHeader protocolHeader)

  describe "parseProtocolHeader" $
    it "can parse whatever 'buildProtocolHeader' builds'" $
      roundtrips buildProtocolHeader parseProtocolHeader

  describe "buildFrameType" $
    forM_ [minBound .. maxBound] $ \ft ->
      it (unwords ["renders the", show ft, "frame type the same as before"]) $
        pureGoldenByteStringBuilderFile ("test_resources/frame-type/" <> map Char.toLower (show ft)) (buildFrameType ft)

  describe "parseFrameType" $
    it "can parse whatever 'buildFrameType' builds'" $
      roundtrips buildFrameType parseFrameType

  describe "parseRawFrame" $
    it "can parse whatever 'buildRawFrame' builds'" $
      roundtrips buildRawFrame parseRawFrame

  describe "parseMethodFrame" $
    it "can parse whatever 'buildMethodFrame' builds'" $
      forAllValid $ \cn ->
        roundtripsWithFloat (buildMethodFrame cn) parseMethodFrame

  describe "parseConnectionStartMethodFramePayload" $
    it "can parse the example that we got from the rabbitmq server" $ do
      payload <- SB.readFile "test_resources/connection-start.dat"
      case parseOnly parseGivenMethodFramePayload payload of
        Left err ->
          expectationFailure $
            unlines
              [ "Parsing the connection start method frame payload failed: ",
                err
              ]
        Right cs ->
          cs
            `shouldBe` ConnectionStart
              { connectionStartVersionMajor = 0,
                connectionStartVersionMinor = 9,
                connectionStartServerProperties =
                  FieldTable
                    { fieldTableMap =
                        M.fromList
                          [ ( "capabilities",
                              FieldTableFieldTable
                                ( FieldTable
                                    { fieldTableMap =
                                        M.fromList
                                          [ ("authentication_failure_close", FieldTableBit True),
                                            ("basic.nack", FieldTableBit True),
                                            ("connection.blocked", FieldTableBit True),
                                            ("consumer_cancel_notify", FieldTableBit True),
                                            ("consumer_priorities", FieldTableBit True),
                                            ("direct_reply_to", FieldTableBit True),
                                            ("exchange_exchange_bindings", FieldTableBit True),
                                            ("per_consumer_qos", FieldTableBit True),
                                            ("publisher_confirms", FieldTableBit True)
                                          ]
                                    }
                                )
                            ),
                            ("cluster_name", "rabbit@nona"),
                            ("copyright", "Copyright (c) 2007-2020 VMware, Inc. or its affiliates."),
                            ("information", "Licensed under the MPL 1.1. Website: https://rabbitmq.com"),
                            ("platform", "Erlang/OTP 22.3"),
                            ("product", "RabbitMQ"),
                            ("version", "3.8.5")
                          ]
                    },
                connectionStartMechanisms = LongString {longStringBytes = "PLAIN AMQPLAIN"},
                connectionStartLocales = LongString {longStringBytes = "en_US"}
              }
