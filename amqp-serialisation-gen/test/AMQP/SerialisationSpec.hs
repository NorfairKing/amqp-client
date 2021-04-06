{-# LANGUAGE OverloadedStrings #-}

module AMQP.SerialisationSpec (spec) where

import AMQP.Serialisation
import AMQP.Serialisation.Base
import AMQP.Serialisation.Content.Gen ()
import AMQP.Serialisation.Frame
import AMQP.Serialisation.Frame.Gen ()
import AMQP.Serialisation.Gen ()
import AMQP.Serialisation.Generated.Methods
import AMQP.Serialisation.Methods.Gen ()
import AMQP.Serialisation.TestUtils
import Data.Attoparsec.ByteString
import qualified Data.ByteString as SB
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
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

  -- Important tests with lots of cases
  modifyMaxSuccess (* 10) $
    modifyMaxSize (* 10) $ do
      describe "parseMethodFrame" $
        it "can parse whatever 'buildMethodFrame' builds'" $
          forAllValid $ \cn ->
            roundtripsWithFloat (buildMethodFrame cn) parseMethodFrame

      describe "parseContentHeaderFrame" $
        it "can parse whatever 'buildContentHeaderFrame' builds'" $
          forAllValid $ \cn ->
            roundtripsWithFloat (buildContentHeaderFrame cn) parseContentHeaderFrame

      describe "parseFrame" $
        it "can parse whataver 'buildFrame' builds'" $
          roundtripsWithFloat buildFrame parseFrame

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
