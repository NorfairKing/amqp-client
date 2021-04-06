{-# LANGUAGE OverloadedStrings #-}

module AMQP.Serialisation.FrameSpec (spec) where

import AMQP.Serialisation.Base
import AMQP.Serialisation.Frame
import AMQP.Serialisation.Frame.Gen ()
import AMQP.Serialisation.Gen ()
import AMQP.Serialisation.Generated.Content
import AMQP.Serialisation.Generated.Methods
import AMQP.Serialisation.Methods.Gen ()
import AMQP.Serialisation.TestUtils
import Control.Monad
import Data.Char as Char
import Test.Syd

spec :: Spec
spec = do
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

  describe "parseGivenMethodFrame" $ do
    describe "QueueDeclare" $ do
      let channelNumber = 42
          exampleQueueDeclare =
            QueueDeclare
              { queueDeclareReserved1 = 0,
                queueDeclareQueue = "example-name",
                queueDeclarePassive = False,
                queueDeclareDurable = True,
                queueDeclareExclusive = False,
                queueDeclareAutoDelete = False,
                queueDeclareNoWait = False,
                queueDeclareArguments = emptyFieldTable
              }
      it "roundtrips on this example" $
        roundtripsFor
          (uncurry buildGivenMethodFrame)
          parseGivenMethodFrame
          (channelNumber, exampleQueueDeclare)
      it "outputs the same as before for this example" $
        pureGoldenByteStringBuilderFile "test_resources/method-frame/queue-declare.dat" (buildGivenMethodFrame channelNumber exampleQueueDeclare)

  describe "parseGivenContentHeader" $ do
    describe "ConnectionContentHeader" $ do
      let exampleConnectionContentHeader = ConnectionContentHeader
      it "roundtrips on this example" $
        roundtripsFor
          buildGivenContentHeader
          parseGivenContentHeader
          exampleConnectionContentHeader
      it "outputs the same as before for this example" $
        pureGoldenByteStringBuilderFile "test_resources/content-header/connection.dat" (buildGivenContentHeader exampleConnectionContentHeader)

  describe "parseGivenContentHeader" $ do
    describe "BasicContentHeader" $ do
      let emptyBasicContentHeader =
            BasicContentHeader
              { basicContentHeaderContentType = Nothing,
                basicContentHeaderContentEncoding = Nothing,
                basicContentHeaderHeaders = Nothing,
                basicContentHeaderDeliveryMode = Nothing,
                basicContentHeaderPriority = Nothing,
                basicContentHeaderCorrelationId = Nothing,
                basicContentHeaderReplyTo = Nothing,
                basicContentHeaderExpiration = Nothing,
                basicContentHeaderMessageId = Nothing,
                basicContentHeaderTimestamp = Nothing,
                basicContentHeaderType = Nothing,
                basicContentHeaderUserId = Nothing,
                basicContentHeaderAppId = Nothing,
                basicContentHeaderReserved = Nothing
              }
      it "roundtrips on the empty example" $
        roundtripsFor
          buildGivenContentHeader
          parseGivenContentHeader
          emptyBasicContentHeader
      it "outputs the same as before for the empty example" $
        pureGoldenByteStringBuilderFile "test_resources/content-header/basic/empty.dat" (buildGivenContentHeader emptyBasicContentHeader)
      let exampleBasicContentHeader =
            BasicContentHeader
              { basicContentHeaderContentType = Just "type",
                basicContentHeaderContentEncoding = Just "encoding",
                basicContentHeaderHeaders = Just emptyFieldTable,
                basicContentHeaderDeliveryMode = Just 4,
                basicContentHeaderPriority = Just 1,
                basicContentHeaderCorrelationId = Just "correlation",
                basicContentHeaderReplyTo = Just "reply-to",
                basicContentHeaderExpiration = Just "expiration",
                basicContentHeaderMessageId = Just "message-id",
                basicContentHeaderTimestamp = Nothing,
                basicContentHeaderType = Just "type",
                basicContentHeaderUserId = Just "user-id",
                basicContentHeaderAppId = Just "app-id",
                basicContentHeaderReserved = Just ""
              }
      it "roundtrips on the example example" $
        roundtripsFor
          buildGivenContentHeader
          parseGivenContentHeader
          exampleBasicContentHeader
      it "outputs the same as before for the example example" $
        pureGoldenByteStringBuilderFile "test_resources/content-header/basic/example.dat" (buildGivenContentHeader exampleBasicContentHeader)
