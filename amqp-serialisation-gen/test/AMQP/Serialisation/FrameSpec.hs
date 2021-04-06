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
