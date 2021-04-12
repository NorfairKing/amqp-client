{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AMQP.ClientSpec (spec) where

import AMQP.Client
import AMQP.Client.TestUtils
import AMQP.Serialisation
import AMQP.Serialisation.Base
import Control.Monad
import qualified Data.ByteString as SB
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import Data.GenValidity.ByteString ()
import Test.QuickCheck
import Test.Syd
import Test.Syd.RabbitMQ
import Test.Syd.Validity

instance GenValid Message where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

spec :: Spec
spec = do
  describe "messageToContentFrames" $ do
    -- The spec specifies: Zero indicates that there are no content body frames.
    it "renders an empty message to 0 content body frames" $
      forAllValid $ \maximumSize -> do
        let ContentFrames _ cbs = messageToContentFrames maximumSize (mkMessage "")
        cbs `shouldBe` []

    it "renders a message to frames that can be reconstructed to that message" $
      forAll (max 9 <$> genValid) $ \maximumSize ->
        forAllValid $ \msg ->
          reconstructMessageFromContentFrames (messageToContentFrames maximumSize msg) `shouldBe` msg

    it "renders this message that fits in one content body frame to one message" $ do
      let ContentFrames _ cbs = messageToContentFrames 100 (mkMessage "hello world")
      length cbs `shouldBe` 1

    it "renders a message to frames that are smaller than the maximum frame size" $
      forAllValid $ \channelNumber ->
        forAll (max 9 <$> genValid) $ \maximumSize ->
          forAllValid $ \body -> do
            let ContentFrames _ cbs = messageToContentFrames maximumSize (mkMessage body)
            forM_ (zip cbs [1 ..]) $ \(cbf, i) -> do
              let renderedFrame = LB.toStrict . SBB.toLazyByteString . buildFrame . ContentBodyPayload channelNumber $ cbf
                  frameSize = SB.length renderedFrame
              if frameSize > word32ToInt maximumSize
                then expectationFailure $ unwords ["Frame number", show (i :: Int), "was of size", show frameSize, "which is not under the maximum frame size of", show maximumSize]
                else pure ()

  rabbitMQSpec $ do
    -- This test is disabled until it succeeds
    itWithOuter "can make a connection and then do nothing" $ \RabbitMQHandle {..} -> do
      let settings = mkConnectionSettings "127.0.0.1" rabbitMQHandlePort
      withConnection settings $ \_ -> do
        pure () :: IO ()
    itWithLocalGuestConnection "can make a connection, open a channel and then do nothing" $ \conn -> do
      _ <- channelOpen conn
      pure () :: IO ()
    itWithLocalGuestConnection "can make a connection and declare a queue" $ \conn -> do
      chan <- channelOpen conn
      let myQueueName = "myQueueName"
      _ <- queueDeclare chan myQueueName defaultQueueSettings
      pure () :: IO ()

    itWithLocalGuestConnection "can send and recieve 100 hello world messages, one by one" $ \conn -> do
      chan <- channelOpen conn
      let myRoutingKey = "myRoutingKey"
      myQueue <- queueDeclare chan "MyQueueName" defaultQueueSettings
      myExchange <- exchangeDeclare chan "myExchangeName" defaultExchangeSettings
      queueBind chan myQueue myExchange myRoutingKey

      let testBody = "hello world"
      replicateM_ 100 $ do
        let msg = mkMessage testBody
        basicPublish
          chan
          myExchange
          myRoutingKey
          msg

        m <- basicGet chan myQueue NoAck
        m `shouldBe` Just msg

    itWithLocalGuestConnection "can send and recieve 100 hello world messages when first sending all of them" $ \conn -> do
      chan <- channelOpen conn
      let myRoutingKey = "myRoutingKey"
      myQueue <- queueDeclare chan "MyQueueName" defaultQueueSettings
      myExchange <- exchangeDeclare chan "myExchangeName" defaultExchangeSettings
      queueBind chan myQueue myExchange myRoutingKey

      let testBody = "hello world"
      let msg = mkMessage testBody
      replicateM_ 100 $
        basicPublish
          chan
          myExchange
          myRoutingKey
          msg

      replicateM_ 100 $ do
        m <- basicGet chan myQueue NoAck
        m `shouldBe` Just msg
    pending "can send and recieve 100 hello world messages when sending and receiving in separate threads"

    itWithOuter "can go through the tutorial steps with any message body" $ \RabbitMQHandle {..} ->
      forAllValid $ \testBody -> do
        let settings = mkConnectionSettings "127.0.0.1" rabbitMQHandlePort
        withConnection settings $ \conn -> do
          chan <- channelOpen conn
          let myRoutingKey = "myRoutingKey"
          myQueue <- queueDeclare chan "MyQueueName" defaultQueueSettings
          myExchange <- exchangeDeclare chan "myExchangeName" defaultExchangeSettings
          queueBind chan myQueue myExchange myRoutingKey

          let msg = mkMessage testBody
          basicPublish
            chan
            myExchange
            myRoutingKey
            msg

          m <- basicGet chan myQueue NoAck
          m `shouldBe` Just msg
    pending "can send and recieve any number of test messages one by one"
    pending "can send and recieve any number of messages when first sending all of them"
    pending "can send and recieve any number of messages when sending and receiving in separate threads"
    describe "frame size" $ do
      -- TODO also test what happens if the maximum frame size is smaller than some of the bigger method frames
      itWithOuter "can publish a message when the maximum frame size is smaller than the message" $ \RabbitMQHandle {..} -> do
        -- Big enough so that we don't get in throuble with method frames
        let size = 4098
            genMessageBodyBiggerThanSize = sized $ \s -> do
              len <- choose (size, size + s)
              ws <- replicateM len genValid -- This isn't particularly fast, but that's fine.
              pure $ SB.pack ws
        forAll genMessageBodyBiggerThanSize $ \testBody -> do
          let settings =
                (mkConnectionSettings "127.0.0.1" rabbitMQHandlePort)
                  { connectionSettingMaximumFrameSize = Just $ fromIntegral size -- Safe because we know that the size is small enough.
                  }
          withConnection settings $ \conn -> do
            chan <- channelOpen conn
            let myRoutingKey = "myRoutingKey"
            myQueue <- queueDeclare chan "MyQueueName" defaultQueueSettings
            myExchange <- exchangeDeclare chan "myExchangeName" defaultExchangeSettings
            queueBind chan myQueue myExchange myRoutingKey

            let msg = mkMessage testBody
            r <-
              basicPublish
                chan
                myExchange
                myRoutingKey
                msg
            r `shouldBe` ()

      itWithOuter "can send and recieve a message when the maximum frame size is smaller than the message" $ \RabbitMQHandle {..} -> do
        -- Big enough so that we don't get in throuble with method frames
        let size = 4098
            genMessageBodyBiggerThanSize = sized $ \s -> do
              len <- choose (size, size + s)
              ws <- replicateM len genValid -- This isn't particularly fast, but that's fine.
              pure $ SB.pack ws
        forAll genMessageBodyBiggerThanSize $ \testBody -> do
          let settings =
                (mkConnectionSettings "127.0.0.1" rabbitMQHandlePort)
                  { connectionSettingMaximumFrameSize = Just $ fromIntegral size -- Safe because we know that the size is small enough.
                  }
          withConnection settings $ \conn -> do
            chan <- channelOpen conn
            let myRoutingKey = "myRoutingKey"
            myQueue <- queueDeclare chan "MyQueueName" defaultQueueSettings
            myExchange <- exchangeDeclare chan "myExchangeName" defaultExchangeSettings
            queueBind chan myQueue myExchange myRoutingKey

            let msg = mkMessage testBody
            basicPublish
              chan
              myExchange
              myRoutingKey
              msg

            m <- basicGet chan myQueue NoAck
            m `shouldBe` Just msg
