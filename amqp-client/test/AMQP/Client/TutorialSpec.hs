{-# LANGUAGE OverloadedStrings #-}

module AMQP.Client.TutorialSpec (spec) where

import AMQP.Client
import AMQP.Client.TestUtils
import Data.GenValidity.ByteString ()
import Test.Syd
import Test.Syd.RabbitMQ

spec :: Spec
spec = rabbitMQSpec $ do
  itWithLocalGuestConnection "can go through the tutorial steps" $ \conn -> do
    chan <- channelOpen conn
    let myRoutingKey = "myRoutingKey"
    myQueue <- queueDeclare chan "MyQueueName" defaultQueueSettings
    myExchange <- exchangeDeclare chan "myExchangeName" defaultExchangeSettings
    queueBind chan myQueue myExchange myRoutingKey

    let testBody = "hello world"
    let msg = mkMessage testBody
    basicPublish
      chan
      myExchange
      myRoutingKey
      msg

    m <- basicGet chan myQueue NoAck
    m `shouldBe` Just msg

  itWithLocalGuestConnection "can go through the tutorial steps with an empty message" $ \conn -> do
    chan <- channelOpen conn
    let myRoutingKey = "myRoutingKey"
    myQueue <- queueDeclare chan "MyQueueName" defaultQueueSettings
    myExchange <- exchangeDeclare chan "myExchangeName" defaultExchangeSettings
    queueBind chan myQueue myExchange myRoutingKey

    let emptyTestBody = ""
    let msg = mkMessage emptyTestBody
    basicPublish
      chan
      myExchange
      myRoutingKey
      msg

    m <- basicGet chan myQueue NoAck
    m `shouldBe` Just msg
