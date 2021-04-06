{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AMQP.ClientSpec (spec) where

import AMQP.Client
import AMQP.Client.TestUtils
import Control.Monad
import Data.GenValidity.ByteString ()
import Test.Syd
import Test.Syd.RabbitMQ
import Test.Syd.Validity

spec :: Spec
spec = rabbitMQSpec $ do
  -- This test is disabled until it succeeds
  itWithOuter "can make a connection and then do nothing" $ \RabbitMQHandle {..} -> do
    let settings =
          ConnectionSettings
            { connectionSettingHostName = "127.0.0.1",
              connectionSettingPort = rabbitMQHandlePort,
              connectionSettingSASLMechanism = PLAINMechanism "guest" "guest"
            }
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
  itWithLocalGuestConnection "can go through the tutorial steps" $ \conn -> do
    chan <- channelOpen conn
    let myRoutingKey = "myRoutingKey"
    myQueue <- queueDeclare chan "MyQueueName" defaultQueueSettings
    myExchange <- exchangeDeclare chan "myExchangeName" defaultExchangeSettings
    queueBind chan myQueue myExchange myRoutingKey

    let testBody = "hello world"
    let msg = newMessage testBody
    basicPublish
      chan
      myExchange
      myRoutingKey
      msg

    m <- basicGet chan myQueue NoAck
    m `shouldBe` Just msg

  pending "can go through the tutorial steps with an empty message"
  -- xitWithLocalGuestConnection "can go through the tutorial steps with an empty message" $ \conn -> do
  --   chan <- channelOpen conn
  --   let myRoutingKey = "myRoutingKey"
  --   myQueue <- queueDeclare chan "MyQueueName" defaultQueueSettings
  --   myExchange <- exchangeDeclare chan "myExchangeName" defaultExchangeSettings
  --   queueBind chan myQueue myExchange myRoutingKey

  --   let emptyTestBody = ""
  --   let msg = newMessage emptyTestBody
  --   basicPublish
  --     chan
  --     myExchange
  --     myRoutingKey
  --     msg

  --   m <- basicGet chan myQueue NoAck
  --   m `shouldBe` Just msg

  itWithLocalGuestConnection "can send and recieve 100 hello world messages, one by one" $ \conn -> do
    chan <- channelOpen conn
    let myRoutingKey = "myRoutingKey"
    myQueue <- queueDeclare chan "MyQueueName" defaultQueueSettings
    myExchange <- exchangeDeclare chan "myExchangeName" defaultExchangeSettings
    queueBind chan myQueue myExchange myRoutingKey

    let testBody = "hello world"
    replicateM_ 100 $ do
      let msg = newMessage testBody
      basicPublish
        chan
        myExchange
        myRoutingKey
        msg

      m <- basicGet chan myQueue NoAck
      m `shouldBe` Just msg

  pending "can send and recieve 100 hello world messages, one by one"
  pending "can send and recieve 100 hello world messages when first sending all of them"
  pending "can send and recieve 100 hello world messages when sending and receiving in separate threads"

  xitWithOuter "can go through the tutorial steps with any message body" $ \RabbitMQHandle {..} ->
    forAllValid $ \testBody -> do
      let settings =
            ConnectionSettings
              { connectionSettingHostName = "127.0.0.1",
                connectionSettingPort = rabbitMQHandlePort,
                connectionSettingSASLMechanism = PLAINMechanism "guest" "guest"
              }
      withConnection settings $ \conn -> do
        chan <- channelOpen conn
        let myRoutingKey = "myRoutingKey"
        myQueue <- queueDeclare chan "MyQueueName" defaultQueueSettings
        myExchange <- exchangeDeclare chan "myExchangeName" defaultExchangeSettings
        queueBind chan myQueue myExchange myRoutingKey

        let msg = newMessage testBody
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
