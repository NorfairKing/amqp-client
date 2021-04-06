{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AMQP.ClientSpec (spec) where

import AMQP.Client
import AMQP.Client.TestUtils
import Test.Syd
import Test.Syd.RabbitMQ

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
    let myQueueName = "myQueueName"
        myExchangeName = "myExchangeName"
        myRoutingKey = "myRoutingKey"
    _ <- queueDeclare chan myQueueName defaultQueueSettings
    _ <- exchangeDeclare chan myExchangeName defaultExchangeSettings
    queueBind chan myQueueName myExchangeName myRoutingKey

    basicPublish
      chan
      myExchangeName
      myRoutingKey
      (newMessage "hello world")
