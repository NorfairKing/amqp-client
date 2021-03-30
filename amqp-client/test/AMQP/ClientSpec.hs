{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AMQP.ClientSpec (spec) where

import AMQP.Client
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
  itWithOuter "can make a connection, open a channel and then do nothing" $ \RabbitMQHandle {..} -> do
    let settings =
          ConnectionSettings
            { connectionSettingHostName = "127.0.0.1",
              connectionSettingPort = rabbitMQHandlePort,
              connectionSettingSASLMechanism = PLAINMechanism "guest" "guest"
            }
    withConnection settings $ \conn -> do
      chan <- channelOpen conn
      pure () :: IO ()
  itWithOuter "can go trhough the tutorial steps" $ \RabbitMQHandle {..} -> do
    let settings =
          ConnectionSettings
            { connectionSettingHostName = "127.0.0.1",
              connectionSettingPort = rabbitMQHandlePort,
              connectionSettingSASLMechanism = PLAINMechanism "guest" "guest"
            }
    withConnection settings $ \conn -> do
      chan <- channelOpen conn
      let myQueueName = "myQueueName"
          myExchangeName = "myExchangeName"
          myRoutingKey = "myRoutingKey"
      queueDeclare chan myQueueName defaultQueueSettings
      exchangeDeclare chan myExchangeName defaultExchangeSettings
      queueBind chan myQueueName myExchangeName myRoutingKey

      pure () :: IO ()
