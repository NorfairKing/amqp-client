{-# LANGUAGE RecordWildCards #-}

module AMQP.ClientSpec (spec) where

import AMQP.Client
import Test.Syd
import Test.Syd.RabbitMQ

spec :: Spec
spec = rabbitMQSpec $ do
  itWithOuter "can make a connection and then do nothing" $ \RabbitMQHandle {..} -> do
    let settings = ConnectionSettings {connectionSettingHostName = "127.0.0.1", connectionSettingPort = rabbitMQHandlePort}
    withConnection settings $ \_ -> do
      pure () :: IO ()
