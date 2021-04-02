{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

-- | This module contains the tests for the relevant occurrences of "test scenario" in the AMQP specification.
module AMQP.Client.TestUtils where

import AMQP.Client
import Test.Syd
import Test.Syd.RabbitMQ

itWithLocalGuestConnection :: String -> (Connection -> IO ()) -> TestDefM (RabbitMQHandle ': otherOuters) () ()
itWithLocalGuestConnection s func =
  itWithOuter s $ \RabbitMQHandle {..} -> do
    let settings =
          ConnectionSettings
            { connectionSettingHostName = "127.0.0.1",
              connectionSettingPort = rabbitMQHandlePort,
              connectionSettingSASLMechanism = PLAINMechanism "guest" "guest"
            }
    withConnection settings $ \conn -> func conn
