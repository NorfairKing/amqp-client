{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

-- | This module contains the tests for the relevant occurrences of "test scenario" in the AMQP specification.
module AMQP.Client.TestUtils where

import AMQP.Client
import AMQP.Serialisation.Base
import AMQP.Serialisation.Generated.DomainTypes
import Control.Monad
import qualified Data.ByteString.Char8 as SB8
import Test.Syd
import Test.Syd.RabbitMQ
import Text.Printf

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
