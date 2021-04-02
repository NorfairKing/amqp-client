{-# LANGUAGE OverloadedStrings #-}

-- | This module contains the tests for the relevant occurrences of "test scenario" in the AMQP specification.
module AMQP.Client.SpecificationSpec (spec) where

import AMQP.Client
import AMQP.Client.TestUtils
import AMQP.Serialisation.Base
import Control.Monad
import qualified Data.ByteString.Char8 as SB8
import Test.Syd
import Test.Syd.RabbitMQ
import Text.Printf

spec :: Spec
spec = rabbitMQSpec $ do
  describe "1.6 Class exchange" $ do
    pending "Client attempts to declare an exchange with each of these standard types: fanout, direct"
    pending "Client attempts to declare an exchange with each of these standard types: topic, headers"
    pending "Client declares a temporary queue and attempts to bind to each required exchange instance (\"amq.fanout\", \"amq.direct\", \"amq.topic\", and \"amq.headers\" if those types are defined)."
    pending "Client checks that the default exchange is active by specifying a queue binding with no exchange name, and publishing a message with a suitable routing key but without specifying the exchange name, then ensuring that the message arrives in the queue correctly."
    describe "1.6.2" $
      describe "1.6.2.1" $
        describe "The client declares as many exchanges as it can until the server reports an error; the number of exchanges successfully declared must be at least sixteen." $
          itWithLocalGuestConnection "can declare 1000 exchanges" $ \conn -> do
            chan <- channelOpen conn
            let myQueueName = "myQueueName"
                myExchangeName i = ShortString $ "myExchangeName-" <> SB8.pack (printf "-%3d" i)
                myRoutingKey = "myRoutingKey"
            _ <- queueDeclare chan myQueueName defaultQueueSettings
            forM_ [1 :: Int .. 16] $ \i -> do
              _ <- exchangeDeclare chan (myExchangeName i) defaultExchangeSettings
              queueBind chan myQueueName (myExchangeName i) myRoutingKey
              pure ()
