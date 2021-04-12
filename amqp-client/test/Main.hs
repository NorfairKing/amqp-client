module Main where

import Spec
import Test.Syd
import Test.Syd.RabbitMQ

main :: IO ()
main = sydTest $ rabbitMQSpec spec
