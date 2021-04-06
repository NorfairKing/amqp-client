{-# OPTIONS_GHC -fno-warn-orphans #-}

module AMQP.Serialisation.Gen where

import AMQP.Serialisation
import AMQP.Serialisation.Content.Gen ()
import AMQP.Serialisation.Frame.Gen ()
import AMQP.Serialisation.Methods.Gen ()
import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Containers ()

instance GenValid ProtocolHeader where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Frame where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
