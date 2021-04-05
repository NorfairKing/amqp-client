{-# OPTIONS_GHC -fno-warn-orphans #-}

module AMQP.Serialisation.Gen where

import AMQP.Serialisation
import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Containers ()

instance GenValid ProtocolHeader where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
