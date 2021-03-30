{-# OPTIONS_GHC -fno-warn-orphans #-}

module AMQP.Serialisation.Methods.Gen where

import AMQP.Serialisation.Generated.Methods
import AMQP.Serialisation.Generated.Methods.Gen ()
import Data.GenValidity

instance GenValid Method where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
