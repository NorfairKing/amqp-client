{-# OPTIONS_GHC -fno-warn-orphans #-}

module AMQP.Serialisation.Content.Gen where

import AMQP.Serialisation.Generated.Content
import AMQP.Serialisation.Generated.Content.Gen ()
import Data.GenValidity

instance GenValid ContentHeader where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
