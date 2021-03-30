{-# OPTIONS_GHC -fno-warn-orphans #-}

module AMQP.Serialisation.Base.Gen where

import AMQP.Serialisation.Base
import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Containers ()

instance GenValid Argument where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid FieldTableValue where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid DecimalValue where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ShortString where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid LongString where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Timestamp where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid FieldTableKey where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid FieldTable where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
