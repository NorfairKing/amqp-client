{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AMQP.Serialisation.Frame.Gen where

import AMQP.Serialisation.Base.Gen ()
import AMQP.Serialisation.Frame
import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Containers ()

instance GenValid FrameType where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid RawFrame where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid a => GenValid (ContentHeaderFrame a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ContentBody where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
