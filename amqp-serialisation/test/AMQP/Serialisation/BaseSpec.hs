{-# OPTIONS -fno-warn-orphans #-}
module AMQP.Serialisation.BaseSpec (spec) where

import AMQP.Serialisation.Base
import AMQP.Serialisation.TestUtils
import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Containers ()
import Test.Syd

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

instance GenValid FieldTableKey where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid FieldTable where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

spec :: Spec
spec = do
  describe "parseFieldTableKey" $
    it "can parse whatever 'buildFieldTableKey' builds'" $
      roundtripsWithFloat buildFieldTableKey parseFieldTableKey

  describe "parseFieldTableArray" $
    it "can parse whatever 'buildFieldTableArray' builds'" $
      roundtripsWithFloat buildFieldTableArray parseFieldTableArray

  describe "parseFieldTableValue" $
    it "can parse whatever 'buildFieldTableValue' builds'" $
      roundtripsWithFloat buildFieldTableValue parseFieldTableValue

  describe "parseFieldTable" $
    it "can parse whatever 'buildFieldTable' builds'" $
      roundtripsWithFloat buildFieldTable parseFieldTable

  describe "parseBit" $
    it "can parse whatever 'buildBit' builds'" $
      roundtrips buildBit parseBit

  describe "parseOctet" $
    it "can parse whatever 'buildOctet' builds'" $
      roundtrips buildOctet parseOctet

  describe "parseShortShortInt" $
    it "can parse whatever 'buildShortShortInt' builds'" $
      roundtrips buildShortShortInt parseShortShortInt

  describe "parseShortShortUInt" $
    it "can parse whatever 'buildShortShortUInt' builds'" $
      roundtrips buildShortShortUInt parseShortShortUInt

  describe "parseShortInt" $
    it "can parse whatever 'buildShortInt' builds'" $
      roundtrips buildShortInt parseShortInt

  describe "parseShortUInt" $
    it "can parse whatever 'buildShortUInt' builds'" $
      roundtrips buildShortUInt parseShortUInt

  describe "parseLongInt" $
    it "can parse whatever 'buildLongInt' builds'" $
      roundtrips buildLongInt parseLongInt

  describe "parseLongUInt" $
    it "can parse whatever 'buildLongUInt' builds'" $
      roundtrips buildLongUInt parseLongUInt

  describe "parseLongLongInt" $
    it "can parse whatever 'buildLongLongInt' builds'" $
      roundtrips buildLongLongInt parseLongLongInt

  describe "parseLongLongUInt" $
    it "can parse whatever 'buildLongLongUInt' builds'" $
      roundtrips buildLongLongUInt parseLongLongUInt

  describe "parseFloat" $
    it "can parse whatever 'buildFloat' builds'" $
      roundtripsFloat buildFloat parseFloat

  describe "parseDouble" $
    it "can parse whatever 'buildDouble' builds'" $
      roundtripsFloat buildDouble parseDouble

  describe "parseDecimalValue" $
    it "can parse whatever 'buildDecimalValue' builds'" $
      roundtrips buildDecimalValue parseDecimalValue

  describe "parseShortString" $
    it "can parse whatever 'buildShortString' builds'" $
      roundtrips buildShortString parseShortString

  describe "parseLongString" $
    it "can parse whatever 'buildLongString' builds'" $
      roundtrips buildLongString parseLongString

  describe "parseTimestamp" $
    it "can parse whatever 'buildTimestamp' builds'" $
      roundtrips buildTimestamp parseTimestamp
