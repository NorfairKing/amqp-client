{-# OPTIONS -fno-warn-orphans #-}
module AMQP.Serialisation.BaseSpec (spec) where

import AMQP.Serialisation.Base
import AMQP.Serialisation.Base.Gen ()
import AMQP.Serialisation.TestUtils
import qualified Data.Attoparsec.ByteString as Attoparsec
import qualified Data.ByteString.Builder as SBB
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

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

  describe "packBits" $ do
    it "builds no bits to 0" $ packBits [] `shouldBe` 0
    it "builds one false bits to 0" $ packBits [False] `shouldBe` 0
    it "builds one true bits to 1" $ packBits [True] `shouldBe` 1

  describe "unpackBits" $ do
    it "can unpack whatever packBits packs." $
      forAllValid $ \w1 ->
        forAllValid $ \w2 ->
          forAllValid $ \w3 ->
            forAllValid $ \w4 ->
              forAllValid $ \w5 ->
                forAllValid $ \w6 ->
                  forAllValid $ \w7 ->
                    forAllValid $ \w8 ->
                      forAll (choose (0, 8)) $ \n ->
                        let l = take n [w1, w2, w3, w4, w5, w6, w7, w8]
                            rendered = packBits l
                         in unpackBits (fromIntegral (length l)) rendered `shouldBe` l

  describe "parseBits" $
    it "can parse whatever 'buildBits' builds'" $
      forAllValid $ \w1 ->
        forAllValid $ \w2 ->
          forAllValid $ \w3 ->
            forAllValid $ \w4 ->
              forAllValid $ \w5 ->
                forAllValid $ \w6 ->
                  forAllValid $ \w7 ->
                    forAllValid $ \w8 ->
                      forAll (choose (0, 8)) $ \n ->
                        let l = take n [w1, w2, w3, w4, w5, w6, w7, w8]
                            rendered = builderToByteString $ buildBits l
                         in case Attoparsec.parseOnly (parseBits (fromIntegral (length l))) rendered of
                              Left err -> expectationFailure err
                              Right bits -> bits `shouldBe` l

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
