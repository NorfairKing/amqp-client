{-# OPTIONS -fno-warn-orphans #-}
module AMQP.SerialisationSpec (spec) where

import AMQP.Serialisation
import Control.Monad
import Data.Attoparsec.ByteString
import Data.ByteString.Builder as ByteString (Builder)
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import Data.Char as Char
import Data.GenValidity
import Data.GenValidity.ByteString ()
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

-- TODO move thees instances into their own package
instance GenValid ProtocolHeader where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid FrameType where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid RawFrame where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid DecimalValue where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

spec :: Spec
spec = do
  describe "buildProtocolHeader" $ do
    it "is 8 octets long" $ LB.length (SBB.toLazyByteString (buildProtocolHeader protocolHeader)) `shouldBe` 8
    it "stays the same" $ pureGoldenByteStringBuilderFile "test_resources/protocol-header.dat" (buildProtocolHeader protocolHeader)

  describe "parseProtocolHeader" $
    it "can parse whatever 'buildProtocolHeader' builds'" $
      roundtrips buildProtocolHeader parseProtocolHeader

  describe "buildFrameType" $
    forM_ [minBound .. maxBound] $ \ft ->
      it (unwords ["renders the", show ft, "frame type the same as before"]) $
        pureGoldenByteStringBuilderFile ("test_resources/frame-type/" <> map Char.toLower (show ft)) (buildFrameType ft)

  describe "parseFrameType" $
    it "can parse whatever 'buildFrameType' builds'" $
      roundtrips buildFrameType parseFrameType

  describe "parseRawFrame" $
    it "can parse whatever 'buildRawFrame' builds'" $
      roundtrips buildRawFrame parseRawFrame

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

roundtrips :: (Show a, Eq a, GenValid a) => (a -> ByteString.Builder) -> Parser a -> Property
roundtrips builder parser =
  forAllValid $ \ft ->
    parseOnly parser (LB.toStrict (SBB.toLazyByteString (builder ft))) `shouldBe` Right ft

-- Needed because NaN /= NaN
roundtripsFloat :: (Show a, Eq a, RealFloat a, GenValid a) => (a -> ByteString.Builder) -> Parser a -> Property
roundtripsFloat builder parser =
  forAllValid $ \f -> do
    let errOrRes = parseOnly parser (LB.toStrict (SBB.toLazyByteString (builder f)))
    case errOrRes of
      Left err -> expectationFailure err
      Right f' ->
        if isNaN f'
          then pure ()
          else f' `shouldBe` f
