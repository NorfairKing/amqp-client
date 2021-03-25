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

roundtrips :: (Show a, Eq a, GenValid a) => (a -> ByteString.Builder) -> Parser a -> Property
roundtrips builder parser =
  forAllValid $ \ft ->
    parseOnly parser (LB.toStrict (SBB.toLazyByteString (builder ft))) `shouldBe` Right ft
