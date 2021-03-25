module AMQP.SerialisationSpec (spec) where

import AMQP.Serialisation
import Control.Monad
import Data.Attoparsec.ByteString
import qualified Data.ByteString as SB
import Data.ByteString.Builder as ByteString (Builder)
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import Data.Char as Char
import Data.GenValidity
import Data.GenValidity.ByteString ()
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

instance GenValid FrameType where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Frame where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

spec :: Spec
spec = do
  describe "protocolHeader" $ do
    it "is 8 octets long" $ LB.length (SBB.toLazyByteString protocolHeader) `shouldBe` 8
    it "stays the same" $ pureGoldenByteStringBuilderFile "test_resources/protocol-header.dat" protocolHeader

  describe "buildFrameType" $
    forM_ [minBound .. maxBound] $ \ft ->
      it (unwords ["renders the", show ft, "frame type the same as before"]) $
        pureGoldenByteStringBuilderFile ("test_resources/frame-type/" <> map Char.toLower (show ft)) (buildFrameType ft)

  describe "parseFrameType" $
    it "can parse whatever 'buildFrameType' builds'" $
      roundtrips buildFrameType parseFrameType

  describe "parseFrame" $
    it "can parse whatever 'buildFrame' builds'" $
      roundtrips buildFrame parseFrame

roundtrips :: (Show a, Eq a, GenValid a) => (a -> ByteString.Builder) -> Parser a -> Property
roundtrips builder parser =
  forAllValid $ \ft ->
    parseOnly parser (LB.toStrict (SBB.toLazyByteString (builder ft))) `shouldBe` Right ft
