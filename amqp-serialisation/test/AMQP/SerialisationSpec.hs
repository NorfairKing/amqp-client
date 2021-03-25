module AMQP.SerialisationSpec (spec) where

import AMQP.Serialisation
import Control.Monad
import qualified Data.ByteString as SB
import Data.ByteString.Builder as ByteString (Builder)
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import Data.Char as Char
import Test.Syd

spec :: Spec
spec = do
  describe "protocolHeader" $ do
    it "is 8 octets long" $ LB.length (SBB.toLazyByteString protocolHeader) `shouldBe` 8
    it "stays the same" $ pureGoldenByteStringBuilderFile "test_resources/protocol-header.dat" protocolHeader

  describe "buildFrameType" $
    forM_ [minBound .. maxBound] $ \ft ->
      it (unwords ["renders the", show ft, "frame type the same as before"]) $
        pureGoldenByteStringBuilderFile ("test_resources/frame-type/" <> map Char.toLower (show ft)) (buildFrameType ft)
