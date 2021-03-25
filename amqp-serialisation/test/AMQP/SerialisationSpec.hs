module AMQP.SerialisationSpec (spec) where

import AMQP.Serialisation
import qualified Data.ByteString as SB
import Data.ByteString.Builder as ByteString (Builder)
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import Test.Syd

spec :: Spec
spec = do
  describe "protocolHeader" $ do
    it "is 8 octets long" $ LB.length (SBB.toLazyByteString protocolHeader) `shouldBe` 8
    it "stays the same" $ pureGoldenByteStringBuilderFile "test_resources/protocol-header.dat" protocolHeader
