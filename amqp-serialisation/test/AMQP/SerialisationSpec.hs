{-# OPTIONS -fno-warn-orphans #-}
module AMQP.SerialisationSpec (spec) where

import AMQP.Serialisation
import Control.Monad
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import Data.ByteString.Builder as ByteString (Builder)
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import Data.Char as Char
import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Containers ()
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

  describe "parseConnectionStartMethodFramePayload" $
    it "can parse the example that we got from the rabbitmq server" $ do
      payload <- SB.readFile "test_resources/connection-start.dat"
      case parseOnly parseConnectionStartMethodFramePayload payload of
        Left err ->
          expectationFailure $
            unlines
              [ "Parsing the connection start method frame payload failed: ",
                err
              ]
        Right _ -> pure ()

roundtrips :: (Show a, Eq a, GenValid a) => (a -> ByteString.Builder) -> Parser a -> Property
roundtrips builder parser =
  forAllValid $ \expected -> do
    let errOrRes = parseOnly parser (builderToByteString (builder expected))
    case errOrRes of
      Left err -> expectationFailure err
      Right actual -> actual `shouldBe` expected

-- This re-encodes before comparing for equality so that NaN values don't cause trouble
roundtripsWithFloat :: (Show a, Eq a, GenValid a) => (a -> ByteString.Builder) -> Parser a -> Property
roundtripsWithFloat builder parser =
  forAllValid $ \expected -> do
    let errOrRes = parseOnly parser (builderToByteString (builder expected))
    case errOrRes of
      Left err -> expectationFailure err
      Right actual -> builderToByteString (builder actual) `shouldBe` builderToByteString (builder expected)

-- Needed because NaN /= NaN
roundtripsFloat :: (Show a, Eq a, RealFloat a, GenValid a) => (a -> ByteString.Builder) -> Parser a -> Property
roundtripsFloat builder parser =
  forAllValid $ \f -> do
    let errOrRes = parseOnly parser (builderToByteString (builder f))
    case errOrRes of
      Left err -> expectationFailure err
      Right f' ->
        if isNaN f'
          then pure ()
          else f' `shouldBe` f

builderToByteString :: ByteString.Builder -> ByteString
builderToByteString = LB.toStrict . SBB.toLazyByteString
