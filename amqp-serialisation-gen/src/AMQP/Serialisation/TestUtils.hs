{-# OPTIONS -fno-warn-orphans #-}
module AMQP.Serialisation.TestUtils where

import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.ByteString.Builder as ByteString (Builder)
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Containers ()
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

roundtrips :: (Show a, Eq a, GenValid a) => (a -> ByteString.Builder) -> Parser a -> Property
roundtrips builder parser =
  forAllValid $ \expected -> do
    let errOrRes = parseOnly parser (builderToByteString (builder expected))
    case errOrRes of
      Left err -> expectationFailure err
      Right actual -> actual `shouldBe` expected

roundtripsFor :: (Show a, Eq a) => (a -> ByteString.Builder) -> Parser a -> a -> IO ()
roundtripsFor builder parser expected = do
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
