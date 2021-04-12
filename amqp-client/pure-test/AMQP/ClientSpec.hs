{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AMQP.ClientSpec (spec) where

import AMQP.Client
import AMQP.Serialisation
import AMQP.Serialisation.Base
import Control.Monad
import qualified Data.ByteString as SB
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import Data.GenValidity.ByteString ()
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

instance GenValid Message where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

spec :: Spec
spec = do
  describe "messageToContentFrames" $ do
    -- The spec specifies: Zero indicates that there are no content body frames.
    it "renders an empty message to 0 content body frames" $
      forAllValid $ \maximumSize -> do
        let ContentFrames _ cbs = messageToContentFrames maximumSize (mkMessage "")
        cbs `shouldBe` []

    it "renders a message to frames that can be reconstructed to that message" $
      forAll (max 9 <$> genValid) $ \maximumSize ->
        forAllValid $ \msg ->
          reconstructMessageFromContentFrames (messageToContentFrames maximumSize msg) `shouldBe` msg

    it "renders this message that fits in one content body frame to one message" $ do
      let ContentFrames _ cbs = messageToContentFrames 100 (mkMessage "hello world")
      length cbs `shouldBe` 1

    it "renders a message to frames that are smaller than the maximum frame size" $
      forAllValid $ \channelNumber_ ->
        forAll (max 9 <$> genValid) $ \maximumSize ->
          forAllValid $ \body -> do
            let ContentFrames _ cbs = messageToContentFrames maximumSize (mkMessage body)
            forM_ (zip cbs [1 ..]) $ \(cbf, i) -> do
              let renderedFrame = LB.toStrict . SBB.toLazyByteString . buildFrame . ContentBodyPayload channelNumber_ $ cbf
                  frameSize = SB.length renderedFrame
              if frameSize > word32ToInt maximumSize
                then expectationFailure $ unwords ["Frame number", show (i :: Int), "was of size", show frameSize, "which is not under the maximum frame size of", show maximumSize]
                else pure ()
