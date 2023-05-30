{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyType.BinarySpec where

import Data.Either
import qualified Data.Map.Strict as M
import Data.Time (fromGregorian)
import Data.Void
import ICal.Conformance
import ICal.Conformance.TestUtils
import ICal.ContentLine
import ICal.PropertyType
import ICal.PropertyType.Gen
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Binary" $ do
    genValidSpec @Binary
    propertyTypeSpec @Binary

  describe "parseBinary" $
    it "roundtrips with renderBinary" $
      forAllValid $ \b -> do
        actual <- shouldConform $ parseBinary (renderBinary b)
        actual `shouldBe` b
