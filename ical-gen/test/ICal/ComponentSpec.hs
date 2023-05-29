{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.ComponentSpec where

import qualified Data.DList as DList
import qualified Data.Text as T
import ICal.Component
import ICal.Component.Gen
import ICal.Conformance.TestUtils
import ICal.ContentLine.Gen
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Component" $ do
    genValidSpec @Component
    it "roundtrips through content lines" $
      forAllValid $ \name ->
        forAllValid $ \component -> do
          let rendered = DList.toList $ renderGeneralComponent name component
          context (T.unpack (renderContentLines rendered)) $ do
            parsed <- shouldConform $ parseGeneralComponent rendered
            parsed `shouldBe` (name, component)

  describe "Calendar" $ do
    genValidSpec @Calendar
    componentSpec @Calendar
