{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyType.DurationSpec where

import Control.Monad
import Data.Time (fromGregorian)
import ICal.ContentLine
import ICal.PropertyType.Duration
import ICal.PropertyType.Gen
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Duration" $ do
    genValidSpec @Duration
    propertyTypeSpec @Duration
  describe "durationP" $ do
    let examples :: [(Duration, ContentLineValue)]
        examples =
          [ (Duration $ fromGregorian 1997 07 14, mkSimpleContentLineValue "19970714")
          ]
    describe "examples" $
      forM_ examples $ \(duration, text) -> do
        it "renders this example duration correctly" $
          durationB duration `shouldBe` text
        it "parses this example text correctly" $
          durationP text `shouldBe` Right duration
