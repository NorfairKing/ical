{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyType.DateSpec where

import Control.Monad
import Data.Time (fromGregorian)
import ICal.ContentLine
import ICal.PropertyType.Date
import ICal.PropertyType.Gen
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Date" $ do
    genValidSpec @Date
    propertyTypeSpec @Date

  describe "dateP" $ do
    let examples :: [(Date, ContentLineValue)]
        examples =
          [ (Date $ fromGregorian 1997 07 14, mkSimpleContentLineValue "19970714")
          ]
    describe "examples" $
      forM_ examples $ \(date, text) -> do
        it "renders this example date correctly" $
          dateB date `shouldBe` text
        it "parses this example text correctly" $
          dateP text `shouldBe` Right date
