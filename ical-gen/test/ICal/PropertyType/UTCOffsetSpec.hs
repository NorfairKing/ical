{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyType.UTCOffsetSpec where

import Control.Monad
import ICal.ContentLine
import ICal.PropertyType.Gen
import ICal.PropertyType.UTCOffset
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "UTCOffset" $ do
    genValidSpec @UTCOffset
    propertyTypeSpec @UTCOffset
  describe "utcOffsetP" $ do
    let examples :: [(UTCOffset, ContentLineValue)]
        examples =
          -- @
          -- Example:  The following UTC offsets are given for standard time for
          --    New York (five hours behind UTC) and Geneva (one hour ahead of
          --    UTC):
          --
          --     -0500
          --
          --     +0100
          -- @
          [ (UTCOffset (-18000), mkSimpleContentLineValue "-0500"),
            (UTCOffset 3600, mkSimpleContentLineValue "+0100")
          ]
    describe "examples" $
      forM_ examples $ \(utcOffset, text) -> do
        it "renders this example utcOffset correctly" $
          utcOffsetB utcOffset `shouldBe` text
        it "parses this example text correctly" $
          utcOffsetP text `shouldBe` Right utcOffset
