{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyType.UTCOffsetSpec where

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

    propertyTypeExampleSpec
      (mkSimpleContentLineValue "-0500")
      (UTCOffset (-18000))
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "+0100")
      (UTCOffset 3600)
