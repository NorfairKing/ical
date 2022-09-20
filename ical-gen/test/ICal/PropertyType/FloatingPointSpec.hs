{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyType.FloatingPointSpec where

import ICal.ContentLine
import ICal.PropertyType.FloatingPoint
import ICal.PropertyType.Gen
import Test.Syd

spec :: Spec
spec = do
  describe "FloatingPoint" $ do
    propertyTypeSpec @FloatingPoint
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "7.8")
      (FloatingPoint 7.8000)
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "100000000000000000000000000000000000000000000000000000000000000000.0")
      (FloatingPoint 1E65)
    -- From the spec:
    -- @
    -- Example:
    --
    --     1000000.0000001
    --     1.333
    --     -3.14
    -- @
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "1000000.0000001")
      (FloatingPoint 1000000.0000001)
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "1.333")
      (FloatingPoint 1.333)
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "-3.14")
      (FloatingPoint (-3.14))
