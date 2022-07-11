{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyType.FloatingPointSpec where

import Control.Monad
import ICal.ContentLine
import ICal.PropertyType.Class
import ICal.PropertyType.FloatingPoint
import ICal.PropertyType.Gen
import Test.Syd

spec :: Spec
spec = do
  describe "FloatingPoint" $ do
    propertyTypeSpec @FloatingPoint
    let examples :: [(FloatingPoint, ContentLineValue)]
        examples =
          [ (FloatingPoint 7.8000, mkSimpleContentLineValue "7.8"),
            (FloatingPoint (1E65), mkSimpleContentLineValue "100000000000000000000000000000000000000000000000000000000000000000.0"),
            -- From the spec:
            -- @
            -- Example:
            --
            --     1000000.0000001
            --     1.333
            --     -3.14
            -- @
            (FloatingPoint 1000000.0000001, mkSimpleContentLineValue "1000000.0000001"),
            (FloatingPoint 1.333, mkSimpleContentLineValue "1.333"),
            (FloatingPoint (-3.14), mkSimpleContentLineValue "-3.14")
          ]
    describe "examples" $
      forM_ examples $ \(float, clv) -> do
        it "renders this example float correctly" $
          propertyTypeB float `shouldBe` clv
        it "parses this example float correctly" $
          propertyTypeP clv `shouldBe` Right float
