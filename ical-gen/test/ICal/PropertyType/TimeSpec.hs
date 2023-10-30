{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyType.TimeSpec where

import Conformance
import qualified Data.Map.Strict as M
import Data.Time (TimeOfDay (..))
import ICal.ContentLine
import ICal.PropertyType.Gen
import ICal.PropertyType.Time
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Time" $ do
    genValidSpec @Time
    propertyTypeSpec @Time

  describe "timeP" $ do
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "230000")
      (TimeFloating $ TimeOfDay 23 00 00)
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "070000Z")
      (TimeUTC $ TimeOfDay 07 00 00)
    propertyTypeExampleSpec
      ( ContentLineValue
          { contentLineValueParams = M.singleton "TZID" ["America/New_York"],
            contentLineValueRaw = "083000"
          }
      )
      (TimeZoned "America/New_York" (TimeOfDay 08 30 00))

    it "fails to parse this counterexample from the spec" $
      case runConformStrict $ timeP (mkSimpleContentLineValue "230000-0800") of
        Left _ -> pure ()
        Right time -> expectationFailure $ "Should have failed to parse, but parsed: " <> show time
