{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyType.TimeSpec where

import Control.Monad
import qualified Data.Map as M
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
    let examples :: [(Time, ContentLineValue)]
        examples =
          [ (TimeFloating $ TimeOfDay 23 00 00, mkSimpleContentLineValue "230000"),
            (TimeUTC $ TimeOfDay 07 00 00, mkSimpleContentLineValue "070000Z"),
            ( TimeZoned "America/New_York" (TimeOfDay 08 30 00),
              ContentLineValue
                { contentLineValueParams = M.singleton "TZID" ["America/New_York"],
                  contentLineValueRaw = "083000"
                }
            )
          ]
    describe "examples" $
      forM_ examples $ \(time, text) -> do
        it "renders this example time correctly" $
          timeB time `shouldBe` text
        it "parses this example text correctly" $
          timeP text `shouldBe` Right time

    it "fails to parse this counterexample from the spec" $
      case timeP (mkSimpleContentLineValue "230000-0800") of
        Left _ -> pure ()
        Right time -> expectationFailure $ "Should have failed to parse, but parsed: " <> show time
