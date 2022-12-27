{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyType.DateSpec where

import Data.Either
import qualified Data.Map as M
import Data.Time (fromGregorian)
import Data.Void
import ICal.Conformance
import ICal.ContentLine
import ICal.PropertyType
import ICal.PropertyType.Gen
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Date" $ do
    genValidSpec @Date
    propertyTypeSpec @Date

  describe "dateP" $ do
    it "renders this date correctly" $
      dateB (Date (fromGregorian 1997 07 14))
        `shouldBe` mkSimpleContentLineValue "19970714"
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "19970714")
      (Date (fromGregorian 1997 07 14))
    propertyTypeParseExampleSpec
      ( ContentLineValue
          { contentLineValueRaw = "19970714",
            contentLineValueParams = M.singleton "VALUE" ["DATE"]
          }
      )
      (Date (fromGregorian 1997 07 14))
    it "fails to parse this date, correctly" $
      runConformStrict
        ( typedPropertyTypeP
            ( ContentLineValue
                { contentLineValueRaw = "19970714",
                  contentLineValueParams = M.singleton "VALUE" ["DATE-TIME"]
                }
            ) ::
            Conform PropertyTypeParseError Void Void Date
        )
        `shouldSatisfy` isLeft
