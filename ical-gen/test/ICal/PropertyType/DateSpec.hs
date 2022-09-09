{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyType.DateSpec where

import qualified Data.Map as M
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
    it "renders this date correctly" $
      dateB
        (Date (fromGregorian 1997 07 14))
        `shouldBe` ( ContentLineValue
                       { contentLineValueParams = M.singleton "VALUE" ["DATE"],
                         contentLineValueRaw = "19970714"
                       }
                   )
    it "parses this date correctly" $
      dateP
        (mkSimpleContentLineValue "19970714")
        `shouldBe` Right (Date (fromGregorian 1997 07 14))
    it "parses this date correctly" $
      dateP
        ( ContentLineValue
            { contentLineValueRaw = "19970714",
              contentLineValueParams = M.singleton "VALUE" ["DATE"]
            }
        )
        `shouldBe` Right (Date (fromGregorian 1997 07 14))
    it "fails to parse this date, correctly" $
      dateP
        ( ContentLineValue
            { contentLineValueRaw = "19970714",
              contentLineValueParams = M.singleton "VALUE" ["DATE-TIME"]
            }
        )
        `shouldBe` Left "Invalid VALUE"
