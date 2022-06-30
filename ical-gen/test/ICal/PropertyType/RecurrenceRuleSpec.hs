{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyType.RecurrenceRuleSpec where

import Control.Monad
import Data.List.NonEmpty (NonEmpty (..))
import ICal.ContentLine
import ICal.Parameter
import ICal.Parameter.Gen
import ICal.PropertyType.Class
import ICal.PropertyType.Gen
import ICal.PropertyType.RecurrenceRule
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Interval" $ do
    genValidSpec @Interval
    parameterSpec @Interval
    let examples :: [(NonEmpty ParamValue, Interval)]
        examples = [(["1"], Interval 1)]
    forM_ examples $ \(pvs, interval) -> do
      it "can parse this example" $
        parameterP pvs `shouldBe` Right interval
      it "can render this example" $
        parameterB interval `shouldBe` pvs

  describe "Until" $ do
    genValidSpec @Until
    parameterSpec @Until
    let examples :: [(NonEmpty ParamValue, Until)]
        examples =
          [ (["20220622"], UntilDate undefined),
            (["20220622T124500"], UntilDateTime undefined)
          ]
    forM_ examples $ \(pvs, until_) -> do
      it "can parse this example" $
        parameterP pvs `shouldBe` Right until_
      it "can render this example" $
        parameterB until_ `shouldBe` pvs

  describe "BySecond" $ do
    genValidSpec @BySecond
    parameterSpec @BySecond
    let examples :: [(NonEmpty ParamValue, BySecond)]
        examples = [(["1"], BySecond 1)]
    forM_ examples $ \(pvs, bySecond) -> do
      it "can parse this example" $
        parameterP pvs `shouldBe` Right bySecond
      it "can render this example" $
        parameterB bySecond `shouldBe` pvs

  describe "ByMinute" $ do
    genValidSpec @ByMinute
    parameterSpec @ByMinute
    let examples :: [(NonEmpty ParamValue, ByMinute)]
        examples = [(["1"], ByMinute 1)]
    forM_ examples $ \(pvs, byMinute) -> do
      it "can parse this example" $
        parameterP pvs `shouldBe` Right byMinute
      it "can render this example" $
        parameterB byMinute `shouldBe` pvs

  describe "ByHour" $ do
    genValidSpec @ByHour
    parameterSpec @ByHour
    let examples :: [(NonEmpty ParamValue, ByHour)]
        examples = [(["1"], ByHour 1)]
    forM_ examples $ \(pvs, byHour) -> do
      it "can parse this example" $
        parameterP pvs `shouldBe` Right byHour
      it "can render this example" $
        parameterB byHour `shouldBe` pvs

  describe "ByMonthDay" $ do
    genValidSpec @ByMonthDay
    parameterSpec @ByMonthDay
    let examples :: [(NonEmpty ParamValue, ByMonthDay)]
        examples = [(["1"], ByMonthDay 1)]
    forM_ examples $ \(pvs, byMonthDay) -> do
      it "can parse this example" $
        parameterP pvs `shouldBe` Right byMonthDay
      it "can render this example" $
        parameterB byMonthDay `shouldBe` pvs

  describe "ByYearDay" $ do
    genValidSpec @ByYearDay
    parameterSpec @ByYearDay
    let examples :: [(NonEmpty ParamValue, ByYearDay)]
        examples = [(["1"], ByYearDay 1)]
    forM_ examples $ \(pvs, byYearDay) -> do
      it "can parse this example" $
        parameterP pvs `shouldBe` Right byYearDay
      it "can render this example" $
        parameterB byYearDay `shouldBe` pvs

  describe "ByWeekNo" $ do
    genValidSpec @ByWeekNo
    parameterSpec @ByWeekNo
    let examples :: [(NonEmpty ParamValue, ByWeekNo)]
        examples = [(["1"], ByWeekNo 1)]
    forM_ examples $ \(pvs, byWeekNo) -> do
      it "can parse this example" $
        parameterP pvs `shouldBe` Right byWeekNo
      it "can render this example" $
        parameterB byWeekNo `shouldBe` pvs

  describe "ByMonth" $ do
    genValidSpec @ByMonth
    parameterSpec @ByMonth
    let examples :: [(NonEmpty ParamValue, ByMonth)]
        examples = [(["1"], ByMonth January)]
    forM_ examples $ \(pvs, byMonth) -> do
      it "can parse this example" $
        parameterP pvs `shouldBe` Right byMonth
      it "can render this example" $
        parameterB byMonth `shouldBe` pvs

  describe "BySetPos" $ do
    genValidSpec @BySetPos
    parameterSpec @BySetPos
    let examples :: [(NonEmpty ParamValue, BySetPos)]
        examples = [(["1"], BySetPos 1)]
    forM_ examples $ \(pvs, bySetPos) -> do
      it "can parse this example" $
        parameterP pvs `shouldBe` Right bySetPos
      it "can render this example" $
        parameterB bySetPos `shouldBe` pvs

  describe "RecurrenceRule" $ do
    genValidSpec @RecurrenceRule
    propertyTypeSpec @RecurrenceRule
    let examples :: [(ContentLineValue, RecurrenceRule)]
        examples = []
    forM_ examples $ \(clv, recurrenceRule) -> do
      it "can parse this example" $
        propertyTypeP clv `shouldBe` Right recurrenceRule
      it "can render this example" $
        propertyTypeB recurrenceRule `shouldBe` clv
