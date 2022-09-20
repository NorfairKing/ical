{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyType.RecurrenceRuleSpec where

import Control.Monad
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (DayOfWeek (..), LocalTime (..), TimeOfDay (..), fromGregorian, localTimeToUTC, utc)
import ICal.ContentLine
import ICal.PropertyType.Date
import ICal.PropertyType.Gen
import ICal.PropertyType.RecurrenceRule
import ICal.PropertyType.RecurrenceRule.Gen
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Interval" $ do
    genValidSpec @Interval
    recurrenceRulePartSpec @Interval
    let examples :: [(Text, Interval)]
        examples = [("1", Interval 1)]
    forM_ examples $ \(pvs, interval) -> do
      it "can parse this example" $
        recurrenceRulePartP pvs `shouldBe` Right interval
      it "can render this example" $
        recurrenceRulePartB interval `shouldBe` pvs

  describe "Until" $ do
    genValidSpec @Until
    recurrenceRulePartSpec @Until
    let examples :: [(Text, Until)]
        examples =
          [ ("20220622", UntilDate $ Date $ fromGregorian 2022 06 22),
            ("20220622T124500Z", UntilDateTimeUTC $ localTimeToUTC utc $ LocalTime (fromGregorian 2022 06 22) (TimeOfDay 12 45 00))
          ]
    forM_ examples $ \(pvs, until_) -> do
      it "can parse this example" $
        recurrenceRulePartP pvs `shouldBe` Right until_
      it "can render this example" $
        recurrenceRulePartB until_ `shouldBe` pvs

  describe "Count" $ do
    genValidSpec @Count
    recurrenceRulePartSpec @Count
    let examples :: [(Text, Count)]
        examples = [("1", Count 1)]
    forM_ examples $ \(pvs, count) -> do
      it "can parse this example" $
        recurrenceRulePartP pvs `shouldBe` Right count
      it "can render this example" $
        recurrenceRulePartB count `shouldBe` pvs

  describe "BySecond" $ do
    genValidSpec @BySecond
    recurrenceRulePartSpec @(Set BySecond)
    let examples :: [(Text, Set BySecond)]
        examples = [("1", [BySecond 1])]
    forM_ examples $ \(pvs, bySecond) -> do
      it "can parse this example" $
        recurrenceRulePartP pvs `shouldBe` Right bySecond
      it "can render this example" $
        recurrenceRulePartB bySecond `shouldBe` pvs

  describe "ByMinute" $ do
    genValidSpec @ByMinute
    recurrenceRulePartSpec @(Set ByMinute)
    let examples :: [(Text, Set ByMinute)]
        examples = [("1", [ByMinute 1])]
    forM_ examples $ \(pvs, byMinute) -> do
      it "can parse this example" $
        recurrenceRulePartP pvs `shouldBe` Right byMinute
      it "can render this example" $
        recurrenceRulePartB byMinute `shouldBe` pvs

  describe "ByHour" $ do
    genValidSpec @ByHour
    recurrenceRulePartSpec @(Set ByHour)
    let examples :: [(Text, Set ByHour)]
        examples = [("1", [ByHour 1])]
    forM_ examples $ \(pvs, byHour) -> do
      it "can parse this example" $
        recurrenceRulePartP pvs `shouldBe` Right byHour
      it "can render this example" $
        recurrenceRulePartB byHour `shouldBe` pvs

  describe "ByDay" $ do
    genValidSpec @ByDay
    recurrenceRulePartSpec @(Set ByDay)
    let examples :: [(Text, Set ByDay)]
        examples =
          [ ("SU", [Every Sunday]),
            ("-1MO", [Specific (-1) Monday]),
            ("2TU", [Specific 2 Tuesday])
          ]
    forM_ examples $ \(pvs, byDay) -> do
      it "can parse this example" $
        recurrenceRulePartP pvs `shouldBe` Right byDay
      it "can render this example" $
        recurrenceRulePartB byDay `shouldBe` pvs

  describe "ByMonthDay" $ do
    genValidSpec @ByMonthDay
    recurrenceRulePartSpec @(Set ByMonthDay)
    let examples :: [(Text, Set ByMonthDay)]
        examples = [("1", [ByMonthDay 1])]
    forM_ examples $ \(pvs, byMonthDay) -> do
      it "can parse this example" $
        recurrenceRulePartP pvs `shouldBe` Right byMonthDay
      it "can render this example" $
        recurrenceRulePartB byMonthDay `shouldBe` pvs

  describe "ByYearDay" $ do
    genValidSpec @ByYearDay
    recurrenceRulePartSpec @(Set ByYearDay)
    let examples :: [(Text, Set ByYearDay)]
        examples = [("1", [ByYearDay 1])]
    forM_ examples $ \(pvs, byYearDay) -> do
      it "can parse this example" $
        recurrenceRulePartP pvs `shouldBe` Right byYearDay
      it "can render this example" $
        recurrenceRulePartB byYearDay `shouldBe` pvs

  describe "ByWeekNo" $ do
    genValidSpec @ByWeekNo
    recurrenceRulePartSpec @(Set ByWeekNo)
    let examples :: [(Text, Set ByWeekNo)]
        examples = [("1", [ByWeekNo 1])]
    forM_ examples $ \(pvs, byWeekNo) -> do
      it "can parse this example" $
        recurrenceRulePartP pvs `shouldBe` Right byWeekNo
      it "can render this example" $
        recurrenceRulePartB byWeekNo `shouldBe` pvs

  describe "ByMonth" $ do
    genValidSpec @ByMonth
    recurrenceRulePartSpec @(Set ByMonth)
    let examples :: [(Text, Set ByMonth)]
        examples = [("1", [ByMonth January])]
    forM_ examples $ \(pvs, byMonth) -> do
      it "can parse this example" $
        recurrenceRulePartP pvs `shouldBe` Right byMonth
      it "can render this example" $
        recurrenceRulePartB byMonth `shouldBe` pvs

  describe "BySetPos" $ do
    genValidSpec @BySetPos
    recurrenceRulePartSpec @(Set BySetPos)
    let examples :: [(Text, Set BySetPos)]
        examples = [("1", [BySetPos 1])]
    forM_ examples $ \(pvs, bySetPos) -> do
      it "can parse this example" $
        recurrenceRulePartP pvs `shouldBe` Right bySetPos
      it "can render this example" $
        recurrenceRulePartB bySetPos `shouldBe` pvs

  describe "RecurrenceRule" $ do
    genValidSpec @RecurrenceRule
    propertyTypeSpec @RecurrenceRule
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "FREQ=YEARLY;INTERVAL=2;BYMINUTE=30;BYHOUR=8,9;BYDAY=SU;BYMONTH=1")
      ( (makeRecurrenceRule Yearly)
          { recurrenceRuleFrequency = Yearly,
            recurrenceRuleInterval = Interval {unInterval = 2},
            recurrenceRuleByMinute = [ByMinute {unByMinute = 30}],
            recurrenceRuleByHour = [ByHour {unByHour = 8}, ByHour {unByHour = 9}],
            recurrenceRuleByDay = [Every Sunday],
            recurrenceRuleByMonth = [ByMonth {unByMonth = January}]
          }
      )
