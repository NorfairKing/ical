{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyType.RecurrenceRuleSpec where

import Data.Set (Set)
import Data.Time (DayOfWeek (..), LocalTime (..), TimeOfDay (..), fromGregorian, localTimeToUTC, utc)
import ICal
import ICal.PropertyType.Gen
import ICal.PropertyType.RecurrenceRule.Gen
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Interval" $ do
    genValidSpec @Interval
    recurrenceRulePartSpec @Interval
    recurrenceRulePartExampleSpec "1" (Interval 1)

  describe "Until" $ do
    genValidSpec @Until
    recurrenceRulePartSpec @Until
    recurrenceRulePartExampleSpec
      "20220622"
      (UntilDate $ Date $ fromGregorian 2022 06 22)
    recurrenceRulePartExampleSpec
      "20220622T124500Z"
      (UntilDateTimeUTC $ localTimeToUTC utc $ LocalTime (fromGregorian 2022 06 22) (TimeOfDay 12 45 00))

  describe "Count" $ do
    genValidSpec @Count
    recurrenceRulePartSpec @Count
    recurrenceRulePartExampleSpec "1" (Count 1)

  describe "BySecond" $ do
    genValidSpec @BySecond
    recurrenceRulePartSpec @(Set BySecond)
    recurrenceRulePartExampleSpec @(Set BySecond) "1" [BySecond 1]

  describe "ByMinute" $ do
    genValidSpec @ByMinute
    recurrenceRulePartSpec @(Set ByMinute)
    recurrenceRulePartExampleSpec @(Set ByMinute) "1" [ByMinute 1]

  describe "ByHour" $ do
    genValidSpec @ByHour
    recurrenceRulePartSpec @(Set ByHour)
    recurrenceRulePartExampleSpec @(Set ByHour) "1" [ByHour 1]

  describe "ByDay" $ do
    genValidSpec @ByDay
    recurrenceRulePartSpec @(Set ByDay)
    recurrenceRulePartExampleSpec @(Set ByDay)
      "SU"
      [Every Sunday]
    recurrenceRulePartExampleSpec @(Set ByDay)
      "-1MO"
      [Specific (-1) Monday]
    recurrenceRulePartExampleSpec @(Set ByDay)
      "2TU"
      [Specific 2 Tuesday]

  describe "ByMonthDay" $ do
    genValidSpec @ByMonthDay
    recurrenceRulePartSpec @(Set ByMonthDay)
    recurrenceRulePartExampleSpec @(Set ByMonthDay) "1" [ByMonthDay 1]

  describe "ByYearDay" $ do
    genValidSpec @ByYearDay
    recurrenceRulePartSpec @(Set ByYearDay)
    recurrenceRulePartExampleSpec @(Set ByYearDay) "1" [ByYearDay 1]

  describe "ByWeekNo" $ do
    genValidSpec @ByWeekNo
    recurrenceRulePartSpec @(Set ByWeekNo)
    recurrenceRulePartExampleSpec @(Set ByWeekNo) "1" [ByWeekNo 1]

  describe "ByMonth" $ do
    genValidSpec @ByMonth
    recurrenceRulePartSpec @(Set ByMonth)
    recurrenceRulePartExampleSpec @(Set ByMonth) "1" [ByMonth January]

  describe "BySetPos" $ do
    genValidSpec @BySetPos
    recurrenceRulePartSpec @(Set BySetPos)
    recurrenceRulePartExampleSpec @(Set BySetPos) "1" [BySetPos 1]

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
