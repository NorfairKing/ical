{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyType.RecurrenceRuleSpec where

import Conformance
import Conformance.TestUtils
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
    -- @
    -- The COUNT rule part defines the number of occurrences at which to
    -- range-bound the recurrence.  The "DTSTART" property value always
    -- counts as the first occurrence.
    -- @
    --
    -- That is the whole of what the spec says about COUNT: it bounds the value
    -- nowhere.  A weekly meeting for two years is an ordinary count and must be
    -- valid.
    it "considers an ordinary count valid" $
      shouldBeValid (Count 100)
    recurrenceRulePartExampleSpec "100" (Count 100)

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
    -- @
    --     weekdaynum  = [[plus / minus] ordwk] weekday
    --
    --     ordwk       = 1*2DIGIT       ;1 to 53
    -- @
    --
    -- Two digits, so up to 53.  Within a YEARLY rule the number is an offset
    -- within the year, which is where the larger ones are meant:
    --
    -- @
    -- The numeric value in a
    -- BYDAY rule part with the FREQ rule part set to YEARLY corresponds
    -- to an offset within the month when the BYMONTH rule part is
    -- present, and corresponds to an offset within the year when the
    -- BYMONTH rule part is not present.
    -- @
    recurrenceRulePartExampleSpec @(Set ByDay)
      "10MO"
      [Specific 10 Monday]
    recurrenceRulePartExampleSpec @(Set ByDay)
      "53SU"
      [Specific 53 Sunday]
    recurrenceRulePartExampleSpec @(Set ByDay)
      "-53MO"
      [Specific (-53) Monday]
    it "considers the largest ordinal valid" $ do
      shouldBeValid (Specific 53 Monday)
      shouldBeValid (Specific (-53) Monday)
    it "rejects an ordinal past the number of weeks in a year" $ do
      isValid (Specific 54 Monday) `shouldBe` False
      isValid (Specific (-54) Monday) `shouldBe` False

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
    -- @
    -- The BYSETPOS rule part specifies a COMMA-separated list of values
    -- that corresponds to the nth occurrence within the set of
    -- recurrence instances specified by the rule.  [...]  Valid values
    -- are 1 to 366 or -366 to -1.
    -- @
    it "considers the largest set position valid" $ do
      shouldBeValid (BySetPos 366)
      shouldBeValid (BySetPos (-366))
    it "rejects a set position past the number of days in a year" $ do
      isValid (BySetPos 367) `shouldBe` False
      isValid (BySetPos (-367)) `shouldBe` False

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

    describe "UNTIL and COUNT together" $ do
      -- @
      -- The UNTIL or COUNT rule parts are OPTIONAL, but they MUST NOT
      -- occur in the same 'recur'.
      -- @
      --
      -- The spec forbids both and does not say which one wins, so which one
      -- survives is arbitrary.  It is pinned here so that it is at least
      -- deterministic and written down, the same way the precedence between
      -- colliding time zone observances was settled.
      let both = "FREQ=DAILY;UNTIL=20200201T000000Z;COUNT=3"
      it "does not parse without fixing something" $
        case runConform (parseRecurrenceRule both) of
          Left _ -> pure ()
          Right (rule, _) ->
            expectationFailure $
              unlines ["Should have needed fixing but parsed cleanly into:", ppShow rule]
      it "keeps the count and drops the until when parsed leniently" $ do
        rule <- shouldConformLenient $ parseRecurrenceRule both
        recurrenceRuleUntilCount rule `shouldBe` Just (Right (Count 3))
