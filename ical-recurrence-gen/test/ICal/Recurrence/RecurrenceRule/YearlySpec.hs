{-# LANGUAGE OverloadedLists #-}

module ICal.Recurrence.RecurrenceRule.YearlySpec (spec) where

import Data.GenValidity.Time ()
import Data.Maybe
import qualified Data.Set as S
import Data.Time (DayOfWeek (..), LocalTime (..), TimeOfDay (..), dayOfWeek, fromGregorian, midnight)
import ICal.PropertyType.RecurrenceRule
import ICal.Recurrence.RecurrenceRule
import ICal.Recurrence.RecurrenceRule.Yearly
import ICal.Recurrence.TestUtils
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  let d = fromGregorian
  let l = LocalTime
  let t = TimeOfDay
  let yearlyDateTimeNextOccurrence lim start i ba bb bc bd be bf bg bh bi bj =
        listToMaybe $ yearlyDateTimeRecurrence lim start i ba bb bc bd be bf bg bh bi bj
  let yearlyDateNextOccurrence lim start i ba bb bc bd be bf bg =
        fmap localDay $ listToMaybe $ yearlyDateTimeRecurrence lim (LocalTime start midnight) i ba bb bc bd be bf [] [] [] bg
  describe "recurRecurrenceRuleLocalTimes" $ do
    specify "BySetPos selects within the whole year, not only within the part of the year after DTSTART" $
      -- [section 3.3.10](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.10)
      --
      -- @
      -- The BYSETPOS rule part specifies a COMMA-separated list of values
      -- that corresponds to the nth occurrence within the set of
      -- recurrence instances specified by the rule.  BYSETPOS operates on
      -- a set of recurrence instances in one interval of the recurrence
      -- rule.  For example, in a WEEKLY rule, the interval would be one
      -- week A set of recurrence instances starts at the beginning of the
      -- interval defined by the FREQ rule part.
      -- @
      --
      -- The set starts at the beginning of the year, so for 2020 it is the
      -- 15th of January, April, July and October.  BYSETPOS=1 selects the
      -- 15th of January 2020, which is before DTSTART and so is not part of
      -- the recurrence set; the 15th of July must not take its place.
      --
      -- @
      -- the BYxxx rule parts
      -- are applied to the current set of evaluated occurrences in the
      -- following order: BYMONTH, BYWEEKNO, BYYEARDAY, BYMONTHDAY, BYDAY,
      -- BYHOUR, BYMINUTE, BYSECOND and BYSETPOS; then COUNT and UNTIL are
      -- evaluated.
      -- @
      let limit = d 2021 12 31
          rule =
            (makeRecurrenceRule Yearly)
              { recurrenceRuleByMonth = [ByMonth January, ByMonth April, ByMonth July, ByMonth October],
                recurrenceRuleBySetPos = [BySetPos 1]
              }
          start = l (d 2020 06 15) midnight
       in shouldRecur (recurRecurrenceRuleLocalTimes limit start rule)
            `shouldReturn` [ l (d 2020 06 15) midnight,
                             l (d 2021 01 15) midnight
                           ]
    specify "BySetPos selects within the whole year, no matter where the limit lies" $ do
      -- The other way the limit leaks into filterSetPos in this function.
      -- The set BYSETPOS numbers "starts at the beginning of the interval
      -- defined by the FREQ rule part", so it is the whole year regardless of
      -- where our limit falls.  The July instance of 2022 lies beyond the
      -- limit, so 2022 contributes nothing at all, and its January instance
      -- must not take that place.
      let rule =
            (makeRecurrenceRule Yearly)
              { recurrenceRuleByMonth = [ByMonth January, ByMonth July],
                recurrenceRuleBySetPos = [BySetPos (-1)]
              }
          start = l (d 2020 01 15) midnight
      shouldRecur (recurRecurrenceRuleLocalTimes (d 2022 12 31) start rule)
        `shouldReturn` [ l (d 2020 01 15) midnight,
                         l (d 2020 07 15) midnight,
                         l (d 2021 07 15) midnight,
                         l (d 2022 07 15) midnight
                       ]
      shouldRecur (recurRecurrenceRuleLocalTimes (d 2022 07 01) start rule)
        `shouldReturn` [ l (d 2020 01 15) midnight,
                         l (d 2020 07 15) midnight,
                         l (d 2021 07 15) midnight
                       ]
    specify "ByWeekNo 53 does not occur in a year that has only 52 weeks" $
      -- [section 3.3.10](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.10)
      --
      -- @
      -- The BYWEEKNO rule part specifies a COMMA-separated list of
      -- ordinals specifying weeks of the year.  Valid values are 1 to 53
      -- or -53 to -1.  This corresponds to weeks according to week
      -- numbering as defined in [ISO.8601.2004].  A week is defined as a
      -- seven day period, starting on the day of the week defined to be
      -- the week start (see WKST).  Week number one of the calendar year
      -- is the first week that contains at least four (4) days in that
      -- calendar year.
      -- @
      --
      -- @
      --    Note: Assuming a Monday week start, week 53 can only occur when
      --    Thursday is January 1 or if it is a leap year and Wednesday is
      --    January 1.
      -- @
      --
      -- So 53 is a valid value that simply does not exist in every year, and
      -- a year that does not have it contributes nothing, the way a February
      -- without a 30th does.  2019, 2021 and 2022 have 52 weeks; 2020 has 53.
      let limit = d 2021 12 31
          rule =
            (makeRecurrenceRule Yearly)
              { recurrenceRuleByWeekNo = [ByWeekNo 53],
                recurrenceRuleByDay = [Every Monday]
              }
          start = l (d 2019 01 07) midnight
       in shouldRecur (recurRecurrenceRuleLocalTimes limit start rule)
            `shouldReturn` [ l (d 2019 01 07) midnight,
                             l (d 2020 12 28) midnight
                           ]
    specify "BySetPos selects within the whole year, not only within the part of the year after DTSTART" $
      -- [section 3.3.10](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.10)
      --
      -- @
      -- The BYSETPOS rule part specifies a COMMA-separated list of values
      -- that corresponds to the nth occurrence within the set of
      -- recurrence instances specified by the rule.  BYSETPOS operates on
      -- a set of recurrence instances in one interval of the recurrence
      -- rule.  For example, in a WEEKLY rule, the interval would be one
      -- week A set of recurrence instances starts at the beginning of the
      -- interval defined by the FREQ rule part.
      -- @
      --
      -- The set starts at the beginning of the year, so for 2020 it is the
      -- 15th of January, April, July and October.  BYSETPOS=1 selects the
      -- 15th of January 2020, which is before DTSTART and so is not part of
      -- the recurrence set; the 15th of July must not take its place.
      --
      -- @
      -- the BYxxx rule parts
      -- are applied to the current set of evaluated occurrences in the
      -- following order: BYMONTH, BYWEEKNO, BYYEARDAY, BYMONTHDAY, BYDAY,
      -- BYHOUR, BYMINUTE, BYSECOND and BYSETPOS; then COUNT and UNTIL are
      -- evaluated.
      -- @
      let limit = d 2021 12 31
          rule =
            (makeRecurrenceRule Yearly)
              { recurrenceRuleByMonth = [ByMonth January, ByMonth April, ByMonth July, ByMonth October],
                recurrenceRuleBySetPos = [BySetPos 1]
              }
          start = l (d 2020 06 15) midnight
       in shouldRecur (recurRecurrenceRuleLocalTimes limit start rule)
            `shouldReturn` [ l (d 2020 06 15) midnight,
                             l (d 2021 01 15) midnight
                           ]
    specify "BySetPos selects within the whole year, no matter where the limit lies" $ do
      -- The other way the limit leaks into filterSetPos in this function.
      -- The set BYSETPOS numbers "starts at the beginning of the interval
      -- defined by the FREQ rule part", so it is the whole year regardless of
      -- where our limit falls.  The July instance of 2022 lies beyond the
      -- limit, so 2022 contributes nothing at all, and its January instance
      -- must not take that place.
      let rule =
            (makeRecurrenceRule Yearly)
              { recurrenceRuleByMonth = [ByMonth January, ByMonth July],
                recurrenceRuleBySetPos = [BySetPos (-1)]
              }
          start = l (d 2020 01 15) midnight
      shouldRecur (recurRecurrenceRuleLocalTimes (d 2022 12 31) start rule)
        `shouldReturn` [ l (d 2020 01 15) midnight,
                         l (d 2020 07 15) midnight,
                         l (d 2021 07 15) midnight,
                         l (d 2022 07 15) midnight
                       ]
      shouldRecur (recurRecurrenceRuleLocalTimes (d 2022 07 01) start rule)
        `shouldReturn` [ l (d 2020 01 15) midnight,
                         l (d 2020 07 15) midnight,
                         l (d 2021 07 15) midnight
                       ]
    specify "Count takes the chronologically first instances when a week does not start on a Monday" $
      -- [section 3.3.10](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.10)
      --
      -- @
      -- The COUNT rule part defines the number of occurrences at which to
      -- range-bound the recurrence.  The "DTSTART" property value always
      -- counts as the first occurrence.
      -- @
      --
      -- @
      -- A week is defined as a
      -- seven day period, starting on the day of the week defined to be
      -- the week start (see WKST).
      -- @
      --
      -- With a week start of Sunday, week 1 of 2021 therefore starts on
      -- Sunday the 3rd of January and week 1 of 2022 on Sunday the 2nd.
      -- Counting three occurrences from DTSTART gives the Monday after it
      -- and then the Sunday of week 1 of 2022.
      let limit = d 2023 12 31
          rule =
            (makeRecurrenceRule Yearly)
              { recurrenceRuleUntilCount = Just $ Right $ Count 3,
                recurrenceRuleByWeekNo = [ByWeekNo 1],
                recurrenceRuleByDay = [Every Monday, Every Sunday],
                recurrenceRuleWeekStart = WeekStart Sunday
              }
          start = l (d 2021 01 03) midnight
       in shouldRecur (recurRecurrenceRuleLocalTimes limit start rule)
            `shouldReturn` [ l (d 2021 01 03) midnight,
                             l (d 2021 01 04) midnight,
                             l (d 2022 01 02) midnight
                           ]
    specify "Until does not stop early when a week does not start on a Monday" $
      -- The same out-of-order emission as the case above, but reached through
      -- Until rather than Count, where it is worse: recurUntil returns the
      -- empty set at the first element past the Until, so one day out of order
      -- truncates every day after it.  The Sunday of week 1 of 2022 is lost
      -- even though it falls before the Monday that stops the walk.
      let limit = d 2025 01 01
          rule =
            (makeRecurrenceRule Yearly)
              { recurrenceRuleUntilCount = Just $ Left $ UntilDateTimeFloating $ l (d 2022 01 02) midnight,
                recurrenceRuleByWeekNo = [ByWeekNo 1],
                recurrenceRuleByDay = [Every Monday, Every Sunday],
                recurrenceRuleWeekStart = WeekStart Sunday
              }
          start = l (d 2021 01 03) midnight
       in shouldRecur (recurRecurrenceRuleLocalTimes limit start rule)
            `shouldReturn` [ l (d 2021 01 03) midnight,
                             l (d 2021 01 04) midnight,
                             l (d 2022 01 02) midnight
                           ]
    specify "Count takes the chronologically first instances when a ByMonthDay is negative" $
      -- A second way the occurrences come out of order, which the same sort
      -- also settles.  Without a test for it, replacing that sort with
      -- ordering at the source would silently bring this back.
      --
      -- @
      -- The BYMONTHDAY rule part specifies a COMMA-separated list of days
      -- of the month.  Valid values are 1 to 31 or -31 to -1.  For
      -- example, -10 represents the tenth to the last day of the month.
      -- @
      --
      -- byMonthDayExpand resolves -1 to the 31st but keeps the order of the
      -- values before they were resolved, so the 31st comes out before the
      -- 15th.
      let limit = d 2023 01 01
          rule =
            (makeRecurrenceRule Yearly)
              { recurrenceRuleUntilCount = Just $ Right $ Count 3,
                recurrenceRuleByMonth = [ByMonth January],
                recurrenceRuleByMonthDay = [ByMonthDay (-1), ByMonthDay 15]
              }
          start = l (d 2020 01 15) midnight
       in shouldRecur (recurRecurrenceRuleLocalTimes limit start rule)
            `shouldReturn` [ l (d 2020 01 15) midnight,
                             l (d 2020 01 31) midnight,
                             l (d 2021 01 15) midnight
                           ]
    specify "BySetPos selects within the whole year, not only within the part of the year after DTSTART" $
      -- [section 3.3.10](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.10)
      --
      -- @
      -- The BYSETPOS rule part specifies a COMMA-separated list of values
      -- that corresponds to the nth occurrence within the set of
      -- recurrence instances specified by the rule.  BYSETPOS operates on
      -- a set of recurrence instances in one interval of the recurrence
      -- rule.  For example, in a WEEKLY rule, the interval would be one
      -- week A set of recurrence instances starts at the beginning of the
      -- interval defined by the FREQ rule part.
      -- @
      --
      -- The set starts at the beginning of the year, so for 2020 it is the
      -- 15th of January, April, July and October.  BYSETPOS=1 selects the
      -- 15th of January 2020, which is before DTSTART and so is not part of
      -- the recurrence set; the 15th of July must not take its place.
      --
      -- @
      -- the BYxxx rule parts
      -- are applied to the current set of evaluated occurrences in the
      -- following order: BYMONTH, BYWEEKNO, BYYEARDAY, BYMONTHDAY, BYDAY,
      -- BYHOUR, BYMINUTE, BYSECOND and BYSETPOS; then COUNT and UNTIL are
      -- evaluated.
      -- @
      let limit = d 2021 12 31
          rule =
            (makeRecurrenceRule Yearly)
              { recurrenceRuleByMonth = [ByMonth January, ByMonth April, ByMonth July, ByMonth October],
                recurrenceRuleBySetPos = [BySetPos 1]
              }
          start = l (d 2020 06 15) midnight
       in shouldRecur (recurRecurrenceRuleLocalTimes limit start rule)
            `shouldReturn` [ l (d 2020 06 15) midnight,
                             l (d 2021 01 15) midnight
                           ]
    specify "BySetPos selects within the whole year, no matter where the limit lies" $ do
      -- The other way the limit leaks into filterSetPos in this function.
      -- The set BYSETPOS numbers "starts at the beginning of the interval
      -- defined by the FREQ rule part", so it is the whole year regardless of
      -- where our limit falls.  The July instance of 2022 lies beyond the
      -- limit, so 2022 contributes nothing at all, and its January instance
      -- must not take that place.
      let rule =
            (makeRecurrenceRule Yearly)
              { recurrenceRuleByMonth = [ByMonth January, ByMonth July],
                recurrenceRuleBySetPos = [BySetPos (-1)]
              }
          start = l (d 2020 01 15) midnight
      shouldRecur (recurRecurrenceRuleLocalTimes (d 2022 12 31) start rule)
        `shouldReturn` [ l (d 2020 01 15) midnight,
                         l (d 2020 07 15) midnight,
                         l (d 2021 07 15) midnight,
                         l (d 2022 07 15) midnight
                       ]
      shouldRecur (recurRecurrenceRuleLocalTimes (d 2022 07 01) start rule)
        `shouldReturn` [ l (d 2020 01 15) midnight,
                         l (d 2020 07 15) midnight,
                         l (d 2021 07 15) midnight
                       ]
    specify "ByWeekNo 53 does not occur in a year that has only 52 weeks" $
      -- [section 3.3.10](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.10)
      --
      -- @
      -- The BYWEEKNO rule part specifies a COMMA-separated list of
      -- ordinals specifying weeks of the year.  Valid values are 1 to 53
      -- or -53 to -1.  This corresponds to weeks according to week
      -- numbering as defined in [ISO.8601.2004].  A week is defined as a
      -- seven day period, starting on the day of the week defined to be
      -- the week start (see WKST).  Week number one of the calendar year
      -- is the first week that contains at least four (4) days in that
      -- calendar year.
      -- @
      --
      -- @
      --    Note: Assuming a Monday week start, week 53 can only occur when
      --    Thursday is January 1 or if it is a leap year and Wednesday is
      --    January 1.
      -- @
      --
      -- So 53 is a valid value that simply does not exist in every year, and
      -- a year that does not have it contributes nothing, the way a February
      -- without a 30th does.  2019, 2021 and 2022 have 52 weeks; 2020 has 53.
      let limit = d 2021 12 31
          rule =
            (makeRecurrenceRule Yearly)
              { recurrenceRuleByWeekNo = [ByWeekNo 53],
                recurrenceRuleByDay = [Every Monday]
              }
          start = l (d 2019 01 07) midnight
       in shouldRecur (recurRecurrenceRuleLocalTimes limit start rule)
            `shouldReturn` [ l (d 2019 01 07) midnight,
                             l (d 2020 12 28) midnight
                           ]
    specify "BySetPos selects within the whole year, not only within the part of the year after DTSTART" $
      -- [section 3.3.10](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.10)
      --
      -- @
      -- The BYSETPOS rule part specifies a COMMA-separated list of values
      -- that corresponds to the nth occurrence within the set of
      -- recurrence instances specified by the rule.  BYSETPOS operates on
      -- a set of recurrence instances in one interval of the recurrence
      -- rule.  For example, in a WEEKLY rule, the interval would be one
      -- week A set of recurrence instances starts at the beginning of the
      -- interval defined by the FREQ rule part.
      -- @
      --
      -- The set starts at the beginning of the year, so for 2020 it is the
      -- 15th of January, April, July and October.  BYSETPOS=1 selects the
      -- 15th of January 2020, which is before DTSTART and so is not part of
      -- the recurrence set; the 15th of July must not take its place.
      --
      -- @
      -- the BYxxx rule parts
      -- are applied to the current set of evaluated occurrences in the
      -- following order: BYMONTH, BYWEEKNO, BYYEARDAY, BYMONTHDAY, BYDAY,
      -- BYHOUR, BYMINUTE, BYSECOND and BYSETPOS; then COUNT and UNTIL are
      -- evaluated.
      -- @
      let limit = d 2021 12 31
          rule =
            (makeRecurrenceRule Yearly)
              { recurrenceRuleByMonth = [ByMonth January, ByMonth April, ByMonth July, ByMonth October],
                recurrenceRuleBySetPos = [BySetPos 1]
              }
          start = l (d 2020 06 15) midnight
       in shouldRecur (recurRecurrenceRuleLocalTimes limit start rule)
            `shouldReturn` [ l (d 2020 06 15) midnight,
                             l (d 2021 01 15) midnight
                           ]
    specify "BySetPos selects within the whole year, no matter where the limit lies" $ do
      -- The other way the limit leaks into filterSetPos in this function.
      -- The set BYSETPOS numbers "starts at the beginning of the interval
      -- defined by the FREQ rule part", so it is the whole year regardless of
      -- where our limit falls.  The July instance of 2022 lies beyond the
      -- limit, so 2022 contributes nothing at all, and its January instance
      -- must not take that place.
      let rule =
            (makeRecurrenceRule Yearly)
              { recurrenceRuleByMonth = [ByMonth January, ByMonth July],
                recurrenceRuleBySetPos = [BySetPos (-1)]
              }
          start = l (d 2020 01 15) midnight
      shouldRecur (recurRecurrenceRuleLocalTimes (d 2022 12 31) start rule)
        `shouldReturn` [ l (d 2020 01 15) midnight,
                         l (d 2020 07 15) midnight,
                         l (d 2021 07 15) midnight,
                         l (d 2022 07 15) midnight
                       ]
      shouldRecur (recurRecurrenceRuleLocalTimes (d 2022 07 01) start rule)
        `shouldReturn` [ l (d 2020 01 15) midnight,
                         l (d 2020 07 15) midnight,
                         l (d 2021 07 15) midnight
                       ]
    specify "ByWeekNo with a numeric ByDay does not expand to every day of the week" $ do
      -- [section 3.3.10](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.10)
      --
      -- @
      -- The BYDAY rule part MUST NOT be specified with a numeric value
      -- with the FREQ rule part set to YEARLY when the BYWEEKNO rule part
      -- is specified.
      -- @
      --
      -- So this rule does not conform and there is a choice about what to do
      -- with it, but no reading of it puts an occurrence on a day other than
      -- Monday.  That part is asserted first, and holds whichever way the
      -- non-conformance is resolved.
      let limit = d 2020 12 31
          rule =
            (makeRecurrenceRule Yearly)
              { recurrenceRuleByWeekNo = [ByWeekNo 20],
                recurrenceRuleByDay = [Specific 2 Monday]
              }
          start = l (d 2020 01 01) midnight
      occurrences <- shouldRecur (recurRecurrenceRuleLocalTimes limit start rule)
      S.filter (\lt -> lt /= start && dayOfWeek (localDay lt) /= Monday) occurrences
        `shouldBe` S.empty
      -- The assertion above is also satisfied by producing nothing at all, so
      -- assert that the Monday of week 20 is still there.  That does commit to
      -- reading 2MO as MO; reporting a fixable error and keeping the week day
      -- is the same outcome here.
      occurrences `shouldSatisfy` S.member (l (d 2020 05 11) midnight)
  describe "yearlyDateTimeRecurrence" $ do
    --  An unimportant limit because we don't specify any rules that have no occurrences
    let limit = d 2030 01 01
    describe "No ByX's" $ do
      specify "Every year" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (l (d 2020 08 08) tod) (Interval 1) [] (WeekStart Monday) [] [] [] [] [] [] [] []
            `shouldBe` Just (l (d 2021 08 08) tod)
      specify "Every other year" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (l (d 2020 08 08) tod) (Interval 2) [] (WeekStart Monday) [] [] [] [] [] [] [] []
            `shouldBe` Just (l (d 2022 08 08) tod)
    describe "ByMonth" $ do
      specify "Every year in Sept" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (l (d 2019 09 30) tod) (Interval 1) [ByMonth September] (WeekStart Monday) [] [] [] [] [] [] [] []
            `shouldBe` Just (l (d 2020 09 30) tod)
      specify "Every other year in Sept" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (l (d 2019 10 30) tod) (Interval 2) [ByMonth October] (WeekStart Monday) [] [] [] [] [] [] [] []
            `shouldBe` Just (l (d 2021 10 30) tod)
      specify "Every five years in Sept" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (l (d 2015 11 30) tod) (Interval 5) [ByMonth November] (WeekStart Monday) [] [] [] [] [] [] [] []
            `shouldBe` Just (l (d 2020 11 30) tod)
      specify "Every year in Sept and Nov" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (l (d 2019 09 30) tod) (Interval 1) [ByMonth September, ByMonth November] (WeekStart Monday) [] [] [] [] [] [] [] []
            `shouldBe` Just (l (d 2019 11 30) tod)
    describe "ByWeekNo" $ do
      specify "Every last week of the year" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (l (d 2019 12 31) tod) (Interval 1) [] (WeekStart Monday) [ByWeekNo (-1)] [] [] [] [] [] [] []
            `shouldBe` Just (l (d 2020 12 28) tod)
      specify "Every sixth week in february" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (l (d 2025 02 09) tod) (Interval 1) [ByMonth February] (WeekStart Monday) [ByWeekNo 6] [] [] [] [] [] [] []
            `shouldBe` Just (l (d 2026 02 02) tod)
    describe "ByYearDay" $ do
      specify "Every first and last day of the year, at the end" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (l (d 2019 12 31) tod) (Interval 1) [] (WeekStart Monday) [] [ByYearDay 1, ByYearDay (-1)] [] [] [] [] [] []
            `shouldBe` Just (l (d 2020 01 01) tod)
      specify "Every first and last day of the year, at the start" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (l (d 2019 01 01) tod) (Interval 1) [] (WeekStart Monday) [] [ByYearDay 1, ByYearDay (-1)] [] [] [] [] [] []
            `shouldBe` Just (l (d 2019 12 31) tod)
      specify "Every ByMonth February" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (l (d 2019 02 05) tod) (Interval 1) [ByMonth February] (WeekStart Monday) [] [] [] [] [] [] [] []
            `shouldBe` Just (l (d 2020 02 05) tod)
      specify "Every first day of the year, as long as it's also in the first week of the year" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (l (d 2026 01 01) tod) (Interval 1) [] (WeekStart Monday) [ByWeekNo 1] [ByYearDay 1] [] [] [] [] [] []
            `shouldBe` Just (l (d 2029 01 01) tod)
      specify "Every 1st of march, except on leap years" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (l (d 2019 03 01) tod) (Interval 1) [ByMonth March] (WeekStart Monday) [] [ByYearDay 60] [] [] [] [] [] []
            `shouldBe` Just (l (d 2021 03 01) tod)
    describe "ByMonthDay" $ do
      specify "Every 29th day of every month" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (l (d 2019 01 29) tod) (Interval 1) [] (WeekStart Monday) [] [] [ByMonthDay 29] [] [] [] [] []
            `shouldBe` Just (l (d 2019 03 29) tod)
      specify "Every 15th and 20th day of every month" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (l (d 2020 01 20) tod) (Interval 1) [] (WeekStart Monday) [] [] [ByMonthDay 15, ByMonthDay 20] [] [] [] [] []
            `shouldBe` Just (l (d 2020 02 15) tod)
      specify "Every 15th and 20th day of every ByMonth February and ByMonth March" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (l (d 2020 03 20) tod) (Interval 1) [ByMonth February, ByMonth March] (WeekStart Monday) [] [] [ByMonthDay 15, ByMonthDay 20] [] [] [] [] []
            `shouldBe` Just (l (d 2021 02 15) tod)
      specify "Every 29th day of the month that is also the 60th day of the year (29 feb)" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (l (d 2020 02 29) tod) (Interval 1) [] (WeekStart Monday) [] [ByYearDay 60] [ByMonthDay 29] [] [] [] [] []
            `shouldBe` Just (l (d 2024 02 29) tod)
      specify "Every 30th or 31st day of the month that is also the first week of the year" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (l (d 2018 12 31) tod) (Interval 1) [] (WeekStart Monday) [ByWeekNo 1] [] [ByMonthDay 30, ByMonthDay 31] [] [] [] [] []
            `shouldBe` Just (l (d 2019 12 30) tod)
    describe "ByDay" $ do
      specify "Every monday and wednesday" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (l (d 2020 08 12) tod) (Interval 1) [] (WeekStart Monday) [] [] [] [Every Monday, Every Wednesday] [] [] [] []
            `shouldBe` Just (l (d 2020 08 17) tod)
      specify "Every first monday of the year" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (l (d 2020 01 06) tod) (Interval 1) [] (WeekStart Monday) [] [] [] [Specific 1 Monday] [] [] [] []
            `shouldBe` Just (l (d 2021 01 04) tod)
      specify "Every monday in the first and second weeks of the year" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (l (d 2020 01 06) tod) (Interval 1) [] (WeekStart Monday) [ByWeekNo 1, ByWeekNo 2] [] [] [Every Monday] [] [] [] []
            `shouldBe` Just (l (d 2021 01 04) tod)
      specify "Every saturday in june" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (l (d 2020 06 13) tod) (Interval 1) [ByMonth June] (WeekStart Monday) [] [] [] [Every Saturday] [] [] [] []
            `shouldBe` Just (l (d 2020 06 20) tod)
      specify "Every fourth saturday in june" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (l (d 2019 06 22) tod) (Interval 1) [ByMonth June] (WeekStart Monday) [] [] [] [Specific 4 Saturday] [] [] [] []
            `shouldBe` Just (l (d 2020 06 27) tod)
      specify "Every last saturday in june" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (l (d 2020 06 13) tod) (Interval 1) [ByMonth June] (WeekStart Monday) [] [] [] [Specific (-1) Saturday] [] [] [] []
            `shouldBe` Just (l (d 2020 06 27) tod)
      specify "Every monday, the first of the month" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (l (d 2021 03 01) tod) (Interval 1) [] (WeekStart Monday) [] [] [ByMonthDay 1] [Every Monday] [] [] [] []
            `shouldBe` Just (l (d 2021 11 01) tod)
      specify "Every tuesday, on a year day divisible by 100" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (l (d 2020 10 26) tod) (Interval 1) [] (WeekStart Monday) [] [ByYearDay 100, ByYearDay 200, ByYearDay 300] [] [Every Tuesday] [] [] [] []
            `shouldBe` Just (l (d 2022 07 19) tod)
      specify "Every Monday and Tuesday in the first week of every year" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (l (d 2019 12 31) tod) (Interval 1) [] (WeekStart Monday) [ByWeekNo 1] [] [] [Every Monday, Every Tuesday] [] [] [] []
            `shouldBe` Just (l (d 2021 01 04) tod)
    describe "ByHour" $ do
      specify "16h every other year" $
        yearlyDateTimeNextOccurrence limit (LocalTime (d 2020 08 06) (t 16 00 00)) (Interval 2) [] (WeekStart Monday) [] [] [] [] [ByHour 16] [] [] []
          `shouldBe` Just (LocalTime (d 2022 08 06) (t 16 00 00))
    describe "ByMinute" $ do
      specify "16h20 every third year" $
        yearlyDateTimeNextOccurrence limit (LocalTime (d 2020 08 06) (t 16 20 00)) (Interval 3) [] (WeekStart Monday) [] [] [] [] [ByHour 16] [ByMinute 20] [] []
          `shouldBe` Just (LocalTime (d 2023 08 06) (t 16 20 00))
    describe "BySecond" $ do
      specify "16h20m30s every fourth year" $
        yearlyDateTimeNextOccurrence limit (LocalTime (d 2020 08 06) (t 16 20 30)) (Interval 4) [] (WeekStart Monday) [] [] [] [] [ByHour 16] [ByMinute 20] [BySecond 30] []
          `shouldBe` Just (LocalTime (d 2024 08 06) (t 16 20 30))
      specify "every 15th and 20th second" $
        yearlyDateTimeNextOccurrence limit (LocalTime (d 2020 08 06) (t 15 00 15)) (Interval 1) [] (WeekStart Monday) [] [] [] [] [] [] [BySecond 15, BySecond 20] []
          `shouldBe` Just (LocalTime (d 2020 08 06) (t 15 00 20))
    describe "BySetPos" $ do
      specify "The last weekday of the year" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (LocalTime (d 2022 10 05) tod) (Interval 1) [] (WeekStart Monday) [] [] [] [Every Monday, Every Tuesday, Every Wednesday, Every Thursday, Every Friday] [] [] [] [BySetPos (-1)]
            `shouldBe` Just (LocalTime (d 2022 12 30) tod)
  describe "yearlyDateNextOccurrence limit" $ do
    --  An unimportant limit because we don't specify any rules that have no occurrences
    let limit = d 2030 01 01
    describe "No ByX's" $ do
      specify "Every year" $
        yearlyDateNextOccurrence limit (d 2020 08 08) (Interval 1) [] (WeekStart Monday) [] [] [] [] []
          `shouldBe` Just (d 2021 08 08)
      specify "Every other year" $
        yearlyDateNextOccurrence limit (d 2020 08 08) (Interval 2) [] (WeekStart Monday) [] [] [] [] []
          `shouldBe` Just (d 2022 08 08)
    describe "ByMonth" $ do
      specify "Every year in Sept" $
        yearlyDateNextOccurrence limit (d 2019 09 30) (Interval 1) [ByMonth September] (WeekStart Monday) [] [] [] [] []
          `shouldBe` Just (d 2020 09 30)
      specify "Every other year in Sept" $
        yearlyDateNextOccurrence limit (d 2019 09 30) (Interval 2) [ByMonth September] (WeekStart Monday) [] [] [] [] []
          `shouldBe` Just (d 2021 09 30)
      specify "Every five years in Sept" $
        yearlyDateNextOccurrence limit (d 2015 09 30) (Interval 5) [ByMonth September] (WeekStart Monday) [] [] [] [] []
          `shouldBe` Just (d 2020 09 30)
      specify "Every year in Sept and Nov" $
        yearlyDateNextOccurrence limit (d 2019 09 30) (Interval 1) [ByMonth September, ByMonth November] (WeekStart Monday) [] [] [] [] []
          `shouldBe` Just (d 2019 11 30)
    describe "ByWeekNo" $ do
      specify "Every last week of the year" $
        yearlyDateNextOccurrence limit (d 2019 12 31) (Interval 1) [] (WeekStart Monday) [ByWeekNo (-1)] [] [] [] []
          `shouldBe` Just (d 2020 12 28)
      specify "Every sixth week, in february" $
        yearlyDateNextOccurrence limit (d 2025 02 09) (Interval 1) [ByMonth February] (WeekStart Monday) [ByWeekNo 6] [] [] [] []
          `shouldBe` Just (d 2026 02 02)
      specify "Every first week of the year" $
        yearlyDateNextOccurrence limit (d 2019 12 31) (Interval 1) [] (WeekStart Monday) [ByWeekNo 1] [] [] [] []
          `shouldBe` Just (d 2020 01 01)
    describe "ByYearDay" $ do
      specify "Every first and last day of the year, at the end" $
        yearlyDateNextOccurrence limit (d 2019 12 31) (Interval 1) [] (WeekStart Monday) [] [ByYearDay 1, ByYearDay (-1)] [] [] []
          `shouldBe` Just (d 2020 01 01)
      specify "Every first and last day of the year, at the start" $
        yearlyDateNextOccurrence limit (d 2019 01 01) (Interval 1) [] (WeekStart Monday) [] [ByYearDay 1, ByYearDay (-1)] [] [] []
          `shouldBe` Just (d 2019 12 31)
      specify "Every ByMonth February" $
        yearlyDateNextOccurrence limit (d 2019 02 05) (Interval 1) [ByMonth February] (WeekStart Monday) [] [] [] [] []
          `shouldBe` Just (d 2020 02 05)
      specify "Every first day of the year, as long as it's also in the first week of the year" $
        yearlyDateNextOccurrence limit (d 2026 01 01) (Interval 1) [] (WeekStart Monday) [ByWeekNo 1] [ByYearDay 1] [] [] []
          `shouldBe` Just (d 2029 01 01)
      specify "Every 1st of march, except on leap years" $
        yearlyDateNextOccurrence limit (d 2019 03 01) (Interval 1) [ByMonth March] (WeekStart Monday) [] [ByYearDay 60] [] [] []
          `shouldBe` Just (d 2021 03 01)
    describe "ByMonthDay" $ do
      specify "Every 29th day of every month" $
        yearlyDateNextOccurrence limit (d 2019 01 29) (Interval 1) [] (WeekStart Monday) [] [] [ByMonthDay 29] [] []
          `shouldBe` Just (d 2019 03 29)
      specify "Every 15th and 20th day of every month" $
        yearlyDateNextOccurrence limit (d 2020 01 20) (Interval 1) [] (WeekStart Monday) [] [] [ByMonthDay 15, ByMonthDay 20] [] []
          `shouldBe` Just (d 2020 02 15)
      specify "Every 15th and 20th day of every ByMonth February and ByMonth March" $
        yearlyDateNextOccurrence limit (d 2020 03 20) (Interval 1) [ByMonth February, ByMonth March] (WeekStart Monday) [] [] [ByMonthDay 15, ByMonthDay 20] [] []
          `shouldBe` Just (d 2021 02 15)
      specify "Every 29th day of the month that is also the 60th day of the year (29 feb)" $
        yearlyDateNextOccurrence limit (d 2020 02 29) (Interval 1) [] (WeekStart Monday) [] [ByYearDay 60] [ByMonthDay 29] [] []
          `shouldBe` Just (d 2024 02 29)
      specify "Every 30th or 31st day of the month that is also the first week of the year" $
        yearlyDateNextOccurrence limit (d 2018 12 31) (Interval 1) [] (WeekStart Monday) [ByWeekNo 1] [] [ByMonthDay 30, ByMonthDay 31] [] []
          `shouldBe` Just (d 2019 12 30)
    describe "ByDay" $ do
      specify "Every monday and wednesday" $
        yearlyDateNextOccurrence limit (d 2020 08 12) (Interval 1) [] (WeekStart Monday) [] [] [] [Every Monday, Every Wednesday] []
          `shouldBe` Just (d 2020 08 17)
      specify "Every first monday of the year" $
        yearlyDateNextOccurrence limit (d 2020 01 06) (Interval 1) [] (WeekStart Monday) [] [] [] [Specific 1 Monday] []
          `shouldBe` Just (d 2021 01 04)
      specify "Every 4th monday of the year" $
        yearlyDateNextOccurrence limit (d 2020 01 27) (Interval 1) [] (WeekStart Monday) [] [] [] [Specific 4 Monday] []
          `shouldBe` Just (d 2021 01 25)
      specify "Every monday in the first and second weeks of the year" $
        yearlyDateNextOccurrence limit (d 2020 01 06) (Interval 1) [] (WeekStart Monday) [ByWeekNo 1, ByWeekNo 2] [] [] [Every Monday] []
          `shouldBe` Just (d 2021 01 04)
      specify "Every saturday in june" $
        yearlyDateNextOccurrence limit (d 2020 06 13) (Interval 1) [ByMonth June] (WeekStart Monday) [] [] [] [Every Saturday] []
          `shouldBe` Just (d 2020 06 20)
      specify "Every fourth saturday in june" $
        yearlyDateNextOccurrence limit (d 2019 06 22) (Interval 1) [ByMonth June] (WeekStart Monday) [] [] [] [Specific 4 Saturday] []
          `shouldBe` Just (d 2020 06 27)
      specify "Every last saturday in june" $
        yearlyDateNextOccurrence limit (d 2020 06 13) (Interval 1) [ByMonth June] (WeekStart Monday) [] [] [] [Specific (-1) Saturday] []
          `shouldBe` Just (d 2020 06 27)
      specify "Every monday, the first of the month" $
        yearlyDateNextOccurrence limit (d 2021 03 01) (Interval 1) [] (WeekStart Monday) [] [] [ByMonthDay 1] [Every Monday] []
          `shouldBe` Just (d 2021 11 01)
      specify "Every first monday of the year that is also the first of the month" $
        yearlyDateNextOccurrence limit (d 2001 01 01) (Interval 1) [] (WeekStart Monday) [] [] [ByMonthDay 1] [Specific 1 Monday] []
          `shouldBe` Just (d 2007 01 01)
      specify "Every tuesday, on a year day divisible by 100" $
        yearlyDateNextOccurrence limit (d 2020 10 26) (Interval 1) [] (WeekStart Monday) [] [ByYearDay 100, ByYearDay 200, ByYearDay 300] [] [Every Tuesday] []
          `shouldBe` Just (d 2022 07 19)
      specify "Every Monday and Tuesday in the first week of every year" $
        yearlyDateNextOccurrence limit (d 2019 12 31) (Interval 1) [] (WeekStart Monday) [ByWeekNo 1] [] [] [Every Monday, Every Tuesday] []
          `shouldBe` Just (d 2021 01 04)
