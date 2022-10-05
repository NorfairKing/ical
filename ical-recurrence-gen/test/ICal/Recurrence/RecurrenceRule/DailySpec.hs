{-# LANGUAGE OverloadedLists #-}

module ICal.Recurrence.RecurrenceRule.DailySpec (spec) where

import Data.GenValidity.Time ()
import Data.Maybe
import qualified Data.Set as S
import Data.Time
import ICal.Conformance.TestUtils
import ICal.PropertyType.RecurrenceRule
import ICal.Recurrence.RecurrenceRule
import ICal.Recurrence.RecurrenceRule.Daily
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  let d = fromGregorian
      t = TimeOfDay
  describe "rruleDateTimeOccurrencesUntil" $ do
    specify "it works for this complex example" $
      let limit = d 2024 01 01
          rule =
            (makeRecurrenceRule Daily)
              { recurrenceRuleInterval = Interval 3,
                recurrenceRuleUntilCount = Just $ Right $ Count 10,
                recurrenceRuleByMonthDay = [ByMonthDay 10, ByMonthDay 20, ByMonthDay 30],
                recurrenceRuleByMonth = [ByMonth September, ByMonth October]
              }
          tod = t 04 30 00
          start = LocalTime (d 2020 09 10) tod
       in --  This limit will be reached and cut of 2 recurrences
          shouldConform (recurRecurrenceRuleLocalTimes limit start rule)
            `shouldReturn` [ LocalTime (d 2020 09 10) tod,
                             LocalTime (d 2020 10 10) tod,
                             LocalTime (d 2021 09 20) tod,
                             LocalTime (d 2021 10 20) tod,
                             LocalTime (d 2022 09 30) tod,
                             LocalTime (d 2022 10 30) tod,
                             LocalTime (d 2023 09 10) tod,
                             LocalTime (d 2023 10 10) tod
                           ]
    specify "It works for this BYSETPOS example: The last hour of every day" $
      --  An limit in the future because it won't be reached anyway
      let limit = d 2024 01 01
          rule =
            (makeRecurrenceRule Daily)
              { recurrenceRuleInterval = Interval 1,
                recurrenceRuleUntilCount = Just $ Right $ Count 3,
                recurrenceRuleBySetPos = [BySetPos (-1)],
                recurrenceRuleByHour =
                  [ ByHour 20,
                    ByHour 21,
                    ByHour 22,
                    ByHour 23
                  ],
                recurrenceRuleByMinute = [ByMinute 0],
                recurrenceRuleBySecond = [BySecond 0]
              }
          start = LocalTime (d 2020 08 07) (t 23 00 00)
       in shouldConform (recurRecurrenceRuleLocalTimes limit start rule)
            `shouldReturn` [ LocalTime (d 2020 08 07) (t 23 00 00),
                             LocalTime (d 2020 08 08) (t 23 00 00),
                             LocalTime (d 2020 08 09) (t 23 00 00)
                           ]
    specify "It works for this BYSETPOS example: The last two hours of every day" $
      --  An limit in the future because it won't be reached anyway
      let limit = d 2024 01 01
          rule =
            (makeRecurrenceRule Daily)
              { recurrenceRuleInterval = Interval 1,
                recurrenceRuleUntilCount = Just $ Right $ Count 4,
                recurrenceRuleBySetPos = [BySetPos (-1), BySetPos (-2)],
                recurrenceRuleByHour =
                  [ ByHour 22,
                    ByHour 23
                  ],
                recurrenceRuleByMinute = [ByMinute 0],
                recurrenceRuleBySecond = [BySecond 0]
              }
          start = LocalTime (d 2020 08 07) (t 23 00 00)
       in shouldConform (recurRecurrenceRuleLocalTimes limit start rule)
            `shouldReturn` [ LocalTime (d 2020 08 07) (t 23 00 00),
                             LocalTime (d 2020 08 08) (t 22 00 00),
                             LocalTime (d 2020 08 08) (t 23 00 00),
                             LocalTime (d 2020 08 09) (t 22 00 00)
                           ]
    specify "It works for this meeting that occurs every day for a long time" $
      forAllValid $ \tod -> do
        let limit = d 2030 01 01
            rule = makeRecurrenceRule Daily
            start = LocalTime (d 2020 08 07) tod
        set <- shouldConform (recurRecurrenceRuleLocalTimes limit start rule)
        let intersection = S.filter ((>= d 2029 12 30) . localDay) set
        intersection
          `shouldBe` [ LocalTime (d 2029 12 30) tod,
                       LocalTime (d 2029 12 31) tod,
                       LocalTime (d 2030 01 01) tod
                     ]

  describe "dailyDateTimeRecurrence" $ do
    let dailyDateTimeNextOccurrence lim start i ba bb bc bd be bf bg =
          listToMaybe $ dailyDateTimeRecurrence lim start i ba bb bc bd be bf bg
    --  An unimportant limit because we don't specify any rules that have no occurrances
    let limit = d 2021 01 01
    describe "No ByX's" $ do
      specify "Every day" $
        forAllValid $ \tod ->
          dailyDateTimeNextOccurrence limit (LocalTime (d 2020 08 06) tod) (Interval 1) [] [] [] [] [] [] []
            `shouldBe` Just (LocalTime (d 2020 08 07) tod)
      specify "Every other day" $
        forAllValid $ \tod ->
          dailyDateTimeNextOccurrence limit (LocalTime (d 2020 08 06) tod) (Interval 2) [] [] [] [] [] [] []
            `shouldBe` Just (LocalTime (d 2020 08 08) tod)
    describe "ByMonth" $ do
      specify "Every three days in September" $
        forAllValid $ \tod ->
          dailyDateTimeNextOccurrence limit (LocalTime (d 2020 08 06) tod) (Interval 3) [ByMonth September] [] [] [] [] [] []
            `shouldBe` Just
              (LocalTime (d 2020 09 02) tod)
      specify "Every four days in August" $
        forAllValid $ \tod ->
          dailyDateTimeNextOccurrence limit (LocalTime (d 2020 08 06) tod) (Interval 4) [ByMonth August] [] [] [] [] [] []
            `shouldBe` Just (LocalTime (d 2020 08 10) tod)
    describe "ByMonthDay" $ do
      specify "Every tenth day of the month" $
        forAllValid $ \tod ->
          dailyDateTimeNextOccurrence limit (LocalTime (d 2020 08 10) tod) (Interval 1) [] [ByMonthDay 10] [] [] [] [] []
            `shouldBe` Just (LocalTime (d 2020 09 10) tod)
      specify "Every tenth of September" $
        forAllValid $ \tod ->
          dailyDateTimeNextOccurrence limit (LocalTime (d 2019 09 10) tod) (Interval 1) [ByMonth September] [ByMonthDay 10] [] [] [] [] []
            `shouldBe` Just (LocalTime (d 2020 09 10) tod)
      specify "Every last day of February in a non-leap year" $
        forAllValid $ \tod ->
          dailyDateTimeNextOccurrence limit (LocalTime (d 2018 02 28) tod) (Interval 1) [ByMonth February] [ByMonthDay (-1)] [] [] [] [] []
            `shouldBe` Just (LocalTime (d 2019 02 28) tod)
      specify "Every last day of February in a leap year" $
        forAllValid $ \tod ->
          dailyDateTimeNextOccurrence limit (LocalTime (d 2019 02 28) tod) (Interval 1) [ByMonth February] [ByMonthDay (-1)] [] [] [] [] []
            `shouldBe` Just (LocalTime (d 2020 02 29) tod)
      specify "Every second-to-last day of September" $
        forAllValid $ \tod ->
          dailyDateTimeNextOccurrence limit (LocalTime (d 2019 09 33) tod) (Interval 1) [ByMonth September] [ByMonthDay (-1)] [] [] [] [] []
            `shouldBe` Just (LocalTime (d 2020 09 33) tod)
    describe "ByDay" $ do
      specify "Every tuesday" $
        forAllValid $ \tod ->
          dailyDateTimeNextOccurrence limit (LocalTime (d 2020 08 04) tod) (Interval 1) [] [] [Tuesday] [] [] [] []
            `shouldBe` Just (LocalTime (d 2020 08 11) tod)
      specify "Every tuesday in September" $
        forAllValid $ \tod ->
          dailyDateTimeNextOccurrence limit (LocalTime (d 2020 08 04) tod) (Interval 1) [ByMonth September] [] [Tuesday] [] [] [] []
            `shouldBe` Just (LocalTime (d 2020 09 01) tod)
    describe "ByHour" $ do
      specify "16h every other day" $
        dailyDateTimeNextOccurrence limit (LocalTime (d 2020 08 06) (t 16 00 00)) (Interval 2) [] [] [] [ByHour 16] [] [] []
          `shouldBe` Just (LocalTime (d 2020 08 08) (t 16 00 00))
    describe "ByMinute" $ do
      specify "16h20 every third day" $
        dailyDateTimeNextOccurrence limit (LocalTime (d 2020 08 06) (t 16 20 00)) (Interval 3) [] [] [] [ByHour 16] [ByMinute 20] [] []
          `shouldBe` Just (LocalTime (d 2020 08 09) (t 16 20 00))
    describe "BySecond" $ do
      specify "16h20m30s every fourth day" $
        dailyDateTimeNextOccurrence limit (LocalTime (d 2020 08 06) (t 15 00 00)) (Interval 4) [] [] [] [ByHour 16] [ByMinute 20] [BySecond 30] []
          `shouldBe` Just (LocalTime (d 2020 08 06) (t 16 20 30))
      specify "every 15th and 20th second" $
        dailyDateTimeNextOccurrence limit (LocalTime (d 2020 08 06) (t 15 00 15)) (Interval 1) [] [] [] [] [] [BySecond 15, BySecond 20] []
          `shouldBe` Just (LocalTime (d 2020 08 06) (t 15 00 20))
  describe "rruleDateOccurrencesUntil" $ do
    specify "it works for this complex example" $
      forAllValid $ \tod ->
        let limit = d 2024 01 01
            rule =
              (makeRecurrenceRule Daily)
                { recurrenceRuleInterval = Interval 3,
                  recurrenceRuleUntilCount = Just $ Right $ Count 10,
                  recurrenceRuleByMonthDay = [ByMonthDay 10, ByMonthDay 20, ByMonthDay 30],
                  recurrenceRuleByMonth = [ByMonth September, ByMonth October]
                }
            start = LocalTime (d 2020 09 10) tod
         in --  This limit will be reached and cut of 2 recurrences
            shouldConform (recurRecurrenceRuleLocalTimes limit start rule)
              `shouldReturn` [ LocalTime (d 2020 09 10) tod,
                               LocalTime (d 2020 10 10) tod,
                               LocalTime (d 2021 09 20) tod,
                               LocalTime (d 2021 10 20) tod,
                               LocalTime (d 2022 09 30) tod,
                               LocalTime (d 2022 10 30) tod,
                               LocalTime (d 2023 09 10) tod,
                               LocalTime (d 2023 10 10) tod
                             ]
    specify "It works for this BYSETPOS example: The last hour of every day (this is the same as just 'every day'.)" $
      forAllValid $ \tod ->
        --  An limit in the future because it won't be reached anyway
        let limit = d 2024 01 01
            rule =
              (makeRecurrenceRule Daily)
                { recurrenceRuleInterval = Interval 1,
                  recurrenceRuleUntilCount = Just $ Right $ Count 3,
                  recurrenceRuleBySetPos = [BySetPos (-1)]
                }
            start = LocalTime (d 2020 08 07) tod
         in shouldConform (recurRecurrenceRuleLocalTimes limit start rule)
              `shouldReturn` [ LocalTime (d 2020 08 07) tod,
                               LocalTime (d 2020 08 08) tod,
                               LocalTime (d 2020 08 09) tod
                             ]
  describe "dailyDateNextOccurrence" $ do
    let dailyDateNextOccurrence lim start i ba bb bc =
          fmap localDay . listToMaybe $ dailyDateTimeRecurrence lim (LocalTime start midnight) i ba bb bc [] [] [] []
    --  An unimportant limit because we don't specify any rules that have no occurrances
    let limit = d 2021 01 01
    describe "No ByX's" $ do
      specify "Every day" $
        dailyDateNextOccurrence limit (d 2020 08 06) (Interval 1) [] [] []
          `shouldBe` Just (d 2020 08 07)
      specify "Every other day" $
        dailyDateNextOccurrence limit (d 2020 08 06) (Interval 2) [] [] []
          `shouldBe` Just (d 2020 08 08)
    describe "ByMonth" $ do
      specify "Every three days in September" $
        dailyDateNextOccurrence limit (d 2020 08 06) (Interval 3) [ByMonth September] [] []
          `shouldBe` Just (d 2020 09 02)
      specify "Every four days in August" $
        dailyDateNextOccurrence limit (d 2020 08 06) (Interval 4) [ByMonth August] [] []
          `shouldBe` Just (d 2020 08 10)
    describe "ByMonthDay" $ do
      specify "Every tenth day of the month" $
        dailyDateNextOccurrence limit (d 2020 08 10) (Interval 1) [] [ByMonthDay 10] []
          `shouldBe` Just (d 2020 09 10)
      specify "Every tenth of September" $
        dailyDateNextOccurrence limit (d 2019 09 10) (Interval 1) [ByMonth September] [ByMonthDay 10] []
          `shouldBe` Just (d 2020 09 10)
      specify "Every last day of February in a non-leap year" $
        dailyDateNextOccurrence limit (d 2018 02 28) (Interval 1) [ByMonth February] [ByMonthDay (-1)] []
          `shouldBe` Just (d 2019 02 28)
      specify "Every last day of February in a leap year" $
        dailyDateNextOccurrence limit (d 2019 02 28) (Interval 1) [ByMonth February] [ByMonthDay (-1)] []
          `shouldBe` Just (d 2020 02 29)
      specify "Every second-to-last day of September" $
        dailyDateNextOccurrence limit (d 2019 09 33) (Interval 1) [ByMonth September] [ByMonthDay (-1)] []
          `shouldBe` Just (d 2020 09 33)
    describe "ByDay" $ do
      specify "Every tuesday" $
        dailyDateNextOccurrence limit (d 2020 08 04) (Interval 1) [] [] [Tuesday]
          `shouldBe` Just (d 2020 08 11)
      specify "Every tuesday in September" $
        dailyDateNextOccurrence limit (d 2020 08 04) (Interval 1) [ByMonth September] [] [Tuesday]
          `shouldBe` Just (d 2020 09 01)
