{-# LANGUAGE OverloadedLists #-}

module ICal.Recurrence.RecurrenceRule.MonthlySpec (spec) where

import Data.GenValidity.Time ()
import Data.Maybe
import Data.Time
import ICal.Conformance.TestUtils
import ICal.PropertyType.RecurrenceRule
import ICal.Recurrence.RecurrenceRule
import ICal.Recurrence.RecurrenceRule.Monthly
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  let d = fromGregorian
  let l = LocalTime
  let t = TimeOfDay
  describe "recurRecurrenceRuleLocalTimes" $ do
    specify "it works for this complex example" $
      forAllValid $ \tod ->
        let limit = d 2024 01 01
            rule =
              (makeRecurrenceRule Monthly)
                { recurrenceRuleInterval = Interval 2,
                  recurrenceRuleUntilCount = Just $ Right $ Count 5,
                  recurrenceRuleByDay = [Every Tuesday, Every Sunday],
                  recurrenceRuleByMonth = [ByMonth March, ByMonth April, ByMonth August],
                  recurrenceRuleWeekStart = WeekStart Wednesday,
                  recurrenceRuleBySetPos = [BySetPos (-1)]
                }
            start = LocalTime (d 2020 08 30) tod
         in --  This limit will be reached and cut of 2 recurrences
            shouldConform (recurRecurrenceRuleLocalTimes limit start rule)
              `shouldReturn` [ LocalTime (d 2020 08 30) tod,
                               LocalTime (d 2021 04 27) tod,
                               LocalTime (d 2021 08 31) tod,
                               LocalTime (d 2022 04 26) tod,
                               LocalTime (d 2022 08 30) tod
                             ]
  describe "monthlyDateTimeRecurrence" $ do
    let monthlyDateTimeNextOccurrence start lim i ba bb bc bd be bf bg =
          listToMaybe $ monthlyDateTimeRecurrence start lim i ba bb bc bd be bf bg
    --  An unimportant limit because we don't specify any rules that have no occurrances
    let limit = d 2022 01 01
    describe "No ByX's" $ do
      specify "Every month" $
        forAllValid $ \tod ->
          monthlyDateTimeNextOccurrence limit (l (d 2020 08 08) tod) (Interval 1) [] [] [] [] [] [] []
            `shouldBe` Just (l (d 2020 09 08) tod)
      specify "Every other month" $
        forAllValid $ \tod ->
          monthlyDateTimeNextOccurrence limit (l (d 2020 08 08) tod) (Interval 2) [] [] [] [] [] [] []
            `shouldBe` Just (l (d 2020 10 08) tod)
    describe "ByMonth" $ do
      specify "Every month in Sept" $
        forAllValid $ \tod ->
          monthlyDateTimeNextOccurrence limit (l (d 2019 09 30) tod) (Interval 1) [ByMonth September] [] [] [] [] [] []
            `shouldBe` Just (l (d 2020 09 30) tod)
      specify "Every other month in Sept" $
        forAllValid $ \tod ->
          monthlyDateTimeNextOccurrence limit (l (d 2019 09 30) tod) (Interval 2) [ByMonth September] [] [] [] [] [] []
            `shouldBe` Just (l (d 2020 09 30) tod)
      specify "Every five months in Sept" $
        forAllValid $ \tod ->
          monthlyDateTimeNextOccurrence limit (l (d 2015 09 30) tod) (Interval 5) [ByMonth September] [] [] [] [] [] []
            `shouldBe` Just (l (d 2020 09 30) tod)
    -- No 'ByWeekNo' because it's excluded by the table
    -- No 'ByYearDay' because it's excluded by the table
    describe "ByDay" $ do
      specify "Every wednesday and thursday" $
        forAllValid $ \tod ->
          monthlyDateTimeNextOccurrence limit (l (d 2020 08 12) tod) (Interval 1) [] [] [Every Wednesday, Every Thursday] [] [] [] []
            `shouldBe` Just (l (d 2020 08 13) tod)
      specify "Every other thursday and friday" $
        forAllValid $ \tod ->
          monthlyDateTimeNextOccurrence limit (l (d 2020 08 28) tod) (Interval 2) [] [] [Every Thursday, Every Friday] [] [] [] []
            `shouldBe` Just (l (d 2020 10 01) tod)
      specify "Every sunday, at the end of the year" $
        forAllValid $ \tod ->
          monthlyDateTimeNextOccurrence limit (l (d 2019 12 29) tod) (Interval 2) [] [] [Every Sunday] [] [] [] []
            `shouldBe` Just (l (d 2020 02 02) tod)
      specify "Every other sunday, at the end of the year" $
        forAllValid $ \tod ->
          monthlyDateTimeNextOccurrence limit (l (d 2019 12 29) tod) (Interval 2) [] [] [Every Sunday] [] [] [] []
            `shouldBe` Just (l (d 2020 02 02) tod)
      specify "Every first wednesday of the month" $
        forAllValid $ \tod ->
          monthlyDateTimeNextOccurrence limit (l (d 2020 08 05) tod) (Interval 1) [] [] [Specific 1 Wednesday] [] [] [] []
            `shouldBe` Just (l (d 2020 09 02) tod)
      specify "Every other first thursday" $
        forAllValid $ \tod ->
          monthlyDateTimeNextOccurrence limit (l (d 2020 08 06) tod) (Interval 2) [] [] [Specific 1 Thursday] [] [] [] []
            `shouldBe` Just (l (d 2020 10 01) tod)
      specify "Every last wednesday of the month" $
        forAllValid $ \tod ->
          monthlyDateTimeNextOccurrence limit (l (d 2020 08 26) tod) (Interval 1) [] [] [Specific (-1) Wednesday] [] [] [] []
            `shouldBe` Just (l (d 2020 09 30) tod)
      specify "Every wednesday that falls on the 10th of the month" $
        forAllValid $ \tod ->
          monthlyDateTimeNextOccurrence limit (l (d 2019 07 10) tod) (Interval 1) [] [ByMonthDay 10] [Every Wednesday] [] [] [] []
            `shouldBe` Just (l (d 2020 06 10) tod)
      specify "Every second wednesday that falls on the 10th of the month" $
        forAllValid $ \tod ->
          monthlyDateTimeNextOccurrence limit (l (d 2019 07 10) tod) (Interval 1) [] [ByMonthDay 10] [Specific 2 Wednesday] [] [] [] []
            `shouldBe` Just (l (d 2020 06 10) tod)
      specify "Every last wednesday that falls on the 27th of the month" $
        forAllValid $ \tod ->
          monthlyDateTimeNextOccurrence limit (l (d 2019 03 27) tod) (Interval 1) [] [ByMonthDay 27] [Specific (-1) Wednesday] [] [] [] []
            `shouldBe` Just (l (d 2019 11 27) tod)
    describe "ByHour" $ do
      specify "16h every other month" $
        monthlyDateTimeNextOccurrence limit (LocalTime (d 2020 08 06) (t 16 00 00)) (Interval 2) [] [] [] [ByHour 16] [] [] []
          `shouldBe` Just (LocalTime (d 2020 10 06) (t 16 00 00))
    describe "ByMinute" $ do
      specify "16h20 every third month" $
        monthlyDateTimeNextOccurrence limit (LocalTime (d 2020 08 06) (t 16 20 00)) (Interval 3) [] [] [] [ByHour 16] [ByMinute 20] [] []
          `shouldBe` Just (LocalTime (d 2020 11 06) (t 16 20 00))
    describe "BySecond" $ do
      specify "16h20m30s every fourth month" $
        monthlyDateTimeNextOccurrence limit (LocalTime (d 2020 08 06) (t 16 20 30)) (Interval 4) [] [] [] [ByHour 16] [ByMinute 20] [BySecond 30] []
          `shouldBe` Just (LocalTime (d 2020 12 06) (t 16 20 30))
      specify "every 15th and 20th second" $
        monthlyDateTimeNextOccurrence limit (LocalTime (d 2020 08 06) (t 15 00 15)) (Interval 1) [] [] [] [] [] [BySecond 15, BySecond 20] []
          `shouldBe` Just (LocalTime (d 2020 08 06) (t 15 00 20))
    describe "BySetPos" $ do
      specify "The last weekday of the month" $
        forAllValid $ \tod ->
          monthlyDateTimeNextOccurrence limit (LocalTime (d 2020 04 30) tod) (Interval 1) [] [] [Every Monday, Every Tuesday, Every Wednesday, Every Thursday, Every Friday] [] [] [] [BySetPos (-1)]
            `shouldBe` Just (LocalTime (d 2020 05 29) tod)
  describe "monthlyDateNextOccurrence" $ do
    let monthlyDateNextOccurrence lim start i ba bb bc bd =
          fmap localDay $ listToMaybe $ monthlyDateTimeRecurrence lim (LocalTime start midnight) i ba bb bc [] [] [] bd
    --  An unimportant limit because we don't specify any rules that have no occurrances
    let limit = d 2023 01 01
    describe "No ByX's" $ do
      specify "Every month" $
        monthlyDateNextOccurrence limit (d 2020 08 08) (Interval 1) [] [] [] []
          `shouldBe` Just (d 2020 09 08)
      specify "Every other month" $
        monthlyDateNextOccurrence limit (d 2020 08 08) (Interval 2) [] [] [] []
          `shouldBe` Just (d 2020 10 08)
    describe "ByMonth" $ do
      specify "Every month in Sept" $
        monthlyDateNextOccurrence limit (d 2019 09 30) (Interval 1) [ByMonth September] [] [] []
          `shouldBe` Just (d 2020 09 30)
      specify "Every other month in Sept" $
        monthlyDateNextOccurrence limit (d 2019 09 30) (Interval 2) [ByMonth September] [] [] []
          `shouldBe` Just (d 2020 09 30)
      specify "Every five months in Sept" $
        monthlyDateNextOccurrence limit (d 2015 09 30) (Interval 5) [ByMonth September] [] [] []
          `shouldBe` Just (d 2020 09 30)
    -- No 'ByWeekNo' because it's excluded by the table
    -- No 'ByYearDay' because it's excluded by the table
    describe "ByDay" $ do
      specify "Every wednesday and thursday" $
        monthlyDateNextOccurrence limit (d 2020 08 12) (Interval 1) [] [] [Every Wednesday, Every Thursday] []
          `shouldBe` Just (d 2020 08 13)
      specify "Every other thursday and friday" $
        monthlyDateNextOccurrence limit (d 2020 08 28) (Interval 2) [] [] [Every Thursday, Every Friday] []
          `shouldBe` Just (d 2020 10 01)
      specify "Every sunday, at the end of the year" $
        monthlyDateNextOccurrence limit (d 2019 12 29) (Interval 2) [] [] [Every Sunday] []
          `shouldBe` Just (d 2020 02 02)
      specify "Every other month, ever sunday, at the end of the year" $
        monthlyDateNextOccurrence limit (d 2019 12 29) (Interval 2) [] [] [Every Sunday] []
          `shouldBe` Just (d 2020 02 02)
      specify "Every first wednesday of the month" $
        monthlyDateNextOccurrence limit (d 2020 08 05) (Interval 1) [] [] [Specific 1 Wednesday] []
          `shouldBe` Just (d 2020 09 02)
      specify "Every other first thursday" $
        monthlyDateNextOccurrence limit (d 2020 08 06) (Interval 2) [] [] [Specific 1 Thursday] []
          `shouldBe` Just (d 2020 10 01)
      specify "Every last wednesday of the month" $
        monthlyDateNextOccurrence limit (d 2020 08 26) (Interval 1) [] [] [Specific (-1) Wednesday] []
          `shouldBe` Just (d 2020 09 30)
      specify "Every wednesday that falls on the 10th of the month" $
        monthlyDateNextOccurrence limit (d 2019 07 10) (Interval 1) [] [ByMonthDay 10] [Every Wednesday] []
          `shouldBe` Just (d 2020 06 10)
      specify "Every second wednesday that falls on the 10th of the month" $
        monthlyDateNextOccurrence limit (d 2019 07 10) (Interval 1) [] [ByMonthDay 10] [Specific 2 Wednesday] []
          `shouldBe` Just (d 2020 06 10)
      specify "Every last wednesday that falls on the 27th of the month" $
        monthlyDateNextOccurrence limit (d 2019 03 27) (Interval 1) [] [ByMonthDay 27] [Specific (-1) Wednesday] []
          `shouldBe` Just (d 2019 11 27)
    describe "BySetPos" $ do
      specify "This special case" $
        monthlyDateNextOccurrence limit (d 2021 08 31) (Interval 2) [ByMonth March, ByMonth April, ByMonth August] [] [Every Tuesday, Every Sunday] [BySetPos (-1)]
          `shouldBe` Just (d 2022 04 26)
