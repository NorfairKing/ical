{-# LANGUAGE OverloadedLists #-}

module ICal.Recurrence.RecurrenceRule.YearlySpec (spec) where

import Data.GenValidity.Time ()
import Data.Maybe
import Data.Time
import ICal.PropertyType.RecurrenceRule
import ICal.Recurrence.RecurrenceRule.Yearly
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
          `shouldBe` Just (LocalTime (d 2020 10 06) (t 16 00 00))
    describe "ByMinute" $ do
      specify "16h20 every third year" $
        yearlyDateTimeNextOccurrence limit (LocalTime (d 2020 08 06) (t 16 20 00)) (Interval 3) [] (WeekStart Monday) [] [] [] [] [ByHour 16] [ByMinute 20] [] []
          `shouldBe` Just (LocalTime (d 2020 11 06) (t 16 20 00))
    describe "BySecond" $ do
      specify "16h20m30s every fourth year" $
        yearlyDateTimeNextOccurrence limit (LocalTime (d 2020 08 06) (t 16 20 30)) (Interval 4) [] (WeekStart Monday) [] [] [] [] [ByHour 16] [ByMinute 20] [BySecond 30] []
          `shouldBe` Just (LocalTime (d 2020 12 06) (t 16 20 30))
      specify "every 15th and 20th second" $
        yearlyDateTimeNextOccurrence limit (LocalTime (d 2020 08 06) (t 15 00 15)) (Interval 1) [] (WeekStart Monday) [] [] [] [] [] [] [BySecond 15, BySecond 20] []
          `shouldBe` Just (LocalTime (d 2020 08 06) (t 15 00 20))
    describe "BySetPos" $ do
      specify "The last weekday of the year" $
        forAllValid $ \tod ->
          yearlyDateTimeNextOccurrence limit (LocalTime (d 2020 04 30) tod) (Interval 1) [] (WeekStart Monday) [] [] [] [Every Monday, Every Tuesday, Every Wednesday, Every Thursday, Every Friday] [] [] [] [BySetPos (-1)]
            `shouldBe` Just (LocalTime (d 2020 05 29) tod)
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

    describe "BySetPos" $ do
      specify "This special case" $
        yearlyDateNextOccurrence limit (d 2021 08 31) (Interval 2) [ByMonth March, ByMonth April, ByMonth August] (WeekStart Monday) [] [] [] [Every Tuesday, Every Sunday] [BySetPos (-1)]
          `shouldBe` Just (d 2022 04 26)
