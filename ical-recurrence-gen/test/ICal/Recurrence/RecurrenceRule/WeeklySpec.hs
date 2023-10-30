{-# LANGUAGE OverloadedLists #-}

module ICal.Recurrence.RecurrenceRule.WeeklySpec (spec) where

import Data.GenValidity.Time ()
import Data.Maybe
import Data.Time (DayOfWeek (..), LocalTime (..), TimeOfDay (..), fromGregorian, midnight)
import ICal.PropertyType.RecurrenceRule
import ICal.Recurrence.RecurrenceRule
import ICal.Recurrence.RecurrenceRule.Weekly
import ICal.Recurrence.TestUtils
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
              (makeRecurrenceRule Weekly)
                { recurrenceRuleInterval = Interval 2,
                  recurrenceRuleUntilCount = Just $ Right $ Count 11,
                  recurrenceRuleByDay = [Every Wednesday, Every Thursday],
                  recurrenceRuleByMonth = [ByMonth September, ByMonth November],
                  recurrenceRuleWeekStart = WeekStart Wednesday
                }
            start = LocalTime (d 2020 08 20) tod
         in --  This limit will be reached and cut of 2 recurrences
            shouldRecur (recurRecurrenceRuleLocalTimes limit start rule)
              `shouldReturn` [ LocalTime (d 2020 08 20) tod,
                               LocalTime (d 2020 09 02) tod,
                               LocalTime (d 2020 09 03) tod,
                               LocalTime (d 2020 09 16) tod,
                               LocalTime (d 2020 09 17) tod,
                               LocalTime (d 2020 09 30) tod,
                               LocalTime (d 2020 11 11) tod,
                               LocalTime (d 2020 11 12) tod,
                               LocalTime (d 2020 11 25) tod,
                               LocalTime (d 2020 11 26) tod,
                               LocalTime (d 2021 09 01) tod
                             ]
    specify "It works for this BYSETPOS example: The last hour of every week" $
      --  An limit in the future because it won't be reached anyway
      let limit = d 2024 01 01
          rule =
            (makeRecurrenceRule Weekly)
              { recurrenceRuleInterval = Interval 1,
                recurrenceRuleUntilCount = Just $ Right $ Count 3,
                recurrenceRuleBySetPos = [BySetPos (-1)],
                recurrenceRuleByHour =
                  [ ByHour 22,
                    ByHour 23
                  ],
                recurrenceRuleByMinute = [ByMinute 0],
                recurrenceRuleBySecond = [BySecond 0],
                recurrenceRuleWeekStart = WeekStart Monday
              }
          start = LocalTime (d 2020 08 09) (t 23 00 00)
       in shouldRecur (recurRecurrenceRuleLocalTimes limit start rule)
            `shouldReturn` [ LocalTime (d 2020 08 09) (t 23 00 00),
                             LocalTime (d 2020 08 16) (t 23 00 00),
                             LocalTime (d 2020 08 23) (t 23 00 00)
                           ]
    specify "It works for this BYSETPOS example: The last two hours of every week" $
      --  An limit in the future because it won't be reached anyway
      let limit = d 2024 01 01
          rule =
            (makeRecurrenceRule Weekly)
              { recurrenceRuleInterval = Interval 1,
                recurrenceRuleUntilCount = Just $ Right $ Count 4,
                recurrenceRuleBySetPos = [BySetPos (-1), BySetPos (-2)],
                recurrenceRuleByHour =
                  [ ByHour 22,
                    ByHour 23
                  ],
                recurrenceRuleByMinute = [ByMinute 0],
                recurrenceRuleBySecond = [BySecond 0],
                recurrenceRuleWeekStart = WeekStart Monday
              }
          start = LocalTime (d 2020 08 09) (t 22 00 00)
       in shouldRecur (recurRecurrenceRuleLocalTimes limit start rule)
            `shouldReturn` [ LocalTime (d 2020 08 09) (t 22 00 00),
                             LocalTime (d 2020 08 09) (t 23 00 00),
                             LocalTime (d 2020 08 16) (t 22 00 00),
                             LocalTime (d 2020 08 16) (t 23 00 00)
                           ]
  describe "weeklyDateTimeRecurrence" $ do
    --  An unimportant limit because we don't specify any rules that have no occurrances
    let limit = d 2021 01 01
    let weeklyDateTimeNextOccurrence lim start i ba bb bc bd be bf bg =
          listToMaybe $ weeklyDateTimeRecurrence lim start i ba bb bc bd be bf bg
    describe "No ByX's" $ do
      specify "Every week" $
        forAllValid $ \tod ->
          weeklyDateTimeNextOccurrence limit (l (d 2020 08 08) tod) (Interval 1) [] (WeekStart Monday) [] [] [] [] []
            `shouldBe` Just (l (d 2020 08 15) tod)
      specify "Every other week" $
        forAllValid $ \tod ->
          weeklyDateTimeNextOccurrence limit (l (d 2020 08 08) tod) (Interval 2) [] (WeekStart Monday) [] [] [] [] []
            `shouldBe` Just (l (d 2020 08 22) tod)
    describe "ByMonth" $ do
      specify "Every week in Sept" $
        forAllValid $ \tod ->
          weeklyDateTimeNextOccurrence limit (l (d 2019 09 30) tod) (Interval 1) [ByMonth September] (WeekStart Monday) [] [] [] [] []
            `shouldBe` Just (l (d 2020 09 07) tod)
      specify "Every other week in Sept" $
        forAllValid $ \tod ->
          weeklyDateTimeNextOccurrence limit (l (d 2019 09 30) tod) (Interval 2) [ByMonth September] (WeekStart Monday) [] [] [] [] []
            `shouldBe` Just (l (d 2020 09 14) tod)
    -- No 'ByWeekNo' because it's excluded by the table
    -- No 'ByYearDay' because it's excluded by the table
    -- No 'ByMonthDay' because it's excluded by the table
    describe "ByDay" $ do
      specify "Every wednesday and thursday" $
        forAllValid $ \tod ->
          weeklyDateTimeNextOccurrence limit (l (d 2020 08 05) tod) (Interval 1) [] (WeekStart Monday) [Wednesday, Thursday] [] [] [] []
            `shouldBe` Just (l (d 2020 08 06) tod)
      specify "Every other thursday and friday" $
        forAllValid $ \tod ->
          weeklyDateTimeNextOccurrence limit (l (d 2020 08 07) tod) (Interval 2) [] (WeekStart Monday) [Thursday, Friday] [] [] [] []
            `shouldBe` Just (l (d 2020 08 20) tod)
      specify "Every sunday, at the end of the year" $
        forAllValid $ \tod ->
          weeklyDateTimeNextOccurrence limit (l (d 2019 12 29) tod) (Interval 1) [] (WeekStart Monday) [Sunday] [] [] [] []
            `shouldBe` Just (l (d 2020 01 05) tod)
    describe "BySetPos" $ do
      specify "The first day of every week" $
        forAllValid $ \tod ->
          weeklyDateTimeNextOccurrence limit (l (d 2020 08 07) tod) (Interval 1) [] (WeekStart Friday) [Friday, Saturday] [] [] [] [BySetPos 1]
            `shouldBe` Just (l (d 2020 08 14) tod)
    describe "ByHour" $ do
      specify "16h every other week" $
        weeklyDateTimeNextOccurrence limit (LocalTime (d 2020 08 06) (t 16 00 00)) (Interval 2) [] (WeekStart Monday) [] [ByHour 16] [] [] []
          `shouldBe` Just (LocalTime (d 2020 08 20) (t 16 00 00))
    describe "ByMinute" $ do
      specify "16h20 every third week" $
        weeklyDateTimeNextOccurrence limit (LocalTime (d 2020 08 06) (t 16 20 00)) (Interval 3) [] (WeekStart Monday) [] [ByHour 16] [ByMinute 20] [] []
          `shouldBe` Just (LocalTime (d 2020 08 27) (t 16 20 00))
    describe "BySecond" $ do
      specify "16h20m30s every fourth week" $
        weeklyDateTimeNextOccurrence limit (LocalTime (d 2020 08 06) (t 15 00 00)) (Interval 4) [] (WeekStart Monday) [] [ByHour 16] [ByMinute 20] [BySecond 30] []
          `shouldBe` Just (LocalTime (d 2020 08 06) (t 16 20 30))
      specify "every 15th and 20th second" $
        weeklyDateTimeNextOccurrence limit (LocalTime (d 2020 08 06) (t 15 00 15)) (Interval 1) [] (WeekStart Monday) [] [] [] [BySecond 15, BySecond 20] []
          `shouldBe` Just (LocalTime (d 2020 08 06) (t 15 00 20))
  describe "weeklyDateRecurrence" $ do
    --  An unimportant limit because we don't specify any rules that have no occurrances
    let limit = d 2021 01 01
    let weeklyDateNextOccurrence lim start i ba ws bb bc =
          fmap localDay . listToMaybe $ weeklyDateTimeRecurrence lim (LocalTime start midnight) i ba ws bb [] [] [] bc
    describe "No ByX's" $ do
      specify "Every week" $
        weeklyDateNextOccurrence limit (d 2020 08 08) (Interval 1) [] (WeekStart Monday) [] []
          `shouldBe` Just (d 2020 08 15)
      specify "Every other week" $
        weeklyDateNextOccurrence limit (d 2020 08 08) (Interval 2) [] (WeekStart Monday) [] []
          `shouldBe` Just (d 2020 08 22)
    describe "ByMonth" $ do
      specify "Every week in Sept" $
        weeklyDateNextOccurrence limit (d 2019 09 30) (Interval 1) [ByMonth September] (WeekStart Monday) [] []
          `shouldBe` Just (d 2020 09 07)
      specify "Every other week in Sept" $
        weeklyDateNextOccurrence limit (d 2019 09 30) (Interval 2) [ByMonth September] (WeekStart Monday) [] []
          `shouldBe` Just (d 2020 09 14)
    -- No 'ByWeekNo' because it's excluded by the table
    -- No 'ByYearDay' because it's excluded by the table
    -- No 'ByMonthDay' because it's excluded by the table
    describe "ByDay" $ do
      specify "Every wednesday and thursday" $
        weeklyDateNextOccurrence limit (d 2020 08 05) (Interval 1) [] (WeekStart Monday) [Wednesday, Thursday] []
          `shouldBe` Just (d 2020 08 06)
      specify "Every other thursday and friday" $
        weeklyDateNextOccurrence limit (d 2020 08 07) (Interval 2) [] (WeekStart Monday) [Thursday, Friday] []
          `shouldBe` Just (d 2020 08 20)
      specify "Every sunday, at the end of the year" $
        weeklyDateNextOccurrence limit (d 2019 12 29) (Interval 1) [] (WeekStart Monday) [Sunday] []
          `shouldBe` Just (d 2020 01 05)
      specify "Every other tuesday and sunday with the week starting on monday" $ do
        weeklyDateNextOccurrence limit (d 1997 08 05) (Interval 2) [] (WeekStart Monday) [Tuesday, Sunday] []
          `shouldBe` Just (d 1997 08 10)
        weeklyDateNextOccurrence limit (d 1997 08 10) (Interval 2) [] (WeekStart Monday) [Tuesday, Sunday] []
          `shouldBe` Just (d 1997 08 19)
        weeklyDateNextOccurrence limit (d 1997 08 19) (Interval 2) [] (WeekStart Monday) [Tuesday, Sunday] []
          `shouldBe` Just (d 1997 08 24)
      specify "Every other tuesday and sunday with the week starting on sunday" $ do
        weeklyDateNextOccurrence limit (d 1997 08 05) (Interval 2) [] (WeekStart Sunday) [Tuesday, Sunday] []
          `shouldBe` Just (d 1997 08 17)
        weeklyDateNextOccurrence limit (d 1997 08 17) (Interval 2) [] (WeekStart Sunday) [Tuesday, Sunday] []
          `shouldBe` Just (d 1997 08 19)
        weeklyDateNextOccurrence limit (d 1997 08 19) (Interval 2) [] (WeekStart Sunday) [Tuesday, Sunday] []
          `shouldBe` Just (d 1997 08 31)
    describe "BySetPos" $ do
      specify "The last day of every week" $
        weeklyDateNextOccurrence limit (d 2020 08 07) (Interval 1) [] (WeekStart Friday) [Friday, Saturday] [BySetPos 1]
          `shouldBe` Just (d 2020 08 14)
      specify "The last day of every week in september" $
        weeklyDateNextOccurrence limit (d 2020 09 05) (Interval 1) [] (WeekStart Sunday) [Friday, Saturday] [BySetPos (-1)]
          `shouldBe` Just (d 2020 09 12)
