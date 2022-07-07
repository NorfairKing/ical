{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.PropertyType.RecurrenceRule.Gen where

import Data.GenValidity
import Data.GenValidity.CaseInsensitive ()
import Data.GenValidity.Containers
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import qualified Data.Set as S
import ICal.PropertyType.Gen
import ICal.PropertyType.RecurrenceRule
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

-- | Until we have it in time and then in genvalidity-time
instance GenValid Month where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = genValidStructurallyWithoutExtraChecking

instance GenValid Frequency where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = genValidStructurallyWithoutExtraChecking

instance GenValid Interval where
  shrinkValid = shrinkValidStructurally
  genValid = Interval <$> sized (\s -> max 1 <$> choose (1, fromIntegral s)) -- no point in generating huge words

instance GenValid Until where
  genValid =
    oneof
      [ UntilDate <$> genValid,
        UntilDateTime <$> genImpreciseUTCTime
      ]

instance GenValid Count

instance GenValid BySecond where
  shrinkValid = shrinkValidStructurally
  genValid = BySecond <$> choose (0, 60)

instance GenValid ByMinute where
  shrinkValid = shrinkValidStructurally
  genValid = ByMinute <$> choose (0, 59)

instance GenValid ByHour where
  shrinkValid = shrinkValidStructurally
  genValid = ByHour <$> choose (0, 23)

instance GenValid ByDay where
  shrinkValid = shrinkValidStructurally
  genValid =
    oneof
      [ Every <$> genValid,
        Specific
          <$> oneof
            [ max 1 <$> choose (1, 5),
              min (-1) <$> choose (-5, -1)
            ]
          <*> genValid
      ]

instance GenValid ByMonthDay where
  shrinkValid = shrinkValidStructurally
  genValid =
    ByMonthDay
      <$> oneof
        [ choose (1, 31),
          choose (-31, -1)
        ]

instance GenValid ByYearDay where
  shrinkValid = shrinkValidStructurally
  genValid =
    ByYearDay
      <$> oneof
        [ choose (1, 366),
          choose (-366, -1)
        ]

instance GenValid ByWeekNo where
  shrinkValid = shrinkValidStructurally
  genValid =
    ByWeekNo
      <$> oneof
        [ choose (1, 53),
          choose (-53, -1)
        ]

instance GenValid ByMonth

instance GenValid WeekStart

instance GenValid BySetPos where
  shrinkValid = shrinkValidStructurally
  genValid =
    BySetPos
      <$> sized
        ( \s ->
            oneof
              [ max 1 <$> choose (1, s),
                min (-1) <$> choose (-s, -1)
              ]
        )

instance GenValid RecurrenceRule where
  shrinkValid = shrinkValidStructurally
  genValid = do
    recurrenceRuleFrequency <- genValid
    recurrenceRuleInterval <- genValid
    recurrenceRuleUntilCount <- genValid
    recurrenceRuleBySecond <- genValid
    recurrenceRuleByMinute <- genValid
    recurrenceRuleByHour <- genValid
    recurrenceRuleByDay <-
      genSetOf
        ( case recurrenceRuleFrequency of
            Monthly -> Every <$> genValid
            Yearly -> Every <$> genValid
            _ -> genValid
        )
    recurrenceRuleByMonthDay <- case recurrenceRuleFrequency of
      Weekly -> pure S.empty
      _ -> genValid
    recurrenceRuleByYearDay <- case recurrenceRuleFrequency of
      Daily -> pure S.empty
      Weekly -> pure S.empty
      Monthly -> pure S.empty
      _ -> genValid
    recurrenceRuleByWeekNo <- case recurrenceRuleFrequency of
      Yearly -> genValid
      _ -> pure S.empty
    recurrenceRuleByMonth <- genValid
    recurrenceRuleWeekStart <- genValid
    recurrenceRuleBySetPos <-
      let anyOtherBySpecified =
            any
              not
              [ S.null recurrenceRuleBySecond,
                S.null recurrenceRuleByMinute,
                S.null recurrenceRuleByHour,
                S.null recurrenceRuleByDay,
                S.null recurrenceRuleByMonthDay,
                S.null recurrenceRuleByYearDay,
                S.null recurrenceRuleByWeekNo,
                S.null recurrenceRuleByMonth
              ]
       in if anyOtherBySpecified
            then genValid
            else pure S.empty
    pure RecurrenceRule {..}

genDailyRecurrence :: Gen RecurrenceRule
genDailyRecurrence = do
  let recurrenceRuleFrequency = Daily
  recurrenceRuleInterval <- genValid
  recurrenceRuleUntilCount <- genValid
  recurrenceRuleBySecond <- genValid
  recurrenceRuleByMinute <- genValid
  recurrenceRuleByHour <- genValid
  recurrenceRuleByDay <-
    genSetOf
      ( case recurrenceRuleFrequency of
          Monthly -> Every <$> genValid
          Yearly -> Every <$> genValid
          _ -> genValid
      )
  recurrenceRuleByMonthDay <- case recurrenceRuleFrequency of
    Weekly -> pure S.empty
    _ -> genValid
  let recurrenceRuleByYearDay = S.empty
      recurrenceRuleByWeekNo = S.empty
  recurrenceRuleByMonth <- genValid
  recurrenceRuleWeekStart <- genValid
  recurrenceRuleBySetPos <-
    let anyOtherBySpecified =
          any
            not
            [ S.null recurrenceRuleBySecond,
              S.null recurrenceRuleByMinute,
              S.null recurrenceRuleByHour,
              S.null recurrenceRuleByDay,
              S.null recurrenceRuleByMonthDay,
              S.null recurrenceRuleByYearDay,
              S.null recurrenceRuleByWeekNo,
              S.null recurrenceRuleByMonth
            ]
     in if anyOtherBySpecified
          then genValid
          else pure S.empty
  pure RecurrenceRule {..}

recurrenceRulePartSpec ::
  forall a.
  (Show a, Eq a, GenValid a, IsRecurrenceRulePart a) =>
  Spec
recurrenceRulePartSpec = do
  it "always renders a valid text values" $
    forAllValid $ \part ->
      shouldBeValid $ recurrenceRulePartB (part :: a)

  it "parses only valid things" $
    forAllValid $ \a ->
      case recurrenceRulePartP (recurrenceRulePartB (a :: a)) of
        Left _ -> pure ()
        Right a' -> shouldBeValid (a' :: a)

  it "roundtrips through text values" $
    forAllValid $ \part ->
      let values = recurrenceRulePartB (part :: a)
       in case recurrenceRulePartP values of
            Left err -> expectationFailure err
            Right actual -> actual `shouldBe` part
