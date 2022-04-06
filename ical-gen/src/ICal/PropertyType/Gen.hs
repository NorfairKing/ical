{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.PropertyType.Gen where

import Data.GenValidity
import Data.GenValidity.CaseInsensitive ()
import Data.GenValidity.Containers
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import qualified Data.Set as S
import Data.Time (LocalTime (..), TimeOfDay (..))
import ICal.Parameter.Gen ()
import ICal.PropertyType
import ICal.PropertyType.Class
import ICal.PropertyType.RecurrenceRule
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

instance GenValid Date

instance GenValid Time where
  genValid = do
    lt <- genImpreciseTimeOfDay
    oneof
      [ pure $ TimeFloating lt,
        pure $ TimeUTC lt,
        TimeZoned <$> genValid <*> pure lt
      ]

instance GenValid DateTime where
  genValid = do
    lt <- genImpreciseLocalTime
    oneof
      [ pure $ DateTimeFloating lt,
        pure $ DateTimeUTC lt,
        DateTimeZoned <$> genValid <*> pure lt
      ]

-- | Until we have it in time and then in genvalidity-time
instance GenValid Month where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = genValidStructurallyWithoutExtraChecking

instance GenValid Frequency where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = genValidStructurallyWithoutExtraChecking

instance GenValid UntilCount where
  shrinkValid = shrinkValidStructurally
  genValid =
    oneof
      [ pure Indefinitely,
        Count <$> sized (\s -> max 1 <$> choose (1, fromIntegral s)),
        Until <$> genImpreciseLocalTime
      ]

instance GenValid Interval where
  shrinkValid = shrinkValidStructurally
  genValid = Interval <$> sized (\s -> max 1 <$> choose (1, fromIntegral s)) -- no point in generating huge words

instance GenValid BySecond where
  shrinkValid = shrinkValidStructurally
  genValid = Second <$> choose (0, 60)

instance GenValid ByMinute where
  shrinkValid = shrinkValidStructurally
  genValid = Minute <$> choose (0, 59)

instance GenValid ByHour where
  shrinkValid = shrinkValidStructurally
  genValid = Hour <$> choose (0, 23)

instance GenValid ByDay where
  shrinkValid = shrinkValidStructurally
  genValid =
    oneof
      [ Every <$> genValid,
        Specific <$> sized (\s -> oneof [max 1 <$> choose (1, s), min (-1) <$> choose (- s, - 1)]) <*> genValid
      ]

instance GenValid ByMonthDay where
  shrinkValid = shrinkValidStructurally
  genValid =
    MonthDay
      <$> oneof
        [ choose (1, 31),
          choose (-31, - 1)
        ]

instance GenValid ByYearDay where
  shrinkValid = shrinkValidStructurally
  genValid =
    YearDay
      <$> oneof
        [ choose (1, 366),
          choose (-366, - 1)
        ]

instance GenValid ByWeekNo where
  shrinkValid = shrinkValidStructurally
  genValid =
    WeekNo
      <$> oneof
        [ choose (1, 53),
          choose (-53, - 1)
        ]

instance GenValid BySetPos where
  shrinkValid = shrinkValidStructurally
  genValid = SetPos <$> sized (\s -> oneof [max 1 <$> choose (1, s), min (-1) <$> choose (- s, - 1)])

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

genImpreciseLocalTime :: Gen LocalTime
genImpreciseLocalTime = LocalTime <$> genValid <*> genImpreciseTimeOfDay

genImpreciseTimeOfDay :: Gen TimeOfDay
genImpreciseTimeOfDay =
  TimeOfDay
    <$> choose (0, 23)
    <*> choose (0, 59)
    <*> (fromIntegral <$> (choose (0, 60) :: Gen Int))

propertyTypeSpec ::
  forall a.
  (Show a, Eq a, GenValid a, IsPropertyType a) =>
  Spec
propertyTypeSpec = do
  it "always renders to a valid content line" $
    forAllValid $ \propertyType ->
      shouldBeValid $ propertyTypeB (propertyType :: a)
  it "roundtrips through ContentLine" $
    forAllValid $ \propertyType ->
      let value = propertyTypeB (propertyType :: a)
       in case propertyTypeP value of
            Left err -> expectationFailure err
            Right actual -> actual `shouldBe` propertyType
