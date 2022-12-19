{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Text (Text)
import GHC.Stack
import ICal.Conformance
import ICal.Conformance.TestUtils
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
  shrinkValid = fmap Interval . shrinkValid . unInterval
  genValid = Interval <$> sized (\s -> choose (1, max 1 (fromIntegral s))) -- no point in generating huge intervals

instance GenValid Until where
  genValid =
    oneof
      [ UntilDate <$> genValid,
        UntilDateTimeFloating <$> genImpreciseLocalTime,
        UntilDateTimeUTC <$> genImpreciseUTCTime
      ]

instance GenValid Count

instance GenValid BySecond where
  shrinkValid = fmap BySecond . shrinkRangeDown (0, 60) . unBySecond
  genValid = BySecond <$> choose (0, 60)

instance GenValid ByMinute where
  shrinkValid = fmap ByMinute . shrinkRangeDown (0, 59) . unByMinute
  genValid = ByMinute <$> choose (0, 59)

instance GenValid ByHour where
  shrinkValid = fmap ByHour . shrinkRangeDown (0, 23) . unByHour
  genValid = ByHour <$> choose (0, 23)

instance GenValid ByDay where
  shrinkValid = \case
    Every i -> Every <$> shrinkValid i
    Specific i dow ->
      Specific
        <$> shrinkRange2 (-5, -1) (1, 5) i
        <*> shrinkValid dow
  genValid =
    oneof
      [ Every <$> genValid,
        genSpecificByDay
      ]

genSpecificByDay :: Gen ByDay
genSpecificByDay =
  Specific
    <$> oneof
      [ choose (1, 5),
        choose (-5, -1)
      ]
    <*> genValid

instance GenValid ByMonthDay where
  shrinkValid = fmap ByMonthDay . shrinkRange2 (-31, -1) (1, 31) . unByMonthDay
  genValid =
    ByMonthDay
      <$> oneof
        [ choose (1, 31),
          choose (-31, -1)
        ]

instance GenValid ByYearDay where
  shrinkValid = fmap ByYearDay . shrinkRange2 (-366, -1) (1, 366) . unByYearDay
  genValid =
    ByYearDay
      <$> oneof
        [ choose (1, 366),
          choose (-366, -1)
        ]

instance GenValid ByWeekNo where
  shrinkValid = fmap ByWeekNo . shrinkRange2 (-53, -1) (1, 53) . unByWeekNo
  genValid =
    ByWeekNo
      <$> oneof
        [ choose (1, 53),
          choose (-53, -1)
        ]

instance GenValid ByMonth where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = genValidStructurallyWithoutExtraChecking

instance GenValid WeekStart where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = genValidStructurallyWithoutExtraChecking

instance GenValid BySetPos where
  shrinkValid = shrinkValidStructurally
  genValid =
    BySetPos
      <$> sized
        ( \s ->
            oneof
              [ choose (1, max 1 s),
                choose (min (-1) (-s), -1)
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
            Monthly -> genSpecificByDay
            Yearly -> genSpecificByDay
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

recurrenceRulePartExampleSpec ::
  (HasCallStack, Show a, Eq a, IsRecurrenceRulePart a) =>
  Text ->
  a ->
  Spec
recurrenceRulePartExampleSpec text value = withFrozenCallStack $ do
  it "can parse this example correctly" $ do
    actual <- shouldConformStrict $ recurrenceRulePartP text
    actual `shouldBe` value
  it "can render this example correctly" $
    recurrenceRulePartB value `shouldBe` text

recurrenceRulePartSpec ::
  forall a.
  (HasCallStack, Show a, Eq a, GenValid a, IsRecurrenceRulePart a) =>
  Spec
recurrenceRulePartSpec = withFrozenCallStack $ do
  it "always renders a valid text values" $
    forAllValid $ \part ->
      shouldBeValid $ recurrenceRulePartB (part :: a)

  it "parses only valid things" $
    forAllValid $ \a ->
      case runConformStrict $ recurrenceRulePartP (recurrenceRulePartB (a :: a)) of
        Left _ -> pure ()
        Right a' -> shouldBeValid (a' :: a)

  it "roundtrips through text values" $
    forAllValid $ \part -> do
      let values = recurrenceRulePartB (part :: a)
      actual <- shouldConformStrict $ recurrenceRulePartP values
      actual `shouldBe` part
