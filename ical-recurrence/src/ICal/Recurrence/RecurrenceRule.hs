{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ICal.Recurrence.RecurrenceRule
  ( recurRecurrenceRuleDateTimeStarts,
    recurRecurrenceRuleLocalTimes,
  )
where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time as Time
import ICal.Property
import ICal.PropertyType
import ICal.Recurrence.Class
import ICal.Recurrence.RecurrenceRule.Daily
import ICal.Recurrence.RecurrenceRule.Monthly
import ICal.Recurrence.RecurrenceRule.Weekly
import ICal.Recurrence.RecurrenceRule.Yearly

recurRecurrenceRuleDateTimeStarts ::
  -- | Limit
  Day ->
  DateTimeStart ->
  RecurrenceRule ->
  R (Set DateTimeStart)
recurRecurrenceRuleDateTimeStarts limit start recurrenceRule = do
  let localStart = case start of
        DateTimeStartDate (Date d) -> LocalTime d midnight
        DateTimeStartDateTime dt -> case dt of
          DateTimeFloating lt -> lt
          DateTimeUTC ut -> utcToLocalTime utc ut
          DateTimeZoned _ lt -> lt
  localTimes <- recurRecurrenceRuleLocalTimes limit localStart recurrenceRule
  pure $
    flip S.map localTimes $ \lt -> case start of
      DateTimeStartDate _ -> DateTimeStartDate $ Date $ localDay lt
      DateTimeStartDateTime dt -> DateTimeStartDateTime $ case dt of
        DateTimeFloating _ -> DateTimeFloating lt
        DateTimeUTC _ -> DateTimeUTC $ localTimeToUTC utc lt
        DateTimeZoned tzid _ -> DateTimeZoned tzid lt

recurRecurrenceRuleLocalTimes ::
  -- | Limit
  Day ->
  -- | DTSTART
  LocalTime ->
  RecurrenceRule ->
  R (Set LocalTime)
recurRecurrenceRuleLocalTimes limit start recurrenceRule =
  S.insert start <$> case recurrenceRuleUntilCount recurrenceRule of
    Nothing -> goIndefinitely
    Just (Left u) -> goUntil u
    Just (Right c) -> goCount c
  where
    localTimes :: R [LocalTime]
    localTimes = recurrenceRuleDateTimeOccurrences limit start recurrenceRule

    goUntil :: Until -> R (Set LocalTime)
    goUntil u = recurUntil u <$> localTimes
    recurUntil :: Until -> [LocalTime] -> Set LocalTime
    recurUntil _ [] = S.empty
    recurUntil u (l : ls) =
      if l `lessThanUntil` u
        then S.insert l $ recurUntil u ls
        else S.empty

    goCount :: Count -> R (Set LocalTime)
    goCount (Count c) = recurCount (c - 1) <$> localTimes
    recurCount _ [] = S.empty
    recurCount 0 _ = S.empty
    recurCount c (a : as) = S.insert a $ recurCount (pred c) as

    goIndefinitely :: R (Set LocalTime)
    goIndefinitely = S.fromList <$> localTimes

-- TODO: < or <=?
lessThanUntil :: LocalTime -> Until -> Bool
lessThanUntil lt = \case
  UntilDate d -> localDay lt <= unDate d
  UntilDateTimeFloating lt' -> lt <= lt'
  UntilDateTimeUTC ut -> lt <= utcToLocalTime utc ut -- TODO is this correct?!

-- This function takes care of the 'rRuleFrequency' part.
recurrenceRuleDateTimeOccurrences :: Day -> LocalTime -> RecurrenceRule -> R [LocalTime]
recurrenceRuleDateTimeOccurrences limit lt RecurrenceRule {..} = case recurrenceRuleFrequency of
  -- @
  -- The BYDAY rule part MUST NOT be specified with a numeric value when
  -- the FREQ rule part is not set to MONTHLY or YEARLY.  Furthermore,
  -- @
  --
  -- So we 'filterEvery' on the 'byDay's for every frequency except 'MONTHLY' and 'YEARLY'.
  Daily -> do
    every <- filterEvery recurrenceRuleByDay
    pure $
      dailyDateTimeRecurrence
        limit
        lt
        recurrenceRuleInterval
        recurrenceRuleByMonth
        recurrenceRuleByMonthDay
        every
        recurrenceRuleByHour
        recurrenceRuleByMinute
        recurrenceRuleBySecond
        recurrenceRuleBySetPos
  Weekly -> do
    every <- filterEvery recurrenceRuleByDay
    pure $
      weeklyDateTimeRecurrence
        limit
        lt
        recurrenceRuleInterval
        recurrenceRuleByMonth
        recurrenceRuleWeekStart
        every
        recurrenceRuleByHour
        recurrenceRuleByMinute
        recurrenceRuleBySecond
        recurrenceRuleBySetPos
  Monthly ->
    pure $
      monthlyDateTimeRecurrence
        limit
        lt
        recurrenceRuleInterval
        recurrenceRuleByMonth
        recurrenceRuleByMonthDay
        recurrenceRuleByDay
        recurrenceRuleByHour
        recurrenceRuleByMinute
        recurrenceRuleBySecond
        recurrenceRuleBySetPos
  Yearly ->
    pure $
      yearlyDateTimeRecurrence
        limit
        lt
        recurrenceRuleInterval
        recurrenceRuleByMonth
        recurrenceRuleWeekStart
        recurrenceRuleByWeekNo
        recurrenceRuleByYearDay
        recurrenceRuleByMonthDay
        recurrenceRuleByDay
        recurrenceRuleByHour
        recurrenceRuleByMinute
        recurrenceRuleBySecond
        recurrenceRuleBySetPos
  _ -> error $ "not implemented yet: " <> show recurrenceRuleFrequency

-- | Filter the 'Every' to implement the fixable error defined by this part of the spec:
--
-- @
-- The BYDAY rule part MUST NOT be specified with a numeric value when
-- the FREQ rule part is not set to MONTHLY or YEARLY.  Furthermore,
-- @
filterEvery :: Set ByDay -> R (Set DayOfWeek)
filterEvery =
  fmap (S.fromList . catMaybes)
    . mapM
      ( \case
          Every d -> pure $ Just d
          bd -> do
            emitFixableErrorR $ RecurrenceByDayNumeric bd
            pure Nothing
      )
    . S.toList
