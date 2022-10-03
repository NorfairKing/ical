{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ICal.Recurrence.RecurrenceRule
  ( recurRecurrenceRules,
    recurRecurrenceRule,
    recurRecurrenceRuleLocalTimes,
  )
where

import Control.Monad
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time as Time
import ICal.Conformance
import ICal.Property
import ICal.PropertyType
import ICal.Recurrence.Class
import ICal.Recurrence.RecurrenceRule.Daily
import ICal.Recurrence.RecurrenceRule.Monthly
import ICal.Recurrence.RecurrenceRule.Weekly
import ICal.Recurrence.RecurrenceRule.Yearly

-- | Compute the occurrences that the recurrence rules imply
recurRecurrenceRules ::
  -- | Limit
  Day ->
  DateTimeStart ->
  Maybe (Either DateTimeEnd Duration) ->
  Set RecurrenceRule ->
  R (Set EventOccurrence)
recurRecurrenceRules limit start mEndOrDuration recurrenceRules = do
  case S.toList recurrenceRules of
    [] -> pure S.empty
    [recurrenceRule] -> recurRecurrenceRule limit start mEndOrDuration recurrenceRule
    l -> do
      -- The spec says:
      --
      -- @
      -- The recurrence set generated with multiple "RRULE" properties is
      -- undefined.
      -- @
      --
      -- However, we choose to define it as the union of the
      -- reccurence sets defined by the recurrence rules.
      emitFixableError $ RecurrenceMultipleRecurrenceRules recurrenceRules
      S.unions <$> mapM (recurRecurrenceRule limit start mEndOrDuration) l

recurRecurrenceRule ::
  -- | Limit
  Day ->
  DateTimeStart ->
  Maybe (Either DateTimeEnd Duration) ->
  RecurrenceRule ->
  R (Set EventOccurrence)
recurRecurrenceRule limit start mEndOrDuration recurrenceRule = do
  starts <- recurRecurrenceRuleDateTimeStarts limit start recurrenceRule
  fmap S.fromList $
    forM (S.toList starts) $ \newStart -> do
      newMEndOrDuration <- resolveEndOrDurationDate start mEndOrDuration newStart
      pure
        EventOccurrence
          { eventOccurrenceStart = Just newStart,
            eventOccurrenceEndOrDuration = newMEndOrDuration
          }

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
recurRecurrenceRuleLocalTimes = recurUntilCount lessThanUntil

lessThanUntil :: LocalTime -> Until -> Bool
lessThanUntil = undefined

recurUntilCount ::
  (LocalTime -> Until -> Bool) ->
  Day ->
  LocalTime ->
  RecurrenceRule ->
  R (Set LocalTime)
recurUntilCount leFunc limit start recurrenceRule =
  S.insert start <$> case recurrenceRuleUntilCount recurrenceRule of
    Nothing -> goIndefinitely
    Just (Left u) -> goUntil u
    Just (Right c) -> goCount c
  where
    localTimes :: R [LocalTime]
    localTimes = recurrenceRuleDateTimeOccurrences limit start recurrenceRule

    goUntil :: Until -> R (Set LocalTime)
    goUntil u = localTimes >>= recurUntil u

    recurUntil :: Until -> [LocalTime] -> R (Set LocalTime)
    recurUntil _ [] = pure S.empty
    recurUntil u (l : ls) =
      if l `leFunc` u
        then S.insert l <$> recurUntil u ls
        else pure S.empty

    goCount :: Count -> R (Set LocalTime)
    goCount (Count c) = localTimes >>= recurCount (c - 1)
    recurCount _ [] = pure S.empty
    recurCount 0 _ = pure S.empty
    recurCount c (a : as) = S.insert a <$> recurCount (pred c) as

    goIndefinitely :: R (Set LocalTime)
    goIndefinitely =
      iterateMaybeSet
        -- TODO make this faster by not recomputing the list for
        -- every set element
        (\cur -> listToMaybe <$> recurrenceRuleDateTimeOccurrences limit cur recurrenceRule)
        start

iterateMaybeSet :: (Ord a, Monad m) => (a -> m (Maybe a)) -> a -> m (Set a)
iterateMaybeSet func start = go start
  where
    go cur = do
      mRes <- func cur
      case mRes of
        Nothing -> pure $ S.singleton start
        Just next -> S.insert next <$> go next

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
            emitFixableError $ RecurrenceByDayNumeric bd
            pure Nothing
      )
    . S.toList
