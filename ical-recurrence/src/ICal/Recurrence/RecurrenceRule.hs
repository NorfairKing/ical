module ICal.Recurrence.RecurrenceRule
  ( recurRecurrenceRules,
    recurRecurrenceRule,
    recurRecurrenceRuleLocalTimes,
  )
where

import Control.Monad
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time as Time
import ICal.Property
import ICal.PropertyType
import ICal.Recurrence.Class

-- | Compute the occurrences that the recurrence rules imply
--
-- TODO implement this:
-- @
-- The recurrence set generated with multiple "RRULE" properties is
-- undefined.
-- @
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
      -- TODO emit a fixable warning
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
recurRecurrenceRuleLocalTimes = undefined
