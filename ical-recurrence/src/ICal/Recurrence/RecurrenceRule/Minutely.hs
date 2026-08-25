module ICal.Recurrence.RecurrenceRule.Minutely
  ( minutelyDateTimeRecurrence,
  )
where

import Control.Monad
import Data.List
import Data.Set (Set)
import Data.Time as Time
import ICal.PropertyType
import ICal.Recurrence.RecurrenceRule.Util

-- | The occurrences of a minutely rule
--
-- From the table of which rule parts expand and which limit, for MINUTELY:
-- everything down to and including BYMINUTE limits, and only BYSECOND expands.
-- BYWEEKNO is N/A.
minutelyDateTimeRecurrence ::
  Day ->
  LocalTime ->
  Interval ->
  Set ByMonth ->
  Set ByYearDay ->
  Set ByMonthDay ->
  Set DayOfWeek ->
  Set ByHour ->
  Set ByMinute ->
  Set BySecond ->
  Set BySetPos ->
  [LocalTime]
minutelyDateTimeRecurrence
  limit
  start
  interval
  byMonths
  byYearDays
  byMonthDays
  byDays
  byHours
  byMinutes
  bySeconds
  bySetPoss = do
    candidate <- stepLocalTimes 60 limit start interval
    let day = localDay candidate
    guard $ byMonthLimit byMonths day
    guard $ byYearDayLimit byYearDays day
    guard $ byMonthDayLimit byMonthDays day
    guard $ byEveryWeekDayLimit byDays day
    let TimeOfDay hour minute _ = localTimeOfDay candidate
    guard $ byHourLimit byHours hour
    guard $ byMinuteLimit byMinutes minute
    -- The interval is one minute, so this is the set BYSETPOS selects from.
    next <- filterSetPos bySetPoss $
      sort $ do
        let TimeOfDay _ _ startSecond = localTimeOfDay start
        second <- bySecondExpand startSecond bySeconds
        pure $ LocalTime day (TimeOfDay hour minute second)
    guard (next > start) -- Don't take the current one again
    guard (next < LocalTime (addDays 1 limit) midnight) -- Don't go beyond the limit
    pure next
