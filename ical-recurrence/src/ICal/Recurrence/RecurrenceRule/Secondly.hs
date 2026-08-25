module ICal.Recurrence.RecurrenceRule.Secondly
  ( secondlyDateTimeRecurrence,
  )
where

import Control.Monad
import Data.Set (Set)
import Data.Time as Time
import ICal.PropertyType
import ICal.Recurrence.RecurrenceRule.Util

-- | The occurrences of a secondly rule
--
-- From the table of which rule parts expand and which limit, for SECONDLY every
-- rule part limits and none expands, BYWEEKNO being N/A.  There is nothing
-- below a second to expand into, so each interval holds a single instance and
-- BYSETPOS can only either keep it or drop it.
secondlyDateTimeRecurrence ::
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
secondlyDateTimeRecurrence
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
    candidate <- stepLocalTimes 1 limit start interval
    let day = localDay candidate
    guard $ byMonthLimit byMonths day
    guard $ byYearDayLimit byYearDays day
    guard $ byMonthDayLimit byMonthDays day
    guard $ byEveryWeekDayLimit byDays day
    let TimeOfDay hour minute second = localTimeOfDay candidate
    guard $ byHourLimit byHours hour
    guard $ byMinuteLimit byMinutes minute
    guard $ bySecondLimit bySeconds second
    next <- filterSetPos bySetPoss [candidate]
    guard (next > start) -- Don't take the current one again
    guard (next < LocalTime (addDays 1 limit) midnight) -- Don't go beyond the limit
    pure next
