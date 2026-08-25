module ICal.Recurrence.RecurrenceRule.Hourly
  ( hourlyDateTimeRecurrence,
  )
where

import Control.Monad
import Data.List
import Data.Set (Set)
import Data.Time as Time
import ICal.PropertyType
import ICal.Recurrence.RecurrenceRule.Util

-- | The occurrences of an hourly rule
--
-- From the table of which rule parts expand and which limit, for HOURLY:
-- BYMONTH, BYYEARDAY, BYMONTHDAY, BYDAY and BYHOUR limit, while BYMINUTE and
-- BYSECOND expand.  BYWEEKNO is N/A.
--
-- @
-- The BYDAY rule part MUST NOT be specified with a numeric value when
-- the FREQ rule part is not set to MONTHLY or YEARLY.
-- @
--
-- so the days of the week arrive here already stripped of any numeric value.
hourlyDateTimeRecurrence ::
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
hourlyDateTimeRecurrence
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
    candidate <- stepLocalTimes 3600 limit start interval
    let day = localDay candidate
    guard $ byMonthLimit byMonths day
    guard $ byYearDayLimit byYearDays day
    guard $ byMonthDayLimit byMonthDays day
    guard $ byEveryWeekDayLimit byDays day
    let TimeOfDay hour _ _ = localTimeOfDay candidate
    guard $ byHourLimit byHours hour
    -- The interval is one hour, so this is the set BYSETPOS selects from.
    next <- filterSetPos bySetPoss $
      sort $ do
        let TimeOfDay _ startMinute startSecond = localTimeOfDay start
        minute <- byMinuteExpand startMinute byMinutes
        second <- bySecondExpand startSecond bySeconds
        pure $ LocalTime day (TimeOfDay hour minute second)
    guard (next > start) -- Don't take the current one again
    guard (next < LocalTime (addDays 1 limit) midnight) -- Don't go beyond the limit
    pure next
