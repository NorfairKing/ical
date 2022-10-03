module ICal.Recurrence.RecurrenceRule.Daily
  ( dailyDateTimeRecurrence,
  )
where

import Control.Monad
import Data.Set (Set)
import Data.Time as Time
import ICal.PropertyType
import ICal.Recurrence.RecurrenceRule.Util

dailyDateTimeRecurrence ::
  Day ->
  LocalTime ->
  Interval ->
  Set ByMonth ->
  Set ByMonthDay ->
  Set DayOfWeek ->
  Set ByHour ->
  Set ByMinute ->
  Set BySecond ->
  Set BySetPos ->
  [LocalTime]
dailyDateTimeRecurrence
  limit
  lt@(LocalTime d_ tod_)
  interval
  byMonths
  byMonthDays
  byDays
  byHours
  byMinutes
  bySeconds
  bySetPoss = do
    d <- dailyDayRecurrence d_ limit interval byMonths byMonthDays byDays
    tod <- filterSetPos bySetPoss $ timeOfDayExpand tod_ byHours byMinutes bySeconds
    let next = LocalTime d tod
    guard (next > lt) -- Don't take the current one again
    guard (next < LocalTime (addDays 1 limit) midnight) -- Don't go beyond the limit
    pure next

-- | Internal: Get all the relevant days until the limit, not considering any 'Set BySetPos'
dailyDayRecurrence ::
  Day ->
  Day ->
  Interval ->
  Set ByMonth ->
  Set ByMonthDay ->
  Set DayOfWeek ->
  [Day]
dailyDayRecurrence
  d_
  limitDay
  (Interval interval)
  byMonths
  byMonthDays
  byDays = do
    d <- takeWhile (<= limitDay) $ map (\i -> addDays (fromIntegral interval * i) d_) [0 ..]
    guard $ byMonthLimit byMonths d
    guard $ byMonthDayLimit byMonthDays d
    guard $ byEveryWeekDayLimit byDays d
    pure d
