module ICal.Recurrence.RecurrenceRule.Weekly
  ( weeklyDateTimeRecurrence,
  )
where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Set (Set)
import Data.Time as Time
import ICal.PropertyType
import ICal.Recurrence.RecurrenceRule.Util
import ICal.Recurrence.RecurrenceRule.WeekDate

weeklyDateTimeRecurrence ::
  Day ->
  LocalTime ->
  Interval ->
  Set ByMonth ->
  WeekStart ->
  Set DayOfWeek ->
  Set ByHour ->
  Set ByMinute ->
  Set BySecond ->
  Set BySetPos ->
  [LocalTime]
weeklyDateTimeRecurrence
  limit
  start
  interval
  byMonths
  weekStart
  byDays
  byHours
  byMinutes
  bySeconds
  bySetPoss = do
    (y, w) <-
      weeklyWeekRecurrence
        limit
        (localDay start)
        interval
        weekStart
    next <- filterSetPos bySetPoss $
      sort $ do
        -- Need to sort because week days may not be in order.
        dow <- byEveryWeekDayExpand (dayOfWeek (localDay start)) byDays
        d <- maybeToList $ fromWeekDateWithStart weekStart y w dow
        guard $ byMonthLimit byMonths d
        guard (d <= limit) -- Early check
        tod <- timeOfDayExpand (localTimeOfDay start) byHours byMinutes bySeconds
        let next = LocalTime d tod
        pure next

    guard (next < LocalTime (addDays 1 limit) midnight) -- Don't go beyond the limit
    guard (next > start) -- Don't take the current one again
    pure next

weeklyWeekRecurrence ::
  Day ->
  Day ->
  Interval ->
  WeekStart ->
  [(Integer, Word)]
weeklyWeekRecurrence
  limit
  d_
  (Interval interval)
  weekStart =
    do
      let (y, w, _) = toWeekDateWithStart weekStart d_
      let (limitY, limitWN, _) = toWeekDateWithStart weekStart limit
      takeEvery interval $
        takeWhile (<= (limitY, limitWN)) $
          weeksIntoTheFutureStartingFrom weekStart y w
