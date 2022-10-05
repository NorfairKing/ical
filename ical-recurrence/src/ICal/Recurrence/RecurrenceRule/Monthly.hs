module ICal.Recurrence.RecurrenceRule.Monthly
  ( monthlyDateTimeRecurrence,
  )
where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time as Time
import ICal.PropertyType
import ICal.Recurrence.RecurrenceRule.Util

monthlyDateTimeRecurrence ::
  Day ->
  LocalTime ->
  Interval ->
  Set ByMonth ->
  Set ByMonthDay ->
  Set ByDay ->
  Set ByHour ->
  Set ByMinute ->
  Set BySecond ->
  Set BySetPos ->
  [LocalTime]
monthlyDateTimeRecurrence
  limit
  start
  interval
  byMonths
  byMonthDays
  byDays
  byHours
  byMinutes
  bySeconds
  bySetPoss = do
    (year, month) <- monthlyMonthRecurrence (localDay start) limit interval
    m <- maybeToList $ monthNoToMonth month
    guard $ byMonthLimitMonth byMonths m
    let (_, _, md_) = toGregorian (localDay start)
    next <- filterSetPos bySetPoss $
      sort $ do
        d <-
          if S.null byMonthDays
            then byDayExpand year month md_ byDays
            else do
              md <- byMonthDayExpand year m md_ byMonthDays
              d' <- maybeToList $ fromGregorianValid year month md
              guard $ byDayLimit byDays d'
              pure d'
        guard (d <= limit) -- Early check
        tod <- timeOfDayExpand (localTimeOfDay start) byHours byMinutes bySeconds
        let next = LocalTime d tod
        pure next
    guard (next > start) -- Don't take the current one again
    guard (next < LocalTime (addDays 1 limit) midnight) -- Don't go beyond the limit
    pure next

monthlyMonthRecurrence ::
  Day ->
  Day ->
  Interval ->
  [(Integer, Int)]
monthlyMonthRecurrence d_ limit (Interval interval) = do
  let (year_, month_, _) = toGregorian d_
  let (limitYear, limitMonth, _) = toGregorian limit
  takeEvery interval $
    takeWhile (<= (limitYear, limitMonth)) $
      dropWhile (< (year_, month_)) $
        iterate nextMonth (year_, month_)
  where
    nextMonth :: (Integer, Int) -> (Integer, Int)
    nextMonth (y, 12) = (y + 1, 1)
    nextMonth (y, m) = (y, m + 1)
