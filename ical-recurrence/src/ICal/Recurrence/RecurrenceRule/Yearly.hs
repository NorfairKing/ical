module ICal.Recurrence.RecurrenceRule.Yearly
  ( yearlyDateTimeRecurrence,
  )
where

import Data.Set (Set)
import Data.Time as Time
import ICal.PropertyType

yearlyDateTimeRecurrence ::
  Day ->
  LocalTime ->
  Interval ->
  Set ByMonth ->
  WeekStart ->
  Set ByWeekNo ->
  Set ByYearDay ->
  Set ByMonthDay ->
  Set ByDay ->
  Set ByHour ->
  Set ByMinute ->
  Set BySecond ->
  Set BySetPos ->
  [LocalTime]
yearlyDateTimeRecurrence = undefined
