module ICal.Recurrence.RecurrenceRule.Weekly
  ( weeklyDateTimeRecurrence,
  )
where

import Data.Set (Set)
import Data.Time as Time
import ICal.PropertyType

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
weeklyDateTimeRecurrence = undefined
