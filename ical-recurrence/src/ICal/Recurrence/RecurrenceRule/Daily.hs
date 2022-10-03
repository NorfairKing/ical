module ICal.Recurrence.RecurrenceRule.Daily
  ( dailyDateTimeRecurrence,
  )
where

import Data.Set (Set)
import Data.Time as Time
import ICal.PropertyType

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
dailyDateTimeRecurrence = undefined
