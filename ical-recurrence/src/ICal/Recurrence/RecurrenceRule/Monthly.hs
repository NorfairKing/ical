module ICal.Recurrence.RecurrenceRule.Monthly
  ( monthlyDateTimeRecurrence,
  )
where

import Data.Set (Set)
import Data.Time as Time
import ICal.PropertyType

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
monthlyDateTimeRecurrence = undefined
