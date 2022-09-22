module ICal.Recurrence.RecurrenceRule
  ( recurRecurrenceRules,
  )
where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Time as Time
import ICal.Property
import ICal.PropertyType
import ICal.Recurrence.Class
import ICal.Recurrence.RecurrenceDateTimes

-- | Compute the occurrences that the recurrence rules imply
--
-- TODO implement this:
-- @
-- The recurrence set generated with multiple "RRULE" properties is
-- undefined.
-- @
recurRecurrenceRules ::
  -- | Limit
  LocalTime ->
  DateTimeStart ->
  Maybe (Either DateTimeEnd Duration) ->
  Set RecurrenceRule ->
  R (Set EventOccurrence)
recurRecurrenceRules = undefined
