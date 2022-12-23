{-# LANGUAGE RecordWildCards #-}

-- | ICal Recurrence
--
-- This module exists to help you canculate the recurrence set of an event.
--
-- [section 3.8.5](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.5)
--
-- @
-- The following properties specify recurrence information in calendar
-- components.
-- @
--
-- [section 3.8.5.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.5.1)
--
-- @
-- The recurrence set is the complete
-- set of recurrence instances for a calendar component.  The
-- recurrence set is generated by considering the initial "DTSTART"
-- property along with the "RRULE", "RDATE", and "EXDATE" properties
-- contained within the recurring component.
-- [...]
-- The final recurrence set is generated by gathering all of the
-- start DATE-TIME values generated by any of the specified "RRULE"
-- and "RDATE" properties, and then excluding any start DATE-TIME
-- values specified by "EXDATE" properties.
-- @
--
-- However, because recurrence sets can be infinite, we do not reify them
-- entirely and instead provide only functions that produce finite results.
module ICal.Recurrence
  ( RecurringEvent (..),
    Recurrence (..),
    EventOccurrence (..),
    R,
    RecurrenceError (..),
    recurEvents,
    recurRecurrenceDateTimes,
    recurRecurrenceRules,
    removeExceptionDatetimes,
  )
where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Time as Time
import ICal.Property
import ICal.PropertyType
import ICal.Recurrence.Class
import ICal.Recurrence.RecurrenceDateTimes
import ICal.Recurrence.RecurrenceRule

-- | Compute the recurrence set, up to a given limit
recurEvents :: Day -> RecurringEvent -> R (Set EventOccurrence)
recurEvents limit RecurringEvent {..} =
  let -- @
      -- The "DTSTART" property for a "VEVENT" specifies the inclusive
      -- start of the event.  For recurring events, it also specifies the
      -- very first instance in the recurrence set.
      -- @
      -- @
      -- The "DTSTART" property
      -- defines the first instance in the recurrence set.
      -- @
      startEvent =
        EventOccurrence
          { eventOccurrenceStart = recurringEventStart,
            eventOccurrenceEndOrDuration = recurringEventEndOrDuration
          }
   in case recurringEventStart of
        Nothing -> pure $ S.singleton startEvent
        Just startDateTime -> do
          let Recurrence {..} = recurringEventRecurrence
          occurrencesFromRecurrenceDateTimes <- recurRecurrenceDateTimes startDateTime recurringEventEndOrDuration recurrenceRecurrenceDateTimes
          occurrencesFromRecurrenceRules <- recurRecurrenceRules limit startDateTime recurringEventEndOrDuration recurrenceRecurrenceRules
          -- @
          -- The final recurrence set is generated by gathering all of the
          -- start DATE-TIME values generated by any of the specified "RRULE"
          -- and "RDATE" properties, and then excluding any start DATE-TIME
          -- values specified by "EXDATE" properties.
          -- @
          -- @
          -- Where duplicate instances are generated by the "RRULE"
          -- and "RDATE" properties, only one recurrence is considered.
          -- Duplicate instances are ignored.
          -- @
          let preliminarySet = S.union occurrencesFromRecurrenceDateTimes occurrencesFromRecurrenceRules
          pure $ removeExceptionDatetimes recurrenceExceptionDateTimes preliminarySet

-- TODO about why we need the start date
-- @
-- Information, not contained in the rule, necessary to determine the
-- various recurrence instance start time and dates are derived from
-- the Start Time ("DTSTART") component attribute.  For example,
-- "FREQ=YEARLY;BYMONTH=1" doesn't specify a specific day within the
-- month or a time.  This information would be the same as what is
-- specified for "DTSTART".
-- @

-- TODO about invalid dates:
-- @
-- Recurrence rules may generate recurrence instances with an invalid
-- date (e.g., February 30) or nonexistent local time (e.g., 1:30 AM
-- on a day where the local time is moved forward by an hour at 1:00
-- AM).  Such recurrence instances MUST be ignored and MUST NOT be
-- counted as part of the recurrence set.
-- @

-- TODO About computing end and duration:
-- @
-- If the duration of the recurring component is specified with the
-- "DTEND" or "DUE" property, then the same exact duration will apply
-- to all the members of the generated recurrence set.  Else, if the
-- duration of the recurring component is specified with the
-- "DURATION" property, then the same nominal duration will apply to
-- all the members of the generated recurrence set and the exact
-- duration of each recurrence instance will depend on its specific
-- start time.  For example, recurrence instances of a nominal
-- duration of one day will have an exact duration of more or less
-- than 24 hours on a day where a time zone shift occurs.  The
-- duration of a specific recurrence may be modified in an exception
-- component or simply by using an "RDATE" property of PERIOD value
-- type.
-- @

-- | Remove the occurrences that the exception date times imply should be removed
removeExceptionDatetimes ::
  Set ExceptionDateTimes ->
  Set EventOccurrence ->
  Set EventOccurrence
removeExceptionDatetimes exceptions occurrences =
  if S.null exceptions
    then occurrences
    else undefined
