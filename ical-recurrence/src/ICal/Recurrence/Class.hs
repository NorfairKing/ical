{-# LANGUAGE DeriveGeneric #-}

module ICal.Recurrence.Class
  ( RecurringEvent (..),
    Recurrence (..),
    EventOccurrence (..),
    R,
    RecurrenceError (..),
    RecurrenceFixableError (..),
    resolveEndOrDurationDate,
  )
where

import Data.Set (Set)
import qualified Data.Time as Time
import Data.Validity
import Data.Void
import GHC.Generics (Generic)
import ICal.Conformance
import ICal.Property
import ICal.PropertyType

data Recurrence = Recurrence
  { recurrenceExceptionDateTimes :: !(Set ExceptionDateTimes),
    recurrenceRecurrenceDateTimes :: !(Set RecurrenceDateTimes),
    recurrenceRecurrenceRules :: !(Set RecurrenceRule)
  }
  deriving (Show, Eq, Generic)

instance Validity Recurrence

data RecurringEvent = RecurringEvent
  { recurringEventStart :: !(Maybe DateTimeStart),
    recurringEventEndOrDuration :: !(Maybe (Either DateTimeEnd Duration)),
    recurringEventRecurrence :: !Recurrence
  }
  deriving (Show, Eq, Generic)

instance Validity RecurringEvent

data EventOccurrence = EventOccurrence
  { eventOccurrenceStart :: !(Maybe DateTimeStart),
    eventOccurrenceEndOrDuration :: !(Maybe (Either DateTimeEnd Duration))
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity EventOccurrence

data RecurrenceError
  = StartStartMismatch !DateTimeStart !DateTimeStart -- Internal error, should not happen.
  | StartEndMismatch !DateTimeStart !DateTimeEnd
  | ExactDurationMismatch !DateTime !DateTime
  deriving (Show, Eq, Ord)

data RecurrenceFixableError
  = RecurrenceMultipleRecurrenceRules !(Set RecurrenceRule)
  | RecurrenceByDayNumeric !ByDay
  deriving (Show, Eq, Ord)

type R = Conform RecurrenceError RecurrenceFixableError Void

resolveEndOrDurationDate ::
  DateTimeStart ->
  Maybe (Either DateTimeEnd Duration) ->
  DateTimeStart ->
  R (Maybe (Either DateTimeEnd Duration))
resolveEndOrDurationDate originalStart mEndOrDuration newStart = case mEndOrDuration of
  Nothing -> pure Nothing
  Just (Right duration) -> pure $ Just (Right duration)
  Just (Left end) -> do
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
    newEnd <- computeNewEnd originalStart end newStart
    pure $ Just $ Left newEnd

computeNewEnd :: DateTimeStart -> DateTimeEnd -> DateTimeStart -> R DateTimeEnd
computeNewEnd originalStart end newStart =
  case (originalStart, end) of
    (DateTimeStartDate startDate, DateTimeEndDate endDate) ->
      let exactDuration = dateExactDuration startDate endDate
       in case newStart of
            DateTimeStartDate newDate -> pure $ DateTimeEndDate (dateAddDays exactDuration newDate)
            _ -> unfixableError $ StartStartMismatch originalStart newStart
    (DateTimeStartDateTime startDateTime, DateTimeEndDateTime endDateTime) ->
      let exactDuration = dateTimeExactDuration startDateTime endDateTime
       in case newStart of
            DateTimeStartDateTime newDateTime -> error "Not supported yet." exactDuration newDateTime
            _ -> unfixableError $ StartStartMismatch originalStart newStart
    -- These two cases represent invalid ical:
    -- @
    -- The "VEVENT" is also the calendar component used to specify an
    -- anniversary or daily reminder within a calendar.
    -- These events
    -- have a DATE value type for the "DTSTART" property instead of the
    -- default value type of DATE-TIME.  If such a "VEVENT" has a "DTEND"
    -- property, it MUST be specified as a DATE value also.
    -- @
    -- However, this is a new restriction, see the following, so we do
    -- _something_ in this case, to not have to error.
    --
    -- In the section: "A.1. New Restrictions":
    -- @
    -- The value type of the "DTEND" or "DUE" properties MUST match the
    -- value type of "DTSTART" property.
    -- @
    (DateTimeStartDate _, DateTimeEndDateTime _) ->
      unfixableError $ StartEndMismatch originalStart end
    (DateTimeStartDateTime _, DateTimeEndDate _) ->
      unfixableError $ StartEndMismatch originalStart end

-- TODO it is not clear at all whether this is the intended interpretation.
-- In fact, if these two dates are in a timezone with day light savings time, then the exact duration will not be an integer number of days.
dateExactDuration :: Date -> Date -> Integer
dateExactDuration = diffDates

dateTimeExactDuration :: DateTime -> DateTime -> R Time.NominalDiffTime
dateTimeExactDuration dt1 dt2 = case (dt1, dt2) of
  (DateTimeFloating lt1, DateTimeFloating lt2) ->
    -- Assuming the same timezone
    pure $ Time.diffLocalTime lt1 lt2
  (DateTimeUTC ut1, DateTimeUTC ut2) -> pure $ Time.diffUTCTime ut1 ut2
  (DateTimeZoned tzid1 lt1, DateTimeZoned tzid2 lt2) -> error "Exact duration offsets between two zoned times are not supported yet." tzid1 tzid2 lt1 lt2
  _ -> unfixableError $ ExactDurationMismatch dt1 dt2
