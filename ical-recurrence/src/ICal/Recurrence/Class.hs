{-# LANGUAGE DeriveGeneric #-}

module ICal.Recurrence.Class
  ( RecurringEvent (..),
    Recurrence (..),
    EventOccurrence (..),
    R,
    RecurrenceError (..),
    RecurrenceFixableError (..),
  )
where

import Control.Exception
import Data.Set (Set)
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
  deriving (Show, Eq, Ord, Generic)

instance Validity Recurrence

data RecurringEvent = RecurringEvent
  { recurringEventStart :: !(Maybe DateTimeStart),
    recurringEventEndOrDuration :: !(Maybe (Either DateTimeEnd Duration)),
    recurringEventRecurrence :: !Recurrence
  }
  deriving (Show, Eq, Ord, Generic)

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

instance Exception RecurrenceError

data RecurrenceFixableError
  = RecurrenceMultipleRecurrenceRules !(Set RecurrenceRule)
  | RecurrenceByDayNumeric !ByDay
  deriving (Show, Eq, Ord)

instance Exception RecurrenceFixableError

type R = Conform RecurrenceError RecurrenceFixableError Void
