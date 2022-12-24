{-# LANGUAGE DeriveGeneric #-}

module ICal.Recurrence.Class
  ( RecurringEvent (..),
    Recurrence (..),
    EventOccurrence (..),
    R,
    RecurrenceError (..),
    RecurrenceFixableError (..),
    askTimeZoneMap,
    requireTimeZone,
    Resolv,
  )
where

import Control.Exception
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import Data.Validity
import Data.Void
import GHC.Generics (Generic)
import ICal.Component.TimeZone
import ICal.Conformance
import ICal.Parameter
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
  | TimeZoneNotFound !TZIDParam
  | NoApplicableOffset
  deriving (Show, Eq, Ord)

instance Exception RecurrenceError

data RecurrenceFixableError
  = RecurrenceMultipleRecurrenceRules !(Set RecurrenceRule)
  | RecurrenceByDayNumeric !ByDay
  deriving (Show, Eq, Ord)

instance Exception RecurrenceFixableError

type R = ConformT RecurrenceError RecurrenceFixableError Void (Reader (Map TZIDParam TimeZone))

askTimeZoneMap :: R (Map TZIDParam TimeZone)
askTimeZoneMap = lift ask

requireTimeZone :: TZIDParam -> R TimeZone
requireTimeZone tzid = do
  m <- askTimeZoneMap
  case M.lookup tzid m of
    Nothing -> unfixableError $ TimeZoneNotFound tzid
    Just tz -> pure tz

-- Timezone resolution must not require the same timezone map.
-- Otherwise it might infinitely loop.
type Resolv = Conform RecurrenceError RecurrenceFixableError Void
