{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ICal.Recurrence.Class
  ( RecurringEvent (..),
    Recurrence (..),
    EventOccurrence (..),
    ResolvedEvent (..),
    R (..),
    runR,
    RecurrenceError (..),
    RecurrenceFixableError (..),
    askTimeZoneMap,
    requireTimeZone,
    unfixableErrorR,
    emitFixableErrorR,
    Resolv,
  )
where

import Control.Exception
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Time as Time
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

-- An event occurrence, but with the exact start and end computed in a give Time.TimeZone.
data ResolvedEvent = ResolvedEvent
  { resolvedEventStart :: !(Maybe (Either Time.Day Time.LocalTime)),
    resolvedEventEnd :: !(Maybe (Either Time.Day Time.LocalTime))
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity ResolvedEvent

data RecurrenceError
  = StartStartMismatch !DateTimeStart !DateTimeStart -- Internal error, should not happen.
  | StartEndMismatch !DateTimeStart !DateTimeEnd
  | ExactDurationMismatch !DateTime !DateTime
  | TimeZoneNotFound !TZIDParam
  | FailedToResolveLocalTime !TimeZone !Time.LocalTime
  deriving (Show, Eq, Ord)

instance Exception RecurrenceError

data RecurrenceFixableError
  = RecurrenceMultipleRecurrenceRules !(Set RecurrenceRule)
  | RecurrenceByDayNumeric !ByDay
  deriving (Show, Eq, Ord)

instance Exception RecurrenceFixableError

newtype R a = R {unR :: ReaderT (Map TZIDParam TimeZone) (Conform RecurrenceError RecurrenceFixableError Void) a}
  deriving (Functor, Applicative, Monad)

runR :: Map TZIDParam TimeZone -> R a -> Conform RecurrenceError RecurrenceFixableError Void a
runR m (R func) = runReaderT func m

askTimeZoneMap :: R (Map TZIDParam TimeZone)
askTimeZoneMap = R ask

requireTimeZone :: TZIDParam -> R TimeZone
requireTimeZone tzid = do
  m <- askTimeZoneMap
  case M.lookup tzid m of
    Nothing -> unfixableErrorR $ TimeZoneNotFound tzid
    Just tz -> pure tz

unfixableErrorR :: RecurrenceError -> R a
unfixableErrorR = R . lift . unfixableError

emitFixableErrorR :: RecurrenceFixableError -> R ()
emitFixableErrorR = R . lift . emitFixableError

-- Timezone resolution must not require the same timezone map.
-- Otherwise it might infinitely loop.
type Resolv = Conform RecurrenceError RecurrenceFixableError Void
