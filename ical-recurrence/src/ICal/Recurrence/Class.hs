{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ICal.Recurrence.Class
  ( RecurringEvent (..),
    Recurrence (..),
    EventOccurrence (..),
    ResolvedEvent (..),
    Timestamp (..),
    R (..),
    ResolutionCtx,
    UnresolutionCtx,
    RecurrenceError (..),
    RecurrenceFixableError (..),
    unfixableErrorR,
    emitFixableErrorR,
    Resolv,
  )
where

import Control.Exception
import Control.Monad.Reader
import Data.Map.Strict (Map)
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
  { resolvedEventStart :: !(Maybe Timestamp),
    resolvedEventEnd :: !(Maybe Timestamp)
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity ResolvedEvent

data Timestamp
  = TimestampDay !Time.Day
  | TimestampUTCTime !Time.UTCTime
  | TimestampLocalTime !Time.LocalTime
  deriving (Show, Eq, Ord, Generic)

instance Validity Timestamp

data RecurrenceError
  = StartStartMismatch !DateTimeStart !DateTimeStart -- Internal error, should not happen.
  | StartEndMismatch !DateTimeStart !DateTimeEnd
  | ExactDurationMismatch !DateTime !DateTime
  | TimeZoneNotFound !TZIDParam
  | FailedToResolveLocalTime !TimeZone !Time.LocalTime
  | FailedToResolveLocalTimeCached !ResolutionCtx !Time.LocalTime
  | FailedToUnresolveUTCTime !TimeZone !Time.UTCTime
  | FailedToUnresolveUTCTimeCached !UnresolutionCtx !Time.UTCTime
  deriving (Show, Eq, Ord)

instance Exception RecurrenceError

data RecurrenceFixableError
  = RecurrenceMultipleRecurrenceRules !(Set RecurrenceRule)
  | RecurrenceByDayNumeric !ByDay
  deriving (Show, Eq, Ord)

instance Exception RecurrenceFixableError

newtype R a = R {unR :: ReaderT TimeZoneCtx (Conform RecurrenceError RecurrenceFixableError Void) a}
  deriving (Functor, Applicative, Monad, MonadReader TimeZoneCtx)

type TimeZoneCtx = Map TZIDParam (ResolutionCtx, UnresolutionCtx)

type ResolutionCtx = Map Time.LocalTime (UTCOffset, UTCOffset)

type UnresolutionCtx = Map Time.UTCTime (UTCOffset, UTCOffset)

unfixableErrorR :: RecurrenceError -> R a
unfixableErrorR = R . lift . unfixableError

emitFixableErrorR :: RecurrenceFixableError -> R ()
emitFixableErrorR = R . lift . emitFixableError

-- Timezone resolution must not require the same timezone map.
-- Otherwise it might infinitely loop.
type Resolv = Conform RecurrenceError RecurrenceFixableError Void
