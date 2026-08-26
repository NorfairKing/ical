{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module ICal.Recurrence.Types
  ( CalendarRecurrence (..),
    Recurring (..),
    Recurrence (..),
    RecurrenceEnd (..),
    dateTimeEndRecurrenceEnd,
    dateTimeDueRecurrenceEnd,
    recurrenceEndDateTimeEnd,
    Occurrence (..),
    Resolved (..),
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

import Conformance
import Control.Exception
import Control.Monad.Reader
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Time as Time
import Data.Validity
import Data.Void
import GHC.Generics (Generic)
import ICal.Component.Event
import ICal.Component.Journal
import ICal.Component.TimeZone
import ICal.Component.Todo
import ICal.Parameter
import ICal.Property
import ICal.PropertyType

-- | The recurrence sets of a whole calendar, by UID
--
-- The VEVENTs, VTODOs and VJOURNALs of a calendar recur independently of one
-- another: a UID names a recurrence set within one of the three, not across
-- them.
data CalendarRecurrence = CalendarRecurrence
  { calendarRecurrenceEvents :: !(Map UID (Set (Occurrence Event))),
    calendarRecurrenceTodos :: !(Map UID (Set (Occurrence Todo))),
    calendarRecurrenceJournals :: !(Map UID (Set (Occurrence Journal)))
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity CalendarRecurrence

-- | A component's recurrence, and the component itself
--
-- This is everything RFC 5545 uses to define a recurrence set, and nothing
-- else.  It carries the UID and the SEQUENCE because reconciling overrides
-- needs both: the UID says which components form one recurrence set, and the
-- SEQUENCE settles two overrides that name the same instance.
--
-- The component is parameterised so that a caller that has already narrowed a
-- VEVENT down to what it cares about can still recur it.
data Recurring component = Recurring
  { recurringComponent :: !component,
    recurringUID :: !UID,
    recurringSequenceNumber :: !SequenceNumber,
    recurringRecurrenceIdentifier :: !(Maybe RecurrenceIdentifier),
    recurringStart :: !(Maybe DateTimeStart),
    recurringEnd :: !(Maybe (Either RecurrenceEnd Duration)),
    recurringRecurrence :: !Recurrence
  }
  deriving (Show, Eq, Ord, Generic, Functor)

instance (Validity component) => Validity (Recurring component)

data Recurrence = Recurrence
  { recurrenceExceptionDateTimes :: !(Set ExceptionDateTimes),
    recurrenceRecurrenceDateTimes :: !(Set RecurrenceDateTimes),
    recurrenceRecurrenceRules :: !(Set RecurrenceRule)
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Recurrence

-- | The explicit end of an instance
--
-- VEVENT spells this DTEND, VTODO spells it DUE, and VJOURNAL has no end at
-- all.  All three agree on the value type and on what recurrence does with it,
-- so the spelling stays in the projection functions and this is what the
-- recurrence machinery works with.
data RecurrenceEnd
  = RecurrenceEndDate !Date
  | RecurrenceEndDateTime !DateTime
  deriving (Show, Eq, Ord, Generic)

instance Validity RecurrenceEnd

dateTimeEndRecurrenceEnd :: DateTimeEnd -> RecurrenceEnd
dateTimeEndRecurrenceEnd = \case
  DateTimeEndDate date -> RecurrenceEndDate date
  DateTimeEndDateTime dateTime -> RecurrenceEndDateTime dateTime

dateTimeDueRecurrenceEnd :: DateTimeDue -> RecurrenceEnd
dateTimeDueRecurrenceEnd = \case
  DateTimeDueDate date -> RecurrenceEndDate date
  DateTimeDueDateTime dateTime -> RecurrenceEndDateTime dateTime

recurrenceEndDateTimeEnd :: RecurrenceEnd -> DateTimeEnd
recurrenceEndDateTimeEnd = \case
  RecurrenceEndDate date -> DateTimeEndDate date
  RecurrenceEndDateTime dateTime -> DateTimeEndDateTime dateTime

-- | One instance of a recurrence set, and the component it came from
--
-- The component is the override for an instance that has one, and the series
-- otherwise, so its properties are the ones that apply to this instance.
data Occurrence component = Occurrence
  { occurrenceComponent :: !component,
    occurrenceStart :: !(Maybe DateTimeStart),
    occurrenceEnd :: !(Maybe (Either RecurrenceEnd Duration))
  }
  deriving (Show, Eq, Ord, Generic, Functor)

instance (Validity component) => Validity (Occurrence component)

-- | An 'Occurrence' with its start and end resolved to instants
data Resolved component = Resolved
  { resolvedComponent :: !component,
    resolvedStart :: !(Maybe Timestamp),
    resolvedEnd :: !(Maybe Timestamp)
  }
  deriving (Show, Eq, Ord, Generic, Functor)

instance (Validity component) => Validity (Resolved component)

data Timestamp
  = TimestampDay !Time.Day
  | TimestampUTCTime !Time.UTCTime
  | TimestampLocalTime !Time.LocalTime
  deriving (Show, Eq, Ord, Generic)

instance Validity Timestamp

data RecurrenceError
  = StartEndMismatch !DateTimeStart !RecurrenceEnd
  | ExactDurationMismatch !DateTime !DateTime
  | TimeZoneNotFound !TimeZoneIdentifierParam
  | FailedToResolveLocalTime !TimeZone !Time.LocalTime
  | FailedToResolveLocalTimeCached !ResolutionCtx !Time.LocalTime
  | FailedToUnresolveUTCTime !TimeZone !Time.UTCTime
  | FailedToUnresolveUTCTimeCached !UnresolutionCtx !Time.UTCTime
  deriving (Show, Eq, Ord)

instance Exception RecurrenceError

data RecurrenceFixableError
  = RecurrenceMultipleRecurrenceRules !(Set RecurrenceRule)
  | RecurrenceByDayNumeric !ByDay
  | -- | A RECURRENCE-ID that names no instance of its UID's series
    RecurrenceIdentifierUnmatched !UID !DateTimeStart
  | -- | Two components of one UID overriding the same instance at the same SEQUENCE
    RecurrenceIdentifierDuplicate !UID !DateTimeStart
  | -- | Two components of one UID with no RECURRENCE-ID between them
    RecurrenceMultipleSeries !UID
  deriving (Show, Eq, Ord)

instance Exception RecurrenceFixableError

newtype R a = R {unR :: ReaderT TimeZoneCtx (Conform RecurrenceError RecurrenceFixableError Void) a}
  deriving (Functor, Applicative, Monad, MonadReader TimeZoneCtx)

type TimeZoneCtx = Map TimeZoneIdentifierParam (ResolutionCtx, UnresolutionCtx)

type ResolutionCtx = Map Time.LocalTime (UTCOffset, UTCOffset)

type UnresolutionCtx = Map Time.UTCTime (UTCOffset, UTCOffset)

unfixableErrorR :: RecurrenceError -> R a
unfixableErrorR = R . lift . unfixableError

emitFixableErrorR :: RecurrenceFixableError -> R ()
emitFixableErrorR = R . lift . emitFixableError

-- Timezone resolution must not require the same timezone map.
-- Otherwise it might infinitely loop.
type Resolv = Conform RecurrenceError RecurrenceFixableError Void
