{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Component.TimeZone where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans
import Data.DList (DList (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Time as Time
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import GHC.Generics (Generic)
import ICal.Component.Class
import ICal.Conformance
import ICal.ContentLine
import ICal.Property
import ICal.PropertyType.Class
import ICal.PropertyType.DateTime
import ICal.PropertyType.RecurrenceRule
import Text.Megaparsec

-- |
--
-- === [section 3.6.5](https://datatracker.ietf.org/doc/html/rfc5545#section-3.6.5)
--
-- @
-- Component Name:  VTIMEZONE
--
-- Purpose:  Provide a grouping of component properties that defines a
--    time zone.
--
-- Format Definition:  A "VTIMEZONE" calendar component is defined by
--    the following notation:
--
--     timezonec  = "BEGIN" ":" "VTIMEZONE" CRLF
--                  *(
--                  ;
--                  ; 'tzid' is REQUIRED, but MUST NOT occur more
--                  ; than once.
--                  ;
--                  tzid /
--                  ;
--                  ; 'last-mod' and 'tzurl' are OPTIONAL,
--                  ; but MUST NOT occur more than once.
--                  ;
--                  last-mod / tzurl /
--                  ;
--                  ; One of 'standardc' or 'daylightc' MUST occur
--                  ; and each MAY occur more than once.
--                  ;
--                  standardc / daylightc /
--                  ;
--                  ; The following are OPTIONAL,
--                  ; and MAY occur more than once.
--                  ;
--                  x-prop / iana-prop
--                  ;
--                  )
--                  "END" ":" "VTIMEZONE" CRLF
--
--     standardc  = "BEGIN" ":" "STANDARD" CRLF
--                  tzprop
--                  "END" ":" "STANDARD" CRLF
--
--     daylightc  = "BEGIN" ":" "DAYLIGHT" CRLF
--                  tzprop
--                  "END" ":" "DAYLIGHT" CRLF
--
--     tzprop     = *(
--                  ;
--                  ; The following are REQUIRED,
--                  ; but MUST NOT occur more than once.
--                  ;
--                  dtstart / tzoffsetto / tzoffsetfrom /
--                  ;
--                  ; The following is OPTIONAL,
--                  ; but SHOULD NOT occur more than once.
--                  ;
--                  rrule /
--                  ;
--                  ; The following are OPTIONAL,
--                  ; and MAY occur more than once.
--                  ;
--                  comment / rdate / tzname / x-prop / iana-prop
--                  ;
--                  )
--
-- Description:  A time zone is unambiguously defined by the set of time
--    measurement rules determined by the governing body for a given
--    geographic area.  These rules describe, at a minimum, the base
--
--    offset from UTC for the time zone, often referred to as the
--    Standard Time offset.  Many locations adjust their Standard Time
--    forward or backward by one hour, in order to accommodate seasonal
--    changes in number of daylight hours, often referred to as Daylight
--    Saving Time.  Some locations adjust their time by a fraction of an
--    hour.  Standard Time is also known as Winter Time.  Daylight
--    Saving Time is also known as Advanced Time, Summer Time, or Legal
--    Time in certain countries.  The following table shows the changes
--    in time zone rules in effect for New York City starting from 1967.
--    Each line represents a description or rule for a particular
--    observance.
--
--                       Effective Observance Rule
--
--   +-----------+--------------------------+--------+--------------+
--   | Date      | (Date-Time)              | Offset | Abbreviation |
--   +-----------+--------------------------+--------+--------------+
--   | 1967-1973 | last Sun in Apr, 02:00   | -0400  | EDT          |
--   |           |                          |        |              |
--   | 1967-2006 | last Sun in Oct, 02:00   | -0500  | EST          |
--   |           |                          |        |              |
--   | 1974-1974 | Jan 6, 02:00             | -0400  | EDT          |
--   |           |                          |        |              |
--   | 1975-1975 | Feb 23, 02:00            | -0400  | EDT          |
--   |           |                          |        |              |
--   | 1976-1986 | last Sun in Apr, 02:00   | -0400  | EDT          |
--   |           |                          |        |              |
--   | 1987-2006 | first Sun in Apr, 02:00  | -0400  | EDT          |
--   |           |                          |        |              |
--   | 2007-*    | second Sun in Mar, 02:00 | -0400  | EDT          |
--   |           |                          |        |              |
--   | 2007-*    | first Sun in Nov, 02:00  | -0500  | EST          |
--   +-----------+--------------------------+--------+--------------+
--
-- Note: The specification of a global time zone registry is not
--       addressed by this document and is left for future study.
--       However, implementers may find the TZ database [TZDB] a useful
--       reference.  It is an informal, public-domain collection of time
--       zone information, which is currently being maintained by
--       volunteer Internet participants, and is used in several
--       operating systems.  This database contains current and
--       historical time zone information for a wide variety of
--       locations around the globe; it provides a time zone identifier
--       for every unique time zone rule set in actual use since 1970,
--       with historical data going back to the introduction of standard
--       time.
--
--    Interoperability between two calendaring and scheduling
--    applications, especially for recurring events, to-dos or journal
--    entries, is dependent on the ability to capture and convey date
--    and time information in an unambiguous format.  The specification
--    of current time zone information is integral to this behavior.
--
--    If present, the "VTIMEZONE" calendar component defines the set of
--    Standard Time and Daylight Saving Time observances (or rules) for
--    a particular time zone for a given interval of time.  The
--    "VTIMEZONE" calendar component cannot be nested within other
--    calendar components.  Multiple "VTIMEZONE" calendar components can
--    exist in an iCalendar object.  In this situation, each "VTIMEZONE"
--    MUST represent a unique time zone definition.  This is necessary
--    for some classes of events, such as airline flights, that start in
--    one time zone and end in another.
--
--    The "VTIMEZONE" calendar component MUST include the "TZID"
--    property and at least one definition of a "STANDARD" or "DAYLIGHT"
--    sub-component.  The "STANDARD" or "DAYLIGHT" sub-component MUST
--    include the "DTSTART", "TZOFFSETFROM", and "TZOFFSETTO"
--    properties.
--
--    An individual "VTIMEZONE" calendar component MUST be specified for
--    each unique "TZID" parameter value specified in the iCalendar
--    object.  In addition, a "VTIMEZONE" calendar component, referred
--    to by a recurring calendar component, MUST provide valid time zone
--    information for all recurrence instances.
--
--    Each "VTIMEZONE" calendar component consists of a collection of
--    one or more sub-components that describe the rule for a particular
--    observance (either a Standard Time or a Daylight Saving Time
--    observance).  The "STANDARD" sub-component consists of a
--    collection of properties that describe Standard Time.  The
--    "DAYLIGHT" sub-component consists of a collection of properties
--    that describe Daylight Saving Time.  In general, this collection
--    of properties consists of:
--
--    *  the first onset DATE-TIME for the observance;
--
--    *  the last onset DATE-TIME for the observance, if a last onset is
--       known;
--
--    *  the offset to be applied for the observance;
--
--    *  a rule that describes the day and time when the observance
--       takes effect;
--
--    *  an optional name for the observance.
--
--    For a given time zone, there may be multiple unique definitions of
--    the observances over a period of time.  Each observance is
--    described using either a "STANDARD" or "DAYLIGHT" sub-component.
--    The collection of these sub-components is used to describe the
--    time zone for a given period of time.  The offset to apply at any
--    given time is found by locating the observance that has the last
--    onset date and time before the time in question, and using the
--    offset value from that observance.
--
--    The top-level properties in a "VTIMEZONE" calendar component are:
--
--    The mandatory "TZID" property is a text value that uniquely
--    identifies the "VTIMEZONE" calendar component within the scope of
--    an iCalendar object.
--
--    The optional "LAST-MODIFIED" property is a UTC value that
--    specifies the date and time that this time zone definition was
--    last updated.
--
--    The optional "TZURL" property is a url value that points to a
--    published "VTIMEZONE" definition.  "TZURL" SHOULD refer to a
--    resource that is accessible by anyone who might need to interpret
--    the object.  This SHOULD NOT normally be a "file" URL or other URL
--    that is not widely accessible.
--
--    The collection of properties that are used to define the
--    "STANDARD" and "DAYLIGHT" sub-components include:
--
--    The mandatory "DTSTART" property gives the effective onset date
--    and local time for the time zone sub-component definition.
--    "DTSTART" in this usage MUST be specified as a date with a local
--    time value.
--
--    The mandatory "TZOFFSETFROM" property gives the UTC offset that is
--    in use when the onset of this time zone observance begins.
--    "TZOFFSETFROM" is combined with "DTSTART" to define the effective
--    onset for the time zone sub-component definition.  For example,
--    the following represents the time at which the observance of
--    Standard Time took effect in Fall 1967 for New York City:
--
--     DTSTART:19671029T020000
--
--     TZOFFSETFROM:-0400
--
--    The mandatory "TZOFFSETTO" property gives the UTC offset for the
--    time zone sub-component (Standard Time or Daylight Saving Time)
--    when this observance is in use.
--
--    The optional "TZNAME" property is the customary name for the time
--    zone.  This could be used for displaying dates.
--
--    The onset DATE-TIME values for the observance defined by the time
--    zone sub-component is defined by the "DTSTART", "RRULE", and
--    "RDATE" properties.
--
--    The "RRULE" property defines the recurrence rule for the onset of
--    the observance defined by this time zone sub-component.  Some
--    specific requirements for the usage of "RRULE" for this purpose
--    include:
--
--    *  If observance is known to have an effective end date, the
--       "UNTIL" recurrence rule parameter MUST be used to specify the
--       last valid onset of this observance (i.e., the UNTIL DATE-TIME
--       will be equal to the last instance generated by the recurrence
--       pattern).  It MUST be specified in UTC time.
--
--    *  The "DTSTART" and the "TZOFFSETFROM" properties MUST be used
--       when generating the onset DATE-TIME values (instances) from the
--       "RRULE".
--
--    The "RDATE" property can also be used to define the onset of the
--    observance by giving the individual onset date and times.  "RDATE"
--    in this usage MUST be specified as a date with local time value,
--    relative to the UTC offset specified in the "TZOFFSETFROM"
--    property.
--
--    The optional "COMMENT" property is also allowed for descriptive
--    explanatory text.
--
-- Example:  The following are examples of the "VTIMEZONE" calendar
--    component:
--
--    This is an example showing all the time zone rules for New York
--    City since April 30, 1967 at 03:00:00 EDT.
--
--     BEGIN:VTIMEZONE
--     TZID:America/New_York
--     LAST-MODIFIED:20050809T050000Z
--     BEGIN:DAYLIGHT
--     DTSTART:19670430T020000
--     RRULE:FREQ=YEARLY;BYMONTH=4;BYDAY=-1SU;UNTIL=19730429T070000Z
--     TZOFFSETFROM:-0500
--     TZOFFSETTO:-0400
--     TZNAME:EDT
--     END:DAYLIGHT
--     BEGIN:STANDARD
--
--     DTSTART:19671029T020000
--     RRULE:FREQ=YEARLY;BYMONTH=10;BYDAY=-1SU;UNTIL=20061029T060000Z
--     TZOFFSETFROM:-0400
--     TZOFFSETTO:-0500
--     TZNAME:EST
--     END:STANDARD
--     BEGIN:DAYLIGHT
--     DTSTART:19740106T020000
--     RDATE:19750223T020000
--     TZOFFSETFROM:-0500
--     TZOFFSETTO:-0400
--     TZNAME:EDT
--     END:DAYLIGHT
--     BEGIN:DAYLIGHT
--     DTSTART:19760425T020000
--     RRULE:FREQ=YEARLY;BYMONTH=4;BYDAY=-1SU;UNTIL=19860427T070000Z
--     TZOFFSETFROM:-0500
--     TZOFFSETTO:-0400
--     TZNAME:EDT
--     END:DAYLIGHT
--     BEGIN:DAYLIGHT
--     DTSTART:19870405T020000
--     RRULE:FREQ=YEARLY;BYMONTH=4;BYDAY=1SU;UNTIL=20060402T070000Z
--     TZOFFSETFROM:-0500
--     TZOFFSETTO:-0400
--     TZNAME:EDT
--     END:DAYLIGHT
--     BEGIN:DAYLIGHT
--     DTSTART:20070311T020000
--     RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=2SU
--     TZOFFSETFROM:-0500
--     TZOFFSETTO:-0400
--     TZNAME:EDT
--     END:DAYLIGHT
--     BEGIN:STANDARD
--     DTSTART:20071104T020000
--     RRULE:FREQ=YEARLY;BYMONTH=11;BYDAY=1SU
--     TZOFFSETFROM:-0400
--     TZOFFSETTO:-0500
--     TZNAME:EST
--     END:STANDARD
--     END:VTIMEZONE
--
--    This is an example showing time zone information for New York City
--    using only the "DTSTART" property.  Note that this is only
--    suitable for a recurring event that starts on or later than March
--    11, 2007 at 03:00:00 EDT (i.e., the earliest effective transition
--    date and time) and ends no later than March 9, 2008 at 01:59:59
--    EST (i.e., latest valid date and time for EST in this scenario).
--    For example, this can be used for a recurring event that occurs
--    every Friday, 8:00 A.M.-9:00 A.M., starting June 1, 2007, ending
--    December 31, 2007,
--
--     BEGIN:VTIMEZONE
--     TZID:America/New_York
--     LAST-MODIFIED:20050809T050000Z
--     BEGIN:STANDARD
--     DTSTART:20071104T020000
--     TZOFFSETFROM:-0400
--     TZOFFSETTO:-0500
--     TZNAME:EST
--     END:STANDARD
--     BEGIN:DAYLIGHT
--     DTSTART:20070311T020000
--     TZOFFSETFROM:-0500
--     TZOFFSETTO:-0400
--     TZNAME:EDT
--     END:DAYLIGHT
--     END:VTIMEZONE
--
--    This is a simple example showing the current time zone rules for
--    New York City using a "RRULE" recurrence pattern.  Note that there
--    is no effective end date to either of the Standard Time or
--    Daylight Time rules.  This information would be valid for a
--    recurring event starting today and continuing indefinitely.
--
--     BEGIN:VTIMEZONE
--     TZID:America/New_York
--     LAST-MODIFIED:20050809T050000Z
--     TZURL:http://zones.example.com/tz/America-New_York.ics
--     BEGIN:STANDARD
--     DTSTART:20071104T020000
--     RRULE:FREQ=YEARLY;BYMONTH=11;BYDAY=1SU
--     TZOFFSETFROM:-0400
--     TZOFFSETTO:-0500
--     TZNAME:EST
--     END:STANDARD
--     BEGIN:DAYLIGHT
--     DTSTART:20070311T020000
--     RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=2SU
--     TZOFFSETFROM:-0500
--     TZOFFSETTO:-0400
--     TZNAME:EDT
--     END:DAYLIGHT
--     END:VTIMEZONE
--
--    This is an example showing a set of rules for a fictitious time
--    zone where the Daylight Time rule has an effective end date (i.e.,
--    after that date, Daylight Time is no longer observed).
--
--     BEGIN:VTIMEZONE
--     TZID:Fictitious
--     LAST-MODIFIED:19870101T000000Z
--     BEGIN:STANDARD
--     DTSTART:19671029T020000
--     RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10
--     TZOFFSETFROM:-0400
--     TZOFFSETTO:-0500
--     TZNAME:EST
--     END:STANDARD
--     BEGIN:DAYLIGHT
--     DTSTART:19870405T020000
--     RRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4;UNTIL=19980404T070000Z
--     TZOFFSETFROM:-0500
--     TZOFFSETTO:-0400
--     TZNAME:EDT
--     END:DAYLIGHT
--     END:VTIMEZONE
--
--    This is an example showing a set of rules for a fictitious time
--    zone where the first Daylight Time rule has an effective end date.
--    There is a second Daylight Time rule that picks up where the other
--    left off.
--
--     BEGIN:VTIMEZONE
--     TZID:Fictitious
--     LAST-MODIFIED:19870101T000000Z
--     BEGIN:STANDARD
--     DTSTART:19671029T020000
--     RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10
--     TZOFFSETFROM:-0400
--     TZOFFSETTO:-0500
--     TZNAME:EST
--     END:STANDARD
--     BEGIN:DAYLIGHT
--     DTSTART:19870405T020000
--     RRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4;UNTIL=19980404T070000Z
--     TZOFFSETFROM:-0500
--     TZOFFSETTO:-0400
--     TZNAME:EDT
--     END:DAYLIGHT
--     BEGIN:DAYLIGHT
--     DTSTART:19990424T020000
--     RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=4
--     TZOFFSETFROM:-0500
--     TZOFFSETTO:-0400
--     TZNAME:EDT
--     END:DAYLIGHT
--     END:VTIMEZONE
-- @
data TimeZone = TimeZone
  { -- @
    -- ; 'tzid' is REQUIRED, but MUST NOT occur more
    -- ; than once.
    -- ;
    -- tzid /
    -- @
    timeZoneId :: !TZID,
    -- @
    -- ; 'last-mod' and 'tzurl' are OPTIONAL,
    -- ; but MUST NOT occur more than once.
    -- ;
    -- last-mod / tzurl /
    -- @
    -- TODO
    -- @
    -- ; One of 'standardc' or 'daylightc' MUST occur
    -- ; and each MAY occur more than once.
    -- ;
    -- standardc / daylightc /
    -- @
    timeZoneObservances :: !(Set TimeZoneObservance)
    -- @
    -- ; The following are OPTIONAL,
    -- ; and MAY occur more than once.
    -- ;
    -- x-prop / iana-prop
    -- @
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity TimeZone where
  validate tz@TimeZone {..} =
    mconcat
      [ genericValidate tz,
        declare "there is at least one timezone observance" $
          not $
            S.null timeZoneObservances
      ]

instance NFData TimeZone

instance IsComponent TimeZone where
  componentName Proxy = "VTIMEZONE"
  componentP = vTimeZoneP
  componentB = vTimeZoneB

makeTimeZone :: TZID -> Set TimeZoneObservance -> TimeZone
makeTimeZone tzid observances =
  TimeZone
    { timeZoneId = tzid,
      timeZoneObservances = observances
    }

vTimeZoneP :: Component -> CP TimeZone
vTimeZoneP Component {..} = do
  timeZoneId <- requiredProperty componentProperties

  standards <- subComponentsP componentSubcomponents
  daylights <- subComponentsP componentSubcomponents
  let os = map StandardObservance standards <> map DaylightObservance daylights
  timeZoneObservances <- case NE.nonEmpty os of
    Nothing -> error "fail: Must have at least one standardc or daylightc"
    Just ne -> pure $ S.fromList $ NE.toList ne

  pure TimeZone {..}

vTimeZoneB :: TimeZone -> Component
vTimeZoneB TimeZone {..} =
  Component
    { componentProperties =
        requiredPropertyB timeZoneId,
      componentSubcomponents =
        M.unionsWith (<>) $
          map
            ( \case
                StandardObservance s -> namedComponentMapB s
                DaylightObservance d -> namedComponentMapB d
            )
            (S.toList timeZoneObservances)
    }

data TimeZoneObservance
  = StandardObservance !Standard
  | DaylightObservance !Daylight
  deriving (Show, Eq, Ord, Generic)

instance Validity TimeZoneObservance

instance NFData TimeZoneObservance

newtype Standard = Standard {unStandard :: Observance}
  deriving (Show, Eq, Ord, Generic)

instance Validity Standard

instance NFData Standard

instance IsComponent Standard where
  componentName Proxy = "STANDARD"
  componentP = fmap Standard . observanceP
  componentB = observanceB . unStandard

newtype Daylight = Daylight {unDaylight :: Observance}
  deriving (Show, Eq, Ord, Generic)

instance Validity Daylight

instance NFData Daylight

instance IsComponent Daylight where
  componentName Proxy = "DAYLIGHT"
  componentP = fmap Daylight . observanceP
  componentB = observanceB . unDaylight

data Observance = Observance
  { -- @
    -- ; The following are REQUIRED,
    -- ; but MUST NOT occur more than once.
    -- ;
    -- dtstart / tzoffsetto / tzoffsetfrom /
    -- @
    -- @
    -- The mandatory "DTSTART" property gives the effective onset date
    -- and local time for the time zone sub-component definition.
    -- "DTSTART" in this usage MUST be specified as a date with a local
    -- time value.
    -- @
    observanceDateTimeStart :: !Time.LocalTime,
    observanceTimeZoneOffsetTo :: !TimeZoneOffsetTo,
    observanceTimeZoneOffsetFrom :: !TimeZoneOffsetFrom,
    -- @
    -- ; The following is OPTIONAL,
    -- ; but SHOULD NOT occur more than once.
    -- ;
    -- rrule /
    -- @
    observanceRecurrenceRules :: !(Set RecurrenceRule),
    -- @
    -- ; The following are OPTIONAL,
    -- ; and MAY occur more than once.
    -- ;
    -- comment / rdate / tzname / x-prop / iana-prop
    -- @
    observanceComment :: !(Set Comment),
    observanceRecurrenceDateTimes :: !(Set RecurrenceDateTimes),
    observanceTimeZoneName :: !(Set TimeZoneName)
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Observance where
  validate o@Observance {..} =
    mconcat
      [ genericValidate o,
        validateImpreciseLocalTime observanceDateTimeStart,
        validateDateTimeStartRRule (DateTimeStartDateTime (DateTimeFloating observanceDateTimeStart)) observanceRecurrenceRules
      ]

instance NFData Observance

makeObservance :: Time.LocalTime -> TimeZoneOffsetFrom -> TimeZoneOffsetTo -> Observance
makeObservance start from to =
  Observance
    { observanceDateTimeStart = start,
      observanceTimeZoneOffsetFrom = from,
      observanceTimeZoneOffsetTo = to,
      observanceRecurrenceRules = S.empty,
      observanceComment = S.empty,
      observanceRecurrenceDateTimes = S.empty,
      observanceTimeZoneName = S.empty
    }

observanceP :: Component -> CP Observance
observanceP Component {..} = do
  -- @
  -- The mandatory "DTSTART" property gives the effective onset date
  -- and local time for the time zone sub-component definition.
  -- "DTSTART" in this usage MUST be specified as a date with a local
  -- time value.
  -- @
  dtstart <- requiredProperty componentProperties
  observanceDateTimeStart <- case dtstart of
    DateTimeStartDate _ -> error "fail: DTSTART must be specified as a datetime, not a date."
    DateTimeStartDateTime dt -> case dt of
      DateTimeFloating lt -> pure lt
      _ -> error "fail: DTSTART must be specified as a date with a local time value."

  observanceTimeZoneOffsetTo <- requiredProperty componentProperties
  observanceTimeZoneOffsetFrom <- requiredProperty componentProperties
  observanceRecurrenceRules <-
    S.fromList
      <$> (listOfProperties componentProperties >>= traverse (fixUntil (Just dtstart)))
  when (S.size observanceRecurrenceRules > 1) $
    emitWarning $
      WarnMultipleRecurrenceRules observanceRecurrenceRules

  observanceComment <- setOfProperties componentProperties
  observanceRecurrenceDateTimes <- setOfProperties componentProperties
  observanceTimeZoneName <- setOfProperties componentProperties
  pure Observance {..}

observanceB :: Observance -> Component
observanceB Observance {..} =
  Component
    { componentProperties =
        M.unionsWith
          (<>)
          [ requiredPropertyB (DateTimeStartDateTime (DateTimeFloating observanceDateTimeStart)),
            requiredPropertyB observanceTimeZoneOffsetTo,
            requiredPropertyB observanceTimeZoneOffsetFrom,
            setOfPropertiesB observanceRecurrenceRules,
            setOfPropertiesB observanceComment,
            setOfPropertiesB observanceRecurrenceDateTimes,
            setOfPropertiesB observanceTimeZoneName
          ],
      componentSubcomponents = M.empty
    }
