{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.PropertyType.DateTime
  ( DateTime (..),
    dateTimeDate,
    parseDateTimeFloating,
    parseDateTimeUTC,
    renderDateTimeUTC,
    renderDateTimeFloating,
    dateTimeP,
    dateTimeB,
    dateTimeUTCB,
    dateTimeUTCP,
    dateTimeFloatingB,
    dateTimeFloatingP,

    -- * Helpers
    dateTimeFloatingFormatStr,
    dateTimeZonedFormatStr,
    dateTimeUTCFormatStr,
    utcTimeShowsPrec,
    localTimeShowsPrec,
  )
where

import Control.DeepSeq
import qualified Data.Map.Strict as M
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import Data.Void
import GHC.Generics (Generic)
import ICal.Conformance
import ICal.ContentLine
import ICal.Parameter
import ICal.PropertyType.Class
import ICal.PropertyType.Date
import ICal.PropertyType.Time

-- | Date Time
--
-- === [section 3.3.5](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.5)
--
-- @
-- Value Name:  DATE-TIME
--
-- Purpose:  This value type is used to identify values that specify a
--    precise calendar date and time of day.
--
-- Format Definition:  This value type is defined by the following
--    notation:
--
--     date-time  = date "T" time ;As specified in the DATE and TIME
--                                ;value definitions
--
-- Description:  If the property permits, multiple "DATE-TIME" values
--    are specified as a COMMA-separated list of values.  No additional
--    content value encoding (i.e., BACKSLASH character encoding, see
--    Section 3.3.11) is defined for this value type.
--
--    The "DATE-TIME" value type is used to identify values that contain
--    a precise calendar date and time of day.  The format is based on
--    the [ISO.8601.2004] complete representation, basic format for a
--    calendar date and time of day.  The text format is a concatenation
--    of the "date", followed by the LATIN CAPITAL LETTER T character,
--    the time designator, followed by the "time" format.
--
--    The "DATE-TIME" value type expresses time values in three forms:
--
--    The form of date and time with UTC offset MUST NOT be used.  For
--    example, the following is not valid for a DATE-TIME value:
--
--     19980119T230000-0800       ;Invalid time format
--
--    FORM #1: DATE WITH LOCAL TIME
--
--    The date with local time form is simply a DATE-TIME value that
--    does not contain the UTC designator nor does it reference a time
--    zone.  For example, the following represents January 18, 1998, at
--    11 PM:
--
--     19980118T230000
--
--    DATE-TIME values of this type are said to be "floating" and are
--    not bound to any time zone in particular.  They are used to
--    represent the same hour, minute, and second value regardless of
--    which time zone is currently being observed.  For example, an
--    event can be defined that indicates that an individual will be
--    busy from 11:00 AM to 1:00 PM every day, no matter which time zone
--    the person is in.  In these cases, a local time can be specified.
--    The recipient of an iCalendar object with a property value
--    consisting of a local time, without any relative time zone
--    information, SHOULD interpret the value as being fixed to whatever
--    time zone the "ATTENDEE" is in at any given moment.  This means
--    that two "Attendees", in different time zones, receiving the same
--    event definition as a floating time, may be participating in the
--    event at different actual times.  Floating time SHOULD only be
--    used where that is the reasonable behavior.
--
--    In most cases, a fixed time is desired.  To properly communicate a
--    fixed time in a property value, either UTC time or local time with
--    time zone reference MUST be specified.
--
--    The use of local time in a DATE-TIME value without the "TZID"
--    property parameter is to be interpreted as floating time,
--    regardless of the existence of "VTIMEZONE" calendar components in
--    the iCalendar object.
--
--    FORM #2: DATE WITH UTC TIME
--
--    The date with UTC time, or absolute time, is identified by a LATIN
--    CAPITAL LETTER Z suffix character, the UTC designator, appended to
--    the time value.  For example, the following represents January 19,
--    1998, at 0700 UTC:
--
--     19980119T070000Z
--
--    The "TZID" property parameter MUST NOT be applied to DATE-TIME
--    properties whose time values are specified in UTC.
--
--    FORM #3: DATE WITH LOCAL TIME AND TIME ZONE REFERENCE
--
--    The date and local time with reference to time zone information is
--    identified by the use the "TZID" property parameter to reference
--    the appropriate time zone definition.  "TZID" is discussed in
--    detail in Section 3.2.19.  For example, the following represents
--    2:00 A.M. in New York on January 19, 1998:
--
--     TZID=America/New_York:19980119T020000
--
--    If, based on the definition of the referenced time zone, the local
--    time described occurs more than once (when changing from daylight
--    to standard time), the DATE-TIME value refers to the first
--    occurrence of the referenced time.  Thus, TZID=America/
--    New_York:20071104T013000 indicates November 4, 2007 at 1:30 A.M.
--    EDT (UTC-04:00).  If the local time described does not occur (when
--    changing from standard to daylight time), the DATE-TIME value is
--    interpreted using the UTC offset before the gap in local times.
--    Thus, TZID=America/New_York:20070311T023000 indicates March 11,
--    2007 at 3:30 A.M. EDT (UTC-04:00), one hour after 1:30 A.M. EST
--    (UTC-05:00).
--
--
--    A time value MUST only specify the second 60 when specifying a
--    positive leap second.  For example:
--
--     19970630T235960Z
--
--    Implementations that do not support leap seconds SHOULD interpret
--    the second 60 as equivalent to the second 59.
--
-- Example:  The following represents July 14, 1997, at 1:30 PM in New
--    York City in each of the three time formats, using the "DTSTART"
--    property.
--
--     DTSTART:19970714T133000                   ; Local time
--     DTSTART:19970714T173000Z                  ; UTC time
--     DTSTART;TZID=America/New_York:19970714T133000
--                                               ; Local time and time
--                                               ; zone reference
-- @
--
--
-- TODO figure out if we can adhere to this SHOULD:
-- @
--    The recipient of an iCalendar object with a property value
--    consisting of a local time, without any relative time zone
--    information, SHOULD interpret the value as being fixed to whatever
--    time zone the "ATTENDEE" is in at any given moment.  This means
-- @
data DateTime
  = -- |
    -- @
    -- FORM #1: DATE WITH LOCAL TIME
    -- @
    DateTimeFloating !Time.LocalTime
  | -- |
    -- @
    -- FORM #2: DATE WITH UTC TIME
    -- @
    -- TODO: Consider using a LocalTime here too.
    DateTimeUTC !Time.UTCTime
  | -- |
    -- @
    -- FORM #3: DATE WITH LOCAL TIME AND TIME ZONE REFERENCE
    -- @
    DateTimeZoned !TZIDParam !Time.LocalTime -- TODO make this a timezoneID?
  deriving (Eq, Ord, Generic)

instance Validity DateTime where
  validate dt =
    mconcat
      [ genericValidate dt,
        case dt of
          DateTimeFloating l -> validateImpreciseLocalTime l
          DateTimeUTC u -> validateImpreciseUTCTime u
          DateTimeZoned _ l -> validateImpreciseLocalTime l
      ]

instance Show DateTime where
  showsPrec d =
    showParen (d > 10) . \case
      DateTimeFloating l -> showString "DateTimeFloating " . localTimeShowsPrec 11 l
      DateTimeUTC u -> showString "DateTimeUTC " . utcTimeShowsPrec 11 u
      DateTimeZoned tzid l -> showString "DateTimeZoned " . showsPrec 11 tzid . showString " " . localTimeShowsPrec 11 l

localTimeShowsPrec :: Int -> Time.LocalTime -> ShowS
localTimeShowsPrec d (Time.LocalTime day timeOfDay) =
  showParen (d > 10) $
    showString "LocalTime "
      . dayShowsPrec 11 day
      . showString " "
      . timeOfDayShowsPrec 11 timeOfDay

utcTimeShowsPrec :: Int -> Time.UTCTime -> ShowS
utcTimeShowsPrec d (Time.UTCTime day diffTime) =
  showParen (d > 10) $
    showString "UTCTime "
      . dayShowsPrec 11 day
      . showString " "
      . diffTimeShowsPrec 11 diffTime

diffTimeShowsPrec :: Int -> Time.DiffTime -> ShowS
diffTimeShowsPrec d diffTime =
  showParen (d > 10) $
    showString "timeOfDayToTime "
      . timeOfDayShowsPrec 11 (Time.timeToTimeOfDay diffTime)

instance NFData DateTime

instance IsPropertyType DateTime where
  propertyTypeValueType Proxy = TypeDateTime
  propertyTypeP = dateTimeP
  propertyTypeB = dateTimeB

-- | Get the date of a datetime
--
-- WARNING: This gets the specified date in the context that it is specified.
-- The resulting date may have been different in a different timezone.
--
-- TODO: show this with a test case
dateTimeDate :: DateTime -> Date
dateTimeDate =
  Date . \case
    DateTimeFloating lt -> Time.localDay lt
    DateTimeUTC ut -> Time.utctDay ut
    -- This is where the warning is relevant
    DateTimeZoned _ lt -> Time.localDay lt

dateTimeP :: ContentLineValue -> Conform PropertyTypeParseError Void Void DateTime
dateTimeP clv@ContentLineValue {..} = do
  let s = T.unpack contentLineValueRaw
  case lookupParam contentLineValueParams of
    Nothing ->
      (DateTimeUTC <$> dateTimeUTCP clv)
        `altConform` (DateTimeFloating <$> dateTimeFloatingP clv)
    Just conformTzid ->
      DateTimeZoned
        <$> conformMapError ParameterParseError conformTzid
        <*> parseTimeStr dateTimeZonedFormatStr s

dateTimeFloatingP :: ContentLineValue -> Conform PropertyTypeParseError Void Void Time.LocalTime
dateTimeFloatingP = parseDateTimeFloating . contentLineValueRaw

dateTimeUTCP :: ContentLineValue -> Conform PropertyTypeParseError Void Void Time.UTCTime
dateTimeUTCP = parseDateTimeUTC . contentLineValueRaw

dateTimeB :: DateTime -> ContentLineValue
dateTimeB = \case
  DateTimeFloating lt -> dateTimeFloatingB lt
  DateTimeUTC lt -> dateTimeUTCB lt
  DateTimeZoned tzidParam lt -> dateTimeZonedB tzidParam lt

dateTimeFloatingB :: Time.LocalTime -> ContentLineValue
dateTimeFloatingB = mkSimpleContentLineValue . renderDateTimeFloating

dateTimeUTCB :: Time.UTCTime -> ContentLineValue
dateTimeUTCB = mkSimpleContentLineValue . renderDateTimeUTC

dateTimeZonedB :: TZIDParam -> Time.LocalTime -> ContentLineValue
dateTimeZonedB tzidParam lt =
  ContentLineValue
    { contentLineValueParams = M.singleton (parameterName (proxyOf tzidParam)) (parameterB tzidParam),
      contentLineValueRaw = T.pack $ Time.formatTime Time.defaultTimeLocale dateTimeZonedFormatStr lt
    }

parseDateTimeFloating :: Text -> Conform PropertyTypeParseError Void Void Time.LocalTime
parseDateTimeFloating = parseTimeStr dateTimeFloatingFormatStr . T.unpack

renderDateTimeFloating :: Time.LocalTime -> Text
renderDateTimeFloating = T.pack . Time.formatTime Time.defaultTimeLocale dateTimeFloatingFormatStr

dateTimeFloatingFormatStr :: String
dateTimeFloatingFormatStr = "%Y%m%dT%H%M%S"

parseDateTimeUTC :: Text -> Conform PropertyTypeParseError Void Void Time.UTCTime
parseDateTimeUTC = parseTimeStr dateTimeUTCFormatStr . T.unpack

renderDateTimeUTC :: Time.UTCTime -> Text
renderDateTimeUTC = T.pack . Time.formatTime Time.defaultTimeLocale dateTimeUTCFormatStr

dateTimeUTCFormatStr :: String
dateTimeUTCFormatStr = "%Y%m%dT%H%M%SZ"

dateTimeZonedFormatStr :: String
dateTimeZonedFormatStr = "%Y%m%dT%H%M%S"
