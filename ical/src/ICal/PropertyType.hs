{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.PropertyType where

import qualified Data.Map as M
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import GHC.Generics (Generic)
import ICal.ContentLine
import ICal.Parameter
import Text.Megaparsec

-- | Property type
--
-- === [section 3.3](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3)
--
-- @
--     The properties in an iCalendar object are strongly typed.  The
--     definition of each property restricts the value to be one of the
--     value data types, or simply value types, defined in this section.
--     The value type for a property will either be specified implicitly as
--     the default value type or will be explicitly specified with the
--     "VALUE" parameter.  If the value type of a property is one of the
--     alternate valid types, then it MUST be explicitly specified with the
--     "VALUE" parameter.
-- @
--
-- === Laws
--
-- * The property roundtrips through 'ContentLineValue'.
--
-- >>> forAllValid $ \property -> propertyTypeP (propertyTypeB property) == Right property
class IsPropertyType propertyType where
  -- | Parser for the property type
  propertyTypeP :: ContentLineValue -> Either String propertyType

  -- | Builder for the property type
  propertyTypeB :: propertyType -> ContentLineValue

-- | Text
--
-- === [section 3.3.11](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.11)
--
-- @
--     Value Name:  TEXT
--
--     Purpose:  This value type is used to identify values that contain
--        human-readable text.
--
--     Format Definition:  This value type is defined by the following
--        notation:
--         text       = *(TSAFE-CHAR / ":" / DQUOTE / ESCAPED-CHAR)
--            ; Folded according to description above
--
--         ESCAPED-CHAR = ("\\" / "\;" / "\," / "\N" / "\n")
--            ; \\ encodes \, \N or \n encodes newline
--            ; \; encodes ;, \, encodes ,
--
--         TSAFE-CHAR = WSP / %x21 / %x23-2B / %x2D-39 / %x3C-5B /
--                      %x5D-7E / NON-US-ASCII
--            ; Any character except CONTROLs not needed by the current
--            ; character set, DQUOTE, ";", ":", "\", ","
--
--     Description:  If the property permits, multiple TEXT values are
--        specified by a COMMA-separated list of values.
--
--        The language in which the text is represented can be controlled by
--        the "LANGUAGE" property parameter.
--
--        An intentional formatted text line break MUST only be included in
--        a "TEXT" property value by representing the line break with the
--        character sequence of BACKSLASH, followed by a LATIN SMALL LETTER
--        N or a LATIN CAPITAL LETTER N, that is "\n" or "\N".
--
--        The "TEXT" property values may also contain special characters
--        that are used to signify delimiters, such as a COMMA character for
--        lists of values or a SEMICOLON character for structured values.
--        In order to support the inclusion of these special characters in
--        "TEXT" property values, they MUST be escaped with a BACKSLASH
--        character.  A BACKSLASH character in a "TEXT" property value MUST
--        be escaped with another BACKSLASH character.  A COMMA character in
--        a "TEXT" property value MUST be escaped with a BACKSLASH
--        character.  A SEMICOLON character in a "TEXT" property value MUST
--        be escaped with a BACKSLASH character.  However, a COLON character
--        in a "TEXT" property value SHALL NOT be escaped with a BACKSLASH
--        character.
--
--     Example:  A multiple line value of:
--
--         Project XYZ Final Review
--         Conference Room - 3B
--         Come Prepared.
--
--        would be represented as:
--
--         Project XYZ Final Review\nConference Room - 3B\nCome Prepared.
-- @
instance IsPropertyType Text where
  propertyTypeP = Right . unEscapeText . contentLineValueRaw
  propertyTypeB = mkSimpleContentLineValue . escapeText

-- | Escape 'Text'
--
-- FIXME this could probably go a LOT faster
-- @
--     ; \\ encodes \, \N or \n encodes newline
--     ; \; encodes ;, \, encodes ,
-- @
escapeText :: Text -> Text
escapeText =
  T.replace "\n" "\\n"
    . T.replace "," "\\,"
    . T.replace ";" "\\;"
    . T.replace "\\" "\\\\"

-- | Un-Escape 'Text'
--
-- FIXME this could probably go a LOT faster
-- @
--     ; \\ encodes \, \N or \n encodes newline
--     ; \; encodes ;, \, encodes ,
-- @
unEscapeText :: Text -> Text
unEscapeText =
  T.replace "\\\\" "\\"
    . T.replace "\\," ","
    . T.replace "\\;" ";"
    . T.replace "\\n" "\n"
    . T.replace "\\N" "\n"

-- | Date Time
--
-- === [section 3.3.5](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.5)
--
-- @
--     Value Name:  DATE-TIME
--
--     Purpose:  This value type is used to identify values that specify a
--        precise calendar date and time of day.
--
--     Format Definition:  This value type is defined by the following
--        notation:
--
--         date-time  = date "T" time ;As specified in the DATE and TIME
--                                    ;value definitions
--
--     Description:  If the property permits, multiple "DATE-TIME" values
--        are specified as a COMMA-separated list of values.  No additional
--        content value encoding (i.e., BACKSLASH character encoding, see
--        Section 3.3.11) is defined for this value type.
--
--        The "DATE-TIME" value type is used to identify values that contain
--        a precise calendar date and time of day.  The format is based on
--        the [ISO.8601.2004] complete representation, basic format for a
--        calendar date and time of day.  The text format is a concatenation
--        of the "date", followed by the LATIN CAPITAL LETTER T character,
--        the time designator, followed by the "time" format.
--
--        The "DATE-TIME" value type expresses time values in three forms:
--
--        The form of date and time with UTC offset MUST NOT be used.  For
--        example, the following is not valid for a DATE-TIME value:
--
--         19980119T230000-0800       ;Invalid time format
--
--        FORM #1: DATE WITH LOCAL TIME
--
--        The date with local time form is simply a DATE-TIME value that
--        does not contain the UTC designator nor does it reference a time
--        zone.  For example, the following represents January 18, 1998, at
--        11 PM:
--
--         19980118T230000
--
--        DATE-TIME values of this type are said to be "floating" and are
--        not bound to any time zone in particular.  They are used to
--        represent the same hour, minute, and second value regardless of
--        which time zone is currently being observed.  For example, an
--        event can be defined that indicates that an individual will be
--        busy from 11:00 AM to 1:00 PM every day, no matter which time zone
--        the person is in.  In these cases, a local time can be specified.
--        The recipient of an iCalendar object with a property value
--        consisting of a local time, without any relative time zone
--        information, SHOULD interpret the value as being fixed to whatever
--        time zone the "ATTENDEE" is in at any given moment.  This means
--        that two "Attendees", in different time zones, receiving the same
--        event definition as a floating time, may be participating in the
--        event at different actual times.  Floating time SHOULD only be
--        used where that is the reasonable behavior.
--
--        In most cases, a fixed time is desired.  To properly communicate a
--        fixed time in a property value, either UTC time or local time with
--        time zone reference MUST be specified.
--
--        The use of local time in a DATE-TIME value without the "TZID"
--        property parameter is to be interpreted as floating time,
--        regardless of the existence of "VTIMEZONE" calendar components in
--        the iCalendar object.
--
--        FORM #2: DATE WITH UTC TIME
--
--        The date with UTC time, or absolute time, is identified by a LATIN
--        CAPITAL LETTER Z suffix character, the UTC designator, appended to
--        the time value.  For example, the following represents January 19,
--        1998, at 0700 UTC:
--
--         19980119T070000Z
--
--        The "TZID" property parameter MUST NOT be applied to DATE-TIME
--        properties whose time values are specified in UTC.
--
--        FORM #3: DATE WITH LOCAL TIME AND TIME ZONE REFERENCE
--
--        The date and local time with reference to time zone information is
--        identified by the use the "TZID" property parameter to reference
--        the appropriate time zone definition.  "TZID" is discussed in
--        detail in Section 3.2.19.  For example, the following represents
--        2:00 A.M. in New York on January 19, 1998:
--
--         TZID=America/New_York:19980119T020000
--
--        If, based on the definition of the referenced time zone, the local
--        time described occurs more than once (when changing from daylight
--        to standard time), the DATE-TIME value refers to the first
--        occurrence of the referenced time.  Thus, TZID=America/
--        New_York:20071104T013000 indicates November 4, 2007 at 1:30 A.M.
--        EDT (UTC-04:00).  If the local time described does not occur (when
--        changing from standard to daylight time), the DATE-TIME value is
--        interpreted using the UTC offset before the gap in local times.
--        Thus, TZID=America/New_York:20070311T023000 indicates March 11,
--        2007 at 3:30 A.M. EDT (UTC-04:00), one hour after 1:30 A.M. EST
--        (UTC-05:00).
--
--
--        A time value MUST only specify the second 60 when specifying a
--        positive leap second.  For example:
--
--         19970630T235960Z
--
--        Implementations that do not support leap seconds SHOULD interpret
--        the second 60 as equivalent to the second 59.
--
--     Example:  The following represents July 14, 1997, at 1:30 PM in New
--        York City in each of the three time formats, using the "DTSTART"
--        property.
--
--         DTSTART:19970714T133000                   ; Local time
--         DTSTART:19970714T173000Z                  ; UTC time
--         DTSTART;TZID=America/New_York:19970714T133000
--                                                   ; Local time and time
--                                                   ; zone reference
-- @
data DateTime
  = -- |
    -- @
    --     FORM #1: DATE WITH LOCAL TIME
    -- @
    DateTimeFloating !Time.LocalTime
  | -- |
    -- @
    --     FORM #2: DATE WITH UTC TIME
    -- @
    DateTimeUTC !Time.LocalTime
  | -- |
    -- @
    --     FORM #3: DATE WITH LOCAL TIME AND TIME ZONE REFERENCE
    -- @
    DateTimeZoned !TZIDParam !Time.LocalTime -- TODO make this a timezoneID?
  deriving (Show, Eq, Generic)

instance Validity DateTime where
  validate dt =
    mconcat
      [ genericValidate dt,
        let lt = case dt of
              DateTimeFloating l -> l
              DateTimeUTC l -> l
              DateTimeZoned _ l -> l
         in validateImpreciseLocalTime lt
      ]

validateImpreciseLocalTime :: Time.LocalTime -> Validation
validateImpreciseLocalTime lt =
  let tod = Time.localTimeOfDay lt
   in validateImpreciseTimeOfDay tod

instance IsPropertyType DateTime where
  propertyTypeP = dateTimeP
  propertyTypeB = dateTimeB

dateTimeP :: ContentLineValue -> Either String DateTime
dateTimeP clv@ContentLineValue {..} =
  let s = T.unpack contentLineValueRaw
   in case lookupParam contentLineValueParams of
        Just errOrTZID -> DateTimeZoned <$> errOrTZID <*> parseTimeEither dateTimeZonedFormatStr s
        _ ->
          (DateTimeFloating <$> dateTimeFloatingP clv)
            <|> (DateTimeUTC <$> dateTimeUTCP clv)

dateTimeFloatingP :: ContentLineValue -> Either String Time.LocalTime
dateTimeFloatingP ContentLineValue {..} =
  let s = T.unpack contentLineValueRaw
   in parseTimeEither dateTimeFloatingFormatStr s

dateTimeUTCP :: ContentLineValue -> Either String Time.LocalTime
dateTimeUTCP ContentLineValue {..} =
  let s = T.unpack contentLineValueRaw
   in parseTimeEither dateTimeUTCFormatStr s

dateTimeB :: DateTime -> ContentLineValue
dateTimeB =
  \case
    DateTimeFloating lt -> dateTimeFloatingB lt
    DateTimeUTC lt -> dateTimeUTCB lt
    DateTimeZoned tzidParam lt -> dateTimeZonedB tzidParam lt

dateTimeFloatingB :: Time.LocalTime -> ContentLineValue
dateTimeFloatingB = mkSimpleContentLineValue . T.pack . Time.formatTime Time.defaultTimeLocale dateTimeFloatingFormatStr

dateTimeUTCB :: Time.LocalTime -> ContentLineValue
dateTimeUTCB = mkSimpleContentLineValue . T.pack . Time.formatTime Time.defaultTimeLocale dateTimeUTCFormatStr

dateTimeZonedB :: TZIDParam -> Time.LocalTime -> ContentLineValue
dateTimeZonedB tzidParam lt =
  ContentLineValue
    { contentLineValueParams = M.singleton (parameterName (proxyOf tzidParam)) (parameterB tzidParam),
      contentLineValueRaw = T.pack $ Time.formatTime Time.defaultTimeLocale dateTimeZonedFormatStr lt
    }

dateTimeFloatingFormatStr :: String
dateTimeFloatingFormatStr = "%Y%m%dT%H%M%S"

dateTimeUTCFormatStr :: String
dateTimeUTCFormatStr = "%Y%m%dT%H%M%SZ"

dateTimeZonedFormatStr :: String
dateTimeZonedFormatStr = "%Y%m%dT%H%M%S"

-- | Date
--
-- === [section 3.3.4](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.4)
--
-- @
--     Value Name:  DATE
--
--     Purpose:  This value type is used to identify values that contain a
--        calendar date.
--
--     Format Definition:  This value type is defined by the following
--        notation:
--
--         date               = date-value
--
--         date-value         = date-fullyear date-month date-mday
--         date-fullyear      = 4DIGIT
--         date-month         = 2DIGIT        ;01-12
--         date-mday          = 2DIGIT        ;01-28, 01-29, 01-30, 01-31
--                                            ;based on month/year
--
--     Description:  If the property permits, multiple "date" values are
--        specified as a COMMA-separated list of values.  The format for the
--        value type is based on the [ISO.8601.2004] complete
--        representation, basic format for a calendar date.  The textual
--        format specifies a four-digit year, two-digit month, and two-digit
--        day of the month.  There are no separator characters between the
--        year, month, and day component text.
--
--        No additional content value encoding (i.e., BACKSLASH character
--        encoding, see Section 3.3.11) is defined for this value type.
--
--     Example:  The following represents July 14, 1997:
--
--         19970714
-- @
newtype Date = Date {unDate :: Time.Day}
  deriving (Show, Eq, Generic)

instance Validity Date

instance IsPropertyType Date where
  propertyTypeP = dateP
  propertyTypeB = dateB

dateP :: ContentLineValue -> Either String Date
dateP = fmap Date . parseTimeEither dateFormatStr . T.unpack . contentLineValueRaw

dateB :: Date -> ContentLineValue
dateB = mkSimpleContentLineValue . T.pack . Time.formatTime Time.defaultTimeLocale dateFormatStr . unDate

dateFormatStr :: String
dateFormatStr = "%Y%m%d"

-- | Time
--
-- === [section 3.3.12](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.12)
--
-- @
--     Value Name:  TIME
--
--     Purpose:  This value type is used to identify values that contain a
--        time of day.
--
--     Format Definition:  This value type is defined by the following
--        notation:
--
--         time         = time-hour time-minute time-second [time-utc]
--
--         time-hour    = 2DIGIT        ;00-23
--         time-minute  = 2DIGIT        ;00-59
--         time-second  = 2DIGIT        ;00-60
--         ;The "60" value is used to account for positive "leap" seconds.
--
--         time-utc     = "Z"
--
--     Description:  If the property permits, multiple "time" values are
--        specified by a COMMA-separated list of values.  No additional
--        content value encoding (i.e., BACKSLASH character encoding, see
--        Section 3.3.11) is defined for this value type.
--
--        The "TIME" value type is used to identify values that contain a
--        time of day.  The format is based on the [ISO.8601.2004] complete
--        representation, basic format for a time of day.  The text format
--        consists of a two-digit, 24-hour of the day (i.e., values 00-23),
--        two-digit minute in the hour (i.e., values 00-59), and two-digit
--        seconds in the minute (i.e., values 00-60).  The seconds value of
--        60 MUST only be used to account for positive "leap" seconds.
--        Fractions of a second are not supported by this format.
--
--        In parallel to the "DATE-TIME" definition above, the "TIME" value
--        type expresses time values in three forms:
--
--        The form of time with UTC offset MUST NOT be used.  For example,
--        the following is not valid for a time value:
--
--         230000-0800        ;Invalid time format
--
--        FORM #1 LOCAL TIME
--
--        The local time form is simply a time value that does not contain
--        the UTC designator nor does it reference a time zone.  For
--        example, 11:00 PM:
--
--         230000
--        Time values of this type are said to be "floating" and are not
--        bound to any time zone in particular.  They are used to represent
--        the same hour, minute, and second value regardless of which time
--        zone is currently being observed.  For example, an event can be
--        defined that indicates that an individual will be busy from 11:00
--        AM to 1:00 PM every day, no matter which time zone the person is
--        in.  In these cases, a local time can be specified.  The recipient
--        of an iCalendar object with a property value consisting of a local
--        time, without any relative time zone information, SHOULD interpret
--        the value as being fixed to whatever time zone the "ATTENDEE" is
--        in at any given moment.  This means that two "Attendees", may
--        participate in the same event at different UTC times; floating
--        time SHOULD only be used where that is reasonable behavior.
--
--        In most cases, a fixed time is desired.  To properly communicate a
--        fixed time in a property value, either UTC time or local time with
--        time zone reference MUST be specified.
--
--        The use of local time in a TIME value without the "TZID" property
--        parameter is to be interpreted as floating time, regardless of the
--        existence of "VTIMEZONE" calendar components in the iCalendar
--        object.
--
--        FORM #2: UTC TIME
--
--        UTC time, or absolute time, is identified by a LATIN CAPITAL
--        LETTER Z suffix character, the UTC designator, appended to the
--        time value.  For example, the following represents 07:00 AM UTC:
--
--         070000Z
--
--        The "TZID" property parameter MUST NOT be applied to TIME
--        properties whose time values are specified in UTC.
--
--        FORM #3: LOCAL TIME AND TIME ZONE REFERENCE
--
--        The local time with reference to time zone information form is
--        identified by the use the "TZID" property parameter to reference
--        the appropriate time zone definition.  "TZID" is discussed in
--        detail in Section 3.2.19.
--
--     Example:  The following represents 8:30 AM in New York in winter,
--        five hours behind UTC, in each of the three formats:
--
--         083000
--         133000Z
--         TZID=America/New_York:083000
-- @
data Time
  = -- |
    -- @
    --     FORM #1 LOCAL TIME
    -- @
    TimeFloating !Time.TimeOfDay
  | -- |
    -- @
    --     FORM #2: UTC TIME
    -- @
    TimeUTC !Time.TimeOfDay
  | -- |
    -- @
    --     FORM #3: LOCAL TIME AND TIME ZONE REFERENCE
    -- @
    TimeZoned !TZIDParam !Time.TimeOfDay
  deriving (Show, Eq, Generic)

instance Validity Time where
  validate time =
    mconcat
      [ genericValidate time,
        let tod = case time of
              TimeFloating t -> t
              TimeUTC t -> t
              TimeZoned _ t -> t
         in validateImpreciseTimeOfDay tod
      ]

validateImpreciseTimeOfDay :: Time.TimeOfDay -> Validation
validateImpreciseTimeOfDay tod =
  declare "The number of seconds is integer" $
    let sec = Time.todSec tod
     in ceiling sec == (floor sec :: Int)

instance IsPropertyType Time where
  propertyTypeP = timeP
  propertyTypeB = timeB

timeP :: ContentLineValue -> Either String Time
timeP ContentLineValue {..} =
  let s = T.unpack contentLineValueRaw
   in case lookupParam contentLineValueParams of
        Nothing ->
          (TimeFloating <$> parseTimeEither timeFloatingFormatStr s)
            <|> (TimeUTC <$> parseTimeEither timeUTCFormatStr s)
        Just errOrTZID -> TimeZoned <$> errOrTZID <*> parseTimeEither timeZonedFormatStr s

timeB :: Time -> ContentLineValue
timeB =
  \case
    TimeFloating tod -> mkSimpleContentLineValue $ T.pack $ Time.formatTime Time.defaultTimeLocale timeFloatingFormatStr tod
    TimeUTC tod -> mkSimpleContentLineValue $ T.pack $ Time.formatTime Time.defaultTimeLocale timeUTCFormatStr tod
    TimeZoned tzidParam tod ->
      ContentLineValue
        { contentLineValueParams = M.singleton (parameterName (proxyOf tzidParam)) (tzIDParamB tzidParam),
          contentLineValueRaw = T.pack $ Time.formatTime Time.defaultTimeLocale timeZonedFormatStr tod
        }

proxyOf :: a -> Proxy a
proxyOf _ = Proxy

parseTimeEither :: Time.ParseTime t => String -> String -> Either String t
parseTimeEither formatStr s = case Time.parseTimeM True Time.defaultTimeLocale formatStr s of
  Nothing -> Left $ "Could not parse time value: " <> s
  Just t -> Right t

timeFloatingFormatStr :: String
timeFloatingFormatStr = "%H%M%S"

timeUTCFormatStr :: String
timeUTCFormatStr = "%H%M%SZ"

timeZonedFormatStr :: String
timeZonedFormatStr = "%H%M%S"
