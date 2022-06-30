{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Property where

import Control.Applicative
import Data.Proxy
import Data.Text (Text)
import qualified Data.Time as Time
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import GHC.Generics (Generic)
import ICal.ContentLine
import ICal.PropertyType.Class
import ICal.PropertyType.Date
import ICal.PropertyType.DateTime

-- |
--
-- === [section 3.7](https://datatracker.ietf.org/doc/html/rfc5545#section-3.7)
--
-- @
--     The Calendar Properties are attributes that apply to the iCalendar
--     object, as a whole.  These properties do not appear within a calendar
--     component.  They SHOULD be specified after the "BEGIN:VCALENDAR"
--     delimiter string and prior to any calendar component.
-- @
--
-- === Laws
--
-- * The 'ContentLineValue' that is built is valid:
--
-- >>> forAllValid $ \property -> isValid (propertyB property)
--
-- * Anything parsed is valid:
--
-- >>> forAllValid $ \contentLineValue -> isValid (propertyP contentlineValue)
--
-- * The property roundtrips through 'ContentLineValue'.
--
-- >>> forAllValid $ \property -> propertyP (propertyB property) == Right property
class IsProperty property where
  -- Name of the property
  propertyName :: Proxy property -> ContentLineName

  -- | Parser for the property
  propertyP :: ContentLineValue -> Either String property

  -- | Builder for the property
  propertyB :: property -> ContentLineValue

propertyContentLineP ::
  forall property.
  IsProperty property =>
  ContentLine ->
  Either String property
propertyContentLineP ContentLine {..} =
  let name = propertyName (Proxy :: Proxy property)
   in if contentLineName == name
        then propertyP contentLineValue
        else
          Left $
            unwords
              [ "Expected content line with name",
                show name,
                "but got",
                show contentLineName,
                "instead."
              ]

propertyContentLineB :: forall property. IsProperty property => property -> ContentLine
propertyContentLineB = ContentLine (propertyName (Proxy :: Proxy property)) . propertyB

newtype Begin = Begin {unBegin :: Text}
  deriving (Show, Eq, Generic)

instance Validity Begin

instance IsProperty Begin where
  propertyName Proxy = "BEGIN"
  propertyP = fmap Begin . propertyTypeP
  propertyB = propertyTypeB . unBegin

newtype End = End {unEnd :: Text}
  deriving (Show, Eq, Generic)

instance Validity End

instance IsProperty End where
  propertyName Proxy = "END"
  propertyP = fmap End . propertyTypeP
  propertyB = propertyTypeB . unEnd

-- |
--
-- === [section 3.7.3](https://datatracker.ietf.org/doc/html/rfc5545#section-3.7.3)
--
-- @
--     Property Name:  PRODID
--
--     Purpose:  This property specifies the identifier for the product that
--        created the iCalendar object.
--
--     Value Type:  TEXT
--
--     Property Parameters:  IANA and non-standard property parameters can
--        be specified on this property.
--
--     Conformance:  The property MUST be specified once in an iCalendar
--        object.
--
--     Description:  The vendor of the implementation SHOULD assure that
--        this is a globally unique identifier; using some technique such as
--        an FPI value, as defined in [ISO.9070.1991].
--
--        This property SHOULD NOT be used to alter the interpretation of an
--        iCalendar object beyond the semantics specified in this memo.  For
--        example, it is not to be used to further the understanding of non-
--        standard properties.
--
--     Format Definition:  This property is defined by the following
--        notation:
--
--         prodid     = "PRODID" pidparam ":" pidvalue CRLF
--
--         pidparam   = *(";" other-param)
--
--         pidvalue   = text
--         ;Any text that describes the product and version
--         ;and that is generally assured of being unique.
--
--     Example:  The following is an example of this property.  It does not
--        imply that English is the default language.
--
--         PRODID:-//ABC Corporation//NONSGML My Product//EN
-- @
newtype ProdId = ProdId {unProdId :: Text}
  deriving (Show, Eq, Generic)

instance Validity ProdId

instance IsProperty ProdId where
  propertyName Proxy = "PRODID"
  propertyP = fmap ProdId . propertyTypeP
  propertyB = propertyTypeB . unProdId

-- |
--
-- === [section 3.7.4](https://datatracker.ietf.org/doc/html/rfc5545#section-3.7.4)
--
-- @
--     Property Name:  VERSION
--
--     Purpose:  This property specifies the identifier corresponding to the
--        highest version number or the minimum and maximum range of the
--        iCalendar specification that is required in order to interpret the
--        iCalendar object.
--
--     Value Type:  TEXT
--
--     Property Parameters:  IANA and non-standard property parameters can
--        be specified on this property.
--
--     Conformance:  This property MUST be specified once in an iCalendar
--        object.
--
--     Description:  A value of "2.0" corresponds to this memo.
--
--     Format Definition:  This property is defined by the following
--        notation:
--
--         version    = "VERSION" verparam ":" vervalue CRLF
--
--         verparam   = *(";" other-param)
--
--         vervalue   = "2.0"         ;This memo
--                    / maxver
--                    / (minver ";" maxver)
--
--         minver     = <A IANA-registered iCalendar version identifier>
--         ;Minimum iCalendar version needed to parse the iCalendar object.
--
--         maxver     = <A IANA-registered iCalendar version identifier>
--         ;Maximum iCalendar version needed to parse the iCalendar object.
--
--     Example:  The following is an example of this property:
--
--         VERSION:2.0
-- @
newtype Version = Version {unVersion :: Text}
  deriving (Show, Eq, Generic)

instance Validity Version

instance IsProperty Version where
  propertyName Proxy = "VERSION"
  propertyP = versionP
  propertyB = versionB

versionP :: ContentLineValue -> Either String Version
versionP = fmap Version . propertyTypeP

versionB :: Version -> ContentLineValue
versionB = propertyTypeB . unVersion

-- |
--
-- === [section 3.8.4.7](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.4.7)
--
-- @
--     Property Name:  UID
--
--     Purpose:  This property defines the persistent, globally unique
--        identifier for the calendar component.
--
--     Value Type:  TEXT
--
--     Property Parameters:  IANA and non-standard property parameters can
--        be specified on this property.
--
--     Conformance:  The property MUST be specified in the "VEVENT",
--        "VTODO", "VJOURNAL", or "VFREEBUSY" calendar components.
--
--     Description:  The "UID" itself MUST be a globally unique identifier.
--        The generator of the identifier MUST guarantee that the identifier
--        is unique.  There are several algorithms that can be used to
--        accomplish this.  A good method to assure uniqueness is to put the
--        domain name or a domain literal IP address of the host on which
--        the identifier was created on the right-hand side of an "@", and
--        on the left-hand side, put a combination of the current calendar
--        date and time of day (i.e., formatted in as a DATE-TIME value)
--        along with some other currently unique (perhaps sequential)
--        identifier available on the system (for example, a process id
--        number).  Using a DATE-TIME value on the left-hand side and a
--        domain name or domain literal on the right-hand side makes it
--        possible to guarantee uniqueness since no two hosts should be
--        using the same domain name or IP address at the same time.  Though
--        other algorithms will work, it is RECOMMENDED that the right-hand
--        side contain some domain identifier (either of the host itself or
--        otherwise) such that the generator of the message identifier can
--        guarantee the uniqueness of the left-hand side within the scope of
--        that domain.
--
--        This is the method for correlating scheduling messages with the
--        referenced "VEVENT", "VTODO", or "VJOURNAL" calendar component.
--        The full range of calendar components specified by a recurrence
--        set is referenced by referring to just the "UID" property value
--        corresponding to the calendar component.  The "RECURRENCE-ID"
--        property allows the reference to an individual instance within the
--        recurrence set.
--
--        This property is an important method for group-scheduling
--        applications to match requests with later replies, modifications,
--        or deletion requests.  Calendaring and scheduling applications
--        MUST generate this property in "VEVENT", "VTODO", and "VJOURNAL"
--        calendar components to assure interoperability with other group-
--        scheduling applications.  This identifier is created by the
--        calendar system that generates an iCalendar object.
--
--        Implementations MUST be able to receive and persist values of at
--        least 255 octets for this property, but they MUST NOT truncate
--        values in the middle of a UTF-8 multi-octet sequence.
--
--     Format Definition:  This property is defined by the following
--        notation:
--
--         uid        = "UID" uidparam ":" text CRLF
--
--         uidparam   = *(";" other-param)
--
--     Example:  The following is an example of this property:
--
--         UID:19960401T080045Z-4000F192713-0052@example.com
-- @
newtype UID = UID {unUID :: Text}
  deriving (Show, Eq, Generic)

instance Validity UID

instance IsProperty UID where
  propertyName Proxy = "UID"
  propertyP = fmap UID . propertyTypeP
  propertyB = propertyTypeB . unUID

-- |
--
-- === [section 3.8.7.2](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.7.2)
--
-- @
--     Property Name:  DTSTAMP
--
--     Purpose:  In the case of an iCalendar object that specifies a
--        "METHOD" property, this property specifies the date and time that
--        the instance of the iCalendar object was created.  In the case of
--        an iCalendar object that doesn't specify a "METHOD" property, this
--        property specifies the date and time that the information
--        associated with the calendar component was last revised in the
--        calendar store.
--
--     Value Type:  DATE-TIME
--
--     Property Parameters:  IANA and non-standard property parameters can
--        be specified on this property.
--
--     Conformance:  This property MUST be included in the "VEVENT",
--        "VTODO", "VJOURNAL", or "VFREEBUSY" calendar components.
--
--     Description:  The value MUST be specified in the UTC time format.
--
--        This property is also useful to protocols such as [2447bis] that
--        have inherent latency issues with the delivery of content.  This
--        property will assist in the proper sequencing of messages
--        containing iCalendar objects.
--
--        In the case of an iCalendar object that specifies a "METHOD"
--        property, this property differs from the "CREATED" and "LAST-
--        MODIFIED" properties.  These two properties are used to specify
--        when the particular calendar data in the calendar store was
--        created and last modified.  This is different than when the
--        iCalendar object representation of the calendar service
--        information was created or last modified.
--
--        In the case of an iCalendar object that doesn't specify a "METHOD"
--        property, this property is equivalent to the "LAST-MODIFIED"
--        property.
--
--     Format Definition:  This property is defined by the following
--        notation:
--
--         dtstamp    = "DTSTAMP" stmparam ":" date-time CRLF
--
--         stmparam   = *(";" other-param)
--
--     Example:
--
--         DTSTAMP:19971210T080000Z
-- @
newtype DateTimeStamp = DateTimeStamp {unDateTimeStamp :: DateTime}
  deriving (Show, Eq, Generic)

instance Validity DateTimeStamp

instance IsProperty DateTimeStamp where
  propertyName Proxy = "DTSTAMP"
  propertyP = fmap DateTimeStamp . propertyTypeP
  propertyB = propertyTypeB . unDateTimeStamp

-- |
--
-- === [section 3.8.3.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.3.1)
--
-- @
--     Format Definition:  This property is defined by the following
--        notation:
--
--         tzid       = "TZID" tzidpropparam ":" [tzidprefix] text CRLF
--
--         tzidpropparam      = *(";" other-param)
--
--         ;tzidprefix        = "/"
--         ; Defined previously. Just listed here for reader convenience.
--
--     Example:  The following are examples of non-globally unique time zone
--        identifiers:
--
--         TZID:America/New_York
--
--         TZID:America/Los_Angeles
--
--        The following is an example of a fictitious globally unique time
--        zone identifier:
--
--         TZID:/example.org/America/New_York
-- @
newtype TZID = TZID {unTZID :: Text}
  deriving (Show, Eq, Generic)

instance Validity TZID

instance IsProperty TZID where
  propertyName Proxy = "TZID"
  propertyP = fmap TZID . propertyTypeP
  propertyB = propertyTypeB . unTZID

-- |
--
-- === [section 3.8.2.4](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.2.4)
--
-- @
--     Property Name:  DTSTART
--
--     Purpose:  This property specifies when the calendar component begins.
--
--     Value Type:  The default value type is DATE-TIME.  The time value
--        MUST be one of the forms defined for the DATE-TIME value type.
--        The value type can be set to a DATE value type.
--
--     Property Parameters:  IANA, non-standard, value data type, and time
--        zone identifier property parameters can be specified on this
--        property.
--
--     Conformance:  This property can be specified once in the "VEVENT",
--        "VTODO", or "VFREEBUSY" calendar components as well as in the
--        "STANDARD" and "DAYLIGHT" sub-components.  This property is
--        REQUIRED in all types of recurring calendar components that
--        specify the "RRULE" property.  This property is also REQUIRED in
--        "VEVENT" calendar components contained in iCalendar objects that
--        don't specify the "METHOD" property.
--
--     Description:  Within the "VEVENT" calendar component, this property
--        defines the start date and time for the event.
--
--        Within the "VFREEBUSY" calendar component, this property defines
--        the start date and time for the free or busy time information.
--        The time MUST be specified in UTC time.
--
--        Within the "STANDARD" and "DAYLIGHT" sub-components, this property
--        defines the effective start date and time for a time zone
--        specification.  This property is REQUIRED within each "STANDARD"
--        and "DAYLIGHT" sub-components included in "VTIMEZONE" calendar
--        components and MUST be specified as a date with local time without
--        the "TZID" property parameter.
--
--     Format Definition:  This property is defined by the following
--        notation:
--
--         dtstart    = "DTSTART" dtstparam ":" dtstval CRLF
--
--         dtstparam  = *(
--                    ;
--                    ; The following are OPTIONAL,
--                    ; but MUST NOT occur more than once.
--                    ;
--                    (";" "VALUE" "=" ("DATE-TIME" / "DATE")) /
--                    (";" tzidparam) /
--                    ;
--                    ; The following is OPTIONAL,
--                    ; and MAY occur more than once.
--                    ;
--                    (";" other-param)
--                    ;
--                    )
--
--         dtstval    = date-time / date
--         ;Value MUST match value type
--
--     Example:  The following is an example of this property:
--
--         DTSTART:19980118T073000Z
-- @
data DateTimeStart
  = DateTimeStartDate !Date
  | DateTimeStartDateTime !DateTime
  deriving (Show, Eq, Generic)

instance Validity DateTimeStart

instance IsProperty DateTimeStart where
  propertyName Proxy = "DTSTART"
  propertyP cl =
    (DateTimeStartDate <$> propertyTypeP cl)
      <|> (DateTimeStartDateTime <$> propertyTypeP cl)
  propertyB = \case
    DateTimeStartDate date -> propertyTypeB date
    DateTimeStartDateTime dateTime -> propertyTypeB dateTime

-- |
--
-- === [section 3.8.7.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.7.1)
--
-- @
--     Property Name:  CREATED
--
--     Purpose:  This property specifies the date and time that the calendar
--        information was created by the calendar user agent in the calendar
--        store.
--
--           Note: This is analogous to the creation date and time for a
--           file in the file system.
--
--     Value Type:  DATE-TIME
--
--     Property Parameters:  IANA and non-standard property parameters can
--        be specified on this property.
--
--     Conformance:  The property can be specified once in "VEVENT",
--        "VTODO", or "VJOURNAL" calendar components.  The value MUST be
--        specified as a date with UTC time.
--
--     Description:  This property specifies the date and time that the
--        calendar information was created by the calendar user agent in the
--        calendar store.
--
--     Format Definition:  This property is defined by the following
--        notation:
--
--         created    = "CREATED" creaparam ":" date-time CRLF
--
--         creaparam  = *(";" other-param)
--
--     Example:  The following is an example of this property:
--
--         CREATED:19960329T133000Z
-- @
--
-- Because the spec says "The value MUST bespecified as a date with UTC time.",
-- we will just store the 'LocalTime' (in the utc timezone) instead of a
-- 'DateTime'
newtype Created = Created {unCreated :: Time.LocalTime}
  deriving (Show, Eq, Generic)

instance Validity Created where
  validate c@Created {..} =
    mconcat
      [ genericValidate c,
        validateImpreciseLocalTime unCreated
      ]

instance IsProperty Created where
  propertyName Proxy = "CREATED"
  propertyP = fmap Created . dateTimeUTCP
  propertyB = dateTimeUTCB . unCreated

-- |
--
-- === [section 3.8.1.12](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.12)
--
-- @
--     Property Name:  SUMMARY
--
--     Purpose:  This property defines a short summary or subject for the
--        calendar component.
--
--     Value Type:  TEXT
--
--     Property Parameters:  IANA, non-standard, alternate text
--        representation, and language property parameters can be specified
--        on this property.
--
--     Conformance:  The property can be specified in "VEVENT", "VTODO",
--        "VJOURNAL", or "VALARM" calendar components.
--
--     Description:  This property is used in the "VEVENT", "VTODO", and
--        "VJOURNAL" calendar components to capture a short, one-line
--        summary about the activity or journal entry.
--
--        This property is used in the "VALARM" calendar component to
--        capture the subject of an EMAIL category of alarm.
--
--     Format Definition:  This property is defined by the following
--        notation:
--         summary    = "SUMMARY" summparam ":" text CRLF
--
--         summparam  = *(
--                    ;
--                    ; The following are OPTIONAL,
--                    ; but MUST NOT occur more than once.
--                    ;
--                    (";" altrepparam) / (";" languageparam) /
--                    ;
--                    ; The following is OPTIONAL,
--                    ; and MAY occur more than once.
--                    ;
--                    (";" other-param)
--                    ;
--                    )
--
--     Example:  The following is an example of this property:
--
--         SUMMARY:Department Party
-- @
newtype Summary = Summary {unSummary :: Text}
  deriving (Show, Eq, Generic)

instance Validity Summary

instance IsProperty Summary where
  propertyName Proxy = "SUMMARY"
  propertyP = fmap Summary . propertyTypeP
  propertyB = propertyTypeB . unSummary

-- |
--
-- === [section 3.8.1.5](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.5)
--
-- @
--     Property Name:  DESCRIPTION
--
--     Purpose:  This property provides a more complete description of the
--        calendar component than that provided by the "SUMMARY" property.
--
--     Value Type:  TEXT
--
--     Property Parameters:  IANA, non-standard, alternate text
--        representation, and language property parameters can be specified
--        on this property.
--     Conformance:  The property can be specified in the "VEVENT", "VTODO",
--        "VJOURNAL", or "VALARM" calendar components.  The property can be
--        specified multiple times only within a "VJOURNAL" calendar
--        component.
--
--     Description:  This property is used in the "VEVENT" and "VTODO" to
--        capture lengthy textual descriptions associated with the activity.
--
--        This property is used in the "VJOURNAL" calendar component to
--        capture one or more textual journal entries.
--
--        This property is used in the "VALARM" calendar component to
--        capture the display text for a DISPLAY category of alarm, and to
--        capture the body text for an EMAIL category of alarm.
--
--     Format Definition:  This property is defined by the following
--        notation:
--
--         description = "DESCRIPTION" descparam ":" text CRLF
--
--         descparam   = *(
--                     ;
--                     ; The following are OPTIONAL,
--                     ; but MUST NOT occur more than once.
--                     ;
--                     (";" altrepparam) / (";" languageparam) /
--                     ;
--                     ; The following is OPTIONAL,
--                     ; and MAY occur more than once.
--                     ;
--                     (";" other-param)
--                     ;
--                     )
--
--     Example:  The following is an example of this property with formatted
--        line breaks in the property value:
--
--         DESCRIPTION:Meeting to provide technical review for "Phoenix"
--           design.\nHappy Face Conference Room. Phoenix design team
--           MUST attend this meeting.\nRSVP to team leader.
-- @
newtype Description = Description {unDescription :: Text}
  deriving (Show, Eq, Generic)

instance Validity Description

instance IsProperty Description where
  propertyName Proxy = "DESCRIPTION"
  propertyP = fmap Description . propertyTypeP
  propertyB = propertyTypeB . unDescription

-- TODO description
newtype TimeZoneName = TimeZoneName {unTimeZoneName :: Text}
  deriving (Show, Eq, Generic)

instance Validity TimeZoneName

instance IsProperty TimeZoneName where
  propertyName Proxy = "TZNAME"
  propertyP = fmap TimeZoneName . propertyTypeP
  propertyB = propertyTypeB . unTimeZoneName
