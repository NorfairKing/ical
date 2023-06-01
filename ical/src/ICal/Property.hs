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

-- @
-- The property value component of a content line has a format that is
-- property specific.  Refer to the section describing each property for
-- a definition of this format.
-- @
module ICal.Property where

import Control.DeepSeq
import Control.Exception
import qualified Data.CaseInsensitive as CI
import Data.Int
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Proxy
import Data.Set (Set)
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Validity hiding (Location)
import Data.Validity.Text ()
import Data.Validity.Time ()
import Data.Void
import GHC.Generics (Generic)
import ICal.Conformance
import ICal.ContentLine
import ICal.Parameter
import ICal.PropertyType
import Text.Read

data PropertyParseError
  = PropertyTypeParseError !PropertyTypeParseError
  | MismatchedPropertyName
      !ContentLineName
      -- ^ Expected
      !ContentLineName
      -- ^ Actual
  | UnknownCalendarScale !Text
  | UnReadableGeographicPosition !Text
  | UnknownStatus !Text
  | UnknownTransparency !Text
  | ValueMismatch !ContentLineName !(Maybe ValueDataType) (Maybe ValueDataType) ![ValueDataType]
  deriving (Show, Eq, Ord)

instance Exception PropertyParseError where
  displayException = \case
    PropertyTypeParseError ptpe -> displayException ptpe
    MismatchedPropertyName expected actual ->
      unwords
        [ "Expected content line with name",
          show expected,
          "but got",
          show actual,
          "instead."
        ]
    UnknownCalendarScale t -> unwords ["Unknown Calendar Scale:", show t]
    UnReadableGeographicPosition t -> unwords ["UnReadable Geographic position:", show t]
    UnknownStatus t -> unwords ["Unknown Status:", show t]
    UnknownTransparency t -> unwords ["Unknown Transparency:", show t]
    ValueMismatch name actualType mDefaultType otherTypes ->
      unlines
        [ unwords ["Mismatched value type for:", show name],
          unwords ["Actual:", show actualType],
          unwords ["Default:", maybe "none" show mDefaultType],
          unwords ["Other options:", show otherTypes]
        ]

-- |
--
-- === [section 3.7](https://datatracker.ietf.org/doc/html/rfc5545#section-3.7)
--
-- @
-- The Calendar Properties are attributes that apply to the iCalendar
-- object, as a whole.  These properties do not appear within a calendar
-- component.  They SHOULD be specified after the "BEGIN:VCALENDAR"
-- delimiter string and prior to any calendar component.
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
  propertyP :: ContentLineValue -> Conform PropertyParseError Void Void property

  -- | Builder for the property
  propertyB :: property -> ContentLineValue

instance IsProperty RecurrenceRule where
  propertyName Proxy = "RRULE"
  propertyP = wrapPropertyTypeP id
  propertyB = recurrenceRuleB

propertyContentLineP ::
  forall property.
  IsProperty property =>
  ContentLine ->
  Conform PropertyParseError Void Void property
propertyContentLineP ContentLine {..} =
  let name = propertyName (Proxy :: Proxy property)
   in if contentLineName == name
        then propertyP contentLineValue
        else unfixableError $ MismatchedPropertyName name contentLineName

propertyContentLineB :: forall property. IsProperty property => property -> ContentLine
propertyContentLineB = ContentLine (propertyName (Proxy :: Proxy property)) . propertyB

viaPropertyTypeP ::
  forall propertyType property.
  IsPropertyType propertyType =>
  (propertyType -> Conform PropertyParseError Void Void property) ->
  (ContentLineValue -> Conform PropertyParseError Void Void property)
viaPropertyTypeP func clv = do
  propertyType <- conformMapError PropertyTypeParseError $ typedPropertyTypeP clv
  func propertyType

wrapPropertyTypeP ::
  IsPropertyType propertyType =>
  (propertyType -> property) ->
  (ContentLineValue -> Conform PropertyParseError Void Void property)
wrapPropertyTypeP func = viaPropertyTypeP (pure . func)

newtype Begin = Begin {unBegin :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity Begin

instance NFData Begin

instance IsProperty Begin where
  propertyName Proxy = "BEGIN"
  propertyP = wrapPropertyTypeP Begin
  propertyB = propertyTypeB . unBegin

newtype End = End {unEnd :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity End

instance NFData End

instance IsProperty End where
  propertyName Proxy = "END"
  propertyP = wrapPropertyTypeP End
  propertyB = propertyTypeB . unEnd

-- | Method
--
-- === [section 3.7.2](https://datatracker.ietf.org/doc/html/rfc5545#section-3.7.2)
-- @
-- Property Name:  METHOD
--
-- Purpose:  This property defines the iCalendar object method
--    associated with the calendar object.
--
-- Value Type:  TEXT
--
-- Property Parameters:  IANA and non-standard property parameters can
--    be specified on this property.
--
-- Conformance:  This property can be specified once in an iCalendar
--    object.
--
-- Description:  When used in a MIME message entity, the value of this
--    property MUST be the same as the Content-Type "method" parameter
--    value.  If either the "METHOD" property or the Content-Type
--    "method" parameter is specified, then the other MUST also be
--    specified.
--
--    No methods are defined by this specification.  This is the subject
--    of other specifications, such as the iCalendar Transport-
--    independent Interoperability Protocol (iTIP) defined by [2446bis].
--
--    If this property is not present in the iCalendar object, then a
--    scheduling transaction MUST NOT be assumed.  In such cases, the
--    iCalendar object is merely being used to transport a snapshot of
--    some calendar information; without the intention of conveying a
--    scheduling semantic.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     method     = "METHOD" metparam ":" metvalue CRLF
--
--     metparam   = *(";" other-param)
--
--     metvalue   = iana-token
--
-- Example:  The following is a hypothetical example of this property to
--    convey that the iCalendar object is a scheduling request:
--
--     METHOD:REQUEST
-- @
newtype Method = Method {unMethod :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity Method

instance NFData Method

instance IsProperty Method where
  propertyName Proxy = "METHOD"
  propertyP = wrapPropertyTypeP Method
  propertyB = propertyTypeB . unMethod

-- | Product Identifier
--
-- === [section 3.7.3](https://datatracker.ietf.org/doc/html/rfc5545#section-3.7.3)
--
-- @
-- Property Name:  PRODID
--
-- Purpose:  This property specifies the identifier for the product that
--    created the iCalendar object.
--
-- Value Type:  TEXT
--
-- Property Parameters:  IANA and non-standard property parameters can
--    be specified on this property.
--
-- Conformance:  The property MUST be specified once in an iCalendar
--    object.
--
-- Description:  The vendor of the implementation SHOULD assure that
--    this is a globally unique identifier; using some technique such as
--    an FPI value, as defined in [ISO.9070.1991].
--
--    This property SHOULD NOT be used to alter the interpretation of an
--    iCalendar object beyond the semantics specified in this memo.  For
--    example, it is not to be used to further the understanding of non-
--    standard properties.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     prodid     = "PRODID" pidparam ":" pidvalue CRLF
--
--     pidparam   = *(";" other-param)
--
--     pidvalue   = text
--     ;Any text that describes the product and version
--     ;and that is generally assured of being unique.
--
-- Example:  The following is an example of this property.  It does not
--    imply that English is the default language.
--
--     PRODID:-//ABC Corporation//NONSGML My Product//EN
-- @
--
-- TODO fulfil this SHOULD:
-- @
-- Description:  The vendor of the implementation SHOULD assure that
--    this is a globally unique identifier; using some technique such as
--    an FPI value, as defined in [ISO.9070.1991].
-- @
newtype ProductIdentifier = ProductIdentifier {unProductIdentifier :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity ProductIdentifier

instance NFData ProductIdentifier

instance IsProperty ProductIdentifier where
  propertyName Proxy = "PRODID"
  propertyP = wrapPropertyTypeP ProductIdentifier
  propertyB = propertyTypeB . unProductIdentifier

-- |
--
-- === [section 3.7.4](https://datatracker.ietf.org/doc/html/rfc5545#section-3.7.4)
--
-- @
-- Property Name:  VERSION
--
-- Purpose:  This property specifies the identifier corresponding to the
--    highest version number or the minimum and maximum range of the
--    iCalendar specification that is required in order to interpret the
--    iCalendar object.
--
-- Value Type:  TEXT
--
-- Property Parameters:  IANA and non-standard property parameters can
--    be specified on this property.
--
-- Conformance:  This property MUST be specified once in an iCalendar
--    object.
--
-- Description:  A value of "2.0" corresponds to this memo.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     version    = "VERSION" verparam ":" vervalue CRLF
--
--     verparam   = *(";" other-param)
--
--     vervalue   = "2.0"         ;This memo
--                / maxver
--                / (minver ";" maxver)
--
--     minver     = <A IANA-registered iCalendar version identifier>
--     ;Minimum iCalendar version needed to parse the iCalendar object.
--
--     maxver     = <A IANA-registered iCalendar version identifier>
--     ;Maximum iCalendar version needed to parse the iCalendar object.
--
-- Example:  The following is an example of this property:
--
--     VERSION:2.0
-- @
newtype Version = Version {unVersion :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity Version

instance NFData Version

instance IsProperty Version where
  propertyName Proxy = "VERSION"
  propertyP = wrapPropertyTypeP Version
  propertyB = propertyTypeB . unVersion

-- | The current version
version20 :: Version
version20 = Version "2.0"

-- | Calendar Scale
--
-- === [section 3.7.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.7.1)
--
-- @
-- Property Name:  CALSCALE
--
-- Purpose:  This property defines the calendar scale used for the
--    calendar information specified in the iCalendar object.
--
-- Value Type:  TEXT
--
-- Property Parameters:  IANA and non-standard property parameters can
--    be specified on this property.
--
-- Conformance:  This property can be specified once in an iCalendar
--    object.  The default value is "GREGORIAN".
--
-- Description:  This memo is based on the Gregorian calendar scale.
--    The Gregorian calendar scale is assumed if this property is not
--    specified in the iCalendar object.  It is expected that other
--    calendar scales will be defined in other specifications or by
--    future versions of this memo.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     calscale   = "CALSCALE" calparam ":" calvalue CRLF
--
--     calparam   = *(";" other-param)
--
--     calvalue   = "GREGORIAN"
--
-- Example:  The following is an example of this property:
--
--     CALSCALE:GREGORIAN
-- @
data CalendarScale
  = CalendarScaleGregorian
  deriving (Show, Eq, Ord, Generic)

instance Validity CalendarScale

instance NFData CalendarScale

instance IsProperty CalendarScale where
  propertyName Proxy = "CALSCALE"
  propertyP = viaPropertyTypeP $ \t ->
    case t :: Text of
      "GREGORIAN" -> pure CalendarScaleGregorian
      _ -> unfixableError $ UnknownCalendarScale t
  propertyB =
    propertyTypeB . \case
      CalendarScaleGregorian -> "GREGORIAN" :: Text

-- | Default Calendar Scale
--
-- @
-- The default value is "GREGORIAN".
-- @
defaultCalendarScale :: CalendarScale
defaultCalendarScale = CalendarScaleGregorian

-- |
--
-- === [section 3.8.4.7](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.4.7)
--
-- @
-- Property Name:  UID
--
-- Purpose:  This property defines the persistent, globally unique
--    identifier for the calendar component.
--
-- Value Type:  TEXT
--
-- Property Parameters:  IANA and non-standard property parameters can
--    be specified on this property.
--
-- Conformance:  The property MUST be specified in the "VEVENT",
--    "VTODO", "VJOURNAL", or "VFREEBUSY" calendar components.
--
-- Description:  The "UID" itself MUST be a globally unique identifier.
--    The generator of the identifier MUST guarantee that the identifier
--    is unique.  There are several algorithms that can be used to
--    accomplish this.  A good method to assure uniqueness is to put the
--    domain name or a domain literal IP address of the host on which
--    the identifier was created on the right-hand side of an "@", and
--    on the left-hand side, put a combination of the current calendar
--    date and time of day (i.e., formatted in as a DATE-TIME value)
--    along with some other currently unique (perhaps sequential)
--    identifier available on the system (for example, a process id
--    number).  Using a DATE-TIME value on the left-hand side and a
--    domain name or domain literal on the right-hand side makes it
--    possible to guarantee uniqueness since no two hosts should be
--    using the same domain name or IP address at the same time.  Though
--    other algorithms will work, it is RECOMMENDED that the right-hand
--    side contain some domain identifier (either of the host itself or
--    otherwise) such that the generator of the message identifier can
--    guarantee the uniqueness of the left-hand side within the scope of
--    that domain.
--
--    This is the method for correlating scheduling messages with the
--    referenced "VEVENT", "VTODO", or "VJOURNAL" calendar component.
--    The full range of calendar components specified by a recurrence
--    set is referenced by referring to just the "UID" property value
--    corresponding to the calendar component.  The "RECURRENCE-ID"
--    property allows the reference to an individual instance within the
--    recurrence set.
--
--    This property is an important method for group-scheduling
--    applications to match requests with later replies, modifications,
--    or deletion requests.  Calendaring and scheduling applications
--    MUST generate this property in "VEVENT", "VTODO", and "VJOURNAL"
--    calendar components to assure interoperability with other group-
--    scheduling applications.  This identifier is created by the
--    calendar system that generates an iCalendar object.
--
--    Implementations MUST be able to receive and persist values of at
--    least 255 octets for this property, but they MUST NOT truncate
--    values in the middle of a UTF-8 multi-octet sequence.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     uid        = "UID" uidparam ":" text CRLF
--
--     uidparam   = *(";" other-param)
--
-- Example:  The following is an example of this property:
--
--     UID:19960401T080045Z-4000F192713-0052@example.com
-- @
newtype UID = UID {unUID :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity UID

instance NFData UID

instance IsProperty UID where
  propertyName Proxy = "UID"
  propertyP = wrapPropertyTypeP UID
  propertyB = propertyTypeB . unUID

-- |
--
-- === [section 3.8.7.2](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.7.2)
--
-- @
-- Property Name:  DTSTAMP
--
-- Purpose:  In the case of an iCalendar object that specifies a
--    "METHOD" property, this property specifies the date and time that
--    the instance of the iCalendar object was created.  In the case of
--    an iCalendar object that doesn't specify a "METHOD" property, this
--    property specifies the date and time that the information
--    associated with the calendar component was last revised in the
--    calendar store.
--
-- Value Type:  DATE-TIME
--
-- Property Parameters:  IANA and non-standard property parameters can
--    be specified on this property.
--
-- Conformance:  This property MUST be included in the "VEVENT",
--    "VTODO", "VJOURNAL", or "VFREEBUSY" calendar components.
--
-- Description:  The value MUST be specified in the UTC time format.
--
--    This property is also useful to protocols such as [2447bis] that
--    have inherent latency issues with the delivery of content.  This
--    property will assist in the proper sequencing of messages
--    containing iCalendar objects.
--
--    In the case of an iCalendar object that specifies a "METHOD"
--    property, this property differs from the "CREATED" and "LAST-
--    MODIFIED" properties.  These two properties are used to specify
--    when the particular calendar data in the calendar store was
--    created and last modified.  This is different than when the
--    iCalendar object representation of the calendar service
--    information was created or last modified.
--
--    In the case of an iCalendar object that doesn't specify a "METHOD"
--    property, this property is equivalent to the "LAST-MODIFIED"
--    property.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     dtstamp    = "DTSTAMP" stmparam ":" date-time CRLF
--
--     stmparam   = *(";" other-param)
--
-- Example:
--
--     DTSTAMP:19971210T080000Z
-- @
newtype DateTimeStamp = DateTimeStamp {unDateTimeStamp :: DateTime}
  deriving (Show, Eq, Ord, Generic)

instance Validity DateTimeStamp

instance NFData DateTimeStamp

instance IsProperty DateTimeStamp where
  propertyName Proxy = "DTSTAMP"
  propertyP = wrapPropertyTypeP DateTimeStamp
  propertyB = propertyTypeB . unDateTimeStamp

-- | Timezone Identifier
--
-- === [section 3.8.3.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.3.1)
--
-- @
-- Format Definition:  This property is defined by the following
--    notation:
--
--     tzid       = "TZID" tzidpropparam ":" [tzidprefix] text CRLF
--
--     tzidpropparam      = *(";" other-param)
--
--     ;tzidprefix        = "/"
--     ; Defined previously. Just listed here for reader convenience.
--
-- Example:  The following are examples of non-globally unique time zone
--    identifiers:
--
--     TZID:America/New_York
--
--     TZID:America/Los_Angeles
--
--    The following is an example of a fictitious globally unique time
--    zone identifier:
--
--     TZID:/example.org/America/New_York
-- @
newtype TimeZoneIdentifier = TimeZoneIdentifier {unTimeZoneIdentifier :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity TimeZoneIdentifier

instance NFData TimeZoneIdentifier

instance IsProperty TimeZoneIdentifier where
  propertyName Proxy = "TZID"
  propertyP = wrapPropertyTypeP TimeZoneIdentifier
  propertyB = propertyTypeB . unTimeZoneIdentifier

tzidParam :: TimeZoneIdentifier -> TimeZoneIdentifierParam
tzidParam = TimeZoneIdentifierParam . CI.mk . unTimeZoneIdentifier

-- |
--
-- === [section 3.8.2.4](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.2.4)
--
-- @
-- Property Name:  DTSTART
--
-- Purpose:  This property specifies when the calendar component begins.
--
-- Value Type:  The default value type is DATE-TIME.  The time value
--    MUST be one of the forms defined for the DATE-TIME value type.
--    The value type can be set to a DATE value type.
--
-- Property Parameters:  IANA, non-standard, value data type, and time
--    zone identifier property parameters can be specified on this
--    property.
--
-- Conformance:  This property can be specified once in the "VEVENT",
--    "VTODO", or "VFREEBUSY" calendar components as well as in the
--    "STANDARD" and "DAYLIGHT" sub-components.  This property is
--    REQUIRED in all types of recurring calendar components that
--    specify the "RRULE" property.  This property is also REQUIRED in
--    "VEVENT" calendar components contained in iCalendar objects that
--    don't specify the "METHOD" property.
--
-- Description:  Within the "VEVENT" calendar component, this property
--    defines the start date and time for the event.
--
--    Within the "VFREEBUSY" calendar component, this property defines
--    the start date and time for the free or busy time information.
--    The time MUST be specified in UTC time.
--
--    Within the "STANDARD" and "DAYLIGHT" sub-components, this property
--    defines the effective start date and time for a time zone
--    specification.  This property is REQUIRED within each "STANDARD"
--    and "DAYLIGHT" sub-components included in "VTIMEZONE" calendar
--    components and MUST be specified as a date with local time without
--    the "TimeZoneIdentifier" property parameter.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     dtstart    = "DTSTART" dtstparam ":" dtstval CRLF
--
--     dtstparam  = *(
--                ;
--                ; The following are OPTIONAL,
--                ; but MUST NOT occur more than once.
--                ;
--                (";" "VALUE" "=" ("DATE-TIME" / "DATE")) /
--                (";" tzidparam) /
--                ;
--                ; The following is OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                (";" other-param)
--                ;
--                )
--
--     dtstval    = date-time / date
--     ;Value MUST match value type
--
-- Example:  The following is an example of this property:
--
--     DTSTART:19980118T073000Z
-- @
data DateTimeStart
  = DateTimeStartDate !Date
  | DateTimeStartDateTime !DateTime
  deriving (Show, Eq, Ord, Generic)

instance Validity DateTimeStart

instance NFData DateTimeStart

instance IsProperty DateTimeStart where
  propertyName Proxy = "DTSTART"
  propertyP clv = do
    mValue <- conformMapError (PropertyTypeParseError . ParameterParseError) $ optionalParam $ contentLineValueParams clv
    case mValue of
      Just TypeDateTime -> wrapPropertyTypeP DateTimeStartDateTime clv
      Just TypeDate -> wrapPropertyTypeP DateTimeStartDate clv
      Just _ -> unfixableError $ ValueMismatch "DTSTART" mValue (Just TypeDateTime) [TypeDate]
      -- @
      -- Value Type:  The default value type is DATE-TIME.
      -- @
      Nothing -> wrapPropertyTypeP DateTimeStartDateTime clv
  propertyB = \case
    DateTimeStartDateTime dateTime -> propertyTypeB dateTime
    -- @
    -- Value Type:  The default value type is DATE-TIME.
    -- @
    DateTimeStartDate date -> typedPropertyTypeB date

-- | Attachment
--
-- === [section 3.8.1.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.1)
--
-- @
-- Property Name:  ATTACH
--
-- Purpose:  This property provides the capability to associate a
--    document object with a calendar component.
--
-- Value Type:  The default value type for this property is URI.  The
--    value type can also be set to BINARY to indicate inline binary
--    encoded content information.
--
-- Property Parameters:  IANA, non-standard, inline encoding, and value
--    data type property parameters can be specified on this property.
--    The format type parameter can be specified on this property and is
--    RECOMMENDED for inline binary encoded content information.
--
-- Conformance:  This property can be specified multiple times in a
--    "VEVENT", "VTODO", "VJOURNAL", or "VALARM" calendar component with
--    the exception of AUDIO alarm that only allows this property to
--    occur once.
--
-- Description:  This property is used in "VEVENT", "VTODO", and
--    "VJOURNAL" calendar components to associate a resource (e.g.,
--    document) with the calendar component.  This property is used in
--    "VALARM" calendar components to specify an audio sound resource or
--    an email message attachment.  This property can be specified as a
--    URI pointing to a resource or as inline binary encoded content.
--
--    When this property is specified as inline binary encoded content,
--    calendar applications MAY attempt to guess the media type of the
--    resource via inspection of its content if and only if the media
--    type of the resource is not given by the "FMTTYPE" parameter.  If
--    the media type remains unknown, calendar applications SHOULD treat
--    it as type "application/octet-stream".
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     attach     = "ATTACH" attachparam ( ":" uri ) /
--                  (
--                    ";" "ENCODING" "=" "BASE64"
--                    ";" "VALUE" "=" "BINARY"
--                    ":" binary
--                  )
--                  CRLF
--
--     attachparam = *(
--                 ;
--                 ; The following is OPTIONAL for a URI value,
--                 ; RECOMMENDED for a BINARY value,
--                 ; and MUST NOT occur more than once.
--                 ;
--                 (";" fmttypeparam) /
--                 ;
--                 ; The following is OPTIONAL,
--                 ; and MAY occur more than once.
--                 ;
--                 (";" other-param)
--                 ;
--                 )
--
-- Example:  The following are examples of this property:
--
--     ATTACH:CID:jsmith.part3.960817T083000.xyzMail@example.com
--
--     ATTACH;FMTTYPE=application/postscript:ftp://example.com/pub/
--      reports/r-960812.ps
-- @
data Attachment
  = AttachmentURI !(Maybe FormatType) !URI
  | AttachmentBinary !(Maybe FormatType) !Binary
  deriving (Show, Eq, Ord, Generic)

instance Validity Attachment

instance NFData Attachment

instance IsProperty Attachment where
  propertyName Proxy = "ATTACH"
  propertyP clv = do
    mValue <- conformMapError (PropertyTypeParseError . ParameterParseError) $ optionalParam $ contentLineValueParams clv
    mFormatType <- conformMapError (PropertyTypeParseError . ParameterParseError) $ optionalParam $ contentLineValueParams clv
    case mValue of
      Just TypeURI -> wrapPropertyTypeP (AttachmentURI mFormatType) clv
      Just TypeBinary -> wrapPropertyTypeP (AttachmentBinary mFormatType) clv
      Just _ -> unfixableError $ ValueMismatch "ATTACH" mValue (Just TypeURI) [TypeBinary]
      -- @
      -- Value Type:  The default value type for this property is URI.
      -- @
      Nothing -> wrapPropertyTypeP (AttachmentURI mFormatType) clv
  propertyB = \case
    -- @
    -- Value Type:  The default value type for this property is URI.
    -- @
    AttachmentURI mFormatType uri -> insertMParam mFormatType $ propertyTypeB uri
    AttachmentBinary mFormatType binary -> insertMParam mFormatType $ typedPropertyTypeB binary

-- | Classification
--
-- === [section 3.8.1.3](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.3)
--
-- @
-- Property Name:  CLASS
--
-- Purpose:  This property defines the access classification for a
--    calendar component.
--
-- Value Type:  TEXT
--
-- Property Parameters:  IANA and non-standard property parameters can
--    be specified on this property.
--
-- Conformance:  The property can be specified once in a "VEVENT",
--    "VTODO", or "VJOURNAL" calendar components.
--
-- Description:  An access classification is only one component of the
--    general security system within a calendar application.  It
--    provides a method of capturing the scope of the access the
--    calendar owner intends for information within an individual
--    calendar entry.  The access classification of an individual
--    iCalendar component is useful when measured along with the other
--    security components of a calendar system (e.g., calendar user
--    authentication, authorization, access rights, access role, etc.).
--    Hence, the semantics of the individual access classifications
--    cannot be completely defined by this memo alone.  Additionally,
--    due to the "blind" nature of most exchange processes using this
--    memo, these access classifications cannot serve as an enforcement
--    statement for a system receiving an iCalendar object.  Rather,
--    they provide a method for capturing the intention of the calendar
--    owner for the access to the calendar component.  If not specified
--    in a component that allows this property, the default value is
--    PUBLIC.  Applications MUST treat x-name and iana-token values they
--    don't recognize the same way as they would the PRIVATE value.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     class      = "CLASS" classparam ":" classvalue CRLF
--
--     classparam = *(";" other-param)
--
--     classvalue = "PUBLIC" / "PRIVATE" / "CONFIDENTIAL" / iana-token
--                / x-name
--     ;Default is PUBLIC
--
-- Example:  The following is an example of this property:
--
--     CLASS:PUBLIC
-- @
data Classification
  = ClassificationPublic
  | ClassificationPrivate
  | ClassificationConfidential
  | ClassificationOther !Text
  deriving (Show, Eq, Ord, Generic)

instance Validity Classification

instance NFData Classification

instance IsProperty Classification where
  propertyName Proxy = "CLASS"
  propertyP = wrapPropertyTypeP parseClassification
  propertyB = propertyTypeB . renderClassification

parseClassification :: Text -> Classification
parseClassification = \case
  "PUBLIC" -> ClassificationPublic
  "PRIVATE" -> ClassificationPrivate
  "CONFIDENTIAL" -> ClassificationConfidential
  t -> ClassificationOther t

renderClassification :: Classification -> Text
renderClassification = \case
  ClassificationPublic -> "PUBLIC"
  ClassificationPrivate -> "PRIVATE"
  ClassificationConfidential -> "CONFIDENTIAL"
  ClassificationOther t -> t

-- | Default 'Classification'
--
-- @
-- ;Default value is PUBLIC
-- @
defaultClassification :: Classification
defaultClassification = ClassificationPublic

-- | Organizer
--
-- === [section 3.8.4.3](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.4.3)
--
-- @
-- Property Name:  ORGANIZER
--
-- Purpose:  This property defines the organizer for a calendar
--    component.
--
-- Value Type:  CAL-ADDRESS
--
-- Property Parameters:  IANA, non-standard, language, common name,
--    directory entry reference, and sent-by property parameters can be
--    specified on this property.
--
-- Conformance:  This property MUST be specified in an iCalendar object
--    that specifies a group-scheduled calendar entity.  This property
--    MUST be specified in an iCalendar object that specifies the
--    publication of a calendar user's busy time.  This property MUST
--    NOT be specified in an iCalendar object that specifies only a time
--    zone definition or that defines calendar components that are not
--    group-scheduled components, but are components only on a single
--    user's calendar.
--
-- Description:  This property is specified within the "VEVENT",
--    "VTODO", and "VJOURNAL" calendar components to specify the
--    organizer of a group-scheduled calendar entity.  The property is
--    specified within the "VFREEBUSY" calendar component to specify the
--    calendar user requesting the free or busy time.  When publishing a
--    "VFREEBUSY" calendar component, the property is used to specify
--    the calendar that the published busy time came from.
--
--    The property has the property parameters "CN", for specifying the
--    common or display name associated with the "Organizer", "DIR", for
--    specifying a pointer to the directory information associated with
--    the "Organizer", "SENT-BY", for specifying another calendar user
--    that is acting on behalf of the "Organizer".  The non-standard
--    parameters may also be specified on this property.  If the
--    "LANGUAGE" property parameter is specified, the identified
--    language applies to the "CN" parameter value.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     organizer  = "ORGANIZER" orgparam ":"
--                  cal-address CRLF
--
--     orgparam   = *(
--                ;
--                ; The following are OPTIONAL,
--                ; but MUST NOT occur more than once.
--                ;
--                (";" cnparam) / (";" dirparam) / (";" sentbyparam) /
--                (";" languageparam) /
--                ;
--                ; The following is OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                (";" other-param)
--                ;
--                )
--
-- Example:  The following is an example of this property:
--
--     ORGANIZER;CN=John Smith:mailto:jsmith@example.com
--
--    The following is an example of this property with a pointer to the
--    directory information associated with the organizer:
--
--     ORGANIZER;CN=JohnSmith;DIR="ldap://example.com:6666/o=DC%20Ass
--      ociates,c=US???(cn=John%20Smith)":mailto:jsmith@example.com
--
--    The following is an example of this property used by another
--    calendar user who is acting on behalf of the organizer, with
--    responses intended to be sent back to the organizer, not the other
--    calendar user:
--
--     ORGANIZER;SENT-BY="mailto:jane_doe@example.com":
--      mailto:jsmith@example.com
-- @
data Organizer = Organizer
  { organizerCalAddress :: !CalAddress,
    organizerCommonName :: !(Maybe CommonName)
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Organizer

instance NFData Organizer

instance IsProperty Organizer where
  propertyName Proxy = "ORGANIZER"
  propertyP clv = flip viaPropertyTypeP clv $ \organizerCalAddress -> do
    organizerCommonName <-
      conformMapError (PropertyTypeParseError . ParameterParseError) (optionalParam (contentLineValueParams clv))

    pure Organizer {..}
  propertyB Organizer {..} =
    insertMParam organizerCommonName $
      propertyTypeB organizerCalAddress

mkOrganizer :: CalAddress -> Organizer
mkOrganizer calAddress =
  Organizer
    { organizerCalAddress = calAddress,
      organizerCommonName = Nothing
    }

-- |
--
-- === [section 3.8.4.4](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.4.4)
--
-- @
-- Property Name:  RECURRENCE-ID
--
-- Purpose:  This property is used in conjunction with the "UID" and
--    "SEQUENCE" properties to identify a specific instance of a
--    recurring "VEVENT", "VTODO", or "VJOURNAL" calendar component.
--    The property value is the original value of the "DTSTART" property
--    of the recurrence instance.
--
-- Value Type:  The default value type is DATE-TIME.  The value type can
--    be set to a DATE value type.  This property MUST have the same
--    value type as the "DTSTART" property contained within the
--    recurring component.  Furthermore, this property MUST be specified
--    as a date with local time if and only if the "DTSTART" property
--    contained within the recurring component is specified as a date
--    with local time.
--
-- Property Parameters:  IANA, non-standard, value data type, time zone
--    identifier, and recurrence identifier range parameters can be
--    specified on this property.
--
-- Conformance:  This property can be specified in an iCalendar object
--    containing a recurring calendar component.
--
-- Description:  The full range of calendar components specified by a
--    recurrence set is referenced by referring to just the "UID"
--    property value corresponding to the calendar component.  The
--    "RECURRENCE-ID" property allows the reference to an individual
--    instance within the recurrence set.
--
--    If the value of the "DTSTART" property is a DATE type value, then
--    the value MUST be the calendar date for the recurrence instance.
--
--    The DATE-TIME value is set to the time when the original
--    recurrence instance would occur; meaning that if the intent is to
--    change a Friday meeting to Thursday, the DATE-TIME is still set to
--    the original Friday meeting.
--
--    The "RECURRENCE-ID" property is used in conjunction with the "UID"
--    and "SEQUENCE" properties to identify a particular instance of a
--    recurring event, to-do, or journal.  For a given pair of "UID" and
--    "SEQUENCE" property values, the "RECURRENCE-ID" value for a
--    recurrence instance is fixed.
--
--    The "RANGE" parameter is used to specify the effective range of
--    recurrence instances from the instance specified by the
--    "RECURRENCE-ID" property value.  The value for the range parameter
--    can only be "THISANDFUTURE" to indicate a range defined by the
--    given recurrence instance and all subsequent instances.
--    Subsequent instances are determined by their "RECURRENCE-ID" value
--    and not their current scheduled start time.  Subsequent instances
--    defined in separate components are not impacted by the given
--    recurrence instance.  When the given recurrence instance is
--    rescheduled, all subsequent instances are also rescheduled by the
--    same time difference.  For instance, if the given recurrence
--    instance is rescheduled to start 2 hours later, then all
--    subsequent instances are also rescheduled 2 hours later.
--
--
--    Similarly, if the duration of the given recurrence instance is
--    modified, then all subsequence instances are also modified to have
--    this same duration.
--
--       Note: The "RANGE" parameter may not be appropriate to
--       reschedule specific subsequent instances of complex recurring
--       calendar component.  Assuming an unbounded recurring calendar
--       component scheduled to occur on Mondays and Wednesdays, the
--       "RANGE" parameter could not be used to reschedule only the
--       future Monday instances to occur on Tuesday instead.  In such
--       cases, the calendar application could simply truncate the
--       unbounded recurring calendar component (i.e., with the "COUNT"
--       or "UNTIL" rule parts), and create two new unbounded recurring
--       calendar components for the future instances.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     recurid    = "RECURRENCE-ID" ridparam ":" ridval CRLF
--
--     ridparam   = *(
--                ;
--                ; The following are OPTIONAL,
--                ; but MUST NOT occur more than once.
--                ;
--                (";" "VALUE" "=" ("DATE-TIME" / "DATE")) /
--                (";" tzidparam) / (";" rangeparam) /
--                ;
--                ; The following is OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                (";" other-param)
--                ;
--                )
--
--     ridval     = date-time / date
--     ;Value MUST match value type
--
-- Example:  The following are examples of this property:
--
--     RECURRENCE-ID;VALUE=DATE:19960401
--
--     RECURRENCE-ID;RANGE=THISANDFUTURE:19960120T120000Z
-- @
data RecurrenceIdentifier
  = RecurrenceIdentifierDate (Maybe RecurrenceIdentifierRange) !Date
  | RecurrenceIdentifierDateTime (Maybe RecurrenceIdentifierRange) !DateTime
  deriving (Show, Eq, Ord, Generic)

instance Validity RecurrenceIdentifier

instance NFData RecurrenceIdentifier

instance IsProperty RecurrenceIdentifier where
  propertyName Proxy = "RECURRENCE-ID"
  propertyP clv = do
    mValue <- conformMapError (PropertyTypeParseError . ParameterParseError) $ optionalParam $ contentLineValueParams clv
    mRecurrenceRangeIdentifier <- conformMapError (PropertyTypeParseError . ParameterParseError) $ optionalParam $ contentLineValueParams clv
    case mValue of
      Just TypeDateTime -> wrapPropertyTypeP (RecurrenceIdentifierDateTime mRecurrenceRangeIdentifier) clv
      Just TypeDate -> wrapPropertyTypeP (RecurrenceIdentifierDate mRecurrenceRangeIdentifier) clv
      Just _ -> unfixableError $ ValueMismatch "RECURRENCE-ID" mValue (Just TypeDateTime) [TypeDate]
      -- @
      -- Value Type:  The default value type is DATE-TIME.
      -- @
      Nothing -> wrapPropertyTypeP (RecurrenceIdentifierDateTime mRecurrenceRangeIdentifier) clv
  propertyB = \case
    -- @
    -- Value Type:  The default value type is DATE-TIME.
    -- @
    RecurrenceIdentifierDateTime mRecurrenceRangeIdentifier dt -> insertMParam mRecurrenceRangeIdentifier $ propertyTypeB dt
    RecurrenceIdentifierDate mRecurrenceRangeIdentifier d -> insertMParam mRecurrenceRangeIdentifier $ typedPropertyTypeB d

validateMRecurrenceIdentifierMDateTimeStart :: Maybe DateTimeStart -> Maybe RecurrenceIdentifier -> Validation
validateMRecurrenceIdentifierMDateTimeStart mdts mrid = case (,) <$> mdts <*> mrid of
  Nothing -> valid
  Just (dts, rid) -> validateRecurrenceIdentifierDateTimeStart dts rid

-- @
-- This property MUST have the same
-- value type as the "DTSTART" property contained within the
-- recurring component.
-- Furthermore, this property MUST be specified
-- as a date with local time if and only if the "DTSTART" property
-- contained within the recurring component is specified as a date
-- with local time.
-- @
validateRecurrenceIdentifierDateTimeStart :: DateTimeStart -> RecurrenceIdentifier -> Validation
validateRecurrenceIdentifierDateTimeStart dts rid = case (dts, rid) of
  (DateTimeStartDate _, RecurrenceIdentifierDate _ _) -> valid
  (DateTimeStartDateTime _, RecurrenceIdentifierDateTime _ _) -> valid
  _ -> invalid "Value type of RecurrenceIdentifier and DTSTART differ."

-- | Created
--
-- === [section 3.8.7.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.7.1)
--
-- @
-- Property Name:  CREATED
--
-- Purpose:  This property specifies the date and time that the calendar
--    information was created by the calendar user agent in the calendar
--    store.
--
--       Note: This is analogous to the creation date and time for a
--       file in the file system.
--
-- Value Type:  DATE-TIME
--
-- Property Parameters:  IANA and non-standard property parameters can
--    be specified on this property.
--
-- Conformance:  The property can be specified once in "VEVENT",
--    "VTODO", or "VJOURNAL" calendar components.  The value MUST be
--    specified as a date with UTC time.
--
-- Description:  This property specifies the date and time that the
--    calendar information was created by the calendar user agent in the
--    calendar store.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     created    = "CREATED" creaparam ":" date-time CRLF
--
--     creaparam  = *(";" other-param)
--
-- Example:  The following is an example of this property:
--
--     CREATED:19960329T133000Z
-- @
--
-- Because the spec says "The value MUST bespecified as a date with UTC time.",
-- we will just store the 'LocalTime' (in the utc timezone) instead of a
-- 'DateTime'
newtype Created = Created {unCreated :: Time.UTCTime}
  deriving (Show, Eq, Ord, Generic)

instance Validity Created where
  validate c@Created {..} =
    mconcat
      [ genericValidate c,
        validateImpreciseUTCTime unCreated
      ]

instance NFData Created

instance IsProperty Created where
  propertyName Proxy = "CREATED"
  propertyP = fmap Created . conformMapError PropertyTypeParseError . dateTimeUTCP
  propertyB = dateTimeUTCB . unCreated

-- |
--
-- === [section 3.8.1.12](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.12)
--
-- @
-- Property Name:  SUMMARY
--
-- Purpose:  This property defines a short summary or subject for the
--    calendar component.
--
-- Value Type:  TEXT
--
-- Property Parameters:  IANA, non-standard, alternate text
--    representation, and language property parameters can be specified
--    on this property.
--
-- Conformance:  The property can be specified in "VEVENT", "VTODO",
--    "VJOURNAL", or "VALARM" calendar components.
--
-- Description:  This property is used in the "VEVENT", "VTODO", and
--    "VJOURNAL" calendar components to capture a short, one-line
--    summary about the activity or journal entry.
--
--    This property is used in the "VALARM" calendar component to
--    capture the subject of an EMAIL category of alarm.
--
-- Format Definition:  This property is defined by the following
--    notation:
--     summary    = "SUMMARY" summparam ":" text CRLF
--
--     summparam  = *(
--                ;
--                ; The following are OPTIONAL,
--                ; but MUST NOT occur more than once.
--                ;
--                (";" altrepparam) / (";" languageparam) /
--                ;
--                ; The following is OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                (";" other-param)
--                ;
--                )
--
-- Example:  The following is an example of this property:
--
--     SUMMARY:Department Party
-- @
newtype Summary = Summary {unSummary :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity Summary

instance NFData Summary

instance IsProperty Summary where
  propertyName Proxy = "SUMMARY"
  propertyP = wrapPropertyTypeP Summary
  propertyB = propertyTypeB . unSummary

-- |
--
-- === [section 3.8.1.5](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.5)
--
-- @
-- Property Name:  DESCRIPTION
--
-- Purpose:  This property provides a more complete description of the
--    calendar component than that provided by the "SUMMARY" property.
--
-- Value Type:  TEXT
--
-- Property Parameters:  IANA, non-standard, alternate text
--    representation, and language property parameters can be specified
--    on this property.
-- Conformance:  The property can be specified in the "VEVENT", "VTODO",
--    "VJOURNAL", or "VALARM" calendar components.  The property can be
--    specified multiple times only within a "VJOURNAL" calendar
--    component.
--
-- Description:  This property is used in the "VEVENT" and "VTODO" to
--    capture lengthy textual descriptions associated with the activity.
--
--    This property is used in the "VJOURNAL" calendar component to
--    capture one or more textual journal entries.
--
--    This property is used in the "VALARM" calendar component to
--    capture the display text for a DISPLAY category of alarm, and to
--    capture the body text for an EMAIL category of alarm.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     description = "DESCRIPTION" descparam ":" text CRLF
--
--     descparam   = *(
--                 ;
--                 ; The following are OPTIONAL,
--                 ; but MUST NOT occur more than once.
--                 ;
--                 (";" altrepparam) / (";" languageparam) /
--                 ;
--                 ; The following is OPTIONAL,
--                 ; and MAY occur more than once.
--                 ;
--                 (";" other-param)
--                 ;
--                 )
--
-- Example:  The following is an example of this property with formatted
--    line breaks in the property value:
--
--     DESCRIPTION:Meeting to provide technical review for "Phoenix"
--       design.\nHappy Face Conference Room. Phoenix design team
--       MUST attend this meeting.\nRSVP to team leader.
-- @
data Description = Description
  { descriptionContents :: !Text,
    descriptionAlternateTextRepresentation :: !(Maybe AlternateTextRepresentation),
    descriptionLanguage :: !(Maybe Language)
  }
  deriving (Show, Eq, Ord, Generic)

instance IsString Description where
  fromString = makeDescription . fromString

instance Validity Description

instance NFData Description

instance IsProperty Description where
  propertyName Proxy = "DESCRIPTION"
  propertyP clv = do
    descriptionAlternateTextRepresentation <- conformMapError (PropertyTypeParseError . ParameterParseError) $ optionalParam $ contentLineValueParams clv
    descriptionLanguage <- conformMapError (PropertyTypeParseError . ParameterParseError) $ optionalParam $ contentLineValueParams clv
    wrapPropertyTypeP (\descriptionContents -> Description {..}) clv
  propertyB Description {..} =
    insertMParam descriptionAlternateTextRepresentation $
      insertMParam descriptionLanguage $
        propertyTypeB descriptionContents

makeDescription :: Text -> Description
makeDescription descriptionContents =
  let descriptionAlternateTextRepresentation = Nothing
      descriptionLanguage = Nothing
   in Description {..}

-- | Geographic Position
--
-- === [section 3.8.1.6](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.6)
--
-- @
-- Property Name:  GEO
--
-- Purpose:  This property specifies information related to the global
--    position for the activity specified by a calendar component.
--
--
-- Value Type:  FLOAT.  The value MUST be two SEMICOLON-separated FLOAT
--    values.
--
-- Property Parameters:  IANA and non-standard property parameters can
--    be specified on this property.
--
-- Conformance:  This property can be specified in "VEVENT" or "VTODO"
--    calendar components.
--
-- Description:  This property value specifies latitude and longitude,
--    in that order (i.e., "LAT LON" ordering).  The longitude
--    represents the location east or west of the prime meridian as a
--    positive or negative real number, respectively.  The longitude and
--    latitude values MAY be specified up to six decimal places, which
--    will allow for accuracy to within one meter of geographical
--    position.  Receiving applications MUST accept values of this
--    precision and MAY truncate values of greater precision.
--
--    Values for latitude and longitude shall be expressed as decimal
--    fractions of degrees.  Whole degrees of latitude shall be
--    represented by a two-digit decimal number ranging from 0 through
--    90.  Whole degrees of longitude shall be represented by a decimal
--    number ranging from 0 through 180.  When a decimal fraction of a
--    degree is specified, it shall be separated from the whole number
--    of degrees by a decimal point.
--
--    Latitudes north of the equator shall be specified by a plus sign
--    (+), or by the absence of a minus sign (-), preceding the digits
--    designating degrees.  Latitudes south of the Equator shall be
--    designated by a minus sign (-) preceding the digits designating
--    degrees.  A point on the Equator shall be assigned to the Northern
--    Hemisphere.
--
--    Longitudes east of the prime meridian shall be specified by a plus
--    sign (+), or by the absence of a minus sign (-), preceding the
--    digits designating degrees.  Longitudes west of the meridian shall
--    be designated by minus sign (-) preceding the digits designating
--    degrees.  A point on the prime meridian shall be assigned to the
--    Eastern Hemisphere.  A point on the 180th meridian shall be
--    assigned to the Western Hemisphere.  One exception to this last
--    convention is permitted.  For the special condition of describing
--    a band of latitude around the earth, the East Bounding Coordinate
--    data element shall be assigned the value +180 (180) degrees.
--
--    Any spatial address with a latitude of +90 (90) or -90 degrees
--    will specify the position at the North or South Pole,
--    respectively.  The component for longitude may have any legal
--    value.
--
--
--    With the exception of the special condition described above, this
--    form is specified in [ANSI INCITS 61-1986].
--
--    The simple formula for converting degrees-minutes-seconds into
--    decimal degrees is:
--
--    decimal = degrees + minutes/60 + seconds/3600.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     geo        = "GEO" geoparam ":" geovalue CRLF
--
--     geoparam   = *(";" other-param)
--
--     geovalue   = float ";" float
--     ;Latitude and Longitude components
--
-- Example:  The following is an example of this property:
--
--     GEO:37.386013;-122.082932
-- @
data GeographicPosition = GeographicPosition
  { geographicPositionLat :: !Double,
    geographicPositionLon :: !Double
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity GeographicPosition where
  validate gp@GeographicPosition {..} =
    mconcat
      [ genericValidate gp,
        declare "The latitude is between -90 and 90" $ -90 < geographicPositionLat && geographicPositionLat <= 90,
        validateNotNaN geographicPositionLon,
        validateNotInfinite geographicPositionLon
      ]

instance NFData GeographicPosition

instance IsProperty GeographicPosition where
  propertyName Proxy = "GEO"
  propertyP = viaPropertyTypeP (\t -> maybe (unfixableError $ UnReadableGeographicPosition t) pure $ parseGeographicPosition t)
  propertyB = mkSimpleContentLineValue . renderGeographicPosition

parseGeographicPosition :: Text -> Maybe GeographicPosition
parseGeographicPosition t = case T.splitOn ";" t of
  [latText, lonText] -> do
    geographicPositionLat <- readMaybe (T.unpack latText)
    geographicPositionLon <- readMaybe (T.unpack lonText)
    pure GeographicPosition {..}
  _ -> Nothing

renderGeographicPosition :: GeographicPosition -> Text
renderGeographicPosition GeographicPosition {..} =
  T.pack $
    (concat :: [String] -> String)
      [ show geographicPositionLat,
        ";" :: String,
        show geographicPositionLon
      ]

-- | Last Modified
--
-- === [section 3.8.7.3](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.7.3)
--
-- @
-- Property Name:  LAST_MODIFIED
--
-- Purpose:  This property specifies the date and time that the
--    information associated with the calendar component was last
--    revised in the calendar store.
--
--       Note: This is analogous to the modification date and time for a
--       file in the file system.
--
-- Value Type:  DATE-TIME
--
-- Property Parameters:  IANA and non-standard property parameters can
--    be specified on this property.
--
-- Conformance:  This property can be specified in the "VEVENT",
--    "VTODO", "VJOURNAL", or "VTIMEZONE" calendar components.
--
-- Description:  The property value MUST be specified in the UTC time
--    format.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     last-mod   = "LAST-MODIFIED" lstparam ":" date-time CRLF
--
--     lstparam   = *(";" other-param)
--
-- Example:  The following is an example of this property:
--
--     LAST-MODIFIED:19960817T133000Z
-- @
newtype LastModified = LastModified {unLastModified :: Time.UTCTime}
  deriving (Show, Eq, Ord, Generic)

instance Validity LastModified where
  validate c@LastModified {..} =
    mconcat
      [ genericValidate c,
        validateImpreciseUTCTime unLastModified
      ]

instance NFData LastModified

instance IsProperty LastModified where
  propertyName Proxy = "LAST-MODIFIED"
  propertyP = fmap LastModified . conformMapError PropertyTypeParseError . dateTimeUTCP
  propertyB = dateTimeUTCB . unLastModified

-- | Location
--
-- === [section 3.8.1.7](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.7)
--
-- @
-- Property Name:  LOCATION
--
-- Purpose:  This property defines the intended venue for the activity
--    defined by a calendar component.
--
-- Value Type:  TEXT
--
-- Property Parameters:  IANA, non-standard, alternate text
--    representation, and language property parameters can be specified
--    on this property.
--
-- Conformance:  This property can be specified in "VEVENT" or "VTODO"
--    calendar component.
--
-- Description:  Specific venues such as conference or meeting rooms may
--    be explicitly specified using this property.  An alternate
--    representation may be specified that is a URI that points to
--    directory information with more structured specification of the
--    location.  For example, the alternate representation may specify
--    either an LDAP URL [RFC4516] pointing to an LDAP server entry or a
--    CID URL [RFC2392] pointing to a MIME body part containing a
--    Virtual-Information Card (vCard) [RFC2426] for the location.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     location   = "LOCATION"  locparam ":" text CRLF
--
--     locparam   = *(
--                ;
--                ; The following are OPTIONAL,
--                ; but MUST NOT occur more than once.
--                ;
--                (";" altrepparam) / (";" languageparam) /
--                ;
--                ; The following is OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                (";" other-param)
--                ;
--                )
--
-- Example:  The following are some examples of this property:
--
--     LOCATION:Conference Room - F123\, Bldg. 002
--
--     LOCATION;ALTREP="http://xyzcorp.com/conf-rooms/f123.vcf":
--      Conference Room - F123\, Bldg. 002
-- @
-- TODO add support for alternative representation and language
newtype Location = Location {unLocation :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity Location

instance NFData Location

instance IsProperty Location where
  propertyName Proxy = "LOCATION"
  propertyP = wrapPropertyTypeP Location
  propertyB = propertyTypeB . unLocation

-- | Status
--
-- === [section 3.8.1.11](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.11)
--
-- @
-- Property Name:  STATUS
--
-- Purpose:  This property defines the overall status or confirmation
--    for the calendar component.
--
-- Value Type:  TEXT
--
-- Property Parameters:  IANA and non-standard property parameters can
--    be specified on this property.
--
-- Conformance:  This property can be specified once in "VEVENT",
--    "VTODO", or "VJOURNAL" calendar components.
--
-- Description:  In a group-scheduled calendar component, the property
--    is used by the "Organizer" to provide a confirmation of the event
--    to the "Attendees".  For example in a "VEVENT" calendar component,
--    the "Organizer" can indicate that a meeting is tentative,
--    confirmed, or cancelled.  In a "VTODO" calendar component, the
--    "Organizer" can indicate that an action item needs action, is
--    completed, is in process or being worked on, or has been
--    cancelled.  In a "VJOURNAL" calendar component, the "Organizer"
--    can indicate that a journal entry is draft, final, or has been
--    cancelled or removed.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     status          = "STATUS" statparam ":" statvalue CRLF
--
--     statparam       = *(";" other-param)
--
--     statvalue       = (statvalue-event
--                     /  statvalue-todo
--                     /  statvalue-jour)
--
--     statvalue-event = "TENTATIVE"    ;Indicates event is tentative.
--                     / "CONFIRMED"    ;Indicates event is definite.
--                     / "CANCELLED"    ;Indicates event was cancelled.
--     ;Status values for a "VEVENT"
--
--     statvalue-todo  = "NEEDS-ACTION" ;Indicates to-do needs action.
--                     / "COMPLETED"    ;Indicates to-do completed.
--                     / "IN-PROCESS"   ;Indicates to-do in process of.
--                     / "CANCELLED"    ;Indicates to-do was cancelled.
--     ;Status values for "VTODO".
--
--     statvalue-jour  = "DRAFT"        ;Indicates journal is draft.
--                     / "FINAL"        ;Indicates journal is final.
--                     / "CANCELLED"    ;Indicates journal is removed.
--     ;Status values for "VJOURNAL".
--
-- Example:  The following is an example of this property for a "VEVENT"
--    calendar component:
--
--     STATUS:TENTATIVE
--
--    The following is an example of this property for a "VTODO"
--    calendar component:
--
--     STATUS:NEEDS-ACTION
--
--    The following is an example of this property for a "VJOURNAL"
--    calendar component:
--
--     STATUS:DRAFT
-- @
data Status
  = StatusTentative
  | StatusConfirmed
  | StatusCancelled
  deriving (Show, Eq, Ord, Generic)

instance Validity Status

instance NFData Status

instance IsProperty Status where
  propertyName Proxy = "STATUS"
  propertyP = viaPropertyTypeP $ \t -> case parseStatus t of
    Nothing -> unfixableError $ UnknownStatus t
    Just s -> pure s
  propertyB = propertyTypeB . renderStatus

parseStatus :: Text -> Maybe Status
parseStatus = \case
  "TENTATIVE" -> pure StatusTentative
  "CONFIRMED" -> pure StatusConfirmed
  "CANCELLED" -> pure StatusCancelled
  _ -> Nothing

renderStatus :: Status -> Text
renderStatus = \case
  StatusTentative -> "TENTATIVE"
  StatusConfirmed -> "CONFIRMED"
  StatusCancelled -> "CANCELLED"

-- |
--
-- === [section 3.8.2.2](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.2.2)
--
-- @
-- Property Name:  DTEND
--
-- Purpose:  This property specifies the date and time that a calendar
--    component ends.
--
-- Value Type:  The default value type is DATE-TIME.  The value type can
--    be set to a DATE value type.
--
-- Property Parameters:  IANA, non-standard, value data type, and time
--    zone identifier property parameters can be specified on this
--    property.
--
-- Conformance:  This property can be specified in "VEVENT" or
--    "VFREEBUSY" calendar components.
--
-- Description:  Within the "VEVENT" calendar component, this property
--    defines the date and time by which the event ends.  The value type
--    of this property MUST be the same as the "DTSTART" property, and
--    its value MUST be later in time than the value of the "DTSTART"
--    property.  Furthermore, this property MUST be specified as a date
--    with local time if and only if the "DTSTART" property is also
--    specified as a date with local time.
--
--    Within the "VFREEBUSY" calendar component, this property defines
--    the end date and time for the free or busy time information.  The
--    time MUST be specified in the UTC time format.  The value MUST be
--    later in time than the value of the "DTSTART" property.
--
-- Format Definition:  This property is defined by the following
--    notation:
--     dtend      = "DTEND" dtendparam ":" dtendval CRLF
--
--     dtendparam = *(
--                ;
--                ; The following are OPTIONAL,
--                ; but MUST NOT occur more than once.
--                ;
--                (";" "VALUE" "=" ("DATE-TIME" / "DATE")) /
--                (";" tzidparam) /
--                ;
--                ; The following is OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                (";" other-param)
--                ;
--                )
--
--     dtendval   = date-time / date
--     ;Value MUST match value type
--
-- Example:  The following is an example of this property:
--
--     DTEND:19960401T150000Z
--
--     DTEND;VALUE=DATE:19980704
-- @
data DateTimeEnd
  = DateTimeEndDate !Date
  | DateTimeEndDateTime !DateTime
  deriving (Show, Eq, Ord, Generic)

instance Validity DateTimeEnd

instance NFData DateTimeEnd

instance IsProperty DateTimeEnd where
  propertyName Proxy = "DTEND"
  propertyP clv = do
    mValue <- conformMapError (PropertyTypeParseError . ParameterParseError) $ optionalParam $ contentLineValueParams clv
    case mValue of
      Just TypeDateTime -> wrapPropertyTypeP DateTimeEndDateTime clv
      Just TypeDate -> wrapPropertyTypeP DateTimeEndDate clv
      Just _ -> unfixableError $ ValueMismatch "DTEND" mValue (Just TypeDateTime) [TypeDate]
      -- @
      -- Value Type:  The default value type is DATE-TIME.
      -- @
      Nothing -> wrapPropertyTypeP DateTimeEndDateTime clv

  propertyB = \case
    DateTimeEndDateTime dateTime -> propertyTypeB dateTime
    -- @
    -- Value Type:  The default value type is DATE-TIME.
    -- @
    DateTimeEndDate date -> typedPropertyTypeB date

-- | Duration
--
-- === [section 3.8.2.5](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.2.5)
--
-- @
-- Property Name:  DURATION
--
-- Purpose:  This property specifies a positive duration of time.
--
-- Value Type:  DURATION
--
-- Property Parameters:  IANA and non-standard property parameters can
--    be specified on this property.
--
-- Conformance:  This property can be specified in "VEVENT", "VTODO", or
--    "VALARM" calendar components.
--
-- Description:  In a "VEVENT" calendar component the property may be
--    used to specify a duration of the event, instead of an explicit
--    end DATE-TIME.  In a "VTODO" calendar component the property may
--    be used to specify a duration for the to-do, instead of an
--    explicit due DATE-TIME.  In a "VALARM" calendar component the
--    property may be used to specify the delay period prior to
--    repeating an alarm.  When the "DURATION" property relates to a
--    "DTSTART" property that is specified as a DATE value, then the
--    "DURATION" property MUST be specified as a "dur-day" or "dur-week"
--    value.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     duration   = "DURATION" durparam ":" dur-value CRLF
--                  ;consisting of a positive duration of time.
--
--     durparam   = *(";" other-param)
--
-- Example:  The following is an example of this property that specifies
--    an interval of time of one hour and zero minutes and zero seconds:
--
--     DURATION:PT1H0M0S
--
--    The following is an example of this property that specifies an
--    interval of time of 15 minutes.
--
--     DURATION:PT15M
-- @
instance IsProperty Duration where
  propertyName Proxy = "DURATION"
  propertyP = wrapPropertyTypeP id
  propertyB = propertyTypeB

-- | Time Transparency
--
-- === [section 3.8.2.7](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.2.7)
--
-- @
-- Property Name:  TRANSP
--
-- Purpose:  This property defines whether or not an event is
--    transparent to busy time searches.
--
-- Value Type:  TEXT
--
-- Property Parameters:  IANA and non-standard property parameters can
--    be specified on this property.
--
-- Conformance:  This property can be specified once in a "VEVENT"
--    calendar component.
--
-- Description:  Time Transparency is the characteristic of an event
--    that determines whether it appears to consume time on a calendar.
--    Events that consume actual time for the individual or resource
--    associated with the calendar SHOULD be recorded as OPAQUE,
--    allowing them to be detected by free/busy time searches.  Other
--    events, which do not take up the individual's (or resource's) time
--    SHOULD be recorded as TRANSPARENT, making them invisible to free/
--    busy time searches.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     transp     = "TRANSP" transparam ":" transvalue CRLF
--
--     transparam = *(";" other-param)
--
--     transvalue = "OPAQUE"
--                 ;Blocks or opaque on busy time searches.
--                 / "TRANSPARENT"
--                 ;Transparent on busy time searches.
--     ;Default value is OPAQUE
--
-- Example:  The following is an example of this property for an event
--    that is transparent or does not block on free/busy time searches:
--
--     TRANSP:TRANSPARENT
--
--    The following is an example of this property for an event that is
--    opaque or blocks on free/busy time searches:
--
--     TRANSP:OPAQUE
-- @
data Transparency
  = TransparencyTransparent
  | TransparencyOpaque
  deriving (Show, Eq, Ord, Generic)

instance Validity Transparency

instance NFData Transparency

instance IsProperty Transparency where
  propertyName Proxy = "TRANSP"
  propertyP = viaPropertyTypeP $ \t -> case parseTransparency t of
    Nothing -> unfixableError $ UnknownTransparency t
    Just s -> pure s
  propertyB = propertyTypeB . renderTransparency

parseTransparency :: Text -> Maybe Transparency
parseTransparency = \case
  "TRANSPARENT" -> pure TransparencyTransparent
  "OPAQUE" -> pure TransparencyOpaque
  _ -> Nothing

renderTransparency :: Transparency -> Text
renderTransparency = \case
  TransparencyTransparent -> "TRANSPARENT"
  TransparencyOpaque -> "OPAQUE"

-- | Default 'Transparency'
--
-- @
-- ;Default value is OPAQUE
-- @
defaultTransparency :: Transparency
defaultTransparency = TransparencyOpaque

-- | Uniform Resource Locator
--
-- === [section 3.8.2.5](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.2.5)
-- @
-- Property Name:  URL
--
-- Purpose:  This property defines a Uniform Resource Locator (URL)
--    associated with the iCalendar object.
--
-- Value Type:  URI
--
-- Property Parameters:  IANA and non-standard property parameters can
--    be specified on this property.
--
-- Conformance:  This property can be specified once in the "VEVENT",
--    "VTODO", "VJOURNAL", or "VFREEBUSY" calendar components.
--
-- Description:  This property may be used in a calendar component to
--    convey a location where a more dynamic rendition of the calendar
--    information associated with the calendar component can be found.
--    This memo does not attempt to standardize the form of the URI, nor
--    the format of the resource pointed to by the property value.  If
--    the URL property and Content-Location MIME header are both
--    specified, they MUST point to the same resource.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     url        = "URL" urlparam ":" uri CRLF
--
--     urlparam   = *(";" other-param)
--
-- Example:  The following is an example of this property:
--
--     URL:http://example.com/pub/calendars/jsmith/mytime.ics
-- @
newtype URL = URL {unURL :: URI}
  deriving (Show, Eq, Ord, Generic)

instance Validity URL

instance NFData URL

instance IsProperty URL where
  propertyName Proxy = "URL"
  propertyP = wrapPropertyTypeP URL
  propertyB = propertyTypeB . unURL

-- TODO description
newtype TimeZoneName = TimeZoneName {unTimeZoneName :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity TimeZoneName

instance NFData TimeZoneName

instance IsProperty TimeZoneName where
  propertyName Proxy = "TZNAME"
  propertyP = wrapPropertyTypeP TimeZoneName
  propertyB = propertyTypeB . unTimeZoneName

-- @
-- Property Name:  COMMENT
--
-- Purpose:  This property specifies non-processing information intended
--    to provide a comment to the calendar user.
--
-- Value Type:  TEXT
--
-- Property Parameters:  IANA, non-standard, alternate text
--    representation, and language property parameters can be specified
--    on this property.
-- Conformance:  This property can be specified multiple times in
--    "VEVENT", "VTODO", "VJOURNAL", and "VFREEBUSY" calendar components
--    as well as in the "STANDARD" and "DAYLIGHT" sub-components.
--
-- Description:  This property is used to specify a comment to the
--    calendar user.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     comment    = "COMMENT" commparam ":" text CRLF
--
--     commparam  = *(
--                ;
--                ; The following are OPTIONAL,
--                ; but MUST NOT occur more than once.
--                ;
--                (";" altrepparam) / (";" languageparam) /
--                ;
--                ; The following is OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                (";" other-param)
--                ;
--                )
--
-- Example:  The following is an example of this property:
--
--     COMMENT:The meeting really needs to include both ourselves
--       and the customer. We can't hold this meeting without them.
--       As a matter of fact\, the venue for the meeting ought to be at
--       their site. - - John
-- @
data Comment = Comment
  { commentContents :: Text,
    commentAlternateTextRepresentation :: !(Maybe AlternateTextRepresentation),
    commentLanguage :: !(Maybe Language)
  }
  deriving (Show, Eq, Ord, Generic)

instance IsString Comment where
  fromString = makeComment . fromString

instance Validity Comment

instance NFData Comment

instance IsProperty Comment where
  propertyName Proxy = "COMMENT"
  propertyP clv = do
    commentAlternateTextRepresentation <- conformMapError (PropertyTypeParseError . ParameterParseError) $ optionalParam $ contentLineValueParams clv
    commentLanguage <- conformMapError (PropertyTypeParseError . ParameterParseError) $ optionalParam $ contentLineValueParams clv
    wrapPropertyTypeP (\commentContents -> Comment {..}) clv
  propertyB Comment {..} =
    insertMParam commentAlternateTextRepresentation $
      insertMParam commentLanguage $
        propertyTypeB commentContents

makeComment :: Text -> Comment
makeComment commentContents =
  let commentAlternateTextRepresentation = Nothing
      commentLanguage = Nothing
   in Comment {..}

-- | Timezone Offset From
--
-- === [section 3.8.3.3](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.3.3)
--
-- @
-- Property Name:  TZOFFSETFROM
--
-- Purpose:  This property specifies the offset that is in use prior to
--    this time zone observance.
--
-- Value Type:  UTC-OFFSET
--
-- Property Parameters:  IANA and non-standard property parameters can
--    be specified on this property.
--
-- Conformance:  This property MUST be specified in "STANDARD" and
--    "DAYLIGHT" sub-components.
--
-- Description:  This property specifies the offset that is in use prior
--    to this time observance.  It is used to calculate the absolute
--    time at which the transition to a given observance takes place.
--    This property MUST only be specified in a "VTIMEZONE" calendar
--    component.  A "VTIMEZONE" calendar component MUST include this
--    property.  The property value is a signed numeric indicating the
--    number of hours and possibly minutes from UTC.  Positive numbers
--    represent time zones east of the prime meridian, or ahead of UTC.
--    Negative numbers represent time zones west of the prime meridian,
--    or behind UTC.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     tzoffsetfrom       = "TZOFFSETFROM" frmparam ":" utc-offset
--                          CRLF
--
--     frmparam   = *(";" other-param)
--
-- Example:  The following are examples of this property:
--
--     TZOFFSETFROM:-0500
--
--     TZOFFSETFROM:+1345
-- @
newtype TimeZoneOffsetFrom = TimeZoneOffsetFrom {unTimeZoneOffsetFrom :: UTCOffset}
  deriving (Show, Eq, Ord, Generic)

instance Validity TimeZoneOffsetFrom

instance NFData TimeZoneOffsetFrom

instance IsProperty TimeZoneOffsetFrom where
  propertyName Proxy = "TZOFFSETFROM"
  propertyP = wrapPropertyTypeP TimeZoneOffsetFrom
  propertyB = propertyTypeB . unTimeZoneOffsetFrom

-- | Timezone Offset To
--
-- === [section 3.8.3.4](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.3.4)
--
-- @
-- Property Name:  TZOFFSETTO
--
-- Purpose:  This property specifies the offset that is in use in this
--    time zone observance.
--
-- Value Type:  UTC-OFFSET
--
-- Property Parameters:  IANA and non-standard property parameters can
--    be specified on this property.
--
-- Conformance:  This property MUST be specified in "STANDARD" and
--    "DAYLIGHT" sub-components.
--
-- Description:  This property specifies the offset that is in use in
--    this time zone observance.  It is used to calculate the absolute
--    time for the new observance.  The property value is a signed
--    numeric indicating the number of hours and possibly minutes from
--    UTC.  Positive numbers represent time zones east of the prime
--    meridian, or ahead of UTC.  Negative numbers represent time zones
--    west of the prime meridian, or behind UTC.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     tzoffsetto = "TZOFFSETTO" toparam ":" utc-offset CRLF
--
--     toparam    = *(";" other-param)
--
-- Example:  The following are examples of this property:
--
--     TZOFFSETTO:-0400
--
--     TZOFFSETTO:+1245
--
-- @
newtype TimeZoneOffsetTo = TimeZoneOffsetTo {unTimeZoneOffsetTo :: UTCOffset}
  deriving (Show, Eq, Ord, Generic)

instance Validity TimeZoneOffsetTo

instance NFData TimeZoneOffsetTo

instance IsProperty TimeZoneOffsetTo where
  propertyName Proxy = "TZOFFSETTO"
  propertyP = wrapPropertyTypeP TimeZoneOffsetTo
  propertyB = propertyTypeB . unTimeZoneOffsetTo

-- | Attendee
--
-- === [section 3.8.4.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.4.1)
--
-- @
-- Property Name:  ATTENDEE
--
-- Purpose:  This property defines an "Attendee" within a calendar
--    component.
--
-- Value Type:  CAL-ADDRESS
--
-- Property Parameters:  IANA, non-standard, language, calendar user
--    type, group or list membership, participation role, participation
--    status, RSVP expectation, delegatee, delegator, sent by, common
--    name, or directory entry reference property parameters can be
--    specified on this property.
--
-- Conformance:  This property MUST be specified in an iCalendar object
--    that specifies a group-scheduled calendar entity.  This property
--    MUST NOT be specified in an iCalendar object when publishing the
--    calendar information (e.g., NOT in an iCalendar object that
--    specifies the publication of a calendar user's busy time, event,
--    to-do, or journal).  This property is not specified in an
--    iCalendar object that specifies only a time zone definition or
--    that defines calendar components that are not group-scheduled
--    components, but are components only on a single user's calendar.
--
-- Description:  This property MUST only be specified within calendar
--    components to specify participants, non-participants, and the
--    chair of a group-scheduled calendar entity.  The property is
--    specified within an "EMAIL" category of the "VALARM" calendar
--    component to specify an email address that is to receive the email
--    type of iCalendar alarm.
--
--    The property parameter "CN" is for the common or displayable name
--    associated with the calendar address; "ROLE", for the intended
--    role that the attendee will have in the calendar component;
--    "PARTSTAT", for the status of the attendee's participation;
--    "RSVP", for indicating whether the favor of a reply is requested;
--    "CUTYPE", to indicate the type of calendar user; "MEMBER", to
--    indicate the groups that the attendee belongs to; "DELEGATED-TO",
--    to indicate the calendar users that the original request was
--    delegated to; and "DELEGATED-FROM", to indicate whom the request
--    was delegated from; "SENT-BY", to indicate whom is acting on
--    behalf of the "ATTENDEE"; and "DIR", to indicate the URI that
--    points to the directory information corresponding to the attendee.
--    These property parameters can be specified on an "ATTENDEE"
--    property in either a "VEVENT", "VTODO", or "VJOURNAL" calendar
--    component.  They MUST NOT be specified in an "ATTENDEE" property
--    in a "VFREEBUSY" or "VALARM" calendar component.  If the
--
--    "LANGUAGE" property parameter is specified, the identified
--    language applies to the "CN" parameter.
--
--    A recipient delegated a request MUST inherit the "RSVP" and "ROLE"
--    values from the attendee that delegated the request to them.
--
--    Multiple attendees can be specified by including multiple
--    "ATTENDEE" properties within the calendar component.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     attendee   = "ATTENDEE" attparam ":" cal-address CRLF
--
--     attparam   = *(
--                ;
--                ; The following are OPTIONAL,
--                ; but MUST NOT occur more than once.
--                ;
--                (";" cutypeparam) / (";" memberparam) /
--                (";" roleparam) / (";" partstatparam) /
--                (";" rsvpparam) / (";" deltoparam) /
--                (";" delfromparam) / (";" sentbyparam) /
--                (";" cnparam) / (";" dirparam) /
--                (";" languageparam) /
--                ;
--                ; The following is OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                (";" other-param)
--                ;
--                )
--
-- Example:  The following are examples of this property's use for a
--    to-do:
--
--     ATTENDEE;MEMBER="mailto:DEV-GROUP@example.com":
--      mailto:joecool@example.com
--     ATTENDEE;DELEGATED-FROM="mailto:immud@example.com":
--      mailto:ildoit@example.com
--
--    The following is an example of this property used for specifying
--    multiple attendees to an event:
--
--     ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=TENTATIVE;CN=Henry
--      Cabot:mailto:hcabot@example.com
--     ATTENDEE;ROLE=REQ-PARTICIPANT;DELEGATED-FROM="mailto:bob@
--      example.com";PARTSTAT=ACCEPTED;CN=Jane Doe:mailto:jdoe@
--      example.com
--
--    The following is an example of this property with a URI to the
--    directory information associated with the attendee:
--
--     ATTENDEE;CN=John Smith;DIR="ldap://example.com:6666/o=ABC%
--      20Industries,c=US???(cn=Jim%20Dolittle)":mailto:jimdo@
--      example.com
--
--    The following is an example of this property with "delegatee" and
--    "delegator" information for an event:
--
--     ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=TENTATIVE;DELEGATED-FROM=
--      "mailto:iamboss@example.com";CN=Henry Cabot:mailto:hcabot@
--      example.com
--     ATTENDEE;ROLE=NON-PARTICIPANT;PARTSTAT=DELEGATED;DELEGATED-TO=
--      "mailto:hcabot@example.com";CN=The Big Cheese:mailto:iamboss
--      @example.com
--     ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=ACCEPTED;CN=Jane Doe
--      :mailto:jdoe@example.com
--
-- Example:  The following is an example of this property's use when
--    another calendar user is acting on behalf of the "Attendee":
--
--     ATTENDEE;SENT-BY=mailto:jan_doe@example.com;CN=John Smith:
--      mailto:jsmith@example.com
-- @
data Attendee = Attendee
  { attendeeCalAddress :: !CalAddress,
    attendeeParticipationRole :: !ParticipationRole,
    attendeeParticipationStatus :: !ParticipationStatus,
    attendeeRSVPExpectation :: !RSVPExpectation,
    attendeeCommonName :: !(Maybe CommonName)
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Attendee

instance NFData Attendee

instance IsProperty Attendee where
  propertyName Proxy = "ATTENDEE"
  propertyP clv = flip viaPropertyTypeP clv $ \attendeeCalAddress -> do
    attendeeParticipationRole <-
      fromMaybe defaultParticipationRole
        <$> conformMapError (PropertyTypeParseError . ParameterParseError) (optionalParam (contentLineValueParams clv))
    attendeeParticipationStatus <-
      fromMaybe defaultParticipationStatus
        <$> conformMapError (PropertyTypeParseError . ParameterParseError) (optionalParam (contentLineValueParams clv))
    attendeeRSVPExpectation <-
      fromMaybe defaultRSVPExpectation
        <$> conformMapError (PropertyTypeParseError . ParameterParseError) (optionalParam (contentLineValueParams clv))
    attendeeCommonName <-
      conformMapError (PropertyTypeParseError . ParameterParseError) (optionalParam (contentLineValueParams clv))

    pure Attendee {..}
  propertyB Attendee {..} =
    insertParamWithDefault defaultParticipationStatus attendeeParticipationStatus $
      insertParamWithDefault defaultParticipationRole attendeeParticipationRole $
        insertParamWithDefault defaultRSVPExpectation attendeeRSVPExpectation $
          insertMParam attendeeCommonName $
            propertyTypeB attendeeCalAddress

mkAttendee :: CalAddress -> Attendee
mkAttendee calAddress =
  Attendee
    { attendeeCalAddress = calAddress,
      attendeeParticipationRole = defaultParticipationRole,
      attendeeParticipationStatus = defaultParticipationStatus,
      attendeeRSVPExpectation = defaultRSVPExpectation,
      attendeeCommonName = Nothing
    }

-- | Exception Date-Times
--
-- === [section 3.8.5.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.5.1)
--
-- @
-- Property Name:  EXDATE
--
-- Purpose:  This property defines the list of DATE-TIME exceptions for
--    recurring events, to-dos, journal entries, or time zone
--    definitions.
--
-- Value Type:  The default value type for this property is DATE-TIME.
--    The value type can be set to DATE.
--
-- Property Parameters:  IANA, non-standard, value data type, and time
--    zone identifier property parameters can be specified on this
--    property.
--
-- Conformance:  This property can be specified in recurring "VEVENT",
--    "VTODO", and "VJOURNAL" calendar components as well as in the
--    "STANDARD" and "DAYLIGHT" sub-components of the "VTIMEZONE"
--    calendar component.
--
-- Description:  The exception dates, if specified, are used in
--    computing the recurrence set.  The recurrence set is the complete
--    set of recurrence instances for a calendar component.  The
--    recurrence set is generated by considering the initial "DTSTART"
--    property along with the "RRULE", "RDATE", and "EXDATE" properties
--    contained within the recurring component.  The "DTSTART" property
--    defines the first instance in the recurrence set.  The "DTSTART"
--    property value SHOULD match the pattern of the recurrence rule, if
--    specified.  The recurrence set generated with a "DTSTART" property
--    value that doesn't match the pattern of the rule is undefined.
--    The final recurrence set is generated by gathering all of the
--    start DATE-TIME values generated by any of the specified "RRULE"
--    and "RDATE" properties, and then excluding any start DATE-TIME
--    values specified by "EXDATE" properties.  This implies that start
--    DATE-TIME values specified by "EXDATE" properties take precedence
--    over those specified by inclusion properties (i.e., "RDATE" and
--    "RRULE").  When duplicate instances are generated by the "RRULE"
--    and "RDATE" properties, only one recurrence is considered.
--    Duplicate instances are ignored.
--
--    The "EXDATE" property can be used to exclude the value specified
--    in "DTSTART".  However, in such cases, the original "DTSTART" date
--    MUST still be maintained by the calendaring and scheduling system
--    because the original "DTSTART" value has inherent usage
--    dependencies by other properties such as the "RECURRENCE-ID".
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     exdate     = "EXDATE" exdtparam ":" exdtval *("," exdtval) CRLF
--
--     exdtparam  = *(
--                ;
--                ; The following are OPTIONAL,
--                ; but MUST NOT occur more than once.
--                ;
--                (";" "VALUE" "=" ("DATE-TIME" / "DATE")) /
--                ;
--                (";" tzidparam) /
--                ;
--                ; The following is OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                (";" other-param)
--                ;
--                )
--
--     exdtval    = date-time / date
--     ;Value MUST match value type
--
-- Example:  The following is an example of this property:
--
--     EXDATE:19960402T010000Z,19960403T010000Z,19960404T010000Z
-- @
--
-- TODO check this SHOULD:
-- @
--    defines the first instance in the recurrence set.  The "DTSTART"
--    property value SHOULD match the pattern of the recurrence rule, if
-- @
data ExceptionDateTimes
  = ExceptionDateTimes !DateTimes
  | ExceptionDates !(Set Date)
  deriving (Show, Eq, Ord, Generic)

instance Validity ExceptionDateTimes

instance NFData ExceptionDateTimes

instance IsProperty ExceptionDateTimes where
  propertyName Proxy = "EXDATE"
  propertyP clv = do
    mValue <- conformMapError (PropertyTypeParseError . ParameterParseError) $ optionalParam $ contentLineValueParams clv
    -- @
    -- Value Type:  The default value type for this property is DATE-TIME.
    -- @
    case mValue of
      Just TypeDateTime -> wrapPropertyTypeP ExceptionDateTimes clv
      Just TypeDate -> wrapPropertyTypeP ExceptionDates clv
      Just _ -> unfixableError $ ValueMismatch "EXDATE" mValue (Just TypeDateTime) [TypeDate]
      Nothing -> wrapPropertyTypeP ExceptionDateTimes clv

  propertyB = \case
    ExceptionDateTimes dts -> propertyTypeB dts
    -- @
    -- Value Type:  The default value type for this property is DATE-TIME.
    -- @
    ExceptionDates ds -> typedPropertyTypeB ds

-- | Recurrence Date-Times
--
-- === [section 3.8.5.2](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.5.2)
--
-- @
-- Property Name:  RDATE
--
-- Purpose:  This property defines the list of DATE-TIME values for
--    recurring events, to-dos, journal entries, or time zone
--    definitions.
--
-- Value Type:  The default value type for this property is DATE-TIME.
--    The value type can be set to DATE or PERIOD.
--
-- Property Parameters:  IANA, non-standard, value data type, and time
--    zone identifier property parameters can be specified on this
--    property.
--
-- Conformance:  This property can be specified in recurring "VEVENT",
--    "VTODO", and "VJOURNAL" calendar components as well as in the
--    "STANDARD" and "DAYLIGHT" sub-components of the "VTIMEZONE"
--    calendar component.
--
-- Description:  This property can appear along with the "RRULE"
--    property to define an aggregate set of repeating occurrences.
--    When they both appear in a recurring component, the recurrence
--
--    instances are defined by the union of occurrences defined by both
--    the "RDATE" and "RRULE".
--
--    The recurrence dates, if specified, are used in computing the
--    recurrence set.  The recurrence set is the complete set of
--    recurrence instances for a calendar component.  The recurrence set
--    is generated by considering the initial "DTSTART" property along
--    with the "RRULE", "RDATE", and "EXDATE" properties contained
--    within the recurring component.  The "DTSTART" property defines
--    the first instance in the recurrence set.  The "DTSTART" property
--    value SHOULD match the pattern of the recurrence rule, if
--    specified.  The recurrence set generated with a "DTSTART" property
--    value that doesn't match the pattern of the rule is undefined.
--    The final recurrence set is generated by gathering all of the
--    start DATE-TIME values generated by any of the specified "RRULE"
--    and "RDATE" properties, and then excluding any start DATE-TIME
--    values specified by "EXDATE" properties.  This implies that start
--    DATE-TIME values specified by "EXDATE" properties take precedence
--    over those specified by inclusion properties (i.e., "RDATE" and
--    "RRULE").  Where duplicate instances are generated by the "RRULE"
--    and "RDATE" properties, only one recurrence is considered.
--    Duplicate instances are ignored.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     rdate      = "RDATE" rdtparam ":" rdtval *("," rdtval) CRLF
--
--     rdtparam   = *(
--                ;
--                ; The following are OPTIONAL,
--                ; but MUST NOT occur more than once.
--                ;
--                (";" "VALUE" "=" ("DATE-TIME" / "DATE" / "PERIOD")) /
--                (";" tzidparam) /
--                ;
--                ; The following is OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                (";" other-param)
--                ;
--                )
--
--     rdtval     = date-time / date / period
--     ;Value MUST match value type
--
--
--
--
-- Example:  The following are examples of this property:
--
--     RDATE:19970714T123000Z
--     RDATE;TZID=America/New_York:19970714T083000
--
--     RDATE;VALUE=PERIOD:19960403T020000Z/19960403T040000Z,
--      19960404T010000Z/PT3H
--
--     RDATE;VALUE=DATE:19970101,19970120,19970217,19970421
--      19970526,19970704,19970901,19971014,19971128,19971129,19971225
-- @
data RecurrenceDateTimes
  = RecurrenceDateTimes !DateTimes
  | RecurrenceDates !(Set Date)
  | RecurrencePeriods !(Set Period)
  deriving (Show, Eq, Ord, Generic)

instance Validity RecurrenceDateTimes

instance NFData RecurrenceDateTimes

instance IsProperty RecurrenceDateTimes where
  propertyName Proxy = "RDATE"
  propertyP clv = do
    mValue <- conformMapError (PropertyTypeParseError . ParameterParseError) $ optionalParam $ contentLineValueParams clv
    case mValue of
      Just TypeDateTime -> wrapPropertyTypeP RecurrenceDateTimes clv
      Just TypePeriod -> wrapPropertyTypeP RecurrencePeriods clv
      Just TypeDate -> wrapPropertyTypeP RecurrenceDates clv
      Just _ -> unfixableError $ ValueMismatch "RDATE" mValue (Just TypeDateTime) [TypePeriod, TypeDate]
      -- @
      -- Value Type:  The default value type for this property is DATE-TIME.
      -- @
      Nothing -> wrapPropertyTypeP RecurrenceDateTimes clv

  propertyB = \case
    RecurrenceDateTimes dts -> propertyTypeB dts
    -- @
    -- Value Type:  The default value type for this property is DATE-TIME.
    -- @
    RecurrenceDates ds -> typedPropertyTypeB ds
    RecurrencePeriods ps -> typedPropertyTypeB ps

-- | Action
--
-- === [section 3.8.6.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.6.1)
--
-- @
-- Property Name:  ACTION
--
-- Purpose:  This property defines the action to be invoked when an
--    alarm is triggered.
--
-- Value Type:  TEXT
--
-- Property Parameters:  IANA and non-standard property parameters can
--    be specified on this property.
--
-- Conformance:  This property MUST be specified once in a "VALARM"
--    calendar component.
--
-- Description:  Each "VALARM" calendar component has a particular type
--    of action with which it is associated.  This property specifies
--    the type of action.  Applications MUST ignore alarms with x-name
--    and iana-token values they don't recognize.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     action      = "ACTION" actionparam ":" actionvalue CRLF
--
--     actionparam = *(";" other-param)
--
--
--     actionvalue = "AUDIO" / "DISPLAY" / "EMAIL"
--                 / iana-token / x-name
--
-- Example:  The following are examples of this property in a "VALARM"
--    calendar component:
--
--     ACTION:AUDIO
--
--     ACTION:DISPLAY
-- @
data Action
  = ActionAudio
  | ActionDisplay
  | ActionEmail
  | ActionOther !Text
  deriving (Show, Eq, Ord, Generic)

instance Validity Action

instance NFData Action

instance IsProperty Action where
  propertyName Proxy = "ACTION"
  propertyP = wrapPropertyTypeP parseAction
  propertyB = propertyTypeB . renderAction

parseAction :: Text -> Action
parseAction = \case
  "AUDIO" -> ActionAudio
  "DISPLAY" -> ActionDisplay
  "EMAIL" -> ActionEmail
  t -> ActionOther t

renderAction :: Action -> Text
renderAction = \case
  ActionAudio -> "AUDIO"
  ActionDisplay -> "DISPLAY"
  ActionEmail -> "EMAIL"
  ActionOther t -> t

-- | Repeat
--
-- === [section 3.8.6.2](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.6.2)
--
-- @
-- Property Name:  REPEAT
--
-- Purpose:  This property defines the number of times the alarm should
--    be repeated, after the initial trigger.
--
-- Value Type:  INTEGER
--
-- Property Parameters:  IANA and non-standard property parameters can
--    be specified on this property.
--
-- Conformance:  This property can be specified in a "VALARM" calendar
--    component.
--
-- Description:  This property defines the number of times an alarm
--    should be repeated after its initial trigger.  If the alarm
--    triggers more than once, then this property MUST be specified
--    along with the "DURATION" property.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     repeat  = "REPEAT" repparam ":" integer CRLF
--     ;Default is "0", zero.
--
--     repparam   = *(";" other-param)
--
-- Example:  The following is an example of this property for an alarm
--    that repeats 4 additional times with a 5-minute delay after the
--    initial triggering of the alarm:
--
--     REPEAT:4
--     DURATION:PT5M
-- @
data Repeat = Repeat {unRepeat :: Int32}
  deriving (Show, Eq, Ord, Generic)

instance Validity Repeat

instance NFData Repeat

instance IsProperty Repeat where
  propertyName Proxy = "REPEAT"
  propertyP = wrapPropertyTypeP Repeat
  propertyB = propertyTypeB . unRepeat

-- | Action
--
-- === [section 3.8.6.3](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.6.3)
--
-- @
-- Property Name:  TRIGGER
--
-- Purpose:  This property specifies when an alarm will trigger.
--
-- Value Type:  The default value type is DURATION.  The value type can
--    be set to a DATE-TIME value type, in which case the value MUST
--    specify a UTC-formatted DATE-TIME value.
-- Property Parameters:  IANA, non-standard, value data type, time zone
--    identifier, or trigger relationship property parameters can be
--    specified on this property.  The trigger relationship property
--    parameter MUST only be specified when the value type is
--    "DURATION".
--
-- Conformance:  This property MUST be specified in the "VALARM"
--    calendar component.
--
-- Description:  This property defines when an alarm will trigger.  The
--    default value type is DURATION, specifying a relative time for the
--    trigger of the alarm.  The default duration is relative to the
--    start of an event or to-do with which the alarm is associated.
--    The duration can be explicitly set to trigger from either the end
--    or the start of the associated event or to-do with the "RELATED"
--    parameter.  A value of START will set the alarm to trigger off the
--    start of the associated event or to-do.  A value of END will set
--    the alarm to trigger off the end of the associated event or to-do.
--
--    Either a positive or negative duration may be specified for the
--    "TRIGGER" property.  An alarm with a positive duration is
--    triggered after the associated start or end of the event or to-do.
--    An alarm with a negative duration is triggered before the
--    associated start or end of the event or to-do.
--
--    The "RELATED" property parameter is not valid if the value type of
--    the property is set to DATE-TIME (i.e., for an absolute date and
--    time alarm trigger).  If a value type of DATE-TIME is specified,
--    then the property value MUST be specified in the UTC time format.
--    If an absolute trigger is specified on an alarm for a recurring
--    event or to-do, then the alarm will only trigger for the specified
--    absolute DATE-TIME, along with any specified repeating instances.
--
--    If the trigger is set relative to START, then the "DTSTART"
--    property MUST be present in the associated "VEVENT" or "VTODO"
--    calendar component.  If an alarm is specified for an event with
--    the trigger set relative to the END, then the "DTEND" property or
--    the "DTSTART" and "DURATION " properties MUST be present in the
--    associated "VEVENT" calendar component.  If the alarm is specified
--    for a to-do with a trigger set relative to the END, then either
--    the "DUE" property or the "DTSTART" and "DURATION " properties
--    MUST be present in the associated "VTODO" calendar component.
--
--    Alarms specified in an event or to-do that is defined in terms of
--    a DATE value type will be triggered relative to 00:00:00 of the
--    user's configured time zone on the specified date, or relative to
--    00:00:00 UTC on the specified date if no configured time zone can
--    be found for the user.  For example, if "DTSTART" is a DATE value
--    set to 19980205 then the duration trigger will be relative to
--    19980205T000000 America/New_York for a user configured with the
--    America/New_York time zone.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     trigger    = "TRIGGER" (trigrel / trigabs) CRLF
--
--     trigrel    = *(
--                ;
--                ; The following are OPTIONAL,
--                ; but MUST NOT occur more than once.
--                ;
--                (";" "VALUE" "=" "DURATION") /
--                (";" trigrelparam) /
--                ;
--                ; The following is OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                (";" other-param)
--                ;
--                ) ":"  dur-value
--
--     trigabs    = *(
--                ;
--                ; The following is REQUIRED,
--                ; but MUST NOT occur more than once.
--                ;
--                (";" "VALUE" "=" "DATE-TIME") /
--                ;
--                ; The following is OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                (";" other-param)
--                ;
--                ) ":" date-time
--
-- Example:  A trigger set 15 minutes prior to the start of the event or
--    to-do.
--
--     TRIGGER:-PT15M
--
--    A trigger set five minutes after the end of an event or the due
--    date of a to-do.
--
--     TRIGGER;RELATED=END:PT5M
--
--    A trigger set to an absolute DATE-TIME.
--
--     TRIGGER;VALUE=DATE-TIME:19980101T050000Z
-- @
data Trigger
  = TriggerDuration !AlarmTriggerRelationship !Duration
  | TriggerDateTime !Time.UTCTime
  deriving (Show, Eq, Ord, Generic)

instance Validity Trigger

instance NFData Trigger

instance IsProperty Trigger where
  propertyName Proxy = "TRIGGER"
  propertyP clv = do
    mValue <- conformMapError (PropertyTypeParseError . ParameterParseError) $ optionalParam $ contentLineValueParams clv
    let parseDurationTrigger = do
          duration <- conformMapError PropertyTypeParseError $ propertyTypeP clv
          relationship <- conformMapError (PropertyTypeParseError . ParameterParseError) $ fromMaybe defaultAlarmTriggerRelationship <$> optionalParam (contentLineValueParams clv)
          pure $ TriggerDuration relationship duration

    case mValue of
      Just TypeDateTime ->
        conformMapError PropertyTypeParseError $ TriggerDateTime <$> dateTimeUTCP clv
      Just TypeDuration -> parseDurationTrigger
      Just _ -> unfixableError $ ValueMismatch "TRIGGER" mValue (Just TypeDuration) [TypeDateTime]
      -- @
      -- Value Type:  The default value type is DURATION.
      -- @
      Nothing -> parseDurationTrigger
  propertyB = \case
    TriggerDuration relationship duration ->
      insertParamWithDefault defaultAlarmTriggerRelationship relationship $
        propertyTypeB duration
    TriggerDateTime date -> typedPropertyTypeB (DateTimeUTC date)

-- | Image
--
-- === [section 5.10 of RFC 7986](https://datatracker.ietf.org/doc/html/rfc7986#section-5.10)
--
-- @
-- Property Name:  IMAGE
--
-- Purpose:  This property specifies an image associated with the
--    calendar or a calendar component.
--
-- Value Type:  URI or BINARY -- no default.  The value MUST be data
--    with a media type of "image" or refer to such data.
--
-- Property Parameters:  IANA, non-standard, display, inline encoding,
--    and value data type property parameters can be specified on this
--    property.  The format type parameter can be specified on this
--    property and is RECOMMENDED for inline binary-encoded content
--    information.
--
-- Conformance:  This property can be specified multiple times in an
--    iCalendar object or in "VEVENT", "VTODO", or "VJOURNAL" calendar
--    components.
--
-- Description:  This property specifies an image for an iCalendar
--    object or a calendar component via a URI or directly with inline
--    data that can be used by calendar user agents when presenting the
--    calendar data to a user.  Multiple properties MAY be used to
--    specify alternative sets of images with, for example, varying
--    media subtypes, resolutions, or sizes.  When multiple properties
--    are present, calendar user agents SHOULD display only one of them,
--    picking one that provides the most appropriate image quality, or
--    display none.  The "DISPLAY" parameter is used to indicate the
--    intended display mode for the image.  The "ALTREP" parameter,
--
--    defined in [RFC5545], can be used to provide a "clickable" image
--    where the URI in the parameter value can be "launched" by a click
--    on the image in the calendar user agent.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
-- image      = "IMAGE" imageparam
--              (
--                (
--                  ";" "VALUE" "=" "URI"
--                  ":" uri
--                ) /
--                (
--                  ";" "ENCODING" "=" "BASE64"
--                  ";" "VALUE" "=" "BINARY"
--                  ":" binary
--                )
--              )
--              CRLF
--
-- imageparam = *(
--               ;
--               ; The following is OPTIONAL for a URI value,
--               ; RECOMMENDED for a BINARY value,
--               ; and MUST NOT occur more than once.
--               ;
--               (";" fmttypeparam) /
--               ;
--               ; The following are OPTIONAL,
--               ; and MUST NOT occur more than once.
--               ;
--               (";" altrepparam) / (";" displayparam) /
--               ;
--               ; The following is OPTIONAL,
--               ; and MAY occur more than once.
--               ;
--               (";" other-param)
--               ;
--               )
--
-- Example:  The following is an example of this property:
--
-- IMAGE;VALUE=URI;DISPLAY=BADGE;FMTTYPE=image/png:h
--  ttp://example.com/images/party.png
-- @
data Image = Image
  { imageContents :: !(Either URI Binary),
    imageFormatType :: !(Maybe FormatType),
    imageDisplay :: !(NonEmpty Display)
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Image

instance NFData Image

instance IsProperty Image where
  propertyName Proxy = "IMAGE"
  propertyP clv = do
    mValue <- conformMapError (PropertyTypeParseError . ParameterParseError) $ optionalParam $ contentLineValueParams clv
    imageFormatType <- conformMapError (PropertyTypeParseError . ParameterParseError) $ optionalParam $ contentLineValueParams clv
    imageDisplay <- fromMaybe defaultDisplay <$> conformMapError (PropertyTypeParseError . ParameterParseError) (optionalParam (contentLineValueParams clv))

    case mValue of
      Just TypeURI -> wrapPropertyTypeP (\u -> let imageContents = Left u in Image {..}) clv
      Just TypeBinary -> wrapPropertyTypeP (\b -> let imageContents = Right b in Image {..}) clv
      Just _ -> unfixableError $ ValueMismatch "IMAGE" mValue Nothing [TypeURI, TypeBinary]
      Nothing -> unfixableError $ ValueMismatch "IMAGE" mValue Nothing [TypeURI, TypeBinary]

  propertyB Image {..} =
    insertParamWithDefault defaultDisplay imageDisplay $
      insertMParam imageFormatType $ case imageContents of
        Left uri -> typedPropertyTypeB uri
        Right binary -> typedPropertyTypeB binary
