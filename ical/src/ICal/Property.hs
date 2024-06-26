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

import Conformance
import Control.DeepSeq
import Control.Exception
import qualified Data.CaseInsensitive as CI
import Data.Int
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
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
import Data.Word
import GHC.Generics (Generic)
import ICal.ContentLine
import ICal.Parameter
import ICal.PropertyType
import Text.Read

data PropertyParseError
  = PropertyTypeParseError !PropertyTypeParseError
  | MismatchedPropertyName
      -- Expected
      !ContentLineName
      -- Actual
      !ContentLineName
  | UnknownCalendarScale !Text
  | UnReadableGeographicPosition !Text
  | UnknownStatus !Text
  | UnknownTransparency !Text
  | ValueMismatch !ContentLineName !(Maybe ValueDataType) (Maybe ValueDataType) ![ValueDataType]
  | UnReadableStatusCode !Text
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
    UnReadableGeographicPosition t -> unwords ["Unreadable Geographic position:", show t]
    UnknownStatus t -> unwords ["Unknown Status:", show t]
    UnknownTransparency t -> unwords ["Unknown Transparency:", show t]
    UnReadableStatusCode t -> unwords ["Unreadable status code:", show t]
    ValueMismatch name actualType mDefaultType otherTypes ->
      unlines
        [ unwords ["Mismatched value type for:", show name],
          unwords ["Actual:", show actualType],
          unwords ["Default:", maybe "none" show mDefaultType],
          unwords ["Other options:", show otherTypes]
        ]

data PropertyFixableError
  = PropertyTypeFixableError !PropertyTypeFixableError
  deriving (Show, Eq, Ord)

instance Exception PropertyFixableError where
  displayException = \case
    PropertyTypeFixableError ptfe -> displayException ptfe

-- | Calendar Properties
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
  propertyP :: ContentLineValue -> Conform PropertyParseError PropertyFixableError Void property

  -- | Builder for the property
  propertyB :: property -> ContentLineValue

propertyContentLineP ::
  forall property.
  (IsProperty property) =>
  ContentLine ->
  Conform PropertyParseError PropertyFixableError Void property
propertyContentLineP ContentLine {..} =
  let name = propertyName (Proxy :: Proxy property)
   in if contentLineName == name
        then propertyP contentLineValue
        else unfixableError $ MismatchedPropertyName name contentLineName

propertyContentLineB :: forall property. (IsProperty property) => property -> ContentLine
propertyContentLineB = ContentLine (propertyName (Proxy :: Proxy property)) . propertyB

viaPropertyTypeP ::
  forall propertyType property.
  (IsPropertyType propertyType) =>
  (propertyType -> Conform PropertyParseError PropertyFixableError Void property) ->
  (ContentLineValue -> Conform PropertyParseError PropertyFixableError Void property)
viaPropertyTypeP func clv = do
  propertyType <- conformMapAll PropertyTypeParseError PropertyTypeFixableError id $ typedPropertyTypeP clv
  func propertyType

wrapPropertyTypeP ::
  (IsPropertyType propertyType) =>
  (propertyType -> property) ->
  (ContentLineValue -> Conform PropertyParseError PropertyFixableError Void property)
wrapPropertyTypeP func = viaPropertyTypeP (pure . func)

viaPropertyTypeListP ::
  forall propertyType property.
  (IsPropertyType propertyType) =>
  ([propertyType] -> Conform PropertyParseError PropertyFixableError Void property) ->
  (ContentLineValue -> Conform PropertyParseError PropertyFixableError Void property)
viaPropertyTypeListP func clv = do
  propertyType <- conformMapAll PropertyTypeParseError PropertyTypeFixableError id $ propertyTypeListP clv
  func propertyType

propertyParamP ::
  (IsParameter param) =>
  ContentLineValue ->
  Conform
    PropertyParseError
    PropertyFixableError
    Void
    (Maybe param)
propertyParamP clv =
  conformMapAll (PropertyTypeParseError . ParameterParseError) (PropertyTypeFixableError . ParameterParseFixableError) id $
    optionalParam $
      contentLineValueParams clv

propertyParamWithDefaultP ::
  (IsParameter param) =>
  param ->
  ContentLineValue ->
  Conform
    PropertyParseError
    PropertyFixableError
    Void
    param
propertyParamWithDefaultP defaultValue clv = fromMaybe defaultValue <$> propertyParamP clv

propertyParamListP ::
  (IsParameter param) =>
  ContentLineValue ->
  Conform
    PropertyParseError
    PropertyFixableError
    Void
    [param]
propertyParamListP clv =
  conformMapAll (PropertyTypeParseError . ParameterParseError) (PropertyTypeFixableError . ParameterParseFixableError) id $
    listParam $
      contentLineValueParams clv

-- | BEGIN of a calendar property
newtype Begin = Begin {unBegin :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity Begin

instance NFData Begin

instance IsProperty Begin where
  propertyName Proxy = "BEGIN"
  propertyP = wrapPropertyTypeP Begin
  propertyB = propertyTypeB . unBegin

-- | END of a calendar property
newtype End = End {unEnd :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity End

instance NFData End

instance IsProperty End where
  propertyName Proxy = "END"
  propertyP = wrapPropertyTypeP End
  propertyB = propertyTypeB . unEnd

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

-- | Method
--
-- === [section 3.7.2](https://datatracker.ietf.org/doc/html/rfc5545#section-3.7.2)
--
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
    mValue <- propertyParamP clv
    mFormatType <- propertyParamP clv
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

-- | Categories
--
-- === [section 3.8.1.2](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.2)
--
-- @
-- Property Name:  CATEGORIES
--
-- Purpose:  This property defines the categories for a calendar
--    component.
--
-- Value Type:  TEXT
--
-- Property Parameters:  IANA, non-standard, and language property
--    parameters can be specified on this property.
--
-- Conformance:  The property can be specified within "VEVENT", "VTODO",
--    or "VJOURNAL" calendar components.
--
-- escription:  This property is used to specify categories or subtypes
--    of the calendar component.  The categories are useful in searching
--    for a calendar component of a particular type and category.
--    Within the "VEVENT", "VTODO", or "VJOURNAL" calendar components,
--    more than one category can be specified as a COMMA-separated list
--    of categories.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     categories = "CATEGORIES" catparam ":" text *("," text)
--                  CRLF
--
--     catparam   = *(
--                ;
--                ; The following is OPTIONAL,
--                ; but MUST NOT occur more than once.
--                ;
--                (";" languageparam ) /
--                ;
--                ; The following is OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                (";" other-param)
--                ;
--                )
--
-- Example:  The following are examples of this property:
--
--     CATEGORIES:APPOINTMENT,EDUCATION
--
--     CATEGORIES:MEETING
data Categories = Categories
  { categories :: [Text], -- TODO make this a nonempty list, as well as the EXDATE sets
    categoriesLanguage :: Maybe Language
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Categories where
  validate cs@Categories {..} =
    mconcat
      [ genericValidate cs,
        decorateList categories $ \category ->
          declare "The category is a nonempty text" $ not $ T.null category
      ]

instance NFData Categories

instance IsProperty Categories where
  propertyName Proxy = "CATEGORIES"
  propertyP clv = flip viaPropertyTypeListP clv $ \categories -> do
    categoriesLanguage <- propertyParamP clv
    pure Categories {..}
  propertyB Categories {..} =
    insertMParam categoriesLanguage $
      propertyTypeListB categories

makeCategories :: [Text] -> Categories
makeCategories categories =
  let categoriesLanguage = Nothing
   in Categories {..}

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

-- | Comment
--
-- === [section 3.8.1.4](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.4)
--
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

instance Validity Comment

instance NFData Comment

instance IsString Comment where
  fromString = makeComment . fromString

instance IsProperty Comment where
  propertyName Proxy = "COMMENT"
  propertyP clv = do
    commentAlternateTextRepresentation <- propertyParamP clv
    commentLanguage <- propertyParamP clv
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
--
-- === [section 5.2 of RFC 7986](https://datatracker.ietf.org/doc/html/rfc7986#section-5.2)
--
-- @
-- This specification modifies the definition of the "DESCRIPTION"
-- property to allow it to be defined in an iCalendar object.  The
-- following additions are made to the definition of this property,
-- originally specified in Section 3.8.1.5 of [RFC5545].
--
-- Purpose:  This property specifies the description of the calendar.
--
-- Conformance:  This property can be specified multiple times in an
--    iCalendar object.  However, each property MUST represent the
--    description of the calendar in a different language.
--
-- Description:  This property is used to specify a lengthy textual
--    description of the iCalendar object that can be used by calendar
--    user agents when describing the nature of the calendar data to a
--    user.  Whilst a calendar only has a single description, multiple
--    language variants can be specified by including this property
--    multiple times with different "LANGUAGE" parameter values on each.
-- @
data Description = Description
  { descriptionContents :: !Text,
    descriptionAlternateTextRepresentation :: !(Maybe AlternateTextRepresentation),
    descriptionLanguage :: !(Maybe Language)
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Description

instance NFData Description

instance IsString Description where
  fromString = makeDescription . fromString

instance IsProperty Description where
  propertyName Proxy = "DESCRIPTION"
  propertyP clv = do
    descriptionAlternateTextRepresentation <- propertyParamP clv
    descriptionLanguage <- propertyParamP clv
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
  propertyP =
    viaPropertyTypeP
      ( \t ->
          maybe (unfixableError $ UnReadableGeographicPosition t) pure $ parseGeographicPosition t
      )
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
data Location = Location
  { locationContents :: !Text,
    locationAlternateTextRepresentation :: !(Maybe AlternateTextRepresentation),
    locationLanguage :: !(Maybe Language)
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Location

instance NFData Location

instance IsString Location where
  fromString = makeLocation . fromString

instance IsProperty Location where
  propertyName Proxy = "LOCATION"
  propertyP clv = do
    locationAlternateTextRepresentation <- propertyParamP clv
    locationLanguage <- propertyParamP clv
    wrapPropertyTypeP (\locationContents -> Location {..}) clv
  propertyB Location {..} =
    insertMParam locationAlternateTextRepresentation $
      insertMParam locationLanguage $
        propertyTypeB locationContents

makeLocation :: Text -> Location
makeLocation locationContents =
  let locationAlternateTextRepresentation = Nothing
      locationLanguage = Nothing
   in Location {..}

-- | Percent Complete
--
-- === [section 3.8.1.8](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.8)
--
-- @
-- Property Name:  PERCENT-COMPLETE
--
-- Purpose:  This property is used by an assignee or delegatee of a
--    to-do to convey the percent completion of a to-do to the
--    "Organizer".
--
-- Value Type:  INTEGER
--
-- Property Parameters:  IANA and non-standard property parameters can
--    be specified on this property.
--
-- Conformance:  This property can be specified once in a "VTODO"
--    calendar component.
--
-- Description:  The property value is a positive integer between 0 and
--    100.  A value of "0" indicates the to-do has not yet been started.
--    A value of "100" indicates that the to-do has been completed.
--    Integer values in between indicate the percent partially complete.
--
--    When a to-do is assigned to multiple individuals, the property
--    value indicates the percent complete for that portion of the to-do
--    assigned to the assignee or delegatee.  For example, if a to-do is
--    assigned to both individuals "A" and "B".  A reply from "A" with a
--    percent complete of "70" indicates that "A" has completed 70% of
--    the to-do assigned to them.  A reply from "B" with a percent
--    complete of "50" indicates "B" has completed 50% of the to-do
--    assigned to them.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     percent = "PERCENT-COMPLETE" pctparam ":" integer CRLF
--
--     pctparam   = *(";" other-param)
--
-- Example:  The following is an example of this property to show 39%
--    completion:
--
--     PERCENT-COMPLETE:39
-- @
newtype PercentComplete = PercentComplete {unPercentComplete :: Word8}
  deriving (Show, Eq, Ord, Generic)

instance Validity PercentComplete where
  validate pc@(PercentComplete i) =
    mconcat
      [ genericValidate pc,
        declare "the percentage is between 0 and 100" $ 0 <= i && i <= 100
      ]

instance NFData PercentComplete

instance IsProperty PercentComplete where
  propertyName Proxy = "PERCENT-COMPLETE"
  propertyP = wrapPropertyTypeP $ PercentComplete . (fromIntegral :: Int32 -> Word8) -- TODO unfixable error for out of range
  propertyB = propertyTypeB . (fromIntegral :: Word8 -> Int32) . unPercentComplete

-- | Priority
--
-- === [section 3.8.1.9](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.9)
--
-- @
-- Property Name:  PRIORITY
--
-- Purpose:  This property defines the relative priority for a calendar
--    component.
--
-- Value Type:  INTEGER
--
-- Property Parameters:  IANA and non-standard property parameters can
--    be specified on this property.
--
-- Conformance:  This property can be specified in "VEVENT" and "VTODO"
--    calendar components.
--
-- Description:  This priority is specified as an integer in the range 0
--    to 9.  A value of 0 specifies an undefined priority.  A value of 1
--    is the highest priority.  A value of 2 is the second highest
--    priority.  Subsequent numbers specify a decreasing ordinal
--    priority.  A value of 9 is the lowest priority.
--
--    A CUA with a three-level priority scheme of "HIGH", "MEDIUM", and
--    "LOW" is mapped into this property such that a property value in
--    the range of 1 to 4 specifies "HIGH" priority.  A value of 5 is
--    the normal or "MEDIUM" priority.  A value in the range of 6 to 9
--    is "LOW" priority.
--
--    A CUA with a priority schema of "A1", "A2", "A3", "B1", "B2", ...,
--    "C3" is mapped into this property such that a property value of 1
--    specifies "A1", a property value of 2 specifies "A2", a property
--    value of 3 specifies "A3", and so forth up to a property value of
--    9 specifies "C3".
--
--    Other integer values are reserved for future use.
--
--    Within a "VEVENT" calendar component, this property specifies a
--    priority for the event.  This property may be useful when more
--    than one event is scheduled for a given time period.
--
--    Within a "VTODO" calendar component, this property specified a
--    priority for the to-do.  This property is useful in prioritizing
--    multiple action items for a given time period.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     priority   = "PRIORITY" prioparam ":" priovalue CRLF
--     ;Default is zero (i.e., undefined).
--
--     prioparam  = *(";" other-param)
--
--     priovalue   = integer       ;Must be in the range [0..9]
--        ; All other values are reserved for future use.
--
-- Example:  The following is an example of a property with the highest
--    priority:
--
--     PRIORITY:1
--
--    The following is an example of a property with a next highest
--    priority:
--
--     PRIORITY:2
--
--    The following is an example of a property with no priority.  This
--    is equivalent to not specifying the "PRIORITY" property:
--
--     PRIORITY:0
-- @
newtype Priority = Priority {unPriority :: Word8}
  deriving (Show, Eq, Ord, Generic)

instance Validity Priority

instance NFData Priority

instance IsProperty Priority where
  propertyName Proxy = "PRIORITY"
  propertyP = wrapPropertyTypeP $ Priority . (fromIntegral :: Int32 -> Word8) -- TODO unfixable error for out of range
  propertyB = propertyTypeB . (fromIntegral :: Word8 -> Int32) . unPriority

-- @
--     ;Default is zero (i.e., undefined).
-- @
defaultPriority :: Priority
defaultPriority = Priority 0

-- | Resources
--
-- === [section 3.8.1.10](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.10)
--
-- @
-- Property Name:  RESOURCES
--
-- Purpose:  This property defines the equipment or resources
--    anticipated for an activity specified by a calendar component.
--
-- Value Type:  TEXT
--
-- Property Parameters:  IANA, non-standard, alternate text
--    representation, and language property parameters can be specified
--    on this property.
--
-- Conformance:  This property can be specified in "VEVENT" or
--    "VTODO" calendar component.
--
-- Description:  The property value is an arbitrary text.  More than one
--    resource can be specified as a COMMA-separated list of resources.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     resources  = "RESOURCES" resrcparam ":" text *("," text) CRLF
--
--     resrcparam = *(
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
--     RESOURCES:EASEL,PROJECTOR,VCR
--
--     RESOURCES;LANGUAGE=fr:Nettoyeur haute pression
-- @
--
-- The above includes:
--
-- * [Erratum 2677](https://www.rfc-editor.org/rfc/inline-errata/rfc5545.html#btn_2677)
data Resources = Resources
  { resources :: [Text], -- TODO make this a nonempty list
    resourcesLanguage :: Maybe Language
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Resources where
  validate cs@Resources {..} =
    mconcat
      [ genericValidate cs,
        decorateList resources $ \category ->
          declare "The category is a nonempty text" $ not $ T.null category
      ]

instance NFData Resources

instance IsProperty Resources where
  propertyName Proxy = "RESOURCES"
  propertyP clv = flip viaPropertyTypeListP clv $ \resources -> do
    resourcesLanguage <- propertyParamP clv
    pure Resources {..}
  propertyB Resources {..} =
    insertMParam resourcesLanguage $
      propertyTypeListB resources

makeResources :: [Text] -> Resources
makeResources resources =
  let resourcesLanguage = Nothing
   in Resources {..}

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
  | StatusNeedsAction
  | StatusInProgress
  | StatusDraft
  | StatusFinal
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
  "NEEDS-ACTION" -> pure StatusNeedsAction
  "IN-PROGRESS" -> pure StatusInProgress
  "DRAFT" -> pure StatusDraft
  "FINAL" -> pure StatusFinal
  _ -> Nothing

renderStatus :: Status -> Text
renderStatus = \case
  StatusTentative -> "TENTATIVE"
  StatusConfirmed -> "CONFIRMED"
  StatusCancelled -> "CANCELLED"
  StatusNeedsAction -> "NEEDS-ACTION"
  StatusInProgress -> "IN-PROGRESS"
  StatusDraft -> "DRAFT"
  StatusFinal -> "FINAL"

-- TODO validation for the per-component-type allowed-statuses

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
data Summary = Summary
  { summaryContents :: !Text,
    summaryAlternateTextRepresentation :: !(Maybe AlternateTextRepresentation),
    summaryLanguage :: !(Maybe Language)
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Summary

instance NFData Summary

instance IsString Summary where
  fromString = makeSummary . fromString

instance IsProperty Summary where
  propertyName Proxy = "SUMMARY"
  propertyP clv = do
    summaryAlternateTextRepresentation <- propertyParamP clv
    summaryLanguage <- propertyParamP clv
    wrapPropertyTypeP (\summaryContents -> Summary {..}) clv
  propertyB Summary {..} =
    insertMParam summaryAlternateTextRepresentation $
      insertMParam summaryLanguage $
        propertyTypeB summaryContents

makeSummary :: Text -> Summary
makeSummary summaryContents =
  let summaryAlternateTextRepresentation = Nothing
      summaryLanguage = Nothing
   in Summary {..}

-- | Date Time Completed
--
-- === [section 3.8.2.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.2.1)
--
-- @
-- Property Name:  COMPLETED
--
-- Purpose:  This property defines the date and time that a to-do was
--    actually completed.
--
-- Value Type:  DATE-TIME
--
-- Property Parameters:  IANA and non-standard property parameters can
--    be specified on this property.
--
-- Conformance:  The property can be specified in a "VTODO" calendar
--    component.  The value MUST be specified as a date with UTC time.
--
-- Description:  This property defines the date and time that a to-do
--    was actually completed.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     completed  = "COMPLETED" compparam ":" date-time CRLF
--
--     compparam  = *(";" other-param)
--
-- Example:  The following is an example of this property:
--
--  COMPLETED:19960401T150000Z
-- @
newtype DateTimeCompleted = DateTimeCompleted {unDateTimeCompleted :: DateTime} -- TODO specify as UTC Time. Do the same For DateTimeStamp
  deriving (Show, Eq, Ord, Generic)

instance Validity DateTimeCompleted

instance NFData DateTimeCompleted

instance IsProperty DateTimeCompleted where
  propertyName Proxy = "COMPLETED"
  propertyP = wrapPropertyTypeP DateTimeCompleted
  propertyB = propertyTypeB . unDateTimeCompleted

-- | Date Time End
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
    mValue <- propertyParamP clv
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

-- | Date Time Due
--
-- === [section 3.8.2.3](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.2.3)
--
-- @
-- Property Name:  DUE
--
-- Purpose:  This property defines the date and time that a to-do is
--    expected to be completed.
--
-- Value Type:  The default value type is DATE-TIME.  The value type can
--    be set to a DATE value type.
--
-- Property Parameters:  IANA, non-standard, value data type, and time
--    zone identifier property parameters can be specified on this
--    property.
--
-- Conformance:  The property can be specified once in a "VTODO"
--    calendar component.
--
-- Description:  This property defines the date and time before which a
--    to-do is expected to be completed.  For cases where this property
--    is specified in a "VTODO" calendar component that also specifies a
--    "DTSTART" property, the value type of this property MUST be the
--    same as the "DTSTART" property, and the value of this property
--
--    MUST be later in time than the value of the "DTSTART" property.
--    Furthermore, this property MUST be specified as a date with local
--    time if and only if the "DTSTART" property is also specified as a
--    date with local time.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     due        = "DUE" dueparam ":" dueval CRLF
--
--     dueparam   = *(
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
--     dueval     = date-time / date
--     ;Value MUST match value type
--
-- Example:  The following is an example of this property:
--
--     DUE:19980430T000000Z
-- @
data DateTimeDue
  = DateTimeDueDate !Date
  | DateTimeDueDateTime !DateTime
  deriving (Show, Eq, Ord, Generic)

instance Validity DateTimeDue

instance NFData DateTimeDue

instance IsProperty DateTimeDue where
  propertyName Proxy = "DUE"
  propertyP clv = do
    mValue <- propertyParamP clv
    case mValue of
      Just TypeDateTime -> wrapPropertyTypeP DateTimeDueDateTime clv
      Just TypeDate -> wrapPropertyTypeP DateTimeDueDate clv
      Just _ -> unfixableError $ ValueMismatch "DUE" mValue (Just TypeDateTime) [TypeDate]
      -- @
      -- Value Type:  The default value type is DATE-TIME.
      -- @
      Nothing -> wrapPropertyTypeP DateTimeDueDateTime clv

  propertyB = \case
    DateTimeDueDateTime dateTime -> propertyTypeB dateTime
    -- @
    -- Value Type:  The default value type is DATE-TIME.
    -- @
    DateTimeDueDate date -> typedPropertyTypeB date

-- | Date-Time Start
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
    mValue <- propertyParamP clv
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

-- | Free/Busy Time
--
-- === [section 3.8.2.6](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.2.6)
--
-- @
-- Property Name:  FREEBUSY
--
-- Purpose:  This property defines one or more free or busy time
--    intervals.
--
-- Value Type:  PERIOD
--
-- Property Parameters:  IANA, non-standard, and free/busy time type
--    property parameters can be specified on this property.
--
-- Conformance:  The property can be specified in a "VFREEBUSY" calendar
--    component.
--
-- Description:  These time periods can be specified as either a start
--    and end DATE-TIME or a start DATE-TIME and DURATION.  The date and
--    time MUST be a UTC time format.
--
--    "FREEBUSY" properties within the "VFREEBUSY" calendar component
--    SHOULD be sorted in ascending order, based on start time and then
--    end time, with the earliest periods first.
--
--    The "FREEBUSY" property can specify more than one value, separated
--    by the COMMA character.  In such cases, the "FREEBUSY" property
--    values MUST all be of the same "FBTYPE" property parameter type
--    (e.g., all values of a particular "FBTYPE" listed together in a
--    single property).
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     freebusy   = "FREEBUSY" fbparam ":" fbvalue CRLF
--
--     fbparam    = *(
--                ;
--                ; The following is OPTIONAL,
--                ; but MUST NOT occur more than once.
--                ;
--                (";" fbtypeparam) /
--                ;
--                ; The following is OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                (";" other-param)
--                ;
--                )
--
--     fbvalue    = period *("," period)
--     ;Time value MUST be in the UTC time format.
--
-- Example:  The following are some examples of this property:
--
--     FREEBUSY;FBTYPE=BUSY-UNAVAILABLE:19970308T160000Z/PT8H30M
--
--     FREEBUSY;FBTYPE=FREE:19970308T160000Z/PT3H,19970308T200000Z/PT1H
--
--     FREEBUSY;FBTYPE=FREE:19970308T160000Z/PT3H,19970308T200000Z/PT1H
--      ,19970308T230000Z/19970309T000000Z
-- @
data FreeBusyIntervals = FreeBusyIntervals
  { freeBusyIntervals :: [Period], -- TODO make this a nonempty list
    freeBusyIntervalsType :: FreeBusyTimeType
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity FreeBusyIntervals

instance NFData FreeBusyIntervals

instance IsProperty FreeBusyIntervals where
  propertyName Proxy = "FREEBUSY"
  propertyP clv = flip viaPropertyTypeListP clv $ \freeBusyIntervals -> do
    freeBusyIntervalsType <- propertyParamWithDefaultP defaultFreeBusyTimeType clv
    pure FreeBusyIntervals {..}
  propertyB FreeBusyIntervals {..} =
    insertParam freeBusyIntervalsType $
      propertyTypeListB freeBusyIntervals

makeFreeBusyIntervals :: [Period] -> FreeBusyIntervals
makeFreeBusyIntervals freeBusyIntervals =
  let freeBusyIntervalsType = defaultFreeBusyTimeType
   in FreeBusyIntervals {..}

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
tzidParam = TimeZoneIdentifierParam . ciToParamValue . CI.mk . unTimeZoneIdentifier

-- | Timezone Name
--
-- === [section 3.8.3.2](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.3.2)
--
-- @
-- Property Name:  TZNAME
--
-- Purpose:  This property specifies the customary designation for a
--    time zone description.
--
-- Value Type:  TEXT
--
-- Property Parameters:  IANA, non-standard, and language property
--    parameters can be specified on this property.
--
-- Conformance:  This property can be specified in "STANDARD" and
--    "DAYLIGHT" sub-components.
--
-- Description:  This property specifies a customary name that can be
--    used when displaying dates that occur during the observance
--    defined by the time zone sub-component.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     tzname     = "TZNAME" tznparam ":" text CRLF
--
--     tznparam   = *(
--                ;
--                ; The following is OPTIONAL,
--                ; but MUST NOT occur more than once.
--                ;
--                (";" languageparam) /
--                ;
--                ; The following is OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                (";" other-param)
--                ;
--                )
--
-- Example:  The following are examples of this property:
--
--     TZNAME:EST
--
--     TZNAME;LANGUAGE=fr-CA:HNE
-- @
data TimeZoneName = TimeZoneName
  { timeZoneNameContents :: !Text,
    timeZoneNameLanguage :: !(Maybe Language)
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity TimeZoneName

instance NFData TimeZoneName

instance IsString TimeZoneName where
  fromString = makeTimeZoneName . fromString

instance IsProperty TimeZoneName where
  propertyName Proxy = "TZNAME"
  propertyP clv = do
    timeZoneNameLanguage <- propertyParamP clv
    wrapPropertyTypeP (\timeZoneNameContents -> TimeZoneName {..}) clv
  propertyB TimeZoneName {..} =
    insertMParam timeZoneNameLanguage $
      propertyTypeB timeZoneNameContents

makeTimeZoneName :: Text -> TimeZoneName
makeTimeZoneName timeZoneNameContents =
  let timeZoneNameLanguage = Nothing
   in TimeZoneName {..}

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

-- | Time Zone URL
--
-- === [section 3.8.4.5](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.4.5)
--
-- @
-- Property Name:  TZURL
--
-- Purpose:  This property provides a means for a "VTIMEZONE" component
--    to point to a network location that can be used to retrieve an up-
--    to-date version of itself.
--
-- Value Type:  URI
--
-- Property Parameters:  IANA and non-standard property parameters can
--    be specified on this property.
--
-- Conformance:  This property can be specified in a "VTIMEZONE"
--    calendar component.
--
-- Description:  This property provides a means for a "VTIMEZONE"
--    component to point to a network location that can be used to
--    retrieve an up-to-date version of itself.  This provides a hook to
--    handle changes government bodies impose upon time zone
--    definitions.  Retrieval of this resource results in an iCalendar
--    object containing a single "VTIMEZONE" component and a "METHOD"
--    property set to PUBLISH.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     tzurl      = "TZURL" tzurlparam ":" uri CRLF
--
--     tzurlparam = *(";" other-param)
--
-- Example:  The following is an example of this property:
--
--  TZURL:http://timezones.example.org/tz/America-Los_Angeles.ics
-- @
newtype TimeZoneURL = TimeZoneURL {unTimeZoneURL :: URI}
  deriving (Show, Eq, Ord, Generic)

instance Validity TimeZoneURL

instance NFData TimeZoneURL

instance IsProperty TimeZoneURL where
  propertyName Proxy = "TZURL"
  propertyP = wrapPropertyTypeP TimeZoneURL
  propertyB = propertyTypeB . unTimeZoneURL

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
--       @example.com
--     ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=ACCEPTED;CN=Jane Doe
--      :mailto:jdoe@example.com
--
-- Example:  The following is an example of this property's use when
--    another calendar user is acting on behalf of the "Attendee":
--
--     ATTENDEE;SENT-BY=mailto:jan_doe@example.com;CN=John Smith:
--      mailto:jsmith@example.com
-- @
--
-- This last example is corrected in [Erratum 2516](https://www.rfc-editor.org/errata/eid2516):
--
-- @
-- Example:  The following is an example of this property's use when
--       another calendar user is acting on behalf of the "Attendee":
--
--        ATTENDEE;SENT-BY="mailto:jan_doe@example.com";CN=John Smith:
--                         ^                          ^
--         mailto:jsmith@example.com
-- @
data Attendee = Attendee
  { attendeeCalAddress :: !CalAddress,
    attendeeCommonName :: !(Maybe CommonName),
    attendeeCalendarUserType :: !CalendarUserType,
    attendeeDelegators :: ![Delegator],
    attendeeDelegatees :: ![Delegatee],
    attendeeDirectoryEntryReference :: !(Maybe DirectoryEntryReference),
    attendeeLanguage :: !(Maybe Language),
    attendeeMemberships :: ![Membership],
    attendeeParticipationStatus :: !ParticipationStatus,
    attendeeParticipationRole :: !ParticipationRole,
    attendeeRSVPExpectation :: !RSVPExpectation,
    attendeeSentBy :: !(Maybe SentBy)
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Attendee

instance NFData Attendee

instance IsProperty Attendee where
  propertyName Proxy = "ATTENDEE"
  propertyP clv = flip viaPropertyTypeP clv $ \attendeeCalAddress -> do
    attendeeCommonName <- propertyParamP clv
    attendeeCalendarUserType <- fromMaybe defaultCalendarUserType <$> propertyParamP clv
    attendeeDelegators <- propertyParamListP clv
    attendeeDelegatees <- propertyParamListP clv
    attendeeDirectoryEntryReference <- propertyParamP clv
    attendeeLanguage <- propertyParamP clv
    attendeeMemberships <- propertyParamListP clv
    attendeeParticipationStatus <- propertyParamWithDefaultP defaultParticipationStatus clv
    attendeeParticipationRole <- propertyParamWithDefaultP defaultParticipationRole clv
    attendeeRSVPExpectation <- propertyParamWithDefaultP defaultRSVPExpectation clv
    attendeeSentBy <- propertyParamP clv
    pure Attendee {..}
  propertyB Attendee {..} =
    insertMParam attendeeCommonName
      . insertParamWithDefault defaultCalendarUserType attendeeCalendarUserType
      . insertParamList attendeeDelegators
      . insertParamList attendeeDelegatees
      . insertMParam attendeeDirectoryEntryReference
      . insertMParam attendeeLanguage
      . insertParamList attendeeMemberships
      . insertParamWithDefault defaultParticipationStatus attendeeParticipationStatus
      . insertParamWithDefault defaultParticipationRole attendeeParticipationRole
      . insertParamWithDefault defaultRSVPExpectation attendeeRSVPExpectation
      . insertMParam attendeeSentBy
      $ propertyTypeB attendeeCalAddress

makeAttendee :: CalAddress -> Attendee
makeAttendee calAddress =
  Attendee
    { attendeeCalAddress = calAddress,
      attendeeCommonName = Nothing,
      attendeeCalendarUserType = defaultCalendarUserType,
      attendeeDelegators = [],
      attendeeDelegatees = [],
      attendeeDirectoryEntryReference = Nothing,
      attendeeLanguage = Nothing,
      attendeeMemberships = [],
      attendeeParticipationStatus = defaultParticipationStatus,
      attendeeParticipationRole = defaultParticipationRole,
      attendeeRSVPExpectation = defaultRSVPExpectation,
      attendeeSentBy = Nothing
    }

-- | Contact
--
-- === [section 3.8.4.2](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.4.2)
--
-- @
-- Property Name:  CONTACT
--
-- Purpose:  This property is used to represent contact information or
--    alternately a reference to contact information associated with the
--    calendar component.
--
-- Value Type:  TEXT
--
-- Property Parameters:  IANA, non-standard, alternate text
--    representation, and language property parameters can be specified
--    on this property.
--
-- Conformance:  This property can be specified in a "VEVENT", "VTODO",
--    "VJOURNAL", or "VFREEBUSY" calendar component.
--
-- Description:  The property value consists of textual contact
--    information.  An alternative representation for the property value
--    can also be specified that refers to a URI pointing to an
--    alternate form, such as a vCard [RFC2426], for the contact
--    information.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     contact    = "CONTACT" contparam ":" text CRLF
--
--     contparam  = *(
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
-- Example:  The following is an example of this property referencing
--    textual contact information:
--
--     CONTACT:Jim Dolittle\, ABC Industries\, +1-919-555-1234
--
--    The following is an example of this property with an alternate
--    representation of an LDAP URI to a directory entry containing the
--    contact information:
--
--     CONTACT;ALTREP="ldap://example.com:6666/o=ABC%20Industries\,
--      c=US???(cn=Jim%20Dolittle)":Jim Dolittle\, ABC Industries\,
--      +1-919-555-1234
--
--    The following is an example of this property with an alternate
--    representation of a MIME body part containing the contact
--    information, such as a vCard [RFC2426] embedded in a text/
--    directory media type [RFC2425]:
--
--     CONTACT;ALTREP="CID:part3.msg970930T083000SILVER@example.com":
--      Jim Dolittle\, ABC Industries\, +1-919-555-1234
--
--    The following is an example of this property referencing a network
--    resource, such as a vCard [RFC2426] object containing the contact
--    information:
--
--     CONTACT;ALTREP="http://example.com/pdi/jdoe.vcf":Jim
--       Dolittle\, ABC Industries\, +1-919-555-1234
-- @
data Contact = Contact
  { contactInfo :: !Text,
    contactAlternateTextRepresentation :: !(Maybe AlternateTextRepresentation),
    contactLanguage :: !(Maybe Language)
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Contact

instance NFData Contact

instance IsProperty Contact where
  propertyName Proxy = "CONTACT"
  propertyP clv = flip viaPropertyTypeP clv $ \contactInfo -> do
    contactAlternateTextRepresentation <- propertyParamP clv
    contactLanguage <- propertyParamP clv
    pure Contact {..}
  propertyB Contact {..} =
    insertMParam contactAlternateTextRepresentation
      . insertMParam contactLanguage
      $ propertyTypeB contactInfo

makeContact :: Text -> Contact
makeContact contactInfo =
  let contactAlternateTextRepresentation = Nothing
      contactLanguage = Nothing
   in Contact {..}

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
    organizerCommonName :: !(Maybe CommonName),
    organizerDirectoryEntryReference :: !(Maybe DirectoryEntryReference),
    organizerLanguage :: !(Maybe Language),
    organizerSentBy :: !(Maybe SentBy)
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Organizer

instance NFData Organizer

instance IsProperty Organizer where
  propertyName Proxy = "ORGANIZER"
  propertyP clv = flip viaPropertyTypeP clv $ \organizerCalAddress -> do
    organizerCommonName <- propertyParamP clv
    organizerDirectoryEntryReference <- propertyParamP clv
    organizerLanguage <- propertyParamP clv
    organizerSentBy <- propertyParamP clv
    pure Organizer {..}
  propertyB Organizer {..} =
    insertMParam organizerCommonName
      . insertMParam organizerDirectoryEntryReference
      . insertMParam organizerLanguage
      . insertMParam organizerSentBy
      $ propertyTypeB organizerCalAddress

makeOrganizer :: CalAddress -> Organizer
makeOrganizer calAddress =
  Organizer
    { organizerCalAddress = calAddress,
      organizerCommonName = Nothing,
      organizerDirectoryEntryReference = Nothing,
      organizerLanguage = Nothing,
      organizerSentBy = Nothing
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
    mValue <- propertyParamP clv
    mRecurrenceRangeIdentifier <- propertyParamP clv
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

-- | Related To
--
-- === [section 3.8.4.5](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.4.5)
--
-- @
-- Property Name:  RELATED-TO
--
-- Purpose:  This property is used to represent a relationship or
--    reference between one calendar component and another.
--
-- Value Type:  TEXT
--
-- Property Parameters:  IANA, non-standard, and relationship type
--    property parameters can be specified on this property.
--
-- Conformance:  This property can be specified in the "VEVENT",
--    "VTODO", and "VJOURNAL" calendar components.
--
-- Description:  The property value consists of the persistent, globally
--    unique identifier of another calendar component.  This value would
--    be represented in a calendar component by the "UID" property.
--
--    By default, the property value points to another calendar
--    component that has a PARENT relationship to the referencing
--    object.  The "RELTYPE" property parameter is used to either
--    explicitly state the default PARENT relationship type to the
--    referenced calendar component or to override the default PARENT
--    relationship type and specify either a CHILD or SIBLING
--    relationship.  The PARENT relationship indicates that the calendar
--    component is a subordinate of the referenced calendar component.
--    The CHILD relationship indicates that the calendar component is a
--    superior of the referenced calendar component.  The SIBLING
--    relationship indicates that the calendar component is a peer of
--    the referenced calendar component.
--
--    Changes to a calendar component referenced by this property can
--    have an implicit impact on the related calendar component.  For
--    example, if a group event changes its start or end date or time,
--    then the related, dependent events will need to have their start
--    and end dates changed in a corresponding way.  Similarly, if a
--    PARENT calendar component is cancelled or deleted, then there is
--    an implied impact to the related CHILD calendar components.  This
--    property is intended only to provide information on the
--    relationship of calendar components.  It is up to the target
--    calendar system to maintain any property implications of this
--    relationship.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     related    = "RELATED-TO" relparam ":" text CRLF
--
--     relparam   = *(
--                ;
--                ; The following is OPTIONAL,
--                ; but MUST NOT occur more than once.
--                ;
--                (";" reltypeparam) /
--                ;
--                ; The following is OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                (";" other-param)
--                ;
--                )
--
--    The following is an example of this property:
--
--     RELATED-TO:jsmith.part7.19960817T083000.xyzMail@example.com
--
--     RELATED-TO:19960401-080045-4000F192713-0052@example.com
-- @
data RelatedTo = RelatedTo
  { relatedToRelative :: !Text,
    relatedToRelationshipType :: !RelationshipType
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity RelatedTo

instance NFData RelatedTo

instance IsProperty RelatedTo where
  propertyName Proxy = "RELATED-TO"
  propertyP clv = flip viaPropertyTypeP clv $ \relatedToRelative -> do
    relatedToRelationshipType <- propertyParamWithDefaultP defaultRelationshipType clv
    pure RelatedTo {..}
  propertyB RelatedTo {..} =
    insertParamWithDefault defaultRelationshipType relatedToRelationshipType $
      propertyTypeB relatedToRelative

makeRelatedTo :: Text -> RelatedTo
makeRelatedTo relatedToRelative =
  let relatedToRelationshipType = defaultRelationshipType
   in RelatedTo {..}

-- | Uniform Resource Locator
--
-- === [section 3.8.4.6](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.4.6)
--
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
--
-- === [section 5.5 of RFC 7986](https://datatracker.ietf.org/doc/html/rfc7986#section-5.5)
--
-- @
-- This specification modifies the definition of the "URL" property to
-- allow it to be defined in an iCalendar object.  The following
-- additions are made to the definition of this property, originally
-- specified in Section 3.8.4.6 of [RFC5545].
--
-- Purpose:  This property may be used to convey a location where a more
--    dynamic rendition of the calendar information can be found.
--
-- Conformance:  This property can be specified once in an iCalendar
--    object.
-- @
newtype URL = URL {unURL :: URI}
  deriving (Show, Eq, Ord, Generic)

instance Validity URL

instance NFData URL

instance IsProperty URL where
  propertyName Proxy = "URL"
  propertyP = wrapPropertyTypeP URL
  propertyB = propertyTypeB . unURL

-- | Unique Identifier
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
--
-- === [section 5.3 of RFC 7986](https://datatracker.ietf.org/doc/html/rfc7986#section-5.3)
--
-- @
-- This specification modifies the definition of the "UID" property to
-- allow it to be defined in an iCalendar object.  The following
-- additions are made to the definition of this property, originally
-- specified in Section 3.8.4.7 of [RFC5545].
--
-- Purpose:  This property specifies the persistent, globally unique
--    identifier for the iCalendar object.  This can be used, for
--    example, to identify duplicate calendar streams that a client may
--    have been given access to.  It can be used in conjunction with the
--    "LAST-MODIFIED" property also specified on the "VCALENDAR" object
--    to identify the most recent version of a calendar.
--
-- Conformance:  This property can be specified once in an iCalendar
--    object.
--
-- The description of the "UID" property in [RFC5545] contains some
-- recommendations on how the value can be constructed.  In particular,
-- it suggests use of host names, IP addresses, and domain names to
-- construct the value.  However, this is no longer considered good
-- practice, particularly from a security and privacy standpoint, since
-- use of such values can leak key information about a calendar user or
-- their client and network environment.  This specification updates
-- [RFC5545] by stating that "UID" values MUST NOT include any data that
-- might identify a user, host, domain, or any other security- or
-- privacy-sensitive information.  It is RECOMMENDED that calendar user
-- agents now generate "UID" values that are hex-encoded random
-- Universally Unique Identifier (UUID) values as defined in
-- Sections 4.4 and 4.5 of [RFC4122].
--
-- The following is an example of such a property value:
--
-- UID:5FC53010-1267-4F8E-BC28-1D7AE55A7C99
--
-- Additionally, if calendar user agents choose to use other forms of
-- opaque identifiers for the "UID" value, they MUST have a length less
-- than 255 octets and MUST conform to the "iana-token" ABNF syntax
-- defined in Section 3.1 of [RFC5545].
-- @
newtype UID = UID {unUID :: Text}
  deriving (Show, Eq, Ord, Generic)

instance Validity UID

instance NFData UID

instance IsProperty UID where
  propertyName Proxy = "UID"
  propertyP = wrapPropertyTypeP UID
  propertyB = propertyTypeB . unUID

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
-- The above does not include [Erratum 5215](https://www.rfc-editor.org/errata/eid5215),
-- which suggests that this property may not be included in time zone
-- definitions because the section on time zone definitions does not iclude the
-- exdate property:
--
-- @
-- Section 3.8.5.1 says:
--
--    Purpose:  This property defines the list of DATE-TIME exceptions for
--    recurring events, to-dos, journal entries, or time zone
--    definitions.
--
-- ...
--
--    Conformance:  This property can be specified in recurring "VEVENT",
--    "VTODO", and "VJOURNAL" calendar components as well as in the
--    "STANDARD" and "DAYLIGHT" sub-components of the "VTIMEZONE"
--    calendar component.
--
-- It should say:
--
--    Purpose:  This property defines the list of DATE-TIME exceptions for
--    recurring events, to-dos or journal entries.
--
-- ...
--
--    Conformance:  This property can be specified in recurring "VEVENT",
--    "VTODO", and "VJOURNAL" calendar components.
--
-- Notes:
--
-- Section 3.8.5.1 describes Exception Date-Times (EXDATE).
--
-- tzprop (section 3.6.5) does not allow EXDATE.
--
-- (Of course, the problem could be that 3.6.5 should include EXDATE.)
-- @
--
-- TODO parse exception dates for those too, just to be sure?
--
--
--
-- The above also does not include [Erratum 6316](https://www.rfc-editor.org/errata/eid6316)
-- because, while I agree with the sentiment, it adds an additional restriction
-- that is not adhered to in existing implementations.
-- As such we have to deal with the less restrictive version.
--
-- @
-- Section 3.8.5.1 says:
--
--     Value Type:  The default value type for this property is DATE-TIME.
--        The value type can be set to DATE.
--
-- It should say:
--
--     Value Type:  The default value type for this property is DATE-TIME.
--        The value type can be set to DATE.  This property MUST have the same
--        value type as the "DTSTART" property contained within the
--        recurring component.  Furthermore, this property MUST be specified
--        as a date with local time if and only if the "DTSTART" property
--        contained within the recurring component is specified as a date
--        with local time.
--
-- Notes:
--
-- EXDATE excludes a specific instance of a recurring event and therefore
-- should have the same value type as DTSTART. This is analogous to
-- RECURRENCE-ID which overrides a specific instance and has the same value
-- type as DTSTART.
--
-- I will note however that there is iCalendar data in the wild with
-- DTSTART;VALUE=DATE-TIME and EXDATE;VALUE=DATE. If this errata is rejected as
-- incorrect, then a new errata should be opened with additional text
-- describing how EXDATE;VALUE=DATE is supposed to be handled when
-- DTSTART;VALUE=DATE-TIME. For instance, does EXDATE;VALUE=DATE exclude ALL
-- instances of a FREQ=HOURLY recurrence on the given day?
-- @
--
--
-- TODO check this SHOULD and warn about it:
--
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
    mValue <- propertyParamP clv
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
--
-- These examples are corrected in [Erratum 2527](https://www.rfc-editor.org/errata/eid2527):
--
-- @
-- Example:  The following are examples of this property:
--
--      RDATE:19970714T123000Z
--      RDATE;TZID=America/New_York:19970714T083000
--
--      RDATE;VALUE=PERIOD:19960403T020000Z/19960403T040000Z,
--       19960404T010000Z/PT3H
--
--      RDATE;VALUE=DATE:19970101,19970120,19970217,19970421,
--                                                          ^
--       19970526,19970704,19970901,19971014,19971128,19971129,19971225
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
    mValue <- propertyParamP clv
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

-- | Recurrence Rule
--
-- === [section 3.8.5.3](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.5.3)
instance IsProperty RecurrenceRule where
  propertyName Proxy = "RRULE"
  propertyP = wrapPropertyTypeP id
  propertyB = recurrenceRuleB

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

-- | Repeat Count
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

-- | Trigger
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
    mValue <- propertyParamP clv
    let parseDurationTrigger = do
          duration <- conformMapAll PropertyTypeParseError PropertyTypeFixableError id $ propertyTypeP clv
          relationship <- propertyParamWithDefaultP defaultAlarmTriggerRelationship clv
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
newtype DateTimeStamp = DateTimeStamp {unDateTimeStamp :: DateTime} -- TODO specify as UTC Time. Do the same For DateTimeCompleted
  deriving (Show, Eq, Ord, Generic)

instance Validity DateTimeStamp

instance NFData DateTimeStamp

instance IsProperty DateTimeStamp where
  propertyName Proxy = "DTSTAMP"
  propertyP = wrapPropertyTypeP DateTimeStamp
  propertyB = propertyTypeB . unDateTimeStamp

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
--
-- === [section 5.3 of RFC 7986](https://datatracker.ietf.org/doc/html/rfc7986#section-5.3)
--
-- @
-- This specification modifies the definition of the "LAST-MODIFIED"
-- property to allow it to be defined in an iCalendar object.  The
-- following additions are made to the definition of this property,
-- originally specified in Section 3.8.7.3 of [RFC5545].
--
-- Purpose:  This property specifies the date and time that the
--    information associated with the calendar was last revised.
--
-- Conformance:  This property can be specified once in an iCalendar
--    object.
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

-- | Sequence Number
--
-- === [section 3.8.7.4](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.7.4)
--
-- @
-- Property Name:  SEQUENCE
--
-- Purpose:  This property defines the revision sequence number of the
--    calendar component within a sequence of revisions.
--
-- Value Type:  INTEGER
--
-- Property Parameters:  IANA and non-standard property parameters can
--    be specified on this property.
--
-- Conformance:  The property can be specified in "VEVENT", "VTODO", or
--    "VJOURNAL" calendar component.
--
-- Description:  When a calendar component is created, its sequence
--    number is 0.  It is monotonically incremented by the "Organizer's"
--    CUA each time the "Organizer" makes a significant revision to the
--    calendar component.
--
--    The "Organizer" includes this property in an iCalendar object that
--    it sends to an "Attendee" to specify the current version of the
--    calendar component.
--
--    The "Attendee" includes this property in an iCalendar object that
--    it sends to the "Organizer" to specify the version of the calendar
--    component to which the "Attendee" is referring.
--
--    A change to the sequence number is not the mechanism that an
--    "Organizer" uses to request a response from the "Attendees".  The
--    "RSVP" parameter on the "ATTENDEE" property is used by the
--    "Organizer" to indicate that a response from the "Attendees" is
--    requested.
--
--    Recurrence instances of a recurring component MAY have different
--    sequence numbers.
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     seq = "SEQUENCE" seqparam ":" integer CRLF
--     ; Default is "0"
--
--     seqparam   = *(";" other-param)
--
-- Example:  The following is an example of this property for a calendar
--    component that was just created by the "Organizer":
--
--     SEQUENCE:0
--
--    The following is an example of this property for a calendar
--    component that has been revised two different times by the
--    "Organizer":
--
--     SEQUENCE:2
-- @
newtype SequenceNumber = SequenceNumber {unSequenceNumber :: Word16}
  deriving (Show, Eq, Ord, Generic)

instance Validity SequenceNumber

instance NFData SequenceNumber

instance IsProperty SequenceNumber where
  propertyName Proxy = "SEQUENCE"
  propertyP = wrapPropertyTypeP $ SequenceNumber . (fromIntegral :: Int32 -> Word16) -- TODO unfixable error for out of range
  propertyB = propertyTypeB . (fromIntegral :: Word16 -> Int32) . unSequenceNumber

-- @
--     ; Default is "0"
-- @
defaultSequenceNumber :: SequenceNumber
defaultSequenceNumber = SequenceNumber 0

-- | Request Status
--
-- === [section 3.8.8.3](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.8.3)
--
-- @
-- Property Name:  REQUEST-STATUS
--
-- Purpose:  This property defines the status code returned for a
--    scheduling request.
--
-- Value Type:  TEXT
--
-- Property Parameters:  IANA, non-standard, and language property
--    parameters can be specified on this property.
--
-- Conformance:  The property can be specified in the "VEVENT", "VTODO",
--    "VJOURNAL", or "VFREEBUSY" calendar component.
--
-- Description:  This property is used to return status code information
--    related to the processing of an associated iCalendar object.  The
--    value type for this property is TEXT.
--
--    The value consists of a short return status component, a longer
--    return status description component, and optionally a status-
--    specific data component.  The components of the value are
--    separated by the SEMICOLON character.
--
--    The short return status is a PERIOD character separated pair or
--    3-tuple of integers.  For example, "3.1" or "3.1.1".  The
--    successive levels of integers provide for a successive level of
--    status code granularity.
--
--    The following are initial classes for the return status code.
--    Individual iCalendar object methods will define specific return
--    status codes for these classes.  In addition, other classes for
--    the return status code may be defined using the registration
--    process defined later in this memo.
--
-- +--------+----------------------------------------------------------+
-- | Short  | Longer Return Status Description                         |
-- | Return |                                                          |
-- | Status |                                                          |
-- | Code   |                                                          |
-- +--------+----------------------------------------------------------+
-- | 1.xx   | Preliminary success.  This class of status code          |
-- |        | indicates that the request has been initially processed  |
-- |        | but that completion is pending.                          |
-- |        |                                                          |
-- | 2.xx   | Successful.  This class of status code indicates that    |
-- |        | the request was completed successfully.  However, the    |
-- |        | exact status code can indicate that a fallback has been  |
-- |        | taken.                                                   |
-- |        |                                                          |
-- | 3.xx   | Client Error.  This class of status code indicates that  |
-- |        | the request was not successful.  The error is the result |
-- |        | of either a syntax or a semantic error in the client-    |
-- |        | formatted request.  Request should not be retried until  |
-- |        | the condition in the request is corrected.               |
-- |        |                                                          |
-- | 4.xx   | Scheduling Error.  This class of status code indicates   |
-- |        | that the request was not successful.  Some sort of error |
-- |        | occurred within the calendaring and scheduling service,  |
-- |        | not directly related to the request itself.              |
-- +--------+----------------------------------------------------------+
--
-- Format Definition:  This property is defined by the following
--    notation:
--
--     rstatus    = "REQUEST-STATUS" rstatparam ":"
--                  statcode ";" statdesc [";" extdata]
--
--     rstatparam = *(
--                ;
--                ; The following is OPTIONAL,
--                ; but MUST NOT occur more than once.
--                ;
--                (";" languageparam) /
--                ;
--                ; The following is OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                (";" other-param)
--                ;
--                )
--
--     statcode   = 1*DIGIT 1*2("." 1*DIGIT)
--     ;Hierarchical, numeric return status code
--
--     statdesc   = text
--     ;Textual status description
--
--     extdata    = text
--     ;Textual exception data.  For example, the offending property
--     ;name and value or complete property line.
--
-- Example:  The following are some possible examples of this property.
--
--    The COMMA and SEMICOLON separator characters in the property value
--    are BACKSLASH character escaped because they appear in a text
--    value.
--
--     REQUEST-STATUS:2.0;Success
--
--     REQUEST-STATUS:3.1;Invalid property value;DTSTART:96-Apr-01
--
--     REQUEST-STATUS:2.8; Success\, repeating event ignored. Scheduled
--      as a single event.;RRULE:FREQ=WEEKLY\;INTERVAL=2
--
--     REQUEST-STATUS:4.1;Event conflict.  Date-time is busy.
--
--     REQUEST-STATUS:3.7;Invalid calendar user;ATTENDEE:
--      mailto:jsmith@example.com
-- @
data RequestStatus = RequestStatus
  { requestStatusCode :: !Text, -- TODO more specific type for "1*DIGIT 1*2("." 1*DIGIT)"
    requestStatusDescription :: !Text,
    requestStatusExceptions :: ![Text],
    requestStatusLanguage :: !(Maybe Language)
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity RequestStatus

instance NFData RequestStatus

instance IsProperty RequestStatus where
  propertyName Proxy = "REQUEST-STATUS"
  propertyP clv = do
    let t = contentLineValueRaw clv
    case map unEscapeText (splitOnSemicolons t) of
      requestStatusCode : requestStatusDescription : requestStatusExceptions -> do
        requestStatusLanguage <- propertyParamP clv
        pure RequestStatus {..}
      _ -> unfixableError $ UnReadableStatusCode t
  propertyB RequestStatus {..} =
    insertMParam requestStatusLanguage $
      mkSimpleContentLineValue $
        T.intercalate ";" $
          map escapeText (requestStatusCode : requestStatusDescription : requestStatusExceptions)

makeRequestStatus :: Text -> Text -> [Text] -> RequestStatus
makeRequestStatus requestStatusCode requestStatusDescription requestStatusExceptions =
  let requestStatusLanguage = Nothing
   in RequestStatus {..}

-- Split on semicolons, but not escaped semicolons.
splitOnSemicolons :: Text -> [Text]
splitOnSemicolons = map T.pack . go [] . T.unpack
  where
    go :: String -> String -> [String]
    go acc = \case
      [] -> [reverse acc]
      '\\' : '\\' : rest -> go ('\\' : '\\' : acc) rest
      '\\' : ';' : rest -> go (';' : '\\' : acc) rest
      ';' : rest -> reverse acc : go [] rest
      c : rest -> go (c : acc) rest

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
    -- | Alternate representation
    -- @
    -- The "ALTREP" parameter,
    -- defined in [RFC5545], can be used to provide a "clickable" image
    -- where the URI in the parameter value can be "launched" by a click
    -- on the image in the calendar user agent.
    -- @
    imageAlternateTextRepresentation :: !(Maybe AlternateTextRepresentation),
    imageDisplay :: !(NonEmpty Display)
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Image

instance NFData Image

instance IsProperty Image where
  propertyName Proxy = "IMAGE"
  propertyP clv = do
    mValue <- propertyParamP clv
    imageFormatType <- propertyParamP clv
    imageAlternateTextRepresentation <- propertyParamP clv
    imageDisplay <- fromMaybe defaultDisplay . NE.nonEmpty <$> propertyParamListP clv

    case mValue of
      Just TypeURI -> wrapPropertyTypeP (\u -> let imageContents = Left u in Image {..}) clv
      Just TypeBinary -> wrapPropertyTypeP (\b -> let imageContents = Right b in Image {..}) clv
      _ -> unfixableError $ ValueMismatch "IMAGE" mValue Nothing [TypeURI, TypeBinary]

  propertyB Image {..} =
    insertMParam imageFormatType $
      insertMParam imageAlternateTextRepresentation $
        (if imageDisplay == defaultDisplay then id else insertParamNE imageDisplay) $
          case imageContents of
            Left uri -> typedPropertyTypeB uri
            Right binary -> typedPropertyTypeB binary

makeURIImage :: URI -> Image
makeURIImage uri =
  let imageContents = Left uri
      imageFormatType = Nothing
      imageAlternateTextRepresentation = Nothing
      imageDisplay = defaultDisplay
   in Image {..}

makeBinaryImage :: Binary -> Image
makeBinaryImage binary =
  let imageContents = Right binary
      imageFormatType = Nothing
      imageAlternateTextRepresentation = Nothing
      imageDisplay = defaultDisplay
   in Image {..}
