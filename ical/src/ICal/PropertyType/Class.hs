{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module ICal.PropertyType.Class
  ( PropertyTypeParseError (..),
    PropertyTypeFixableError (..),
    IsPropertyType (..),

    -- * Helpers for defining IsPropertyType

    -- ** Parsers
    typedPropertyTypeP,
    propertyTypeListP,
    propertyTypeSetP,
    parseTimesSetText,
    parseTimesListText,
    parseTimeStr,

    -- ** Builders
    typedPropertyTypeB,
    propertyTypeListB,
    propertyTypeSetB,

    -- ** Other
    proxyOf,
    escapeText,
    unEscapeText,

    -- * Validation Helpers
    validateImpreciseLocalTime,
    validateImpreciseTimeOfDay,
    validateImpreciseUTCTime,
  )
where

import Control.Exception
import Data.CaseInsensitive (CI)
import Data.Int
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time as Time
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import Data.Void
import ICal.Conformance
import ICal.ContentLine
import ICal.Parameter.Class
import ICal.Parameter.ValueDataType
import Text.Megaparsec
import Text.Read

data PropertyTypeParseError
  = ParameterParseError !ParameterParseError
  | TimeStrParseError
      !String
      -- ^ Format String
      !String
      -- ^ Input String
  | UnexpectedValueType
      !ValueDataType
      -- ^ Actual
      !ValueDataType
      -- ^ Expected
  | UnparseableBinary
      !Text
      -- ^ Input
      !Text
      -- ^ Error
  | UnparseableBoolean !Text
  | UnparseableInteger !Text
  | UnparseableURI !Text
  | UnparseableUTCOffset !Text
  | UnparseablePeriod !Text
  | UnparseableDuration !(ParseErrorBundle Text Void)
  | UnparseableFloatingPoint !Text
  | RecurrenceRulePartNotFound !Text
  | UnknownFrequency !Text
  | UnReadableInterval !Text
  | UnReadableCount !Text
  | UnReadableBySecond !Text
  | UnReadableByMinute !Text
  | UnReadableByHour !Text
  | UnReadableByDay !Text
  | UnReadableByMonthDay !Text
  | UnReadableByYearDay !Text
  | UnReadableByWeekNo !Text
  | UnReadableByMonth !Text
  | UnReadableBySetPos !Text
  | UnReadableDayOfWeek !(CI Text)
  deriving (Show, Eq, Ord)

instance Exception PropertyTypeParseError where
  displayException = \case
    ParameterParseError pe -> displayException pe
    TimeStrParseError formatStr inputStr ->
      unlines
        [ unwords ["Could not parse time value:", show inputStr],
          unwords ["using format string:", show formatStr]
        ]
    UnexpectedValueType actual expected ->
      unlines
        [ "Uxpected value type.",
          unwords ["actual:   ", show actual],
          unwords ["expected: ", show expected]
        ]
    UnparseableBinary t err ->
      unlines
        [ unwords ["Unparseable BINARY:", show err],
          show t
        ]
    UnparseableBoolean t -> unwords ["Unparseable BOOLEAN", show t]
    UnparseableInteger t -> unwords ["Unparseable INTEGER", show t]
    UnparseableURI t -> unwords ["Unparseable URI", show t]
    UnparseableUTCOffset t -> unwords ["Unparseable UTC Offset", show t]
    UnparseablePeriod t -> unwords ["Unparseable Period", show t]
    UnparseableDuration t -> unwords ["Unparseable Duration", show t]
    RecurrenceRulePartNotFound t -> unwords ["Recurrence rule part not found:", show t]
    UnparseableFloatingPoint t -> unwords ["Unparseable Floating point number", show t]
    UnknownFrequency s -> unwords ["Unknown FREQ value:", show s]
    UnReadableInterval s -> unwords ["Unreadable INTERVAL value:", show s]
    UnReadableCount s -> unwords ["Unreadable COUNT value:", show s]
    UnReadableBySecond s -> unwords ["Unreadable BYSECOND value:", show s]
    UnReadableByMinute s -> unwords ["Unreadable BYMINUTE value:", show s]
    UnReadableByHour s -> unwords ["Unreadable BYHOUR value:", show s]
    UnReadableByDay s -> unwords ["Unreadable BYDAY value:", show s]
    UnReadableByMonthDay s -> unwords ["Unreadable BYMONTHDAY value:", show s]
    UnReadableByYearDay s -> unwords ["Unreadable BYYEARDAY value:", show s]
    UnReadableByWeekNo s -> unwords ["Unreadable BYWEEKNO value:", show s]
    UnReadableByMonth s -> unwords ["Unreadable BYMONTH value:", show s]
    UnReadableBySetPos s -> unwords ["Unreadable BYSETPOS value:", show s]
    UnReadableDayOfWeek s -> unwords ["Unknown day of week value:", show s]

data PropertyTypeFixableError
  = UrlTextEncoded !Text
  deriving (Show, Eq, Ord)

instance Exception PropertyTypeFixableError where
  displayException = \case
    UrlTextEncoded t -> unwords ["URL was TEXT-encoded but should not have been:", show t]

-- | Property type
--
-- === [section 3.3](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3)
--
-- @
-- The properties in an iCalendar object are strongly typed.  The
-- definition of each property restricts the value to be one of the
-- value data types, or simply value types, defined in this section.
-- The value type for a property will either be specified implicitly as
-- the default value type or will be explicitly specified with the
-- "VALUE" parameter.  If the value type of a property is one of the
-- alternate valid types, then it MUST be explicitly specified with the
-- "VALUE" parameter.
-- @
--
-- === Laws
--
-- * The property roundtrips through 'ContentLineValue'.
--
-- >>> forAllValid $ \property -> propertyTypeP (propertyTypeB property) == Right property
class IsPropertyType propertyType where
  propertyTypeValueType :: Proxy propertyType -> ValueDataType

  -- | Parser for the property type
  propertyTypeP :: ContentLineValue -> Conform PropertyTypeParseError PropertyTypeFixableError Void propertyType

  -- | Builder for the property type
  propertyTypeB :: propertyType -> ContentLineValue

-- | Integer
--
-- === [section 3.3.8](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.8)
--
-- @
-- Value Name:  INTEGER
--
-- Purpose:  This value type is used to identify properties that contain
--    a signed integer value.
--
-- Format Definition:  This value type is defined by the following
--    notation:
--
--     integer    = (["+"] / "-") 1*DIGIT
--
-- Description:  If the property permits, multiple "integer" values are
--    specified by a COMMA-separated list of values.  The valid range
--    for "integer" is -2147483648 to 2147483647.  If the sign is not
--    specified, then the value is assumed to be positive.
--
--    No additional content value encoding (i.e., BACKSLASH character
--    encoding, see Section 3.3.11) is defined for this value type.
--
-- Example:
--
--     1234567890
--     -1234567890
--     +1234567890
--     432109876
-- @
instance IsPropertyType Int32 where
  -- @
  -- The valid range
  -- for "integer" is -2147483648 to 2147483647.
  -- @
  propertyTypeValueType Proxy = TypeInteger
  propertyTypeP clv =
    let t = contentLineValueRaw clv
        s = T.unpack t
        go s' = case readMaybe s' of
          Nothing -> unfixableError $ UnparseableInteger t
          Just i -> pure i
     in case s of
          '+' : rest -> go rest
          _ -> go s
  propertyTypeB = mkSimpleContentLineValue . T.pack . show

-- | Boolean
--
-- === [section 3.3.2](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.2)
--
-- @
-- Value Name:  BOOLEAN
--
-- Purpose:  This value type is used to identify properties that contain
--    either a "TRUE" or "FALSE" Boolean value.
--
-- Format Definition:  This value type is defined by the following
--    notation:
--
--     boolean    = "TRUE" / "FALSE"
--
-- Description:  These values are case-insensitive text.  No additional
--    content value encoding (i.e., BACKSLASH character encoding, see
--    Section 3.3.11) is defined for this value type.
--
-- Example:  The following is an example of a hypothetical property that
--    has a BOOLEAN value type:
--
--     TRUE
-- @
instance IsPropertyType Bool where
  propertyTypeValueType Proxy = TypeBoolean
  propertyTypeP clv =
    let t = contentLineValueRaw clv
     in case t of
          "TRUE" -> pure True
          "FALSE" -> pure False
          _ -> unfixableError $ UnparseableBoolean t
  propertyTypeB =
    mkSimpleContentLineValue . \case
      True -> "TRUE"
      False -> "FALSE"

-- | Text
--
-- === [section 3.3.11](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.11)
--
-- @
-- Value Name:  TEXT
--
-- Purpose:  This value type is used to identify values that contain
--    human-readable text.
--
-- Format Definition:  This value type is defined by the following
--    notation:
--     text       = *(TSAFE-CHAR / ":" / DQUOTE / ESCAPED-CHAR)
--        ; Folded according to description above
--
--     ESCAPED-CHAR = ("\\" / "\;" / "\," / "\N" / "\n")
--        ; \\ encodes \, \N or \n encodes newline
--        ; \; encodes ;, \, encodes ,
--
--     TSAFE-CHAR = WSP / %x21 / %x23-2B / %x2D-39 / %x3C-5B /
--                  %x5D-7E / NON-US-ASCII
--        ; Any character except CONTROLs not needed by the current
--        ; character set, DQUOTE, ";", ":", "\", ","
--
-- Description:  If the property permits, multiple TEXT values are
--    specified by a COMMA-separated list of values.
--
--    The language in which the text is represented can be controlled by
--    the "LANGUAGE" property parameter.
--
--    An intentional formatted text line break MUST only be included in
--    a "TEXT" property value by representing the line break with the
--    character sequence of BACKSLASH, followed by a LATIN SMALL LETTER
--    N or a LATIN CAPITAL LETTER N, that is "\n" or "\N".
--
--    The "TEXT" property values may also contain special characters
--    that are used to signify delimiters, such as a COMMA character for
--    lists of values or a SEMICOLON character for structured values.
--    In order to support the inclusion of these special characters in
--    "TEXT" property values, they MUST be escaped with a BACKSLASH
--    character.  A BACKSLASH character in a "TEXT" property value MUST
--    be escaped with another BACKSLASH character.  A COMMA character in
--    a "TEXT" property value MUST be escaped with a BACKSLASH
--    character.  A SEMICOLON character in a "TEXT" property value MUST
--    be escaped with a BACKSLASH character.  However, a COLON character
--    in a "TEXT" property value SHALL NOT be escaped with a BACKSLASH
--    character.
--
-- Example:  A multiple line value of:
--
--     Project XYZ Final Review
--     Conference Room - 3B
--     Come Prepared.
--
--    would be represented as:
--
--     Project XYZ Final Review\nConference Room - 3B\nCome Prepared.
-- @
instance IsPropertyType Text where
  propertyTypeValueType Proxy = TypeText
  propertyTypeP = pure . unEscapeText . contentLineValueRaw
  propertyTypeB = mkSimpleContentLineValue . escapeText

propertyTypeListP :: IsPropertyType propertyType => ContentLineValue -> Conform PropertyTypeParseError PropertyTypeFixableError Void [propertyType]
propertyTypeListP clv =
  if T.null (contentLineValueRaw clv)
    then pure []
    else
      let clvs = do
            raw <- T.splitOn "," (contentLineValueRaw clv)
            pure (clv {contentLineValueRaw = raw})
       in mapM typedPropertyTypeP clvs

propertyTypeListB :: IsPropertyType propertyType => [propertyType] -> ContentLineValue
propertyTypeListB = \case
  [] -> emptyContentLineValue
  (pt : pts) ->
    let clv = propertyTypeB pt
        raw =
          T.intercalate "," $
            contentLineValueRaw clv : map (contentLineValueRaw . propertyTypeB) pts
     in clv {contentLineValueRaw = raw}

propertyTypeSetP ::
  (Ord propertyType, IsPropertyType propertyType) =>
  ContentLineValue ->
  Conform PropertyTypeParseError PropertyTypeFixableError Void (Set propertyType)
propertyTypeSetP = fmap S.fromList . propertyTypeListP

propertyTypeSetB ::
  IsPropertyType propertyType =>
  Set propertyType ->
  ContentLineValue
propertyTypeSetB = propertyTypeListB . S.toList

typedPropertyTypeP ::
  forall propertyType.
  IsPropertyType propertyType =>
  ContentLineValue ->
  Conform PropertyTypeParseError PropertyTypeFixableError Void propertyType
typedPropertyTypeP clv = do
  mValueDataType <- conformMapAll ParameterParseError absurd id $ optionalParam (contentLineValueParams clv)
  let typ = propertyTypeValueType (Proxy :: Proxy propertyType)
  case mValueDataType of
    Just typ' ->
      if typ == typ'
        then pure ()
        else unfixableError $ UnexpectedValueType typ' typ
    _ -> pure ()
  propertyTypeP clv

typedPropertyTypeB ::
  forall propertyType.
  IsPropertyType propertyType =>
  propertyType ->
  ContentLineValue
typedPropertyTypeB =
  insertParam (propertyTypeValueType (Proxy :: Proxy propertyType))
    . propertyTypeB

-- | Escape 'Text'
--
-- FIXME this could probably go a LOT faster
-- @
-- ; \\ encodes \, \N or \n encodes newline
-- ; \; encodes ;, \, encodes ,
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
-- ; \\ encodes \, \N or \n encodes newline
-- ; \; encodes ;, \, encodes ,
-- @
unEscapeText :: Text -> Text
unEscapeText =
  T.replace "\\\\" "\\"
    . T.replace "\\," ","
    . T.replace "\\;" ";"
    . T.replace "\\n" "\n"
    . T.replace "\\N" "\n"

validateImpreciseUTCTime :: Time.UTCTime -> Validation
validateImpreciseUTCTime = validateImpreciseLocalTime . Time.utcToLocalTime Time.utc

validateImpreciseLocalTime :: Time.LocalTime -> Validation
validateImpreciseLocalTime lt =
  let tod = Time.localTimeOfDay lt
   in validateImpreciseTimeOfDay tod

validateImpreciseTimeOfDay :: Time.TimeOfDay -> Validation
validateImpreciseTimeOfDay tod =
  declare "The number of seconds is integer" $
    let sec = Time.todSec tod
     in ceiling sec == (floor sec :: Int)

proxyOf :: a -> Proxy a
proxyOf !_ = Proxy

parseTimeStr :: Time.ParseTime t => String -> String -> Conform PropertyTypeParseError void void' t
parseTimeStr formatStr s = case Time.parseTimeM True Time.defaultTimeLocale formatStr s of
  Nothing -> unfixableError $ TimeStrParseError formatStr s
  Just t -> pure t

parseTimesListText :: Time.ParseTime t => String -> Text -> Conform PropertyTypeParseError void void' [t]
parseTimesListText formatStr t =
  if T.null t
    then pure []
    else
      let texts = T.splitOn "," t
       in mapM (parseTimeStr formatStr . T.unpack) texts

parseTimesSetText :: (Ord t, Time.ParseTime t) => String -> Text -> Conform PropertyTypeParseError void void' (Set t)
parseTimesSetText formatStr t = S.fromList <$> parseTimesListText formatStr t
