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

-- [section 3.3](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3)
class IsPropertyType propertyType where
  -- | Parser for the property type
  propertyTypeP :: ContentLineValue -> Either String propertyType

  -- | Builder for the property type
  propertyTypeB :: propertyType -> ContentLineValue

-- [section 3.3.11](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.11)
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

-- [section 3.3.5](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.5)
data DateTime
  = DateTimeFloating !Time.LocalTime
  | DateTimeUTC !Time.LocalTime
  | DateTimeZoned !TZIDParam !Time.LocalTime -- TODO make this a timezoneID?
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

-- [section 3.3.4](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.4)
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

-- [section 3.3.12](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.12)
--
-- @
--      Purpose:  This value type is used to identify values that contain a
--         time of day.
--
--      Format Definition:  This value type is defined by the following
--         notation:
--
--          time         = time-hour time-minute time-second [time-utc]
--
--          time-hour    = 2DIGIT        ;00-23
--          time-minute  = 2DIGIT        ;00-59
--          time-second  = 2DIGIT        ;00-60
--          ;The "60" value is used to account for positive "leap" seconds.
--
--          time-utc     = "Z"
--
-- @
data Time
  = TimeFloating !Time.TimeOfDay
  | TimeUTC !Time.TimeOfDay
  | TimeZoned !TZIDParam !Time.TimeOfDay
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
