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

module ICal.PropertyType.Class where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time as Time
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import ICal.ContentLine

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

validateImpreciseLocalTime :: Time.LocalTime -> Validation
validateImpreciseLocalTime lt =
  let tod = Time.localTimeOfDay lt
   in validateImpreciseTimeOfDay tod

validateImpreciseTimeOfDay :: Time.TimeOfDay -> Validation
validateImpreciseTimeOfDay tod =
  declare "The number of seconds is integer" $
    let sec = Time.todSec tod
     in ceiling sec == (floor sec :: Int)
