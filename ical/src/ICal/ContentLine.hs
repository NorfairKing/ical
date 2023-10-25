{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.ContentLine
  ( ContentLine (..),
    mkSimpleContentLine,
    ContentLineName (..),
    ContentLineValue (..),
    emptyContentLineValue,
    mkSimpleContentLineValue,
    parseContentLineFromUnfoldedLine,
    renderContentLineToUnfoldedLine,
    ParamName (..),
    ParamValue (..),
    paramValueCI,
    haveToQuoteText,

    -- * Raw parser
    P,
    contentLineP,
    contentLineNameP,
    contentLineValueP,
    paramNameP,
    paramValueP,

    -- * Raw builders
    contentLineB,
    contentLineNameB,
    renderContentLineName,
    contentLineValueB,
    paramNameB,
    paramValueB,

    -- * Validation helpers
    validateSafeChar,
    validateQSafeChar,
    validateNameChar,
    validateVendorIdChar,
  )
where

import Control.Arrow (left)
import Control.DeepSeq
import Control.Monad
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Char as Char
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Builder as Text
import Data.Validity
import Data.Validity.CaseInsensitive ()
import Data.Validity.Containers ()
import Data.Validity.Text
import Data.Void
import GHC.Generics (Generic)
import ICal.UnfoldedLine
import Text.Megaparsec
import Text.Megaparsec.Char

-- [section 3.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.1)
-- @
-- The iCalendar object is organized into individual lines of text, called
-- content lines.
-- @
--
-- @
-- All names of properties, property parameters, enumerated property
-- values and property parameter values are case-insensitive.  However,
-- all other property values are case-sensitive, unless otherwise
-- stated.
-- @
--
-- [section 3.2](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2)
-- @
-- A property can have attributes with which it is associated.  These
-- "property parameters" contain meta-information about the property or
-- the property value.  Property parameters are provided to specify such
-- information as the location of an alternate text representation for a
-- property value, the language of a text property value, the value type
-- of the property value, and other attributes.
-- @
data ContentLine = ContentLine
  { contentLineName :: !ContentLineName,
    contentLineValue :: !ContentLineValue
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

instance Validity ContentLine

instance NFData ContentLine

instance IsString ContentLine where
  fromString s =
    let t = fromString s
     in case parse contentLineP "" t of
          Left err -> error $ errorBundlePretty err
          Right cln -> cln

mkSimpleContentLine :: CI Text -> Text -> ContentLine
mkSimpleContentLine name value =
  ContentLine
    { contentLineName = ContentLineName name,
      contentLineValue = mkSimpleContentLineValue value
    }

data ContentLineValue = ContentLineValue
  { contentLineValueParams :: !(Map ParamName (NonEmpty ParamValue)),
    contentLineValueRaw :: !Text
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

instance Validity ContentLineValue

instance NFData ContentLineValue

mkSimpleContentLineValue :: Text -> ContentLineValue
mkSimpleContentLineValue value =
  ContentLineValue
    { contentLineValueParams = M.empty,
      contentLineValueRaw = value
    }

emptyContentLineValue :: ContentLineValue
emptyContentLineValue = mkSimpleContentLineValue ""

newtype ContentLineName = ContentLineName {unContentLineName :: CI Text}
  deriving stock (Show, Read, Eq, Ord, Generic)

instance Validity ContentLineName where
  validate cln@(ContentLineName t) =
    mconcat
      [ genericValidate cln,
        declare "The name is not empty" $ not $ T.null $ CI.original t,
        decorateText (CI.original t) validateNameChar
      ]

instance NFData ContentLineName

instance IsString ContentLineName where
  fromString s =
    let t = fromString s
     in case parse contentLineNameP "" t of
          Left err -> error $ errorBundlePretty err
          Right cln -> cln

newtype ParamName = ParamName {unParamName :: CI Text}
  deriving stock (Show, Read, Eq, Ord, Generic)

instance Validity ParamName where
  validate pn@(ParamName t) =
    mconcat
      [ genericValidate pn,
        declare "The name is not empty" $ not $ T.null $ CI.original t,
        decorateText (CI.original t) validateNameChar
      ]

instance NFData ParamName

instance IsString ParamName where
  fromString s =
    let t = fromString s
     in case parse paramNameP "" t of
          Left err -> error $ errorBundlePretty err
          Right pn -> pn

-- https://datatracker.ietf.org/doc/html/rfc5545#section-3.2
-- "Property parameter values that are not in quoted-strings are case-
-- insensitive."
data ParamValue
  = UnquotedParam !(CI Text)
  | QuotedParam Text
  deriving stock (Show, Read, Eq, Ord, Generic)

instance Validity ParamValue where
  validate pv =
    mconcat
      [ genericValidate pv,
        case pv of
          UnquotedParam c -> decorateText (CI.original c) validateSafeChar
          QuotedParam t -> decorateText t validateQSafeChar
      ]

instance NFData ParamValue

instance IsString ParamValue where
  fromString s =
    let t = fromString s
     in if haveToQuoteText t
          then QuotedParam t
          else UnquotedParam (CI.mk t)

paramValueCI :: ParamValue -> CI Text
paramValueCI = \case
  UnquotedParam ci -> ci
  QuotedParam t -> CI.mk t

-- @
-- When parsing a content line, folded lines MUST first be unfolded
-- according to the unfolding procedure described above.
-- @
parseContentLineFromUnfoldedLine :: UnfoldedLine -> Either String ContentLine
parseContentLineFromUnfoldedLine (UnfoldedLine t) = left errorBundlePretty $ parse contentLineP "" t

renderContentLineToUnfoldedLine :: ContentLine -> UnfoldedLine
renderContentLineToUnfoldedLine =
  UnfoldedLine . LT.toStrict . LTB.toLazyText . contentLineB

type P = Parsec Void Text

contentLineP :: P ContentLine
contentLineP = do
  contentLineName <- contentLineNameP
  contentLineValue <- contentLineValueP
  pure ContentLine {..}

-- name          = iana-token / x-name
-- iana-token    = 1*(ALPHA / DIGIT / "-")
-- ; iCalendar identifier registered with IANA
contentLineNameP :: P ContentLineName
contentLineNameP = ContentLineName <$> tokenTextP

contentLineValueP :: P ContentLineValue
contentLineValueP = do
  contentLineValueParams <- fmap M.fromList $
    many $ do
      void $ char ';'
      paramP
  void $ char ':'
  contentLineValueRaw <- contentLineValueRawP
  pure ContentLineValue {..}

contentLineValueRawP :: P Text
contentLineValueRawP = takeRest

-- iana-token    = 1*(ALPHA / DIGIT / "-")
tokenTextP :: P (CI Text)
tokenTextP = CI.mk . T.pack <$> some (letterChar <|> digitChar <|> char '-')

validateNameChar :: Char -> Validation
validateNameChar c = declare "The character is a name character" $ Char.isAlpha c || Char.isDigit c || c == '-'

validateVendorIdChar :: Char -> Validation
validateVendorIdChar c =
  declare "The character is a letter or a digit" $
    Char.isAlpha c || Char.isDigit c

-- param         = param-name "=" param-value *("," param-value)
-- ; Each property defines the specific ABNF for the parameters
-- ; allowed on the property.  Refer to specific properties for
-- ; precise parameter ABNF.
paramP :: P (ParamName, NonEmpty ParamValue)
paramP = do
  name <- paramNameP
  void $ char' '='
  firstValue <- paramValueP
  restOfValues <- many $ do
    void $ char' ','
    paramValueP
  pure (name, firstValue :| restOfValues)

-- param-name    = iana-token / x-name
paramNameP :: P ParamName
paramNameP = ParamName <$> tokenTextP

-- param-value   = paramtext / quoted-string
paramValueP :: P ParamValue
paramValueP = try (QuotedParam <$> quotedStringP) <|> (UnquotedParam <$> paramTextP)

--  paramtext     = *SAFE-CHAR
paramTextP :: P (CI Text)
paramTextP = CI.mk . T.pack <$> many safeCharP

-- quoted-string = DQUOTE *QSAFE-CHAR DQUOTE
quotedStringP :: P Text
quotedStringP = do
  void $ char' '"'
  t <- T.pack <$> many qSafeCharP
  void $ char' '"'
  pure t

-- QSAFE-CHAR    = WSP / %x21 / %x23-7E / NON-US-ASCII
-- ; Any character except CONTROL and DQUOTE
qSafeCharP :: P Char
qSafeCharP = satisfy $ validationIsValid . validateQSafeChar

validateQSafeChar :: Char -> Validation
validateQSafeChar c = do
  let o = ord c
  declare "The character is a quote-safe character" $ case c of
    _ | o < 0x09 -> True
    '\t' -> True -- 0x09, part of WSP
    _ | 0x09 < o && o < 0x20 -> True
    ' ' -> True -- 0x20, part of WSP
    _ | 0x21 == o -> True -- %x21
    '"' -> False -- 0x22
    _ | 0x23 <= o && o <= 0x7E -> True -- %x23-7E
    '\DEL' -> False -- 0x7F
    _ | o >= 0x80 -> True -- NON-US-ASCII
    _ -> False

-- SAFE-CHAR     = WSP / %x21 / %x23-2B / %x2D-39 / %x3C-7E
--               / NON-US-ASCII
-- ; Any character except CONTROL, DQUOTE, ";", ":", ","
safeCharP :: P Char
safeCharP = satisfy $ validationIsValid . validateSafeChar

validateSafeChar :: Char -> Validation
validateSafeChar c = do
  let o = ord c
  declare "The character is a safe character" $ case c of
    _ | o < 0x09 -> True
    '\t' -> True -- 0x09, part of WSP
    _ | 0x09 < o && o < 0x20 -> True
    ' ' -> True -- 0x20, part of WSP
    _ | 0x21 == o -> True -- %x21
    '"' -> False -- 0x22
    _ | 0x23 <= o && o <= 0x2B -> True -- %x23-2B
    ',' -> False -- 0x2C
    _ | 0x2D <= o && o <= 0x39 -> True -- %x2D-39
    ':' -> False -- 0x3A
    ';' -> False -- 0x3B
    _ | 0x3C <= o && o <= 0x7E -> True -- %x3C-7E
    '\DEL' -> False -- 0x7F
    _ | o >= 0x80 -> True -- NON-US-ASCII
    _ -> False

contentLineB :: ContentLine -> Text.Builder
contentLineB ContentLine {..} =
  mconcat
    [ contentLineNameB contentLineName,
      contentLineValueB contentLineValue
    ]

contentLineValueB :: ContentLineValue -> Text.Builder
contentLineValueB ContentLineValue {..} =
  mconcat
    [ contentLineParamsB contentLineValueParams,
      LTB.singleton ':',
      LTB.fromText contentLineValueRaw
    ]

renderContentLineName :: ContentLineName -> Text
renderContentLineName = LT.toStrict . LTB.toLazyText . contentLineNameB

contentLineNameB :: ContentLineName -> Text.Builder
contentLineNameB = LTB.fromText . CI.original . unContentLineName

contentLineParamsB :: Map ParamName (NonEmpty ParamValue) -> Text.Builder
contentLineParamsB = foldMap go . M.toList
  where
    go :: (ParamName, NonEmpty ParamValue) -> Text.Builder
    go (key, values) =
      mconcat
        [ LTB.singleton ';',
          paramNameB key,
          LTB.singleton '=',
          paramValuesB values
        ]

paramValuesB :: NonEmpty ParamValue -> Text.Builder
paramValuesB = mconcat . intersperse (LTB.singleton ',') . map paramValueB . NE.toList

paramNameB :: ParamName -> Text.Builder
paramNameB = LTB.fromText . CI.original . unParamName

paramValueB :: ParamValue -> Text.Builder
paramValueB = \case
  UnquotedParam c -> LTB.fromText (CI.original c)
  QuotedParam t ->
    mconcat
      [ LTB.singleton '"',
        LTB.fromText t,
        LTB.singleton '"'
      ]

haveToQuoteText :: Text -> Bool
haveToQuoteText = T.any haveToQuoteChar

haveToQuoteChar :: Char -> Bool
haveToQuoteChar = \case
  '"' -> True
  ';' -> True
  ':' -> True
  ',' -> True
  c -> Char.isControl c
