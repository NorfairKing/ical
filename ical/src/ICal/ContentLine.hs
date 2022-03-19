{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ICal.ContentLine where

import Control.Arrow (left)
import Control.Monad
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Char as Char
import Data.Map (Map)
import qualified Data.Map as M
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Builder as Text
import Data.Validity
import Data.Validity.Containers
import Data.Validity.Text ()
import Data.Void
import GHC.Generics (Generic)
import ICal.UnfoldedLine
import Text.Megaparsec
import Text.Megaparsec.Char

instance Validity a => Validity (CI a) where
  validate = validate . CI.original

-- https://datatracker.ietf.org/doc/html/rfc5545#section-3.1
-- "The iCalendar object is organized into individual lines of text, called
-- content lines."
--
-- "All names of properties, property parameters, enumerated property
-- values and property parameter values are case-insensitive.  However,
-- all other property values are case-sensitive, unless otherwise
-- stated."
--
-- https://datatracker.ietf.org/doc/html/rfc5545#section-3.2
-- "A property can have attributes with which it is associated.  These
-- "property parameters" contain meta-information about the property or
-- the property value.  Property parameters are provided to specify such
-- information as the location of an alternate text representation for a
-- property value, the language of a text property value, the value type
-- of the property value, and other attributes."
data ContentLine = ContentLine
  { contentLineName :: !(CI Text),
    contentLineParams :: !(Map (CI Text) Text),
    contentLineValue :: !Text
  }
  deriving stock (Show, Eq, Generic)

instance Validity ContentLine where
  validate cl@ContentLine {..} =
    mconcat
      [ genericValidate cl,
        declare "The name is not empty" $ not $ T.null $ CI.original contentLineName,
        decorate "The name contains only key characters" $ validateKeyText contentLineName,
        decorateMap contentLineParams $ \k _ ->
          mconcat
            [ declare "The key is not empty" $ not $ T.null $ CI.original k,
              decorate "The key only contains key characters" $ validateKeyText k
            ]
      ]

validateKeyText :: CI Text -> Validation
validateKeyText c = decorateList (T.unpack (CI.original c)) validateKeyChar

validateKeyChar :: Char -> Validation
validateKeyChar c =
  declare "The character is a key character" $ Char.isAlphaNum c

parseContentLine :: UnfoldedLine -> Either String ContentLine
parseContentLine (UnfoldedLine t) = left errorBundlePretty $ parse contentLineP "" t

type P = Parsec Void Text

contentLineP :: P ContentLine
contentLineP = do
  let tokenChar = letterChar <|> digitChar <|> char '-'
  contentLineName <- CI.mk . T.pack <$> some tokenChar
  contentLineParams <- fmap M.fromList $
    many $ do
      void $ char ';'
      key <- T.pack <$> some tokenChar
      void $ char '='
      value <- T.pack <$> many tokenChar -- TODO or quoted string
      pure (CI.mk key, value)

  void $ char ':'
  contentLineValue <- takeRest

  pure ContentLine {..}

renderContentLine :: ContentLine -> UnfoldedLine
renderContentLine ContentLine {..} =
  UnfoldedLine $
    T.concat
      [ CI.original contentLineName,
        T.concat $
          ( map
              (\(k, v) -> ";" <> CI.original k <> "=" <> v)
              (M.toList contentLineParams)
          ),
        ":",
        contentLineValue
      ]
