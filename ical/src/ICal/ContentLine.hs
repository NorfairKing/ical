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
import Data.Validity.Containers ()
import Data.Validity.Text ()
import GHC.Generics (Generic)
import ICal.UnfoldedLine

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
    contentLineParams :: !(Map (CI Text) (CI Text)),
    contentLineValue :: !Text
  }
  deriving stock (Show, Eq, Generic)

instance Validity ContentLine

parseContentLine :: UnfoldedLine -> Either String ContentLine
parseContentLine = undefined

renderContentLine :: ContentLine -> UnfoldedLine
renderContentLine ContentLine {..} =
  UnfoldedLine $
    T.concat
      [ CI.original contentLineName,
        T.intercalate
          ";"
          ( map
              (\(k, v) -> CI.original k <> "=" <> CI.original v)
              (M.toList contentLineParams)
          ),
        ":",
        contentLineValue
      ]
