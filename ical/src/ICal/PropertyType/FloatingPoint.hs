{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module ICal.PropertyType.FloatingPoint where

import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import GHC.Generics (Generic)
import ICal.ContentLine
import ICal.PropertyType.Class
import Text.Printf
import Text.Read

-- | Floating point number
--
-- === [section 3.3.7](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.7)
--
-- @
-- Value Name:  FLOAT
--
-- Purpose:  This value type is used to identify properties that contain
--    a real-number value.
--
-- Format Definition:  This value type is defined by the following
--    notation:
--
--     float      = (["+"] / "-") 1*DIGIT ["." 1*DIGIT]
--
-- Description:  If the property permits, multiple "float" values are
--    specified by a COMMA-separated list of values.
--
--    No additional content value encoding (i.e., BACKSLASH character
--    encoding, see Section 3.3.11) is defined for this value type.
--
-- Example:
--
--     1000000.0000001
--     1.333
--     -3.14
-- @
newtype FloatingPoint = FloatingPoint {unFloatingPoint :: Double}
  deriving (Show, Eq, Generic)

instance Validity FloatingPoint where
  validate fp@FloatingPoint {..} =
    mconcat
      [ genericValidate fp,
        validateNotNaN unFloatingPoint,
        validateNotInfinite unFloatingPoint
      ]

instance IsPropertyType FloatingPoint where
  propertyTypeP :: ContentLineValue -> Either String FloatingPoint
  propertyTypeP ContentLineValue {..} =
    let s = T.unpack contentLineValueRaw
     in case readMaybe s of
          Nothing -> Left $ unwords ["Could not read FLOAT: " <> s]
          Just f -> Right (FloatingPoint f)
  propertyTypeB :: FloatingPoint -> ContentLineValue
  propertyTypeB = mkSimpleContentLineValue . T.pack . printf "%f" . unFloatingPoint
