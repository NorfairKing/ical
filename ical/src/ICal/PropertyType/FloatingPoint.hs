{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module ICal.PropertyType.FloatingPoint
  ( FloatingPoint (..),
  )
where

import Conformance
import Control.DeepSeq
import Data.Proxy
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import GHC.Generics (Generic)
import ICal.ContentLine
import ICal.Parameter
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

instance NFData FloatingPoint

instance IsPropertyType FloatingPoint where
  propertyTypeValueType Proxy = TypeFloat
  propertyTypeP ContentLineValue {..} = do
    case readMaybe (T.unpack contentLineValueRaw) of
      Nothing -> unfixableError $ UnparseableFloatingPoint contentLineValueRaw
      Just f -> pure (FloatingPoint f)
  propertyTypeB = mkSimpleContentLineValue . T.pack . printf "%f" . unFloatingPoint
