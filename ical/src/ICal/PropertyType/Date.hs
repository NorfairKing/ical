{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.PropertyType.Date where

import Control.DeepSeq
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import Data.Void
import GHC.Generics (Generic)
import ICal.Conformance
import ICal.ContentLine
import ICal.Parameter
import ICal.PropertyType.Class

-- | Date
--
-- === [section 3.3.4](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.4)
--
-- @
-- Value Name:  DATE
--
-- Purpose:  This value type is used to identify values that contain a
--    calendar date.
--
-- Format Definition:  This value type is defined by the following
--    notation:
--
--     date               = date-value
--
--     date-value         = date-fullyear date-month date-mday
--     date-fullyear      = 4DIGIT
--     date-month         = 2DIGIT        ;01-12
--     date-mday          = 2DIGIT        ;01-28, 01-29, 01-30, 01-31
--                                        ;based on month/year
--
-- Description:  If the property permits, multiple "date" values are
--    specified as a COMMA-separated list of values.  The format for the
--    value type is based on the [ISO.8601.2004] complete
--    representation, basic format for a calendar date.  The textual
--    format specifies a four-digit year, two-digit month, and two-digit
--    day of the month.  There are no separator characters between the
--    year, month, and day component text.
--
--    No additional content value encoding (i.e., BACKSLASH character
--    encoding, see Section 3.3.11) is defined for this value type.
--
-- Example:  The following represents July 14, 1997:
--
--     19970714
-- @
newtype Date = Date {unDate :: Time.Day}
  deriving (Eq, Ord, Generic)

instance Show Date where
  showsPrec d (Date day) =
    showParen (d > 10) $
      showString "Date " . dayShowsPrec 11 day

dayShowsPrec :: Int -> Time.Day -> ShowS
dayShowsPrec d day =
  showParen (d > 10) $
    let (y_, m_, d_) = Time.toGregorian day
     in showString "fromGregorian "
          . showsPrec 11 y_
          . showString " "
          . showsPrec 11 m_
          . showString " "
          . showsPrec 11 d_

instance Validity Date

instance NFData Date

instance IsPropertyType Date where
  propertyTypeP = dateP
  propertyTypeB = dateB

instance IsPropertyType (Set Date) where
  propertyTypeP = propertyTypeSetP
  propertyTypeB = propertyTypeSetB

diffDates :: Date -> Date -> Integer
diffDates (Date a) (Date b) = Time.diffDays a b

dateAddDays :: Integer -> Date -> Date
dateAddDays diff (Date a) = Date $ Time.addDays diff a

dateP :: ContentLineValue -> Conform PropertyTypeParseError Void Void Date
dateP ContentLineValue {..} = do
  parseOfValue TypeDate contentLineValueParams
  parseDate contentLineValueRaw

dateB :: Date -> ContentLineValue
dateB = mkSimpleContentLineValue . renderDate

parseDate :: Text -> Conform PropertyTypeParseError void void Date
parseDate = fmap Date . parseTimeStr dateFormatStr . T.unpack

renderDate :: Date -> Text
renderDate = T.pack . Time.formatTime Time.defaultTimeLocale dateFormatStr . unDate

dateFormatStr :: String
dateFormatStr = "%Y%m%d"
