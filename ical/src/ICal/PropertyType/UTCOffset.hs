{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module ICal.PropertyType.UTCOffset where

import Control.DeepSeq
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import GHC.Generics (Generic)
import ICal.Conformance
import ICal.ContentLine
import ICal.PropertyType.Class
import Text.Read

-- | UTC Offset
--
-- === [section 3.3.14](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.14)
--
-- @
--    Value Name:  UTC-OFFSET
--
--    Purpose:  This value type is used to identify properties that contain
--       an offset from UTC to local time.
--
--    Format Definition:  This value type is defined by the following
--       notation:
--
--        utc-offset = time-numzone
--
--        time-numzone = ("+" / "-") time-hour time-minute [time-second]
--
--    Description:  The PLUS SIGN character MUST be specified for positive
--       UTC offsets (i.e., ahead of UTC).  The HYPHEN-MINUS character MUST
--       be specified for negative UTC offsets (i.e., behind of UTC).  The
--
--       value of "-0000" and "-000000" are not allowed.  The time-second,
--       if present, MUST NOT be 60; if absent, it defaults to zero.
--
--       No additional content value encoding (i.e., BACKSLASH character
--       encoding, see Section 3.3.11) is defined for this value type.
--
--    Example:  The following UTC offsets are given for standard time for
--       New York (five hours behind UTC) and Geneva (one hour ahead of
--       UTC):
--
--        -0500
--
--        +0100
-- @
newtype UTCOffset = UTCOffset {unUTCOffset :: Int32}
  deriving (Show, Eq, Ord, Generic)

instance Validity UTCOffset where
  validate uo@(UTCOffset offsetSeconds) =
    mconcat
      [ genericValidate uo,
        declare "the offset is in a sensible range" $
          -utcOffsetAbsBound < offsetSeconds && offsetSeconds < utcOffsetAbsBound
      ]

utcOffsetAbsBound :: Int32
utcOffsetAbsBound = ((24 * 60) + 60) * 60 + 60

instance NFData UTCOffset

instance IsPropertyType UTCOffset where
  propertyTypeP clv =
    let t = contentLineValueRaw clv
     in maybe (unfixableError $ UnparseableUTCOffset t) pure $ parseUTCOffset t
  propertyTypeB = mkSimpleContentLineValue . renderUTCOffset

parseUTCOffset :: Text -> Maybe UTCOffset
parseUTCOffset =
  ( \str ->
      let goOn r = case r of
            [h1, h2, m1, m2] -> do
              h <- readMaybe [h1, h2]
              m <- readMaybe [m1, m2]
              pure $ (h * 60 + m) * 60
            [h1, h2, m1, m2, s1, s2] -> do
              h <- readMaybe [h1, h2]
              m <- readMaybe [m1, m2]
              s <- readMaybe [s1, s2]
              pure $ (h * 60 + m) * 60 + s
            _ -> Nothing
       in UTCOffset <$> case str of
            ('+' : rest) -> goOn rest
            ('-' : rest) -> negate <$> goOn rest
            _ -> goOn str
  )
    . T.unpack

renderUTCOffset :: UTCOffset -> Text
renderUTCOffset =
  T.pack
    . ( \i ->
          let sign = if i >= 0 then '+' else '-'
              a = abs i
              seconds = a `rem` 60
              zeroPad = \case
                [] -> "00"
                [c] -> ['0', c]
                s -> s
              secondStr = zeroPad $ show seconds
              minutes = (a `div` 60) `rem` 60
              minutesStr = zeroPad $ show minutes
              hours = a `div` 60 `div` 60
              hoursStr = zeroPad $ show hours
           in sign : hoursStr ++ minutesStr ++ (if seconds == 0 then "" else secondStr)
      )
    . unUTCOffset

utcOffsetTimeZone :: UTCOffset -> Time.TimeZone
utcOffsetTimeZone (UTCOffset w) = Time.minutesToTimeZone $ fromIntegral w
