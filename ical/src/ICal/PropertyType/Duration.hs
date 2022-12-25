{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module ICal.PropertyType.Duration where

import Control.DeepSeq
import Control.Monad
import Data.Data
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Validity
import Data.Void
import GHC.Generics (Generic)
import ICal.Conformance
import ICal.ContentLine
import ICal.PropertyType.Class
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

-- | Duration
--
-- === [section 3.3.6](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.6)
--
-- @
-- Value Name:  DURATION
--
-- Purpose:  This value type is used to identify properties that contain
--    a duration of time.
--
-- Format Definition:  This value type is defined by the following
--    notation:
--
--     dur-value  = (["+"] / "-") "P" (dur-date / dur-time / dur-week)
--
--     dur-date   = dur-day [dur-time]
--     dur-time   = "T" (dur-hour / dur-minute / dur-second)
--     dur-week   = 1*DIGIT "W"
--     dur-hour   = 1*DIGIT "H" [dur-minute]
--     dur-minute = 1*DIGIT "M" [dur-second]
--     dur-second = 1*DIGIT "S"
--     dur-day    = 1*DIGIT "D"
--
-- Description:  If the property permits, multiple "duration" values are
--    specified by a COMMA-separated list of values.  The format is
--    based on the [ISO.8601.2004] complete representation basic format
--    with designators for the duration of time.  The format can
--    represent nominal durations (weeks and days) and accurate
--    durations (hours, minutes, and seconds).  Note that unlike
--    [ISO.8601.2004], this value type doesn't support the "Y" and "M"
--    designators to specify durations in terms of years and months.
--
--    The duration of a week or a day depends on its position in the
--    calendar.  In the case of discontinuities in the time scale, such
--    as the change from standard time to daylight time and back, the
--    computation of the exact duration requires the subtraction or
--    addition of the change of duration of the discontinuity.  Leap
--    seconds MUST NOT be considered when computing an exact duration.
--    When computing an exact duration, the greatest order time
--    components MUST be added first, that is, the number of days MUST
--    be added first, followed by the number of hours, number of
--    minutes, and number of seconds.
--
--    Negative durations are typically used to schedule an alarm to
--    trigger before an associated time (see Section 3.8.6.3).
--
--    No additional content value encoding (i.e., BACKSLASH character
--    encoding, see Section 3.3.11) are defined for this value type.
--
-- Example:  A duration of 15 days, 5 hours, and 20 seconds would be:
--
--     P15DT5H0M20S
--
--    A duration of 7 weeks would be:
--
--     P7W
-- @
data Duration
  = DurationDate !DurDate
  | DurationTime !DurTime
  | DurationWeek !DurWeek
  deriving (Show, Eq, Ord, Generic, Typeable)

instance Validity Duration

instance NFData Duration

instance IsPropertyType Duration where
  propertyTypeP = parseDuration . contentLineValueRaw
  propertyTypeB = mkSimpleContentLineValue . renderDuration

durationOneDay :: Duration
durationOneDay =
  DurationDate
    DurDate
      { durDateSign = Positive,
        durDateDay = 1,
        durDateHour = 0,
        durDateMinute = 0,
        durDateSecond = 0
      }

durationNominalDiffTime :: Duration -> Time.NominalDiffTime
durationNominalDiffTime = \case
  DurationDate durDate -> durDateNominalDiffTime durDate
  DurationTime durTime -> durTimeNominalDiffTime durTime
  DurationWeek durWeek -> durWeekNominalDiffTime durWeek

data DurDate = DurDate
  { durDateSign :: !Sign,
    durDateDay :: !Word,
    durDateHour :: !Word,
    durDateMinute :: !Word,
    durDateSecond :: !Word
  }
  deriving (Show, Eq, Ord, Generic, Typeable)

instance Validity DurDate

instance NFData DurDate

durDateNominalDiffTime :: DurDate -> Time.NominalDiffTime
durDateNominalDiffTime DurDate {..} =
  product
    [ signSignum durDateSign,
      fromIntegral durDateDay * Time.nominalDay
        + (fromIntegral durDateHour * 60 + fromIntegral durDateMinute) * 60
        + fromIntegral durDateSecond
    ]

data DurTime = DurTime
  { durTimeSign :: !Sign,
    durTimeHour :: !Word,
    durTimeMinute :: !Word,
    durTimeSecond :: !Word
  }
  deriving (Show, Eq, Ord, Generic, Typeable)

instance Validity DurTime

instance NFData DurTime

durTimeNominalDiffTime :: DurTime -> Time.NominalDiffTime
durTimeNominalDiffTime DurTime {..} =
  product
    [ signSignum durTimeSign,
      (fromIntegral durTimeHour * 60 + fromIntegral durTimeMinute) * 60
        + fromIntegral durTimeSecond
    ]

data DurWeek = DurWeek
  { durWeekSign :: !Sign,
    durWeekWeek :: !Word
  }
  deriving (Show, Eq, Ord, Generic, Typeable)

instance Validity DurWeek

instance NFData DurWeek

durWeekNominalDiffTime :: DurWeek -> Time.NominalDiffTime
durWeekNominalDiffTime DurWeek {..} =
  product
    [ signSignum durWeekSign,
      fromIntegral durWeekWeek,
      7,
      Time.nominalDay
    ]

data Sign = Positive | Negative
  deriving (Show, Eq, Ord, Generic, Typeable)

instance Validity Sign

instance NFData Sign

durationSign :: Duration -> Sign
durationSign = \case
  DurationDate dd -> durDateSign dd
  DurationTime dt -> durTimeSign dt
  DurationWeek dw -> durWeekSign dw

signSignum :: Sign -> Time.NominalDiffTime
signSignum = \case
  Positive -> 1
  Negative -> -1

-- @
-- dur-value  = (["+"] / "-") "P" (dur-date / dur-time / dur-week)
--
-- dur-date   = dur-day [dur-time]
-- dur-time   = "T" (dur-hour / dur-minute / dur-second)
-- dur-week   = 1*DIGIT "W"
-- dur-hour   = 1*DIGIT "H" [dur-minute]
-- dur-minute = 1*DIGIT "M" [dur-second]
-- dur-second = 1*DIGIT "S"
-- dur-day    = 1*DIGIT "D"
-- @
renderDuration :: Duration -> Text
renderDuration =
  T.pack . concat . \case
    DurationDate DurDate {..} ->
      [ renderSign durDateSign,
        ['P'],
        digitB 'D' durDateDay,
        ['T'],
        hms durDateHour durDateMinute durDateSecond
      ]
    DurationTime DurTime {..} ->
      [ renderSign durTimeSign,
        ['P', 'T'],
        hms durTimeHour durTimeMinute durTimeSecond
      ]
    DurationWeek DurWeek {..} ->
      [ renderSign durWeekSign,
        ['P'],
        digitB 'W' durWeekWeek
      ]
  where
    hms :: Word -> Word -> Word -> String
    hms h m s =
      concat $
        concat
          [ [digitB 'H' h | h > 0 || (m <= 0 && s <= 0)],
            [digitB 'M' m | m > 0 || (h > 0 && s > 0)],
            [digitB 'S' s | s > 0]
          ]
    digitB :: Char -> Word -> String
    digitB c w = show w ++ [c]

renderSign :: Sign -> [Char]
renderSign = \case
  Positive -> []
  Negative -> "-"

parseDuration :: Text -> Conform PropertyTypeParseError Void Void Duration
parseDuration = either (unfixableError . UnparseableDuration) pure . parse go ""
  where
    go :: Parsec Void Text Duration
    go = do
      sign <-
        fromMaybe Positive
          <$> optional ((Positive <$ char '+') <|> (Negative <$ char '-'))
      void $ char 'P'
      let digitP :: Char -> Parsec Void Text Word
          digitP c = do
            d <- decimal
            void $ char c
            pure d
      let durSecondP = digitP 'S'
      let durMinuteP = do
            m <- digitP 'M'
            s <- fromMaybe 0 <$> optional durSecondP
            pure (m, s)
      let durHourP = do
            h <- digitP 'H'
            (m, s) <- fromMaybe (0, 0) <$> optional durMinuteP
            pure (h, m, s)
      let durTimeP = do
            void $ char 'T'
            (h, m, s) <-
              try durHourP
                <|> try ((\(m, s) -> (0, m, s)) <$> durMinuteP)
                <|> ((\s -> (0, 0, s)) <$> durSecondP)
            pure
              DurTime
                { durTimeSign = sign,
                  durTimeHour = h,
                  durTimeMinute = m,
                  durTimeSecond = s
                }
      let durDayP = digitP 'D'
      let durDateP = do
            d <- durDayP
            DurTime {..} <- durTimeP
            pure
              DurDate
                { durDateSign = durTimeSign,
                  durDateDay = d,
                  durDateHour = durTimeHour,
                  durDateMinute = durTimeMinute,
                  durDateSecond = durTimeSecond
                }
      let durWeekP = do
            w <- digitP 'W'
            pure
              DurWeek
                { durWeekSign = sign,
                  durWeekWeek = w
                }
      try (DurationDate <$> durDateP)
        <|> try (DurationTime <$> durTimeP)
        <|> (DurationWeek <$> durWeekP)
