{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyType.PeriodSpec where

import qualified Data.Map.Strict as M
import Data.Time
import ICal.ContentLine
import ICal.PropertyType.Duration
import ICal.PropertyType.Gen
import ICal.PropertyType.Period
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  genValidSpec @Period
  propertyTypeSpec @Period
  -- From the spec:
  -- @
  -- Example:  The period starting at 18:00:00 UTC, on January 1, 1997 and
  --    ending at 07:00:00 UTC on January 2, 1997 would be:
  --
  --     19970101T180000Z/19970102T070000Z
  --
  --    The period start at 18:00:00 on January 1, 1997 and lasting 5
  --    hours and 30 minutes would be:
  --
  --     19970101T180000Z/PT5H30M
  -- @
  let ex1 =
        PeriodStartEnd
          (UTCTime (fromGregorian 1997 01 01) (timeOfDayToTime (TimeOfDay 18 00 00)))
          (UTCTime (fromGregorian 1997 01 02) (timeOfDayToTime (TimeOfDay 07 00 00)))

  propertyTypeExampleSpec
    (mkSimpleContentLineValue "19970101T180000Z/19970102T070000Z")
    ex1
  propertyTypeParseExampleSpec
    ( ContentLineValue
        { contentLineValueParams = M.singleton "VALUE" ["PERIOD"],
          contentLineValueRaw = "19970101T180000Z/19970102T070000Z"
        }
    )
    ex1

  let ex2 =
        PeriodStartDuration
          (UTCTime (fromGregorian 1997 01 01) (timeOfDayToTime (TimeOfDay 18 00 00)))
          ( DurationTime
              DurTime
                { durTimeSign = Positive,
                  durTimeHour = 5,
                  durTimeMinute = 30,
                  durTimeSecond = 0
                }
          )
  propertyTypeExampleSpec
    (mkSimpleContentLineValue "19970101T180000Z/PT5H30M")
    ex2
  propertyTypeParseExampleSpec
    ( ContentLineValue
        { contentLineValueParams = M.singleton "VALUE" ["PERIOD"],
          contentLineValueRaw = "19970101T180000Z/PT5H30M"
        }
    )
    ex2
  -- We reject the corrected version of the second example from
  -- [Erratum 7332](https://www.rfc-editor.org/errata/eid7332):
  --
  -- @
  -- Section 3.3.9 says:
  --
  -- The period start at 18:00:00 on January 1, 1997 and lasting 5
  -- hours and 30 minutes would be:
  --
  -- 19970101T180000Z/PT5H30M
  --
  -- It should say:
  --
  -- The period start at 18:00:00 on January 1, 1997 and lasting 5
  -- hours and 30 minutes would be:
  --
  -- 19970101T180000/PT5H30M
  --
  -- Notes:
  --
  -- I do not know if this is an editorial or technical issue.
  --
  -- If I understand the datetime value (Section 3.3.5) correct the last
  -- character should only be "Z" if the value is in UTC.
  --
  -- In the first example in section 3.3.9 UTC is explicitely mentioned but not
  -- in the second example.
  -- @
  xdescribe "rejected" $
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "19970101T180000/PT5H30M")
      ex2
