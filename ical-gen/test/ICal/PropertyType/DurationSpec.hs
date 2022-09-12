{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyType.DurationSpec where

import Control.Monad
import ICal.ContentLine
import ICal.PropertyType.Duration
import ICal.PropertyType.Duration.Gen ()
import ICal.PropertyType.Gen
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Duration" $ do
    genValidSpec @DurDate
    genValidSpec @DurTime
    genValidSpec @DurWeek
    genValidSpec @Duration
    propertyTypeSpec @Duration
  describe "durationP" $ do
    -- From the spec:
    -- @
    -- Example:  A duration of 15 days, 5 hours, and 20 seconds would be:
    --
    --     P15DT5H0M20S
    --
    --    A duration of 7 weeks would be:
    --
    --     P7W
    -- @
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "P15DT5H0M20S")
      ( DurationDate $
          DurDate
            { durDateSign = Positive,
              durDateDay = 15,
              durDateHour = 5,
              durDateMinute = 0,
              durDateSecond = 20
            }
      )
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "P7W")
      ( DurationWeek $
          DurWeek
            { durWeekSign = Positive,
              durWeekWeek = 7
            }
      )
