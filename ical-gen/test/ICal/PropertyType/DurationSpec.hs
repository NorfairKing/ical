{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyType.DurationSpec where

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
    -- @
    -- Example:  A trigger set 15 minutes prior to the start of the event or
    --    to-do.
    --
    --     TRIGGER:-PT15M
    --
    --    A trigger set five minutes after the end of an event or the due
    --    date of a to-do.
    --
    --     TRIGGER;RELATED=END:PT5M
    -- @
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "-PT15M")
      (DurationTime (DurTime {durTimeSign = Negative, durTimeHour = 0, durTimeMinute = 15, durTimeSecond = 0}))
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "PT5M")
      (DurationTime (DurTime {durTimeSign = Positive, durTimeHour = 0, durTimeMinute = 5, durTimeSecond = 0}))
    -- @
    -- TRIGGER;RELATED=END:-P2D
    -- @
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "-P2D")
      ( DurationDate
          DurDate
            { durDateSign = Negative,
              durDateDay = 2,
              durDateHour = 0,
              durDateMinute = 0,
              durDateSecond = 0
            }
      )
  describe "durationOneDay" $
    it "is valid" $
      shouldBeValid durationOneDay
  describe "durationNominalDiffTime" $
    it "produces valid valid" $
      producesValid durationNominalDiffTime
  describe "nominalDiffTimeDuration" $ do
    it "produces valid" $
      producesValid
        nominalDiffTimeDuration
    it "roundtrips for whole seconds" $
      forAllValid $ \seconds ->
        let ndt = fromIntegral (seconds :: Int)
         in durationNominalDiffTime (nominalDiffTimeDuration ndt) `shouldBe` ndt
