{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module ICal.Component.TimeZoneSpec (spec) where

import qualified Data.Time as Time
import ICal.Component
import ICal.Component.Gen
import ICal.Property
import ICal.PropertyType
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Observance" $ do
    genValidSpec @Observance

  describe "Standard" $ do
    genValidSpec @Standard
    componentSpec @Standard

  describe "Daylight" $ do
    genValidSpec @Daylight
    componentSpec @Daylight

  describe "TimeZone" $ do
    genValidSpec @TimeZoneObservance
    genValidSpec @Standard
    genValidSpec @Daylight
    genValidSpec @Observance
    genValidSpec @TimeZone
    componentSpec @TimeZone

  componentScenarioDir @TimeZone "test_resources/timezones"

  describe "resolveDateTime" $ do
    xit "Works for any single-standard-observance timezone just like the time library would" $
      forAllValid $ \tzid ->
        forAllValid $ \start ->
          forAllValid $ \from ->
            forAllValid $ \toMinutes ->
              forAllValid $ \lt ->
                let to = TimeZoneOffsetTo $ UTCOffset $ fromIntegral toMinutes
                    tz = makeTimeZone tzid [StandardObservance $ Standard $ makeObservance start from to]
                    timeTz = Time.minutesToTimeZone toMinutes
                 in resolveLocalTime tz lt `shouldBe` Time.localTimeToUTC timeTz lt

    xit "Works for any single-daylight-observance timezone just like the time library would" $
      forAllValid $ \tzid ->
        forAllValid $ \start ->
          forAllValid $ \from ->
            forAllValid $ \toMinutes ->
              forAllValid $ \lt ->
                let to = TimeZoneOffsetTo $ UTCOffset $ fromIntegral toMinutes
                    tz = makeTimeZone tzid [DaylightObservance $ Daylight $ makeObservance start from to]
                    timeTz = Time.minutesToTimeZone toMinutes
                 in resolveLocalTime tz lt `shouldBe` Time.localTimeToUTC timeTz lt

  describe "unresolveDateTime" $ do
    xit "Works for any single-standard-observance timezone just like the time library would" $ do
      forAllValid $ \tzid ->
        forAllValid $ \start ->
          forAllValid $ \from ->
            forAllValid $ \toMinutes ->
              forAllValid $ \ut ->
                let to = TimeZoneOffsetTo $ UTCOffset $ fromIntegral toMinutes
                    tz = makeTimeZone tzid [StandardObservance $ Standard $ makeObservance start from to]
                    timeTz = Time.minutesToTimeZone toMinutes
                 in unresolveLocalTime tz ut `shouldBe` Time.utcToLocalTime timeTz ut

    xit "Works for any single-daylight-observance timezone just like the time library would" $ do
      forAllValid $ \tzid ->
        forAllValid $ \start ->
          forAllValid $ \from ->
            forAllValid $ \toMinutes ->
              forAllValid $ \ut ->
                let to = TimeZoneOffsetTo $ UTCOffset $ fromIntegral toMinutes
                    tz = makeTimeZone tzid [DaylightObservance $ Daylight $ makeObservance start from to]
                    timeTz = Time.minutesToTimeZone toMinutes
                 in unresolveLocalTime tz ut `shouldBe` Time.utcToLocalTime timeTz ut
