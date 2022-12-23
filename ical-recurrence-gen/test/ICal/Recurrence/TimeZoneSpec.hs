{-# LANGUAGE OverloadedLists #-}

module ICal.Recurrence.TimeZoneSpec (spec) where

import qualified Data.Time as Time
import ICal.Component
import ICal.Component.Gen
import ICal.Conformance.TestUtils
import ICal.Property
import ICal.PropertyType
import ICal.Recurrence.TimeZone
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "resolveDateTime" $ do
    it "Works for any single-standard-observance timezone just like the time library would" $
      forAllValid $ \tzid ->
        forAllValid $ \start ->
          forAllValid $ \from ->
            forAllValid $ \toMinutes ->
              forAllValid $ \lt -> do
                let to = TimeZoneOffsetTo $ UTCOffset $ fromIntegral toMinutes
                    tz = makeTimeZone tzid [StandardObservance $ Standard $ makeObservance start from to]
                    timeTz = Time.minutesToTimeZone toMinutes
                mResolved <- shouldConform $ resolveLocalTime tz lt
                case mResolved of
                  Nothing -> pure () -- Fine
                  Just resolved -> resolved `shouldBe` Time.localTimeToUTC timeTz lt

    it "Works for any single-daylight-observance timezone just like the time library would" $
      forAllValid $ \tzid ->
        forAllValid $ \start ->
          forAllValid $ \from ->
            forAllValid $ \toMinutes ->
              forAllValid $ \lt -> do
                let to = TimeZoneOffsetTo $ UTCOffset $ fromIntegral toMinutes
                    tz = makeTimeZone tzid [DaylightObservance $ Daylight $ makeObservance start from to]
                    timeTz = Time.minutesToTimeZone toMinutes
                mResolved <- shouldConform $ resolveLocalTime tz lt
                case mResolved of
                  Nothing -> pure () -- Fine
                  Just resolved -> resolved `shouldBe` Time.localTimeToUTC timeTz lt

  describe "unresolveDateTime" $ do
    it "Works for any single-standard-observance timezone just like the time library would" $ do
      forAllValid $ \tzid ->
        forAllValid $ \start ->
          forAllValid $ \from ->
            forAllValid $ \toMinutes ->
              forAllValid $ \ut ->
                let to = TimeZoneOffsetTo $ UTCOffset $ fromIntegral toMinutes
                    tz = makeTimeZone tzid [StandardObservance $ Standard $ makeObservance start from to]
                    timeTz = Time.minutesToTimeZone toMinutes
                 in unresolveLocalTime tz ut `shouldBe` Time.utcToLocalTime timeTz ut

    it "Works for any single-daylight-observance timezone just like the time library would" $ do
      forAllValid $ \tzid ->
        forAllValid $ \start ->
          forAllValid $ \from ->
            forAllValid $ \toMinutes ->
              forAllValid $ \ut ->
                let to = TimeZoneOffsetTo $ UTCOffset $ fromIntegral toMinutes
                    tz = makeTimeZone tzid [DaylightObservance $ Daylight $ makeObservance start from to]
                    timeTz = Time.minutesToTimeZone toMinutes
                 in unresolveLocalTime tz ut `shouldBe` Time.utcToLocalTime timeTz ut
