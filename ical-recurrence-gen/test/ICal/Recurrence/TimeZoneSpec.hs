{-# LANGUAGE OverloadedLists #-}

module ICal.Recurrence.TimeZoneSpec (spec) where

import Conformance.TestUtils
import qualified Data.Time as Time
import ICal.Component
import ICal.Component.Gen ()
import ICal.Property
import ICal.PropertyType
import ICal.Recurrence
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  xit "resolution and unresolution roundtrip with a single observance" $ do
    forAllValid $ \tzid ->
      forAllValid $ \start ->
        forAllValid $ \fromOffset ->
          forAllValid $ \toOffset ->
            forAllValid $ \lt -> do
              let from = TimeZoneOffsetFrom fromOffset
                  to = TimeZoneOffsetTo toOffset
                  tz = makeTimeZone tzid [StandardObservance $ Standard $ makeObservance start from to]
              actual <- shouldConform $ do
                resolved <- resolveLocalTime tz lt
                unresolveUTCTime tz resolved
              actual `shouldBe` lt

  describe "resolveDateTime" $ do
    it "Works for any single-standard-observance timezone just like the time library would" $
      forAllValid $ \tzid ->
        forAllValid $ \start ->
          forAllValid $ \fromOffset ->
            forAllValid $ \toOffset ->
              forAllValid $ \lt -> do
                let from = TimeZoneOffsetFrom fromOffset
                    to = TimeZoneOffsetTo toOffset
                    tz = makeTimeZone tzid [StandardObservance $ Standard $ makeObservance start from to]
                    expectedTz =
                      utcOffsetTimeZone $
                        if lt < start
                          then fromOffset
                          else toOffset
                resolved <- shouldConform $ resolveLocalTime tz lt
                resolved `shouldBe` Time.localTimeToUTC expectedTz lt

    it "Works for any single-daylight-observance timezone just like the time library would" $
      forAllValid $ \tzid ->
        forAllValid $ \start ->
          forAllValid $ \fromOffset ->
            forAllValid $ \toOffset ->
              forAllValid $ \lt -> do
                let from = TimeZoneOffsetFrom fromOffset
                    to = TimeZoneOffsetTo toOffset
                    tz = makeTimeZone tzid [DaylightObservance $ Daylight $ makeObservance start from to]
                    expectedTz =
                      utcOffsetTimeZone $
                        if lt < start
                          then fromOffset
                          else toOffset
                resolved <- shouldConform $ resolveLocalTime tz lt
                resolved `shouldBe` Time.localTimeToUTC expectedTz lt

  describe "unresolveDateTime" $ do
    it "Works for any single-standard-observance timezone just like the time library would" $ do
      forAllValid $ \tzid ->
        forAllValid $ \start ->
          forAllValid $ \fromOffset ->
            forAllValid $ \toOffset ->
              forAllValid $ \ut -> do
                let from = TimeZoneOffsetFrom fromOffset
                    to = TimeZoneOffsetTo toOffset
                    tz = makeTimeZone tzid [StandardObservance $ Standard $ makeObservance start from to]
                    expectedTz =
                      utcOffsetTimeZone $
                        if ut < Time.localTimeToUTC (utcOffsetTimeZone fromOffset) start
                          then fromOffset
                          else toOffset
                resolved <- shouldConform $ unresolveUTCTime tz ut
                resolved `shouldBe` Time.utcToLocalTime expectedTz ut

    it "Works for any single-daylight-observance timezone just like the time library would" $ do
      forAllValid $ \tzid ->
        forAllValid $ \start ->
          forAllValid $ \fromOffset ->
            forAllValid $ \toOffset ->
              forAllValid $ \ut -> do
                let from = TimeZoneOffsetFrom fromOffset
                    to = TimeZoneOffsetTo toOffset
                    tz = makeTimeZone tzid [DaylightObservance $ Daylight $ makeObservance start from to]
                    expectedTz =
                      utcOffsetTimeZone $
                        if ut < Time.localTimeToUTC (utcOffsetTimeZone fromOffset) start
                          then fromOffset
                          else toOffset
                resolved <- shouldConform $ unresolveUTCTime tz ut
                resolved `shouldBe` Time.utcToLocalTime expectedTz ut
