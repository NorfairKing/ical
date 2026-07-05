{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module ICal.Recurrence.TimeZoneSpec (spec) where

import Conformance.TestUtils
import qualified Data.Time as Time
import ICal.Component
import ICal.Component.Gen ()
import ICal.Parameter (TimeZoneIdentifierParam)
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

  describe "unresolveTimestampR" $ do
    let param = "Europe/Warsaw" :: TimeZoneIdentifierParam
        offset = UTCOffset (2 * 3600) -- +02:00 (CEST)
        tz =
          makeTimeZone
            (TimeZoneIdentifier "Europe/Warsaw")
            [ StandardObservance $
                Standard $
                  makeObservance
                    (Time.LocalTime (Time.fromGregorian 1970 1 1) Time.midnight)
                    (TimeZoneOffsetFrom offset)
                    (TimeZoneOffsetTo offset)
            ]
        limit = Time.fromGregorian 2023 12 31
        run ts = shouldConform $ runR limit [(param, tz)] $ unresolveTimestampR param ts
    it "renders a UTC instant as wall-clock in the target zone (regression: UTC events used to be kept at UTC wall-clock)" $ do
      actual <-
        run
          ( TimestampUTCTime
              (Time.UTCTime (Time.fromGregorian 2023 7 25) (Time.timeOfDayToTime (Time.TimeOfDay 19 0 0)))
          )
      actual `shouldBe` Right (Time.LocalTime (Time.fromGregorian 2023 7 25) (Time.TimeOfDay 21 0 0))
    it "passes any floating local time through unchanged" $
      forAllValid $ \lt -> do
        actual <- run (TimestampLocalTime lt)
        actual `shouldBe` Right lt
    it "passes any date through unchanged" $
      forAllValid $ \d -> do
        actual <- run (TimestampDay d)
        actual `shouldBe` Left d
