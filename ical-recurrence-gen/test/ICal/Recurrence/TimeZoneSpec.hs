{-# LANGUAGE OverloadedLists #-}

module ICal.Recurrence.TimeZoneSpec (spec) where

import qualified Data.Map as M
import qualified Data.Time as Time
import ICal.Component
import ICal.Component.Gen ()
import ICal.Conformance.TestUtils
import ICal.Property
import ICal.PropertyType
import ICal.Recurrence
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
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
                    m = M.singleton (tzidParam tzid) tz
                resolved <- shouldConform $ runR m $ resolveLocalTime tz lt
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
                    m = M.singleton (tzidParam tzid) tz
                resolved <- shouldConform $ runR m $ resolveLocalTime tz lt
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
                        if ut < Time.localTimeToUTC (utcOffsetTimeZone (unTimeZoneOffsetFrom from)) start
                          then fromOffset
                          else toOffset
                    m = M.singleton (tzidParam tzid) tz
                resolved <- shouldConform $ runR m $ unresolveUTCTime tz ut
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
                        if ut < Time.localTimeToUTC (utcOffsetTimeZone (unTimeZoneOffsetFrom from)) start
                          then fromOffset
                          else toOffset
                    m = M.singleton (tzidParam tzid) tz
                resolved <- shouldConform $ runR m $ unresolveUTCTime tz ut
                resolved `shouldBe` Time.utcToLocalTime expectedTz ut
