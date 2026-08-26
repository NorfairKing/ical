{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module ICal.Recurrence.TimeZoneSpec (spec) where

import Conformance.TestUtils
import Control.Monad
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
  it "resolves and unresolves back to any local time that the time zone has" $ do
    forAllValid $ \tzid ->
      forAllValid $ \start ->
        forAllValid $ \fromOffset ->
          forAllValid $ \toOffset ->
            forAllValid $ \lt -> do
              let from = TimeZoneOffsetFrom fromOffset
                  to = TimeZoneOffsetTo toOffset
                  tz = makeTimeZone tzid [StandardObservance $ Standard $ makeObservance start from to]
                  -- A transition that moves the clock forward skips the local
                  -- times between the two offsets, so no instant maps to them
                  -- and resolving one lands on an instant belonging to an
                  -- earlier local time.
                  --
                  -- 'localTimeExists' in 'ICal.Recurrence' defines existence as
                  -- exactly this round trip, and 'recurEvents' drops the
                  -- instances that fail it.  So the gap is derived from the two
                  -- offsets here, the same way the neighbouring tests derive
                  -- which offset each direction picks.  Deriving it from the
                  -- round trip would assert a tautology.  The gap itself is
                  -- covered separately below.
                  inTransitionGap =
                    lt >= start
                      && Time.localTimeToUTC (utcOffsetTimeZone toOffset) lt
                        < Time.localTimeToUTC (utcOffsetTimeZone fromOffset) start
              -- Whether 'time' keeps a leap second across a round trip depends
              -- on both the time of day and the offset: at 23:59:60 with a zero
              -- offset it survives, and everywhere else it normalises into the
              -- following minute.  That is a property of splitting an instant
              -- into a wall clock rather than anything about time zones, so it
              -- is out of scope here.  'isLeapSecond' is the same predicate
              -- 'localTimeExists' uses to rule one out, and it is a syntactic
              -- check on the local time rather than anything derived from this
              -- round trip.  The cases below pin what actually happens to one.
              when (not inTransitionGap && not (isLeapSecond lt)) $ do
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

  describe "a transition that moves the clock forward" $ do
    -- The property above generates its local time independently of the
    -- transition it generates, so the two land within an hour of each other
    -- essentially never and the gap goes uncovered.  These pin it.
    let tz =
          makeTimeZone
            (TimeZoneIdentifier "Test/SpringForward")
            [ StandardObservance $
                Standard $
                  makeObservance
                    (Time.LocalTime (Time.fromGregorian 2023 03 26) (Time.TimeOfDay 02 00 00))
                    (TimeZoneOffsetFrom (UTCOffset 3600))
                    (TimeZoneOffsetTo (UTCOffset 7200))
            ]
        day = Time.fromGregorian 2023 03 26
        roundTrip :: Time.TimeOfDay -> IO Time.LocalTime
        roundTrip tod = shouldConform $ do
          resolved <- resolveLocalTime tz (Time.LocalTime day tod)
          unresolveUTCTime tz resolved
    it "keeps a local time before the gap" $
      roundTrip (Time.TimeOfDay 01 30 00)
        `shouldReturn` Time.LocalTime day (Time.TimeOfDay 01 30 00)
    it "moves the local time at which the gap starts" $
      roundTrip (Time.TimeOfDay 02 00 00)
        `shouldReturn` Time.LocalTime day (Time.TimeOfDay 01 00 00)
    it "moves a local time inside the gap" $
      roundTrip (Time.TimeOfDay 02 30 00)
        `shouldReturn` Time.LocalTime day (Time.TimeOfDay 01 30 00)
    it "keeps the local time at which the gap ends" $
      roundTrip (Time.TimeOfDay 03 00 00)
        `shouldReturn` Time.LocalTime day (Time.TimeOfDay 03 00 00)

  describe "a leap second" $ do
    -- The property above excludes local times holding a leap second.  These pin
    -- the behaviour that justifies the exclusion, so it cannot quietly stop
    -- being justified: whether a leap second survives depends on the time of
    -- day and on the offset, neither of which is a statement about time zones.
    --
    -- Every observance here has one offset for both TZOFFSETFROM and
    -- TZOFFSETTO, so there is no transition and nothing but the offset is in
    -- play.
    let day = Time.fromGregorian 2020 06 30
        roundTrip :: UTCOffset -> Time.LocalTime -> IO Time.LocalTime
        roundTrip offset lt = shouldConform $ do
          let tz =
                makeTimeZone
                  (TimeZoneIdentifier "Test/Fixed")
                  [ StandardObservance $
                      Standard $
                        makeObservance
                          (Time.LocalTime (Time.fromGregorian 1970 01 01) Time.midnight)
                          (TimeZoneOffsetFrom offset)
                          (TimeZoneOffsetTo offset)
                  ]
          resolved <- resolveLocalTime tz lt
          unresolveUTCTime tz resolved
    it "survives at the end of the day at a zero offset" $
      roundTrip (UTCOffset 0) (Time.LocalTime day (Time.TimeOfDay 23 59 60))
        `shouldReturn` Time.LocalTime day (Time.TimeOfDay 23 59 60)
    it "normalises into the following minute earlier in the day" $
      roundTrip (UTCOffset 0) (Time.LocalTime day (Time.TimeOfDay 00 00 60))
        `shouldReturn` Time.LocalTime day (Time.TimeOfDay 00 01 00)
    it "normalises at the end of the day at a non-zero offset" $
      roundTrip (UTCOffset 3600) (Time.LocalTime day (Time.TimeOfDay 23 59 60))
        `shouldReturn` Time.LocalTime (Time.addDays 1 day) Time.midnight

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
