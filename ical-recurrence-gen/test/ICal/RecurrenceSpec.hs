{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ICal.RecurrenceSpec (spec) where

import Conformance
import Conformance.TestUtils
import Control.Applicative
import Control.Monad
import qualified Data.ByteString as SB
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import ICal
import ICal.Recurrence
import ICal.Recurrence.Gen ()
import ICal.Recurrence.TestUtils
import Path
import Path.IO
import Test.QuickCheck (choose, forAll, suchThat)
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  let limit = fromGregorian 2023 01 01
  describe "renderEventOccurrences" $ do
    it "roundtrips with parseEventOccurrences" $
      forAllValid $ \occurrences ->
        parseEventOccurrences (renderEventOccurrences occurrences) `shouldBe` occurrences
    it "roundtrips occurrences when an earlier one has no end" $
      -- An occurrence with neither a DTEND nor a DURATION renders as one line
      -- instead of two, which shifts every occurrence after it in the file.
      --
      -- There have to be two occurrences to see this.  A single one roundtrips
      -- by accident, because the empty line that the final CRLF leaves behind
      -- stands in for the line it never wrote.
      let occurrences =
            S.fromList
              [ EventOccurrence
                  { eventOccurrenceStart = Just $ DateTimeStartDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 01) 0,
                    eventOccurrenceEndOrDuration = Nothing
                  },
                EventOccurrence
                  { eventOccurrenceStart = Just $ DateTimeStartDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 02) 0,
                    eventOccurrenceEndOrDuration = Just $ Left $ DateTimeEndDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 02) 3600
                  }
              ]
       in parseEventOccurrences (renderEventOccurrences occurrences) `shouldBe` occurrences
    it "roundtrips an occurrence whose properties are folded over several lines" $ do
      -- A TZID of the shape that Thunderbird emits makes both content lines
      -- longer than the 75 octets after which they are folded.  Both
      -- properties are present here, so this covers folding on its own.
      let tzid = "/mozilla.org/20050126_1/America/Argentina/Buenos_Aires"
      let occurrences =
            S.singleton
              EventOccurrence
                { eventOccurrenceStart =
                    Just $
                      DateTimeStartDateTime $
                        DateTimeZoned tzid $
                          LocalTime (fromGregorian 2020 01 01) (TimeOfDay 01 00 00),
                  eventOccurrenceEndOrDuration =
                    Just $
                      Left $
                        DateTimeEndDateTime $
                          DateTimeZoned tzid $
                            LocalTime (fromGregorian 2020 01 01) (TimeOfDay 02 00 00)
                }
      -- Assert that this is really folded, so that the test cannot quietly
      -- stop covering folding if the fold width ever changes.
      renderEventOccurrences occurrences `shouldSatisfy` T.isInfixOf "\r\n "
      parseEventOccurrences (renderEventOccurrences occurrences) `shouldBe` occurrences
  describe "recurRecurrenceRuleLocalTimes" $
    it "gives the same occurrences below a limit no matter where the limit is" $
      -- The limit is an implementation detail of this library rather than
      -- anything the recurrence rule says, so raising it must only ever reveal
      -- more occurrences.  It must never change the ones already below it.
      --
      -- This is the general form of the BYSETPOS bugs: those all came from a
      -- limit narrowing a set that a rule part then selected from.
      --
      -- SECONDLY, MINUTELY and HOURLY are left out.  The limit is a day, so the
      -- smallest window this can ask for still holds 86400 seconds, and
      -- generating that many occurrences per case is too slow to be worth it.
      -- They step through the same code as the others.
      forAllValid $ \start ->
        forAll (genValid `suchThat` (\rule -> recurrenceRuleFrequency rule `notElem` [Secondly, Minutely, Hourly])) $ \rule ->
          forAll (choose (0, 40)) $ \offset1 ->
            forAll (choose (0, 40)) $ \offset2 -> do
              let startDay = localDay start
              let nearLimit = addDays (min offset1 offset2) startDay
              let farLimit = addDays (max offset1 offset2) startDay
              let belowNearLimit occurrence = localDay occurrence <= nearLimit
              near <- shouldRecur (recurRecurrenceRuleLocalTimes nearLimit start rule)
              far <- shouldRecur (recurRecurrenceRuleLocalTimes farLimit start rule)
              S.filter belowNearLimit far `shouldBe` S.filter belowNearLimit near
  describe "recurEvents" $ do
    -- A time zone without any transitions, so that the only thing that
    -- matters about it is its offset from UTC.
    let plusOne :: [Text]
        plusOne =
          [ "BEGIN:VTIMEZONE",
            "TZID:Test/PlusOne",
            "BEGIN:STANDARD",
            "DTSTART:19700101T000000",
            "TZOFFSETFROM:+0100",
            "TZOFFSETTO:+0100",
            "TZNAME:P1",
            "END:STANDARD",
            "END:VTIMEZONE"
          ]
    let calendarWith :: [Text] -> [Text] -> Text
        calendarWith timeZones event =
          T.intercalate "\r\n" $
            concat
              [ ["BEGIN:VCALENDAR", "PRODID:test", "VERSION:2.0"],
                timeZones,
                ["BEGIN:VEVENT", "DTSTAMP:20200101T000000Z", "UID:test"],
                event,
                ["END:VEVENT", "END:VCALENDAR", ""]
              ]
    let startsOf :: Day -> Text -> IO (Set (Maybe Timestamp))
        startsOf lim contents = do
          calendar <- shouldConform $ parseVCalendar contents
          shouldConform $
            runR lim (calendarTimeZoneMap calendar) $ do
              occurrences <-
                fmap S.unions $
                  mapM (recurEvents lim . getRecurringEvent) (calendarEvents calendar)
              S.fromList . map resolvedEventStart
                <$> mapM resolveEventOccurrence (S.toList occurrences)
    let utcAt :: Integer -> Int -> Int -> DiffTime -> Maybe Timestamp
        utcAt y m dd tod = Just $ TimestampUTCTime $ UTCTime (fromGregorian y m dd) tod
    -- Europe/Zurich as Google Calendar emits it: +0100 in winter, +0200 in
    -- summer, switching on the last Sunday of March and of October.
    let zurich :: [Text]
        zurich =
          [ "BEGIN:VTIMEZONE",
            "TZID:Europe/Zurich",
            "BEGIN:DAYLIGHT",
            "TZOFFSETFROM:+0100",
            "TZOFFSETTO:+0200",
            "TZNAME:CEST",
            "DTSTART:19700329T020000",
            "RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=-1SU",
            "END:DAYLIGHT",
            "BEGIN:STANDARD",
            "TZOFFSETFROM:+0200",
            "TZOFFSETTO:+0100",
            "TZNAME:CET",
            "DTSTART:19701025T030000",
            "RRULE:FREQ=YEARLY;BYMONTH=10;BYDAY=-1SU",
            "END:STANDARD",
            "END:VTIMEZONE"
          ]
    let occurrencesOf :: Day -> Text -> IO (Set EventOccurrence)
        occurrencesOf lim contents = do
          calendar <- shouldConform $ parseVCalendar contents
          shouldConform $
            runR lim (calendarTimeZoneMap calendar) $
              fmap S.unions $
                mapM (recurEvents lim . getRecurringEvent) (calendarEvents calendar)
    let plusTwo :: [Text]
        plusTwo =
          [ "BEGIN:VTIMEZONE",
            "TZID:Test/PlusTwo",
            "BEGIN:STANDARD",
            "DTSTART:19700101T000000",
            "TZOFFSETFROM:+0200",
            "TZOFFSETTO:+0200",
            "TZNAME:P2",
            "END:STANDARD",
            "END:VTIMEZONE"
          ]
    -- Only the offset that America/New_York had in September 1997 matters here,
    -- so this stands in for it without its transitions.
    let easternDaylight :: [Text]
        easternDaylight =
          [ "BEGIN:VTIMEZONE",
            "TZID:Test/EasternDaylight",
            "BEGIN:DAYLIGHT",
            "DTSTART:19700101T000000",
            "TZOFFSETFROM:-0400",
            "TZOFFSETTO:-0400",
            "TZNAME:EDT",
            "END:DAYLIGHT",
            "END:VTIMEZONE"
          ]
    let minus5 :: [Text]
        minus5 =
          [ "BEGIN:VTIMEZONE",
            "TZID:Test/MinusFive",
            "BEGIN:STANDARD",
            "DTSTART:19700101T000000",
            "TZOFFSETFROM:-0500",
            "TZOFFSETTO:-0500",
            "TZNAME:M5",
            "END:STANDARD",
            "END:VTIMEZONE"
          ]
    describe "Until" $ do
      it "includes the instance that falls exactly on a UTC Until of a zoned event" $
        -- @
        -- The UNTIL rule part defines a DATE or DATE-TIME value that bounds
        -- the recurrence rule in an inclusive manner.
        -- @
        --
        -- The event starts at 01:00 local time, which is 00:00 UTC, so
        -- 20200105T000000Z is exactly the fifth instance.
        startsOf (fromGregorian 2020 02 01) (calendarWith plusOne ["DTSTART;TZID=Test/PlusOne:20200101T010000", "RRULE:FREQ=DAILY;UNTIL=20200105T000000Z"])
          `shouldReturn` S.fromList
            [ utcAt 2020 01 01 0,
              utcAt 2020 01 02 0,
              utcAt 2020 01 03 0,
              utcAt 2020 01 04 0,
              utcAt 2020 01 05 0
            ]
      it "excludes the instance that falls after a UTC Until of a zoned event" $
        -- The mirror of the case above.  West of UTC the same comparison
        -- over-generates rather than dropping an instance, so a fix that gets
        -- the direction of the offset wrong would still pass that one.
        --
        -- The event starts at 23:00 local time, which is 04:00 UTC the next
        -- day, so the third instance is at 20200104T040000Z, past the UNTIL.
        startsOf (fromGregorian 2020 02 01) (calendarWith minus5 ["DTSTART;TZID=Test/MinusFive:20200101T230000", "RRULE:FREQ=DAILY;UNTIL=20200104T000000Z"])
          `shouldReturn` S.fromList
            [ utcAt 2020 01 02 (4 * 3600),
              utcAt 2020 01 03 (4 * 3600)
            ]
      it "still recurs a zoned event whose time zone the calendar does not define" $ do
        -- Comparing the Until in the time zone of DTSTART needs that time zone,
        -- but computing the recurrence set never needed it before, so a
        -- calendar that refers to a time zone it does not define has to keep
        -- recurring rather than becoming an unfixable error.
        --
        -- The instances stay zoned and unresolved here, so this asserts the
        -- starts rather than resolving them.
        calendar <- shouldConform $ parseVCalendar (calendarWith [] ["DTSTART;TZID=Nowhere/Undefined:20200101T010000", "RRULE:FREQ=DAILY;UNTIL=20200105T000000Z"])
        occurrences <-
          shouldConform $
            runR limit (calendarTimeZoneMap calendar) $
              fmap S.unions $
                mapM (recurEvents limit . getRecurringEvent) (calendarEvents calendar)
        S.size occurrences `shouldBe` 4
      it "includes an instance at or before a UTC Until across a daylight saving transition" $
        -- @
        -- The UNTIL rule part defines a DATE or DATE-TIME value that bounds
        -- the recurrence rule in an inclusive manner.
        -- @
        --
        -- The bound is an instant, so whether an instance is inside it is a
        -- question about instants.  Comparing local times answers that
        -- question only while the instance and the bound share an offset from
        -- UTC, because across a transition the local order and the order of
        -- the instants disagree.
        --
        -- Zurich goes back from 03:00 to 02:00 on the 30th of October 2022, so
        -- the 30th at 02:30 is still at +0200 while the Until, an hour later
        -- in absolute terms, is already at +0100.  In local terms the instance
        -- looks later than the bound; in instants it is half an hour earlier.
        --
        -- The 30th at 02:30 falls in the hour that happens twice, which is
        -- unavoidable: this disagreement can only arise for an instance inside
        -- that hour.  It is read as the earlier of the two, which is what
        -- 'chooseResolutionOffset' does.
        startsOf (fromGregorian 2022 11 05) (calendarWith zurich ["DTSTART;TZID=Europe/Zurich:20221029T023000", "RRULE:FREQ=DAILY;UNTIL=20221030T010000Z"])
          `shouldReturn` S.fromList
            [ utcAt 2022 10 29 (30 * 60),
              utcAt 2022 10 30 (30 * 60)
            ]
    describe "RecurrenceDateTimes" $ do
      it "does not produce two instances with the same start" $ do
        -- @
        -- Where duplicate instances are generated by the "RRULE"
        -- and "RDATE" properties, only one recurrence is considered.
        -- Duplicate instances are ignored.
        -- @
        occurrences <-
          occurrencesOf limit $
            calendarWith
              []
              [ "DTSTART:20200101T000000Z",
                "DTEND:20200101T010000Z",
                "RRULE:FREQ=DAILY;COUNT=3",
                "RDATE;VALUE=PERIOD:20200102T000000Z/PT5H"
              ]
        S.size (S.map eventOccurrenceStart occurrences) `shouldBe` S.size occurrences
        -- The size comparison on its own is also satisfied by dropping both of
        -- the colliding instances, or by dropping everything, so pin the whole
        -- set.  The period-valued RDATE is the one that carries the modified
        -- duration, so it is the one that survives.
        occurrences
          `shouldBe` S.fromList
            [ EventOccurrence
                { eventOccurrenceStart = Just $ DateTimeStartDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 01) 0,
                  eventOccurrenceEndOrDuration = Just $ Left $ DateTimeEndDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 01) 3600
                },
              EventOccurrence
                { eventOccurrenceStart = Just $ DateTimeStartDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 02) 0,
                  eventOccurrenceEndOrDuration =
                    Just $
                      Right $
                        DurationTime
                          DurTime
                            { durTimeSign = Positive,
                              durTimeHour = 5,
                              durTimeMinute = 0,
                              durTimeSecond = 0
                            }
                },
              EventOccurrence
                { eventOccurrenceStart = Just $ DateTimeStartDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 03) 0,
                  eventOccurrenceEndOrDuration = Just $ Left $ DateTimeEndDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 03) 3600
                }
            ]
      it "adds a date-valued instance to an event that has a DTEND" $
        -- @
        -- Value Type: The default value type for this property is
        -- DATE-TIME.  The value type can be set to DATE or PERIOD.
        -- @
        startsOf limit (calendarWith [] ["DTSTART:20200101T000000Z", "DTEND:20200101T010000Z", "RDATE;VALUE=DATE:20200102"])
          `shouldReturn` S.fromList
            [ utcAt 2020 01 01 0,
              Just $ TimestampDay $ fromGregorian 2020 01 02
            ]
      it "adds a date-time-valued instance to an all-day event that has a DTEND" $
        -- The mirror of the case above.  computeNewEnd has the same gap in its
        -- all-day arm, so this fails the same way.
        startsOf limit (calendarWith [] ["DTSTART;VALUE=DATE:20200101", "DTEND;VALUE=DATE:20200102", "RDATE:20200103T000000Z"])
          `shouldReturn` S.fromList
            [ Just $ TimestampDay $ fromGregorian 2020 01 01,
              utcAt 2020 01 03 0
            ]
    describe "ExceptionDateTimes" $ do
      it "excludes an instance of a zoned event that is given in UTC" $
        -- 20200102T000000Z is the second instance, which is at 01:00 at
        -- +0100.
        startsOf limit (calendarWith plusOne ["DTSTART;TZID=Test/PlusOne:20200101T010000", "RRULE:FREQ=DAILY;COUNT=3", "EXDATE:20200102T000000Z"])
          `shouldReturn` S.fromList [utcAt 2020 01 01 0, utcAt 2020 01 03 0]
      it "keeps an instance that a UTC ExceptionDateTimes does not name" $
        -- Guards the other direction.  Matching on resolved instants must not
        -- start excluding instances the EXDATE never named: 01:00 UTC is
        -- 02:00 local, which is not an instance of this event.
        startsOf limit (calendarWith plusOne ["DTSTART;TZID=Test/PlusOne:20200101T010000", "RRULE:FREQ=DAILY;COUNT=3", "EXDATE:20200102T010000Z"])
          `shouldReturn` S.fromList [utcAt 2020 01 01 0, utcAt 2020 01 02 0, utcAt 2020 01 03 0]
      it "excludes an instance of a zoned event that is given in another time zone" $
        -- 02:00 at +0200 is the same instant as the second instance, which
        -- is at 01:00 at +0100.
        startsOf limit (calendarWith (plusOne <> plusTwo) ["DTSTART;TZID=Test/PlusOne:20200101T010000", "RRULE:FREQ=DAILY;COUNT=3", "EXDATE;TZID=Test/PlusTwo:20200102T020000"])
          `shouldReturn` S.fromList [utcAt 2020 01 01 0, utcAt 2020 01 03 0]
      it "keeps an instance that an ExceptionDateTimes in another time zone does not name" $
        -- Guards the other direction.  01:00 at +0200 is 23:00 UTC on the
        -- first, which is not an instance, so comparing the wall clocks while
        -- ignoring the time zone would wrongly exclude here.
        startsOf limit (calendarWith (plusOne <> plusTwo) ["DTSTART;TZID=Test/PlusOne:20200101T010000", "RRULE:FREQ=DAILY;COUNT=3", "EXDATE;TZID=Test/PlusTwo:20200102T010000"])
          `shouldReturn` S.fromList [utcAt 2020 01 01 0, utcAt 2020 01 02 0, utcAt 2020 01 03 0]
    describe "nonexistent local times" $ do
      it "ignores an instance that falls in the gap of a daylight saving time transition" $
        -- @
        -- Recurrence rules may generate recurrence instances with an invalid
        -- date (e.g., February 30) or nonexistent local time (e.g., 1:30 AM
        -- on a day where the local time is moved forward by an hour at 1:00
        -- AM).  Such recurrence instances MUST be ignored and MUST NOT be
        -- counted as part of the recurrence set.
        -- @
        startsOf (fromGregorian 2022 03 29) (calendarWith zurich ["DTSTART;TZID=Europe/Zurich:20220325T023000", "RRULE:FREQ=DAILY"])
          `shouldReturn` S.fromList
            [ utcAt 2022 03 25 (1 * 3600 + 30 * 60),
              utcAt 2022 03 26 (1 * 3600 + 30 * 60),
              utcAt 2022 03 28 (30 * 60),
              utcAt 2022 03 29 (30 * 60)
            ]
      it "does not spend a Count on an instance that falls in the gap" $
        -- @
        -- Such recurrence instances MUST be ignored and MUST NOT be
        -- counted as part of the recurrence set.
        -- @
        --
        -- @
        -- the BYxxx rule parts
        -- are applied to the current set of evaluated occurrences in the
        -- following order: [...] and BYSETPOS; then COUNT and UNTIL are
        -- evaluated.
        -- @
        --
        -- An ignored instance never becomes an occurrence, and COUNT counts
        -- occurrences and is evaluated last, so the 27th being skipped must not
        -- use up one of the four.
        --
        -- leapdays.ics already relies on that for the other half of the same
        -- sentence: three counted instances there, with the 29th of February
        -- skipped in every year that does not have one.
        startsOf (fromGregorian 2022 04 30) (calendarWith zurich ["DTSTART;TZID=Europe/Zurich:20220325T023000", "RRULE:FREQ=DAILY;COUNT=4"])
          `shouldReturn` S.fromList
            [ utcAt 2022 03 25 (1 * 3600 + 30 * 60),
              utcAt 2022 03 26 (1 * 3600 + 30 * 60),
              utcAt 2022 03 28 (30 * 60),
              utcAt 2022 03 29 (30 * 60)
            ]
      it "resolves an ambiguous instance to the earlier of the two" $
        -- On the 30th of October 2022 the local time in Zurich goes back from
        -- 03:00 to 02:00, so 02:30 happens twice that day.  This is not broken
        -- today, and a fix for the gap above must not change it.
        startsOf (fromGregorian 2022 10 31) (calendarWith zurich ["DTSTART;TZID=Europe/Zurich:20221029T023000", "RRULE:FREQ=DAILY"])
          `shouldReturn` S.fromList
            [ utcAt 2022 10 29 (30 * 60),
              utcAt 2022 10 30 (30 * 60),
              utcAt 2022 10 31 (1 * 3600 + 30 * 60)
            ]
    describe "Frequency" $ do
      it "recurs an hourly event" $
        startsOf limit (calendarWith [] ["DTSTART:20200101T000000Z", "RRULE:FREQ=HOURLY;COUNT=3"])
          `shouldReturn` S.fromList
            [ utcAt 2020 01 01 0,
              utcAt 2020 01 01 3600,
              utcAt 2020 01 01 7200
            ]
      it "recurs a minutely event" $
        startsOf limit (calendarWith [] ["DTSTART:20200101T000000Z", "RRULE:FREQ=MINUTELY;COUNT=3"])
          `shouldReturn` S.fromList [utcAt 2020 01 01 0, utcAt 2020 01 01 60, utcAt 2020 01 01 120]
      it "recurs a secondly event" $
        startsOf limit (calendarWith [] ["DTSTART:20200101T000000Z", "RRULE:FREQ=SECONDLY;COUNT=3"])
          `shouldReturn` S.fromList [utcAt 2020 01 01 0, utcAt 2020 01 01 1, utcAt 2020 01 01 2]
      it "recurs every 3 hours from 9:00 AM to 5:00 PM on a specific day" $
        -- [section 3.8.5.3](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.5.3)
        --
        -- @
        -- Every 3 hours from 9:00 AM to 5:00 PM on a specific day:
        --
        --  DTSTART;TZID=America/New_York:19970902T090000
        --  RRULE:FREQ=HOURLY;INTERVAL=3;UNTIL=19970902T170000Z
        --
        --  ==> (September 2, 1997 EDT) 09:00,12:00,15:00
        -- @
        --
        -- With the UNTIL corrected to 19970902T210000Z, which is 17:00 at the
        -- -0400 that New York was on in September 1997.  The example as printed
        -- says 17:00 UTC, which is 13:00 there, and would stop after the first
        -- instance.
        --
        -- ICal.Recurrence.RecurrenceRuleSpec cannot express this one, because
        -- it works in local times with no time zone to compare a UTC UNTIL in.
        startsOf (fromGregorian 1997 09 30) (calendarWith easternDaylight ["DTSTART;TZID=Test/EasternDaylight:19970902T090000", "RRULE:FREQ=HOURLY;INTERVAL=3;UNTIL=19970902T210000Z"])
          `shouldReturn` S.fromList
            [ utcAt 1997 09 02 (13 * 3600),
              utcAt 1997 09 02 (16 * 3600),
              utcAt 1997 09 02 (19 * 3600)
            ]
  scenarioDir "test_resources/event" $ \fp -> do
    eventFile <- liftIO $ parseRelFile fp
    when (fileExtension eventFile == Just ".ics") $ do
      it "recurs this file correctly" $ do
        contents <- TE.decodeUtf8 <$> SB.readFile (fromRelFile eventFile)
        event <- shouldConform $ parseComponentFromText contents
        goldenFile <- replaceExtension ".occ" eventFile
        pure $ pureGoldenEventRecurrenceFile goldenFile limit event
  scenarioDir "test_resources/calendar" $ \fp -> do
    eventFile <- liftIO $ parseRelFile fp
    when (fileExtension eventFile == Just ".ics") $ do
      it "recurs this file correctly" $ do
        contents <- TE.decodeUtf8 <$> SB.readFile (fromRelFile eventFile)
        cal <- shouldConform $ parseVCalendar contents
        goldenFile <- replaceExtension ".occ" eventFile
        pure $ pureGoldenCalendarRecurrenceFile goldenFile limit cal
      it "resolves this file correctly" $ do
        contents <- TE.decodeUtf8 <$> SB.readFile (fromRelFile eventFile)
        calendar <- shouldConform $ parseVCalendar contents
        resolvedEvents <- shouldConform $ do
          runR limit (calendarTimeZoneMap calendar) $ do
            occurrences <-
              fmap S.unions $
                mapM
                  (recurEvents limit . getRecurringEvent)
                  (calendarEvents calendar)
            S.fromList <$> mapM resolveEventOccurrence (S.toList occurrences)
        goldenFile <- replaceExtension ".res" eventFile
        pure $ goldenResolvedEventFile goldenFile $ pure resolvedEvents

pureGoldenCalendarRecurrenceFile :: Path Rel File -> Day -> Calendar -> GoldenTest (Set EventOccurrence)
pureGoldenCalendarRecurrenceFile goldenFile limit calendar =
  goldenEventOccurrenceFile goldenFile $
    shouldConform $ do
      runR limit (calendarTimeZoneMap calendar) $
        fmap S.unions $
          mapM
            (recurEvents limit . getRecurringEvent)
            (calendarEvents calendar)

pureGoldenEventRecurrenceFile :: Path Rel File -> Day -> Event -> GoldenTest (Set EventOccurrence)
pureGoldenEventRecurrenceFile goldenFile limit event =
  goldenEventOccurrenceFile goldenFile $ shouldConform $ runRWithoutZones (recurEvents limit (getRecurringEvent event))

goldenEventOccurrenceFile :: Path Rel File -> IO (Set EventOccurrence) -> GoldenTest (Set EventOccurrence)
goldenEventOccurrenceFile goldenFile produceOccurrences =
  GoldenTest
    { goldenTestRead = do
        mGoldenContents <- forgivingAbsence $ TE.decodeUtf8 <$> SB.readFile (fromRelFile goldenFile)
        pure $ parseEventOccurrences <$> mGoldenContents,
      goldenTestProduce = produceOccurrences,
      goldenTestWrite = SB.writeFile (fromRelFile goldenFile) . TE.encodeUtf8 . renderEventOccurrences,
      goldenTestCompare = \actual expected ->
        if actual == expected
          then pure Nothing
          else do
            a <-
              stringsNotEqualButShouldHaveBeenEqual
                (ppShow (S.toList actual))
                (ppShow (S.toList expected))

            pure $
              Just $
                Context
                  a
                  (goldenContext (fromRelFile goldenFile))
    }

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (ys, zs) = splitAt n xs
   in ys : chunksOf n zs

-- | Read a golden file back into the recurrence set it was rendered from.
--
-- The occurrences are chunked by unfolded line rather than by raw line,
-- because a property whose content line is longer than 75 octets is rendered
-- folded over several raw lines.
parseEventOccurrences :: Text -> Set EventOccurrence
parseEventOccurrences contents = case runConform (parseUnfoldedLines contents) of
  Left _ -> S.empty
  Right (unfoldedLines, _) ->
    S.fromList $ mapMaybe parseEventOccurrence $ chunksOf 2 unfoldedLines

parseEventOccurrence :: [UnfoldedLine] -> Maybe EventOccurrence
parseEventOccurrence = \case
  [UnfoldedLine startLine, UnfoldedLine endDurationLine] -> either (const Nothing) (Just . fst) $
    runConform $ do
      eventOccurrenceStart <- case startLine of
        "" -> pure Nothing
        l -> Just <$> parsePropertyFromText (renderUnfoldedLines [UnfoldedLine l])
      eventOccurrenceEndOrDuration <- case endDurationLine of
        "" -> pure Nothing
        l ->
          Just
            <$> (Left <$> parsePropertyFromText (renderUnfoldedLines [UnfoldedLine l]))
              `altConform` (Right <$> parsePropertyFromText (renderUnfoldedLines [UnfoldedLine l]))
      pure EventOccurrence {..}
  _ -> Nothing

renderEventOccurrences :: Set EventOccurrence -> Text
renderEventOccurrences = foldMap renderEventOccurrence

-- | Render an occurrence as exactly two lines, so that an absent property
-- does not shift the occurrences after it in the file.
--
-- 'parseEventOccurrences' reads the file back in two-line chunks and reads an
-- empty line as an absent property, so both lines must always be written.
renderEventOccurrence :: EventOccurrence -> Text
renderEventOccurrence EventOccurrence {..} =
  T.concat
    [ case eventOccurrenceStart of
        Nothing -> "\r\n"
        Just dtstart -> renderPropertyText dtstart,
      case eventOccurrenceEndOrDuration of
        Nothing -> "\r\n"
        Just (Left end) -> renderPropertyText end
        Just (Right dur) -> renderPropertyText dur
    ]

goldenResolvedEventFile :: Path Rel File -> IO (Set ResolvedEvent) -> GoldenTest (Set ResolvedEvent)
goldenResolvedEventFile goldenFile produceResolvedEvents =
  GoldenTest
    { goldenTestRead = do
        mGoldenContents <- forgivingAbsence $ TE.decodeUtf8 <$> SB.readFile (fromRelFile goldenFile)
        pure $ parseResolvedEvents <$> mGoldenContents,
      goldenTestProduce = produceResolvedEvents,
      goldenTestWrite = SB.writeFile (fromRelFile goldenFile) . TE.encodeUtf8 . renderResolvedEvents,
      goldenTestCompare = \actual expected ->
        if actual == expected
          then pure Nothing
          else do
            a <-
              stringsNotEqualButShouldHaveBeenEqual
                (ppShow (S.toList actual))
                (ppShow (S.toList expected))

            pure $
              Just $
                Context
                  a
                  (goldenContext (fromRelFile goldenFile))
    }

parseResolvedEvents :: Text -> Set ResolvedEvent
parseResolvedEvents =
  S.fromList
    . mapMaybe (parseResolvedEvent . T.intercalate "\n")
    . chunksOf 2
    . T.splitOn "\n"

parseResolvedEvent :: Text -> Maybe ResolvedEvent
parseResolvedEvent t = case T.splitOn "\n" t of
  (startLine : endDurationLine : _) -> do
    resolvedEventStart <- goM startLine
    resolvedEventEnd <- goM endDurationLine
    pure ResolvedEvent {..}
  _ -> Nothing
  where
    goM :: Text -> Maybe (Maybe Timestamp)
    goM "" = pure Nothing
    goM s = Just <$> go (T.unpack s)
    go :: String -> Maybe Timestamp
    go s =
      (TimestampLocalTime <$> parseTimeM False defaultTimeLocale localTimeFormat s)
        <|> (TimestampUTCTime <$> parseTimeM False defaultTimeLocale utcTimeFormat s)
        <|> (TimestampDay <$> parseTimeM False defaultTimeLocale dayFormat s)

renderResolvedEvents :: Set ResolvedEvent -> Text
renderResolvedEvents = foldMap renderResolvedEvent . S.toAscList

renderResolvedEvent :: ResolvedEvent -> Text
renderResolvedEvent ResolvedEvent {..} =
  T.pack $
    concat
      [ maybe "" go resolvedEventStart <> "\n",
        maybe "" go resolvedEventEnd <> "\n"
      ]
  where
    go = \case
      TimestampDay d -> formatTime defaultTimeLocale dayFormat d
      TimestampLocalTime lt -> formatTime defaultTimeLocale localTimeFormat lt
      TimestampUTCTime lt -> formatTime defaultTimeLocale utcTimeFormat lt

dayFormat :: String
dayFormat = "Day %F"

localTimeFormat :: String
localTimeFormat = "Local %F %T"

utcTimeFormat :: String
utcTimeFormat = "UTC %F %T"
