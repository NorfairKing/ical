{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ICal.RecurrenceSpec (spec) where

import Conformance
import Conformance.TestUtils
import Control.Applicative
import Control.Monad
import qualified Data.ByteString as SB
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
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
  describe "renderOccurrences" $ do
    it "roundtrips with parseOccurrences" $
      forAllValid $ \occurrences ->
        parseOccurrences (renderOccurrences occurrences) `shouldBe` occurrences
    it "roundtrips occurrences when an earlier one has no end" $
      -- An occurrence with neither a DTEND nor a DURATION renders as one line
      -- instead of two, which shifts every occurrence after it in the file.
      --
      -- There have to be two occurrences to see this.  A single one roundtrips
      -- by accident, because the empty line that the final CRLF leaves behind
      -- stands in for the line it never wrote.
      let occurrences =
            S.fromList
              [ Occurrence
                  { occurrenceComponent = (),
                    occurrenceStart = Just $ DateTimeStartDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 01) 0,
                    occurrenceEnd = Nothing
                  },
                Occurrence
                  { occurrenceComponent = (),
                    occurrenceStart = Just $ DateTimeStartDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 02) 0,
                    occurrenceEnd = Just $ Left $ RecurrenceEndDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 02) 3600
                  }
              ]
       in parseOccurrences (renderOccurrences occurrences) `shouldBe` occurrences
    it "roundtrips an occurrence whose properties are folded over several lines" $ do
      -- A TZID of the shape that Thunderbird emits makes both content lines
      -- longer than the 75 octets after which they are folded.  Both
      -- properties are present here, so this covers folding on its own.
      let tzid = "/mozilla.org/20050126_1/America/Argentina/Buenos_Aires"
      let occurrences =
            S.singleton
              Occurrence
                { occurrenceComponent = (),
                  occurrenceStart =
                    Just $
                      DateTimeStartDateTime $
                        DateTimeZoned tzid $
                          LocalTime (fromGregorian 2020 01 01) (TimeOfDay 01 00 00),
                  occurrenceEnd =
                    Just $
                      Left $
                        RecurrenceEndDateTime $
                          DateTimeZoned tzid $
                            LocalTime (fromGregorian 2020 01 01) (TimeOfDay 02 00 00)
                }
      -- Assert that this is really folded, so that the test cannot quietly
      -- stop covering folding if the fold width ever changes.
      renderOccurrences occurrences `shouldSatisfy` T.isInfixOf "\r\n "
      parseOccurrences (renderOccurrences occurrences) `shouldBe` occurrences
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
  describe "expandRecurring" $ do
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
            runCalendarR lim calendar $ do
              occurrences <- allOccurrences <$> recurEvents lim (calendarEvents calendar)
              S.fromList . map resolvedStart
                <$> mapM resolveOccurrence (S.toList occurrences)
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
    -- The component each occurrence came from is dropped, because these
    -- calendars hold one event and what is being asserted is its instances.
    let occurrencesOf :: Day -> Text -> IO (Set (Occurrence ()))
        occurrencesOf lim contents = do
          calendar <- shouldConform $ parseVCalendar contents
          shouldConform $
            runCalendarR lim calendar $
              S.map void . allOccurrences <$> recurEvents lim (calendarEvents calendar)
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
            runCalendarR limit calendar $
              allOccurrences <$> recurEvents limit (calendarEvents calendar)
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
        S.size (S.map occurrenceStart occurrences) `shouldBe` S.size occurrences
        -- The size comparison on its own is also satisfied by dropping both of
        -- the colliding instances, or by dropping everything, so pin the whole
        -- set.  The period-valued RDATE is the one that carries the modified
        -- duration, so it is the one that survives.
        occurrences
          `shouldBe` S.fromList
            [ Occurrence
                { occurrenceComponent = (),
                  occurrenceStart = Just $ DateTimeStartDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 01) 0,
                  occurrenceEnd = Just $ Left $ RecurrenceEndDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 01) 3600
                },
              Occurrence
                { occurrenceComponent = (),
                  occurrenceStart = Just $ DateTimeStartDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 02) 0,
                  occurrenceEnd =
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
              Occurrence
                { occurrenceComponent = (),
                  occurrenceStart = Just $ DateTimeStartDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 03) 0,
                  occurrenceEnd = Just $ Left $ RecurrenceEndDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 03) 3600
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
    describe "the limit" $ do
      it "keeps an explicitly listed instance that lies beyond it" $
        startsOf (fromGregorian 2020 12 31) (calendarWith [] ["DTSTART:20200101T000000Z", "DTEND:20200101T010000Z", "RDATE:20990101T000000Z"])
          `shouldReturn` S.fromList [utcAt 2020 01 01 0, utcAt 2099 01 01 0]
      it "keeps a DTSTART that lies beyond it" $
        startsOf (fromGregorian 2020 12 31) (calendarWith [] ["DTSTART:20990101T000000Z", "DTEND:20990101T010000Z", "RRULE:FREQ=DAILY;COUNT=3"])
          `shouldReturn` S.fromList [utcAt 2099 01 01 0]
      it "bounds what a recurrence rule generates" $
        startsOf (fromGregorian 2020 01 03) (calendarWith [] ["DTSTART:20200101T000000Z", "DTEND:20200101T010000Z", "RRULE:FREQ=DAILY;COUNT=10"])
          `shouldReturn` S.fromList [utcAt 2020 01 01 0, utcAt 2020 01 02 0, utcAt 2020 01 03 0]
    describe "a leap second that a rule generates" $ do
      -- @
      -- The BYSECOND rule part specifies a COMMA-separated list of seconds
      -- within a minute.  Valid values are 0 to 60.
      -- @
      --
      -- So 60 is legal, and names a leap second.  'bySecondExpand' puts it
      -- straight into a 'TimeOfDay' without normalising, so a rule can generate
      -- a local time that no wall clock ever shows.
      --
      -- @
      -- Recurrence rules may generate recurrence instances with an invalid
      -- date (e.g., February 30) or nonexistent local time (e.g., 1:30 AM
      -- on a day where the local time is moved forward by an hour at 1:00
      -- AM).  Such recurrence instances MUST be ignored and MUST NOT be
      -- counted as part of the recurrence set.
      -- @
      --
      -- These assert the local starts rather than resolved instants, because
      -- resolving is what mangles a leap second and would hide what is being
      -- tested.
      let localStartsOf :: Day -> Text -> IO (Set (Maybe DateTimeStart))
          localStartsOf lim contents = do
            calendar <- shouldConform $ parseVCalendar contents
            shouldConform $
              runCalendarR lim calendar $
                S.map occurrenceStart . allOccurrences
                  <$> recurEvents lim (calendarEvents calendar)
      let floatingAt :: TimeOfDay -> Maybe DateTimeStart
          floatingAt tod =
            Just $
              DateTimeStartDateTime $
                DateTimeFloating $
                  LocalTime (fromGregorian 2020 01 01) tod
      it "is ignored when DTSTART is floating" $
        localStartsOf
          (fromGregorian 2020 01 02)
          (calendarWith [] ["DTSTART:20200101T120000", "RRULE:FREQ=MINUTELY;BYSECOND=60;COUNT=3"])
          `shouldReturn` S.fromList [floatingAt (TimeOfDay 12 00 00)]
      it "is ignored when DTSTART is zoned" $
        localStartsOf
          (fromGregorian 2020 01 02)
          (calendarWith plusOne ["DTSTART;TZID=Test/PlusOne:20200101T120000", "RRULE:FREQ=MINUTELY;BYSECOND=60;COUNT=3"])
          `shouldReturn` S.fromList
            [ Just $
                DateTimeStartDateTime $
                  DateTimeZoned "Test/PlusOne" $
                    LocalTime (fromGregorian 2020 01 01) (TimeOfDay 12 00 00)
            ]
  describe "recurEvents" $ do
    let calendarWithEvents :: [[Text]] -> Text
        calendarWithEvents events =
          T.intercalate "\r\n" $
            concat
              [ ["BEGIN:VCALENDAR", "PRODID:test", "VERSION:2.0"],
                concatMap
                  (\event -> concat [["BEGIN:VEVENT", "DTSTAMP:20200101T000000Z"], event, ["END:VEVENT"]])
                  events,
                ["END:VCALENDAR", ""]
              ]
    -- Narrowed to the SUMMARY of the component each instance came from, so
    -- that one assertion covers both which instances there are and which
    -- component contributed each of them.
    let summariesOf :: Day -> Text -> IO (Map UID (Set (Occurrence (Maybe Summary))))
        summariesOf lim contents = do
          calendar <- shouldConform $ parseVCalendar contents
          shouldConform $
            runCalendarR lim calendar $
              M.map (S.map (fmap eventSummary)) <$> recurEvents lim (calendarEvents calendar)
    it "replaces the instance that an override names instead of adding an occurrence beside it" $
      -- [section 3.8.4.4](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.4.4)
      --
      -- @
      -- This property is used in conjunction with the "UID" and
      -- "SEQUENCE" properties to identify a specific instance of a
      -- recurring "VEVENT", "VTODO", or "VJOURNAL" calendar component.
      -- The property value is the original value of the "DTSTART" property
      -- of the recurrence instance.
      -- @
      --
      -- @
      -- The DATE-TIME value is set to the time when the original
      -- recurrence instance would occur; meaning that if the intent is to
      -- change a Friday meeting to Thursday, the DATE-TIME is still set to
      -- the original Friday meeting.
      -- @
      --
      -- So the RECURRENCE-ID says where the instance used to be, and the
      -- overriding component's own DTSTART says where it is now.  Both cannot
      -- be in the recurrence set: the meeting moved, it did not happen twice.
      --
      -- Google and Outlook emit an override of exactly this shape for every
      -- "this event only" edit, so this is what an ordinary calendar looks
      -- like rather than a corner case.
      summariesOf
        limit
        ( calendarWithEvents
            [ [ "UID:test",
                "DTSTART:20200101T000000Z",
                "DTEND:20200101T010000Z",
                "RRULE:FREQ=DAILY;COUNT=3",
                "SUMMARY:Series"
              ],
              [ "UID:test",
                "RECURRENCE-ID:20200102T000000Z",
                "DTSTART:20200102T120000Z",
                "DTEND:20200102T130000Z",
                "SUMMARY:Override"
              ]
            ]
        )
        `shouldReturn` M.singleton
          (UID "test")
          ( S.fromList
              [ Occurrence
                  { occurrenceComponent = Just "Series",
                    occurrenceStart = Just $ DateTimeStartDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 01) 0,
                    occurrenceEnd = Just $ Left $ RecurrenceEndDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 01) 3600
                  },
                Occurrence
                  { occurrenceComponent = Just "Override",
                    occurrenceStart = Just $ DateTimeStartDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 02) (12 * 3600),
                    occurrenceEnd = Just $ Left $ RecurrenceEndDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 02) (13 * 3600)
                  },
                Occurrence
                  { occurrenceComponent = Just "Series",
                    occurrenceStart = Just $ DateTimeStartDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 03) 0,
                    occurrenceEnd = Just $ Left $ RecurrenceEndDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 03) 3600
                  }
              ]
          )
    it "takes the override with the higher SEQUENCE when two name the same instance" $
      -- [section 3.8.7.4](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.7.4)
      --
      -- @
      -- When a calendar component is created, its sequence
      -- number is 0.  It is monotonically incremented by the "Organizer's"
      -- CUA each time the "Organizer" makes a significant revision to the
      -- calendar component.
      -- @
      --
      -- Two components of one UID with the same RECURRENCE-ID are two
      -- revisions of one instance, so the higher SEQUENCE is the later
      -- revision.  The later revision is listed first here, so file order and
      -- SEQUENCE disagree and only one of them can be what decides.
      summariesOf
        limit
        ( calendarWithEvents
            [ [ "UID:test",
                "DTSTART:20200101T000000Z",
                "DTEND:20200101T010000Z",
                "RRULE:FREQ=DAILY;COUNT=2",
                "SUMMARY:Series"
              ],
              [ "UID:test",
                "RECURRENCE-ID:20200102T000000Z",
                "SEQUENCE:2",
                "DTSTART:20200102T120000Z",
                "DTEND:20200102T130000Z",
                "SUMMARY:Later revision"
              ],
              [ "UID:test",
                "RECURRENCE-ID:20200102T000000Z",
                "SEQUENCE:1",
                "DTSTART:20200102T180000Z",
                "DTEND:20200102T190000Z",
                "SUMMARY:Earlier revision"
              ]
            ]
        )
        `shouldReturn` M.singleton
          (UID "test")
          ( S.fromList
              [ Occurrence
                  { occurrenceComponent = Just "Series",
                    occurrenceStart = Just $ DateTimeStartDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 01) 0,
                    occurrenceEnd = Just $ Left $ RecurrenceEndDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 01) 3600
                  },
                Occurrence
                  { occurrenceComponent = Just "Later revision",
                    occurrenceStart = Just $ DateTimeStartDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 02) (12 * 3600),
                    occurrenceEnd = Just $ Left $ RecurrenceEndDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 02) (13 * 3600)
                  }
              ]
          )
    it "reschedules every later instance by the same difference as a THISANDFUTURE override" $
      -- [section 3.8.4.4](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.4.4)
      --
      -- @
      -- The "RANGE" parameter is used to specify the effective range of
      -- recurrence instances from the instance specified by the
      -- "RECURRENCE-ID" property value.  The value for the range parameter
      -- can only be "THISANDFUTURE" to indicate a range defined by the
      -- given recurrence instance and all subsequent instances.
      -- @
      --
      -- @
      -- When the given recurrence instance is
      -- rescheduled, all subsequent instances are also rescheduled by the
      -- same time difference.  For instance, if the given recurrence
      -- instance is rescheduled to start 2 hours later, then all
      -- subsequent instances are also rescheduled 2 hours later.
      -- @
      --
      -- The third instance moves from 09:00 to 11:00, so the fourth moves too.
      summariesOf
        limit
        ( calendarWithEvents
            [ [ "UID:test",
                "DTSTART:20200101T090000Z",
                "DTEND:20200101T100000Z",
                "RRULE:FREQ=DAILY;COUNT=4",
                "SUMMARY:Series"
              ],
              [ "UID:test",
                "RECURRENCE-ID;RANGE=THISANDFUTURE:20200103T090000Z",
                "DTSTART:20200103T110000Z",
                "DTEND:20200103T120000Z",
                "SUMMARY:Moved"
              ]
            ]
        )
        `shouldReturn` M.singleton
          (UID "test")
          ( S.fromList
              [ Occurrence
                  { occurrenceComponent = Just "Series",
                    occurrenceStart = Just $ DateTimeStartDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 01) (9 * 3600),
                    occurrenceEnd = Just $ Left $ RecurrenceEndDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 01) (10 * 3600)
                  },
                Occurrence
                  { occurrenceComponent = Just "Series",
                    occurrenceStart = Just $ DateTimeStartDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 02) (9 * 3600),
                    occurrenceEnd = Just $ Left $ RecurrenceEndDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 02) (10 * 3600)
                  },
                Occurrence
                  { occurrenceComponent = Just "Moved",
                    occurrenceStart = Just $ DateTimeStartDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 03) (11 * 3600),
                    occurrenceEnd = Just $ Left $ RecurrenceEndDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 03) (12 * 3600)
                  },
                Occurrence
                  { occurrenceComponent = Just "Moved",
                    occurrenceStart = Just $ DateTimeStartDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 04) (11 * 3600),
                    occurrenceEnd = Just $ Left $ RecurrenceEndDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 04) (12 * 3600)
                  }
              ]
          )
    it "gives every later instance the duration of a THISANDFUTURE override" $
      -- @
      -- Similarly, if the duration of the given recurrence instance is
      -- modified, then all subsequence instances are also modified to have
      -- this same duration.
      -- @
      --
      -- The override starts where its instance already started, so nothing
      -- moves and only the duration propagates.  An hour becomes two and a
      -- half.
      summariesOf
        limit
        ( calendarWithEvents
            [ [ "UID:test",
                "DTSTART:20200101T090000Z",
                "DTEND:20200101T100000Z",
                "RRULE:FREQ=DAILY;COUNT=3",
                "SUMMARY:Series"
              ],
              [ "UID:test",
                "RECURRENCE-ID;RANGE=THISANDFUTURE:20200102T090000Z",
                "DTSTART:20200102T090000Z",
                "DTEND:20200102T113000Z",
                "SUMMARY:Longer"
              ]
            ]
        )
        `shouldReturn` M.singleton
          (UID "test")
          ( S.fromList
              [ Occurrence
                  { occurrenceComponent = Just "Series",
                    occurrenceStart = Just $ DateTimeStartDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 01) (9 * 3600),
                    occurrenceEnd = Just $ Left $ RecurrenceEndDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 01) (10 * 3600)
                  },
                Occurrence
                  { occurrenceComponent = Just "Longer",
                    occurrenceStart = Just $ DateTimeStartDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 02) (9 * 3600),
                    occurrenceEnd = Just $ Left $ RecurrenceEndDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 02) (11 * 3600 + 30 * 60)
                  },
                Occurrence
                  { occurrenceComponent = Just "Longer",
                    occurrenceStart = Just $ DateTimeStartDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 03) (9 * 3600),
                    occurrenceEnd = Just $ Left $ RecurrenceEndDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 03) (11 * 3600 + 30 * 60)
                  }
              ]
          )
    it "keeps a zoned instance on the same wall clock when a THISANDFUTURE override reschedules it across a daylight saving change" $
      -- The difference between the override and the instance it names is an
      -- exact duration, as it is for a DTEND, so rescheduling has to be exact
      -- too.  Zurich goes back from 03:00 to 02:00 on the 30th of October 2022,
      -- so the first instance is at +0200 and the two after it are at +0100:
      -- adding two hours to the instant and reading the result back in the
      -- time zone is what puts all three at 11:00 local.
      summariesOf
        (fromGregorian 2022 11 05)
        ( T.intercalate "\r\n" $
            concat
              [ ["BEGIN:VCALENDAR", "PRODID:test", "VERSION:2.0"],
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
                ],
                [ "BEGIN:VEVENT",
                  "DTSTAMP:20200101T000000Z",
                  "UID:test",
                  "DTSTART;TZID=Europe/Zurich:20221029T090000",
                  "DTEND;TZID=Europe/Zurich:20221029T100000",
                  "RRULE:FREQ=DAILY;COUNT=3",
                  "SUMMARY:Series",
                  "END:VEVENT"
                ],
                [ "BEGIN:VEVENT",
                  "DTSTAMP:20200101T000000Z",
                  "UID:test",
                  "RECURRENCE-ID;RANGE=THISANDFUTURE;TZID=Europe/Zurich:20221029T090000",
                  "DTSTART;TZID=Europe/Zurich:20221029T110000",
                  "DTEND;TZID=Europe/Zurich:20221029T120000",
                  "SUMMARY:Moved",
                  "END:VEVENT"
                ],
                ["END:VCALENDAR", ""]
              ]
        )
        `shouldReturn` M.singleton
          (UID "test")
          ( S.fromList
              [ Occurrence
                  { occurrenceComponent = Just "Moved",
                    occurrenceStart = Just $ DateTimeStartDateTime $ DateTimeZoned "Europe/Zurich" $ LocalTime (fromGregorian 2022 10 29) (TimeOfDay 11 00 00),
                    occurrenceEnd = Just $ Left $ RecurrenceEndDateTime $ DateTimeZoned "Europe/Zurich" $ LocalTime (fromGregorian 2022 10 29) (TimeOfDay 12 00 00)
                  },
                Occurrence
                  { occurrenceComponent = Just "Moved",
                    occurrenceStart = Just $ DateTimeStartDateTime $ DateTimeZoned "Europe/Zurich" $ LocalTime (fromGregorian 2022 10 30) (TimeOfDay 11 00 00),
                    occurrenceEnd = Just $ Left $ RecurrenceEndDateTime $ DateTimeZoned "Europe/Zurich" $ LocalTime (fromGregorian 2022 10 30) (TimeOfDay 12 00 00)
                  },
                Occurrence
                  { occurrenceComponent = Just "Moved",
                    occurrenceStart = Just $ DateTimeStartDateTime $ DateTimeZoned "Europe/Zurich" $ LocalTime (fromGregorian 2022 10 31) (TimeOfDay 11 00 00),
                    occurrenceEnd = Just $ Left $ RecurrenceEndDateTime $ DateTimeZoned "Europe/Zurich" $ LocalTime (fromGregorian 2022 10 31) (TimeOfDay 12 00 00)
                  }
              ]
          )
    it "reschedules an all-day instance by whole days" $
      -- An all-day instance has a DATE-valued start and a DATE-valued
      -- RECURRENCE-ID, so the difference between the override and the instance
      -- it names is a number of days rather than an exact duration.
      summariesOf
        limit
        ( calendarWithEvents
            [ [ "UID:test",
                "DTSTART;VALUE=DATE:20200101",
                "DTEND;VALUE=DATE:20200102",
                "RRULE:FREQ=WEEKLY;COUNT=3",
                "SUMMARY:Series"
              ],
              [ "UID:test",
                "RECURRENCE-ID;VALUE=DATE;RANGE=THISANDFUTURE:20200108",
                "DTSTART;VALUE=DATE:20200109",
                "DTEND;VALUE=DATE:20200110",
                "SUMMARY:A day later"
              ]
            ]
        )
        `shouldReturn` M.singleton
          (UID "test")
          ( S.fromList
              [ Occurrence
                  { occurrenceComponent = Just "Series",
                    occurrenceStart = Just $ DateTimeStartDate $ Date $ fromGregorian 2020 01 01,
                    occurrenceEnd = Just $ Left $ RecurrenceEndDate $ Date $ fromGregorian 2020 01 02
                  },
                Occurrence
                  { occurrenceComponent = Just "A day later",
                    occurrenceStart = Just $ DateTimeStartDate $ Date $ fromGregorian 2020 01 09,
                    occurrenceEnd = Just $ Left $ RecurrenceEndDate $ Date $ fromGregorian 2020 01 10
                  },
                Occurrence
                  { occurrenceComponent = Just "A day later",
                    occurrenceStart = Just $ DateTimeStartDate $ Date $ fromGregorian 2020 01 16,
                    occurrenceEnd = Just $ Left $ RecurrenceEndDate $ Date $ fromGregorian 2020 01 17
                  }
              ]
          )
    it "does not reschedule a later instance that has an override of its own" $
      -- @
      -- Subsequent instances
      -- defined in separate components are not impacted by the given
      -- recurrence instance.
      -- @
      --
      -- The third instance has its own override, so the THISANDFUTURE shift of
      -- two hours must not reach it: it is at 15:00 where its own component
      -- puts it, not at 11:00.
      summariesOf
        limit
        ( calendarWithEvents
            [ [ "UID:test",
                "DTSTART:20200101T090000Z",
                "DTEND:20200101T100000Z",
                "RRULE:FREQ=DAILY;COUNT=3",
                "SUMMARY:Series"
              ],
              [ "UID:test",
                "RECURRENCE-ID;RANGE=THISANDFUTURE:20200102T090000Z",
                "DTSTART:20200102T110000Z",
                "DTEND:20200102T120000Z",
                "SUMMARY:Moved"
              ],
              [ "UID:test",
                "RECURRENCE-ID:20200103T090000Z",
                "DTSTART:20200103T150000Z",
                "DTEND:20200103T160000Z",
                "SUMMARY:Its own"
              ]
            ]
        )
        `shouldReturn` M.singleton
          (UID "test")
          ( S.fromList
              [ Occurrence
                  { occurrenceComponent = Just "Series",
                    occurrenceStart = Just $ DateTimeStartDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 01) (9 * 3600),
                    occurrenceEnd = Just $ Left $ RecurrenceEndDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 01) (10 * 3600)
                  },
                Occurrence
                  { occurrenceComponent = Just "Moved",
                    occurrenceStart = Just $ DateTimeStartDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 02) (11 * 3600),
                    occurrenceEnd = Just $ Left $ RecurrenceEndDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 02) (12 * 3600)
                  },
                Occurrence
                  { occurrenceComponent = Just "Its own",
                    occurrenceStart = Just $ DateTimeStartDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 03) (15 * 3600),
                    occurrenceEnd = Just $ Left $ RecurrenceEndDateTime $ DateTimeUTC $ UTCTime (fromGregorian 2020 01 03) (16 * 3600)
                  }
              ]
          )
    -- The scenario directory pins what a calendar with a fixable error recurs
    -- into, and that a conforming run halts on it.  These pin which error it
    -- halted on, which that directory cannot say.
    let fixableErrorsOf :: Day -> Text -> IO [RecurrenceFixableError]
        fixableErrorsOf lim contents = do
          calendar <- shouldConform $ parseVCalendar contents
          case runConformLenient $ runCalendarR lim calendar $ recurEvents lim (calendarEvents calendar) of
            Left err -> expectationFailure $ show err
            Right (_, notes) -> pure $ notesFixableErrors notes
    it "reports an override that names no instance of its series" $
      fixableErrorsOf
        limit
        ( calendarWithEvents
            [ ["UID:test", "DTSTART:20200101T000000Z", "RRULE:FREQ=DAILY;COUNT=3"],
              ["UID:test", "RECURRENCE-ID:20200205T000000Z", "DTSTART:20200205T120000Z"]
            ]
        )
        `shouldReturn` [ RecurrenceIdentifierUnmatched
                           (UID "test")
                           (DateTimeStartDateTime (DateTimeUTC (UTCTime (fromGregorian 2020 02 05) 0)))
                       ]
    it "reports two overrides that name the same instance at the same SEQUENCE" $
      fixableErrorsOf
        limit
        ( calendarWithEvents
            [ ["UID:test", "DTSTART:20200101T000000Z", "RRULE:FREQ=DAILY;COUNT=3"],
              ["UID:test", "RECURRENCE-ID:20200102T000000Z", "DTSTART:20200102T120000Z"],
              ["UID:test", "RECURRENCE-ID:20200102T000000Z", "DTSTART:20200102T180000Z"]
            ]
        )
        `shouldReturn` [ RecurrenceIdentifierDuplicate
                           (UID "test")
                           (DateTimeStartDateTime (DateTimeUTC (UTCTime (fromGregorian 2020 01 02) 0)))
                       ]
    it "reports two components of one UID that both leave out the RECURRENCE-ID" $
      fixableErrorsOf
        limit
        ( calendarWithEvents
            [ ["UID:test", "DTSTART:20200101T000000Z"],
              ["UID:test", "DTSTART:20200301T000000Z"]
            ]
        )
        `shouldReturn` [RecurrenceMultipleSeries (UID "test")]
  describe "recurCalendar" $
    it "recurs the events, to-dos and journals of one UID as three recurrence sets" $ do
      -- @
      -- The full range of calendar components specified by a
      -- recurrence set is referenced by referring to just the "UID"
      -- property value corresponding to the calendar component.
      -- @
      --
      -- A UID names a recurrence set within one kind of component, so the same
      -- UID on a VEVENT, a VTODO and a VJOURNAL is three recurrence sets and
      -- not one.
      --
      -- The starts are zoned and resolved afterwards, which is what shows that
      -- 'runCalendarR' supplied the time zone that the calendar defines: 01:00
      -- at +0100 is midnight UTC.
      calendar <-
        shouldConform $
          parseVCalendar $
            T.intercalate "\r\n" $
              concat
                [ ["BEGIN:VCALENDAR", "PRODID:test", "VERSION:2.0"],
                  [ "BEGIN:VTIMEZONE",
                    "TZID:Test/PlusOne",
                    "BEGIN:STANDARD",
                    "DTSTART:19700101T000000",
                    "TZOFFSETFROM:+0100",
                    "TZOFFSETTO:+0100",
                    "TZNAME:P1",
                    "END:STANDARD",
                    "END:VTIMEZONE"
                  ],
                  [ "BEGIN:VEVENT",
                    "DTSTAMP:20200101T000000Z",
                    "UID:shared",
                    "DTSTART;TZID=Test/PlusOne:20200101T010000",
                    "RRULE:FREQ=DAILY;COUNT=2",
                    "END:VEVENT"
                  ],
                  [ "BEGIN:VTODO",
                    "DTSTAMP:20200101T000000Z",
                    "UID:shared",
                    "DTSTART;TZID=Test/PlusOne:20200201T010000",
                    "RRULE:FREQ=DAILY;COUNT=2",
                    "END:VTODO"
                  ],
                  [ "BEGIN:VJOURNAL",
                    "DTSTAMP:20200101T000000Z",
                    "UID:shared",
                    "DTSTART;TZID=Test/PlusOne:20200301T010000",
                    "RRULE:FREQ=DAILY;COUNT=2",
                    "END:VJOURNAL"
                  ],
                  ["END:VCALENDAR", ""]
                ]
      starts <-
        shouldConform $
          runCalendarR limit calendar $ do
            recurrence <- recurCalendar limit calendar
            let resolveStarts :: Set (Occurrence component) -> R (Set (Maybe Timestamp))
                resolveStarts occurrences =
                  S.fromList . map resolvedStart <$> mapM resolveOccurrence (S.toList occurrences)
            events <- traverse resolveStarts (calendarRecurrenceEvents recurrence)
            todos <- traverse resolveStarts (calendarRecurrenceTodos recurrence)
            journals <- traverse resolveStarts (calendarRecurrenceJournals recurrence)
            pure (events, todos, journals)
      let midnightUTC :: Integer -> Int -> Int -> Maybe Timestamp
          midnightUTC y m d = Just $ TimestampUTCTime $ UTCTime (fromGregorian y m d) 0
      starts
        `shouldBe` ( M.singleton (UID "shared") (S.fromList [midnightUTC 2020 01 01, midnightUTC 2020 01 02]),
                     M.singleton (UID "shared") (S.fromList [midnightUTC 2020 02 01, midnightUTC 2020 02 02]),
                     M.singleton (UID "shared") (S.fromList [midnightUTC 2020 03 01, midnightUTC 2020 03 02])
                   )

  -- A VTODO ends at its DUE where a VEVENT ends at its DTEND, and the two
  -- properties agree on their value type and on what recurrence does with
  -- them, so an occurrence carries whichever of them the component spelled as
  -- one 'RecurrenceEnd'.  The golden format writes that back out as a DTEND
  -- line whatever the component called it.
  scenarioDir "test_resources/event" $ \fp -> do
    eventFile <- liftIO $ parseRelFile fp
    when (fileExtension eventFile == Just ".ics") $ do
      it "recurs this file correctly" $ do
        contents <- TE.decodeUtf8 <$> SB.readFile (fromRelFile eventFile)
        event <- shouldConform $ parseComponentFromText contents
        goldenFile <- replaceExtension ".occ" eventFile
        pure $ pureGoldenEventRecurrenceFile goldenFile limit event
  scenarioDir "test_resources/todo" $ \fp -> do
    todoFile <- liftIO $ parseRelFile fp
    when (fileExtension todoFile == Just ".ics") $ do
      it "recurs this file correctly" $ do
        contents <- TE.decodeUtf8 <$> SB.readFile (fromRelFile todoFile)
        todo <- shouldConform $ parseComponentFromText contents
        goldenFile <- replaceExtension ".occ" todoFile
        pure $
          goldenOccurrenceFile goldenFile $
            shouldConform $
              runRWithoutZones (expandRecurring limit (todoRecurring todo))

  -- A VJOURNAL has no property for the end of an instance at all, so its
  -- occurrences never have one and the golden writes an empty line where the
  -- end would go.
  scenarioDir "test_resources/journal" $ \fp -> do
    journalFile <- liftIO $ parseRelFile fp
    when (fileExtension journalFile == Just ".ics") $ do
      it "recurs this file correctly" $ do
        contents <- TE.decodeUtf8 <$> SB.readFile (fromRelFile journalFile)
        journal <- shouldConform $ parseComponentFromText contents
        goldenFile <- replaceExtension ".occ" journalFile
        pure $
          goldenOccurrenceFile goldenFile $
            shouldConform $
              runRWithoutZones (expandRecurring limit (journalRecurring journal))

  -- Events that a conforming implementation must refuse to recur, and what
  -- recurring them anyway produces.
  --
  -- 'scenarioDir' does not recur into subdirectories, so these are not also
  -- picked up by the block above, which requires recurring without any fixable
  -- error at all.
  scenarioDir "test_resources/event/fixable" $ \fp -> do
    eventFile <- liftIO $ parseRelFile fp
    when (fileExtension eventFile == Just ".ics") $ do
      it "cannot recur this event without fixing something" $ do
        contents <- TE.decodeUtf8 <$> SB.readFile (fromRelFile eventFile)
        event <- shouldConform $ parseComponentFromText contents
        let recurring = eventRecurring (event :: Event)
        -- 'runConform' fixes nothing, so it halts on the first fixable error.
        -- The warning type here is 'Void', so this and 'runConformStrict'
        -- cannot disagree.
        case runConform $ runRWithoutZones $ expandRecurring limit recurring of
          Left _ -> pure ()
          Right (occurrences, _) ->
            expectationFailure $
              unlines
                [ "Should have needed fixing but recurred cleanly into:",
                  ppShow (S.toList occurrences)
                ]

      it "recurs this event leniently" $ do
        contents <- TE.decodeUtf8 <$> SB.readFile (fromRelFile eventFile)
        event <- shouldConform $ parseComponentFromText contents
        let recurring = eventRecurring (event :: Event)
        goldenFile <- replaceExtension ".occ" eventFile
        pure $
          goldenOccurrenceFile goldenFile $
            shouldConformLenient $
              runRWithoutZones $
                expandRecurring limit recurring

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
          runCalendarR limit calendar $ do
            occurrences <- allOccurrences <$> recurEvents limit (calendarEvents calendar)
            S.fromList <$> mapM resolveOccurrence (S.toList occurrences)
        goldenFile <- replaceExtension ".res" eventFile
        pure $ goldenResolvedFile goldenFile $ pure resolvedEvents

  -- Calendars that a conforming implementation must refuse to recur, and what
  -- recurring them anyway produces.
  --
  -- These are the fixable errors that only a group of components sharing a UID
  -- can have, so unlike test_resources/event/fixable they need a whole
  -- calendar rather than a single component.
  --
  -- 'scenarioDir' does not recur into subdirectories, so these are not also
  -- picked up by the block above, which requires recurring without any fixable
  -- error at all.
  scenarioDir "test_resources/calendar/fixable" $ \fp -> do
    calendarFile <- liftIO $ parseRelFile fp
    when (fileExtension calendarFile == Just ".ics") $ do
      it "cannot recur this calendar without fixing something" $ do
        contents <- TE.decodeUtf8 <$> SB.readFile (fromRelFile calendarFile)
        calendar <- shouldConform $ parseVCalendar contents
        -- 'runConform' fixes nothing, so it halts on the first fixable error.
        -- The warning type here is 'Void', so this and 'runConformStrict'
        -- cannot disagree.
        case runConform $ runCalendarR limit calendar $ recurEvents limit (calendarEvents calendar) of
          Left _ -> pure ()
          Right (occurrences, _) ->
            expectationFailure $
              unlines
                [ "Should have needed fixing but recurred cleanly into:",
                  ppShow (M.toList (M.map S.toList occurrences))
                ]

      it "recurs this calendar leniently" $ do
        contents <- TE.decodeUtf8 <$> SB.readFile (fromRelFile calendarFile)
        calendar <- shouldConform $ parseVCalendar contents
        goldenFile <- replaceExtension ".occ" calendarFile
        pure $
          goldenOccurrenceFile goldenFile $
            shouldConformLenient $
              runCalendarR limit calendar $
                allOccurrences <$> recurEvents limit (calendarEvents calendar)

pureGoldenCalendarRecurrenceFile :: Path Rel File -> Day -> Calendar -> GoldenTest (Set (Occurrence ()))
pureGoldenCalendarRecurrenceFile goldenFile limit calendar =
  goldenOccurrenceFile goldenFile $
    shouldConform $
      runCalendarR limit calendar $
        allOccurrences <$> recurEvents limit (calendarEvents calendar)

-- | Every instance of every recurrence set, with the UID that groups them
-- thrown away
--
-- The golden formats and the assertions that predate grouping are about the
-- instances themselves, and every one of those scenarios holds a single UID.
allOccurrences :: (Ord component) => Map UID (Set (Occurrence component)) -> Set (Occurrence component)
allOccurrences = S.unions . M.elems

pureGoldenEventRecurrenceFile :: Path Rel File -> Day -> Event -> GoldenTest (Set (Occurrence ()))
pureGoldenEventRecurrenceFile goldenFile limit event =
  goldenOccurrenceFile goldenFile $ shouldConform $ runRWithoutZones (expandRecurring limit (eventRecurring event))

-- | Pin the recurrence set that a scenario produces
--
-- The golden format records where each instance starts and ends, and not which
-- component it came from, so the component is dropped before the comparison.
goldenOccurrenceFile ::
  Path Rel File ->
  IO (Set (Occurrence component)) ->
  GoldenTest (Set (Occurrence ()))
goldenOccurrenceFile goldenFile produceOccurrences =
  GoldenTest
    { goldenTestRead = do
        mGoldenContents <- forgivingAbsence $ TE.decodeUtf8 <$> SB.readFile (fromRelFile goldenFile)
        pure $ parseOccurrences <$> mGoldenContents,
      goldenTestProduce = S.map void <$> produceOccurrences,
      goldenTestWrite = SB.writeFile (fromRelFile goldenFile) . TE.encodeUtf8 . renderOccurrences,
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
parseOccurrences :: Text -> Set (Occurrence ())
parseOccurrences contents = case runConform (parseUnfoldedLines contents) of
  Left _ -> S.empty
  Right (unfoldedLines, _) ->
    S.fromList $ mapMaybe parseOccurrence $ chunksOf 2 unfoldedLines

parseOccurrence :: [UnfoldedLine] -> Maybe (Occurrence ())
parseOccurrence = \case
  [UnfoldedLine startLine, UnfoldedLine endDurationLine] -> either (const Nothing) (Just . fst) $
    runConform $ do
      occurrenceStart <- case startLine of
        "" -> pure Nothing
        l -> Just <$> parsePropertyFromText (renderUnfoldedLines [UnfoldedLine l])
      occurrenceEnd <- case endDurationLine of
        "" -> pure Nothing
        l ->
          Just
            <$> (Left . dateTimeEndRecurrenceEnd <$> parsePropertyFromText (renderUnfoldedLines [UnfoldedLine l]))
              `altConform` (Right <$> parsePropertyFromText (renderUnfoldedLines [UnfoldedLine l]))
      pure Occurrence {occurrenceComponent = (), ..}
  _ -> Nothing

renderOccurrences :: Set (Occurrence ()) -> Text
renderOccurrences = foldMap renderOccurrence

-- | Render an occurrence as exactly two lines, so that an absent property
-- does not shift the occurrences after it in the file.
--
-- 'parseOccurrences' reads the file back in two-line chunks and reads an
-- empty line as an absent property, so both lines must always be written.
renderOccurrence :: Occurrence () -> Text
renderOccurrence Occurrence {..} =
  T.concat
    [ case occurrenceStart of
        Nothing -> "\r\n"
        Just dtstart -> renderPropertyText dtstart,
      case occurrenceEnd of
        Nothing -> "\r\n"
        Just (Left end) -> renderPropertyText (recurrenceEndDateTimeEnd end)
        Just (Right dur) -> renderPropertyText dur
    ]

-- | Pin the instants that a scenario resolves to
--
-- As with 'goldenOccurrenceFile', the component is not part of the format.
goldenResolvedFile ::
  Path Rel File ->
  IO (Set (Resolved component)) ->
  GoldenTest (Set (Resolved ()))
goldenResolvedFile goldenFile produceResolveds =
  GoldenTest
    { goldenTestRead = do
        mGoldenContents <- forgivingAbsence $ TE.decodeUtf8 <$> SB.readFile (fromRelFile goldenFile)
        pure $ parseResolveds <$> mGoldenContents,
      goldenTestProduce = S.map void <$> produceResolveds,
      goldenTestWrite = SB.writeFile (fromRelFile goldenFile) . TE.encodeUtf8 . renderResolveds,
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

parseResolveds :: Text -> Set (Resolved ())
parseResolveds =
  S.fromList
    . mapMaybe (parseResolved . T.intercalate "\n")
    . chunksOf 2
    . T.splitOn "\n"

parseResolved :: Text -> Maybe (Resolved ())
parseResolved t = case T.splitOn "\n" t of
  (startLine : endDurationLine : _) -> do
    resolvedStart <- goM startLine
    resolvedEnd <- goM endDurationLine
    pure Resolved {resolvedComponent = (), ..}
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

renderResolveds :: Set (Resolved ()) -> Text
renderResolveds = foldMap renderResolved . S.toAscList

renderResolved :: Resolved () -> Text
renderResolved Resolved {..} =
  T.pack $
    concat
      [ maybe "" go resolvedStart <> "\n",
        maybe "" go resolvedEnd <> "\n"
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
