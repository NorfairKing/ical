{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ICal.RecurrenceSpec where

import Data.Time
import ICal.Property
import ICal.PropertyType
import ICal.Recurrence
import ICal.Recurrence.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Recurrence" $ do
    genValidSpec @Recurrence
  describe "RecurringEvent" $ do
    genValidSpec @RecurringEvent
  describe "EventOccurrence" $ do
    genValidSpec @EventOccurrence

  describe "recurRecurrenceDateTimes" $ do
    -- The following tests are built from these these examples from the spec:
    -- @
    -- RDATE:19970714T123000Z
    -- @
    it "works for any interpretation of the date time in UTC example from the spec" $
      forAllValid $ \date ->
        recurRecurrenceDateTimes
          (DateTimeStartDate date)
          Nothing
          [RecurrenceDateTimes (DateTimesUTC [UTCTime (fromGregorian 1997 07 14) (timeOfDayToTime (TimeOfDay 23 00 00))])]
          `shouldBe` [ EventOccurrence
                         { eventOccurrenceStart =
                             Just (DateTimeStartDateTime (DateTimeUTC (UTCTime (fromGregorian 1997 07 14) (timeOfDayToTime (TimeOfDay 23 00 00))))),
                           eventOccurrenceEndOrDuration = Nothing
                         }
                     ]

    -- @
    -- RDATE;TZID=America/New_York:19970714T083000
    -- @
    it "works for any interpretation of the date time with timezone example from the spec" $
      forAllValid $ \date ->
        recurRecurrenceDateTimes
          (DateTimeStartDate date)
          Nothing
          [RecurrenceDateTimes (DateTimesZoned "America/New_York" [LocalTime (fromGregorian 1997 07 14) (TimeOfDay 08 30 00)])]
          `shouldBe` [ EventOccurrence
                         { eventOccurrenceStart =
                             Just (DateTimeStartDateTime (DateTimeZoned "America/New_York" (LocalTime (fromGregorian 1997 07 14) (TimeOfDay 23 00 00)))),
                           eventOccurrenceEndOrDuration = Nothing
                         }
                     ]

    -- @
    -- RDATE;VALUE=PERIOD:19960403T020000Z/19960403T040000Z,
    --  19960404T010000Z/PT3H
    -- @
    it "works for any interpretation of the period example from the spec" $
      forAllValid $ \date ->
        recurRecurrenceDateTimes
          (DateTimeStartDate date)
          Nothing
          [ RecurrencePeriods
              [ PeriodStartEnd
                  (UTCTime (fromGregorian 1996 04 03) (timeOfDayToTime (TimeOfDay 02 00 00)))
                  (UTCTime (fromGregorian 1996 04 03) (timeOfDayToTime (TimeOfDay 04 00 00))),
                PeriodStartDuration
                  (UTCTime (fromGregorian 1996 04 04) (timeOfDayToTime (TimeOfDay 01 00 00)))
                  (DurationTime (DurTime {durTimeSign = Positive, durTimeHour = 3, durTimeMinute = 0, durTimeSecond = 0}))
              ]
          ]
          `shouldBe` [ EventOccurrence
                         { eventOccurrenceStart =
                             Just (DateTimeStartDateTime (DateTimeUTC (UTCTime (fromGregorian 1996 04 03) (timeOfDayToTime (TimeOfDay 02 00 00))))),
                           eventOccurrenceEndOrDuration = Just (Left (DateTimeEndDateTime (DateTimeUTC (UTCTime (fromGregorian 1996 04 03) (timeOfDayToTime (TimeOfDay 04 00 00))))))
                         },
                       EventOccurrence
                         { eventOccurrenceStart =
                             Just (DateTimeStartDateTime (DateTimeUTC (UTCTime (fromGregorian 1996 04 04) (timeOfDayToTime (TimeOfDay 01 00 00))))),
                           eventOccurrenceEndOrDuration = Just (Right (DurationTime (DurTime {durTimeSign = Positive, durTimeHour = 3, durTimeMinute = 0, durTimeSecond = 0})))
                         }
                     ]

    -- @
    -- RDATE;VALUE=DATE:19970101,19970120,19970217,19970421
    --  19970526,19970704,19970901,19971014,19971128,19971129,19971225
    -- @
    it "works for any interpretation of the second dates example from the spec" $
      forAllValid $ \date ->
        recurRecurrenceDateTimes
          (DateTimeStartDate date)
          Nothing
          [ RecurrenceDates
              [ Date $ fromGregorian 1997 01 01,
                Date $ fromGregorian 1997 01 20,
                Date $ fromGregorian 1997 02 17,
                Date $ fromGregorian 1997 04 21,
                Date $ fromGregorian 1997 05 26,
                Date $ fromGregorian 1997 07 24,
                Date $ fromGregorian 1997 09 01,
                Date $ fromGregorian 1997 10 14,
                Date $ fromGregorian 1997 11 28,
                Date $ fromGregorian 1997 11 29,
                Date $ fromGregorian 1997 12 25
              ]
          ]
          `shouldBe` [ EventOccurrence
                         { eventOccurrenceStart =
                             Just (DateTimeStartDate (Date (fromGregorian 1997 01 01))),
                           eventOccurrenceEndOrDuration = Nothing
                         },
                       EventOccurrence
                         { eventOccurrenceStart =
                             Just (DateTimeStartDate (Date (fromGregorian 1997 01 20))),
                           eventOccurrenceEndOrDuration = Nothing
                         },
                       EventOccurrence
                         { eventOccurrenceStart =
                             Just (DateTimeStartDate (Date (fromGregorian 1997 02 17))),
                           eventOccurrenceEndOrDuration = Nothing
                         },
                       EventOccurrence
                         { eventOccurrenceStart =
                             Just (DateTimeStartDate (Date (fromGregorian 1997 04 21))),
                           eventOccurrenceEndOrDuration = Nothing
                         },
                       EventOccurrence
                         { eventOccurrenceStart =
                             Just (DateTimeStartDate (Date (fromGregorian 1997 05 26))),
                           eventOccurrenceEndOrDuration = Nothing
                         },
                       EventOccurrence
                         { eventOccurrenceStart =
                             Just (DateTimeStartDate (Date (fromGregorian 1997 07 04))),
                           eventOccurrenceEndOrDuration = Nothing
                         },
                       EventOccurrence
                         { eventOccurrenceStart =
                             Just (DateTimeStartDate (Date (fromGregorian 1997 09 01))),
                           eventOccurrenceEndOrDuration = Nothing
                         },
                       EventOccurrence
                         { eventOccurrenceStart =
                             Just (DateTimeStartDate (Date (fromGregorian 1997 10 14))),
                           eventOccurrenceEndOrDuration = Nothing
                         },
                       EventOccurrence
                         { eventOccurrenceStart =
                             Just (DateTimeStartDate (Date (fromGregorian 1997 11 28))),
                           eventOccurrenceEndOrDuration = Nothing
                         },
                       EventOccurrence
                         { eventOccurrenceStart =
                             Just (DateTimeStartDate (Date (fromGregorian 1997 11 29))),
                           eventOccurrenceEndOrDuration = Nothing
                         },
                       EventOccurrence
                         { eventOccurrenceStart =
                             Just (DateTimeStartDate (Date (fromGregorian 1997 12 25))),
                           eventOccurrenceEndOrDuration = Nothing
                         }
                     ]

    -- @
    -- RDATE;VALUE=DATE:19970304,19970504,19970704,19970904
    -- @
    it "works for any interpretation of the second dates example from the spec" $
      forAllValid $ \date ->
        recurRecurrenceDateTimes
          (DateTimeStartDate date)
          Nothing
          [ RecurrenceDates
              [ Date $ fromGregorian 1977 03 04,
                Date $ fromGregorian 1997 05 04,
                Date $ fromGregorian 1997 07 04,
                Date $ fromGregorian 1997 09 04
              ]
          ]
          `shouldBe` [ EventOccurrence
                         { eventOccurrenceStart =
                             Just (DateTimeStartDate (Date (fromGregorian 1977 03 04))),
                           eventOccurrenceEndOrDuration = Nothing
                         },
                       EventOccurrence
                         { eventOccurrenceStart =
                             Just (DateTimeStartDate (Date (fromGregorian 1977 05 04))),
                           eventOccurrenceEndOrDuration = Nothing
                         },
                       EventOccurrence
                         { eventOccurrenceStart =
                             Just (DateTimeStartDate (Date (fromGregorian 1977 07 04))),
                           eventOccurrenceEndOrDuration = Nothing
                         },
                       EventOccurrence
                         { eventOccurrenceStart =
                             Just (DateTimeStartDate (Date (fromGregorian 1977 09 04))),
                           eventOccurrenceEndOrDuration = Nothing
                         }
                     ]

    -- @
    -- BEGIN:DAYLIGHT
    -- DTSTART:19740106T020000
    -- RDATE:19750223T020000
    -- TZOFFSETFROM:-0500
    -- TZOFFSETTO:-0400
    -- TZNAME:EDT
    -- END:DAYLIGHT
    -- @
    it "works for the timezone example from the spec" $
      recurRecurrenceDateTimes
        (DateTimeStartDateTime (DateTimeFloating (LocalTime (fromGregorian 1974 01 06) (TimeOfDay 02 00 00))))
        Nothing
        [RecurrenceDateTimes (DateTimesFloating [LocalTime (fromGregorian 1975 02 23) (TimeOfDay 02 00 00)])]
        `shouldBe` [ EventOccurrence
                       { eventOccurrenceStart = Just (DateTimeStartDateTime (DateTimeFloating (LocalTime (fromGregorian 1977 02 23) (TimeOfDay 02 00 00)))),
                         eventOccurrenceEndOrDuration = Nothing
                       }
                   ]
