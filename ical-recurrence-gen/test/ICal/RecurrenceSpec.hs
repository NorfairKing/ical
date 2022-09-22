{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ICal.RecurrenceSpec where

import Data.Time
import ICal.Conformance
import ICal.Conformance.TestUtils
import ICal.Property
import ICal.PropertyType
import ICal.Recurrence
import ICal.Recurrence.Gen ()
import Test.QuickCheck (property)
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
    let exampleSpec mStart mEndOrDuration recurrenceDateTimess expected =
          it "works for this example" $
            let supplyStart p = case mStart of
                  Nothing -> forAllValid p
                  Just start -> property $ p start
             in supplyStart $ \dateTimeStart -> do
                  actual <- shouldConformStrict $ recurRecurrenceDateTimes dateTimeStart mEndOrDuration recurrenceDateTimess
                  actual `shouldBe` expected

    -- [Section 3.8.5.2.  Recurrence Date-Times](https://www.rfc-editor.org/rfc/rfc5545#section-3.8.5.2)
    --
    -- @
    -- RDATE:19970714T123000Z
    -- @
    exampleSpec
      Nothing
      Nothing
      [RecurrenceDateTimes (DateTimesUTC [UTCTime (fromGregorian 1997 07 14) (timeOfDayToTime (TimeOfDay 23 00 00))])]
      [ EventOccurrence
          { eventOccurrenceStart =
              Just (DateTimeStartDateTime (DateTimeUTC (UTCTime (fromGregorian 1997 07 14) (timeOfDayToTime (TimeOfDay 23 00 00))))),
            eventOccurrenceEndOrDuration = Nothing
          }
      ]

    -- [Section 3.8.5.2.  Recurrence Date-Times](https://www.rfc-editor.org/rfc/rfc5545#section-3.8.5.2)
    --
    -- @
    -- RDATE;TZID=America/New_York:19970714T083000
    -- @
    exampleSpec
      Nothing
      Nothing
      [RecurrenceDateTimes (DateTimesZoned "America/New_York" [LocalTime (fromGregorian 1997 07 14) (TimeOfDay 08 30 00)])]
      [ EventOccurrence
          { eventOccurrenceStart =
              Just (DateTimeStartDateTime (DateTimeZoned "America/New_York" (LocalTime (fromGregorian 1997 07 14) (TimeOfDay 08 30 00)))),
            eventOccurrenceEndOrDuration = Nothing
          }
      ]

    -- [Section 3.8.5.2.  Recurrence Date-Times](https://www.rfc-editor.org/rfc/rfc5545#section-3.8.5.2)
    --
    -- @
    -- RDATE;VALUE=PERIOD:19960403T020000Z/19960403T040000Z,
    --  19960404T010000Z/PT3H
    -- @
    exampleSpec
      Nothing
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
      [ EventOccurrence
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

    -- [Section 3.8.5.2.  Recurrence Date-Times](https://www.rfc-editor.org/rfc/rfc5545#section-3.8.5.2)
    --
    -- @
    -- RDATE;VALUE=DATE:19970101,19970120,19970217,19970421
    --  19970526,19970704,19970901,19971014,19971128,19971129,19971225
    -- @
    exampleSpec
      Nothing
      Nothing
      [ RecurrenceDates
          [ Date $ fromGregorian 1997 01 01,
            Date $ fromGregorian 1997 01 20,
            Date $ fromGregorian 1997 02 17,
            Date $ fromGregorian 1997 04 21,
            Date $ fromGregorian 1997 05 26,
            Date $ fromGregorian 1997 07 04,
            Date $ fromGregorian 1997 09 01,
            Date $ fromGregorian 1997 10 14,
            Date $ fromGregorian 1997 11 28,
            Date $ fromGregorian 1997 11 29,
            Date $ fromGregorian 1997 12 25
          ]
      ]
      [ EventOccurrence
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
    -- [Section 3.1.1.  List and Field Separators](https://www.rfc-editor.org/rfc/rfc5545#section-3.1.1)
    --
    -- @
    -- RDATE;VALUE=DATE:19970304,19970504,19970704,19970904
    -- @
    exampleSpec
      Nothing
      Nothing
      [ RecurrenceDates
          [ Date $ fromGregorian 1997 03 04,
            Date $ fromGregorian 1997 05 04,
            Date $ fromGregorian 1997 07 04,
            Date $ fromGregorian 1997 09 04
          ]
      ]
      [ EventOccurrence
          { eventOccurrenceStart =
              Just (DateTimeStartDate (Date (fromGregorian 1997 03 04))),
            eventOccurrenceEndOrDuration = Nothing
          },
        EventOccurrence
          { eventOccurrenceStart =
              Just (DateTimeStartDate (Date (fromGregorian 1997 05 04))),
            eventOccurrenceEndOrDuration = Nothing
          },
        EventOccurrence
          { eventOccurrenceStart =
              Just (DateTimeStartDate (Date (fromGregorian 1997 07 04))),
            eventOccurrenceEndOrDuration = Nothing
          },
        EventOccurrence
          { eventOccurrenceStart =
              Just (DateTimeStartDate (Date (fromGregorian 1997 09 04))),
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
    exampleSpec
      (Just (DateTimeStartDateTime (DateTimeFloating (LocalTime (fromGregorian 1974 01 06) (TimeOfDay 02 00 00)))))
      Nothing
      [RecurrenceDateTimes (DateTimesFloating [LocalTime (fromGregorian 1975 02 23) (TimeOfDay 02 00 00)])]
      [ EventOccurrence
          { eventOccurrenceStart = Just (DateTimeStartDateTime (DateTimeFloating (LocalTime (fromGregorian 1975 02 23) (TimeOfDay 02 00 00)))),
            eventOccurrenceEndOrDuration = Nothing
          }
      ]
