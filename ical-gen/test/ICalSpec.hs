{-# LANGUAGE OverloadedStrings #-}

module ICalSpec where

import qualified Data.ByteString as SB
import qualified Data.Text as T
import Data.Time
import ICal
import ICal.Component.Gen ()
import ICal.Conformance.TestUtils
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  -- From the spec:
  --
  -- @
  -- The following is a simple example of an iCalendar object:
  --
  --     BEGIN:VCALENDAR
  --     VERSION:2.0
  --     PRODID:-//hacksw/handcal//NONSGML v1.0//EN
  --     BEGIN:VEVENT
  --     UID:19970610T172345Z-AF23B2@example.com
  --     DTSTAMP:19970610T172345Z
  --     DTSTART:19970714T170000Z
  --     DTEND:19970715T040000Z
  --     SUMMARY:Bastille Day Party
  --     END:VEVENT
  --     END:VCALENDAR
  -- @
  let exampleText =
        T.pack $
          concatMap
            (<> "\r\n")
            [ "BEGIN:VCALENDAR",
              "VERSION:2.0",
              "PRODID:-//hacksw/handcal//NONSGML v1.0//EN",
              "BEGIN:VEVENT",
              "UID:19970610T172345Z-AF23B2@example.com",
              "DTSTAMP:19970610T172345Z",
              "DTSTART:19970714T170000Z",
              "DTEND:19970715T040000Z",
              "SUMMARY:Bastille Day Party",
              "END:VEVENT",
              "END:VCALENDAR"
            ]
  let expectedICal =
        [ (makeCalendar (ProdId "-//hacksw/handcal//NONSGML v1.0//EN"))
            { calendarEvents =
                [ ( makeEvent
                      (UID "19970610T172345Z-AF23B2@example.com")
                      ( DateTimeStamp
                          ( DateTimeUTC
                              ( localTimeToUTC
                                  utc
                                  ( LocalTime
                                      (fromGregorian 1997 06 10)
                                      (TimeOfDay 17 23 45)
                                  )
                              )
                          )
                      )
                  )
                    { eventDateTimeStart =
                        Just
                          ( DateTimeStartDateTime
                              ( DateTimeUTC
                                  ( localTimeToUTC
                                      utc
                                      ( LocalTime
                                          (fromGregorian 1997 07 14)
                                          (TimeOfDay 17 00 00)
                                      )
                                  )
                              )
                          ),
                      eventSummary = Just $ Summary "Bastille Day Party",
                      eventDateTimeEndDuration =
                        Just
                          ( Left
                              ( DateTimeEndDateTime
                                  ( DateTimeUTC
                                      ( localTimeToUTC
                                          utc
                                          ( LocalTime
                                              (fromGregorian 1997 07 15)
                                              (TimeOfDay 04 00 00)
                                          )
                                      )
                                  )
                              )
                          )
                    }
                ]
            }
        ]
  it "renders this example from the spec correctly" $ do
    actual <- shouldConformStrict $ parseICalendar (renderICalendar expectedICal)
    expected <- shouldConformStrict $ parseICalendar exampleText
    actual `shouldBe` expected

  it "parses this example from the spec correctly" $ do
    iCalendar <- shouldConformStrict $ parseICalendar exampleText
    iCalendar `shouldBe` expectedICal

  -- Tests based on example calendars
  scenarioDirRecur "test_resources/calendars" $ \calFile ->
    it "can parse this calendar" $ do
      contents <- SB.readFile calFile
      ical <- shouldConformStrict $ parseICalendarByteString contents
      shouldBeValid ical
      let rendered = renderICalendarByteString ical
      ical' <- shouldConformStrict $ parseICalendarByteString rendered
      ical' `shouldBe` ical

  describe "renderVCalendar" $
    it "roundtrips with parseVCalendar" $
      forAllValid $ \vCalendar ->
        let rendered = renderVCalendar vCalendar
            ctx = unlines ["Rendered VCALENDAR:", T.unpack rendered]
         in context ctx $ do
              vCalendar' <- shouldConformStrict $ parseVCalendar rendered
              vCalendar' `shouldBe` vCalendar

  describe "renderICalendar" $
    it "roundtrips with parseICalendar" $
      forAllValid $ \iCalendar ->
        let rendered = renderICalendar iCalendar
            ctx = unlines ["Rendered VCALENDAR stream:", T.unpack rendered]
         in context ctx $ do
              iCalendar' <- shouldConformStrict $ parseICalendar rendered
              iCalendar' `shouldBe` iCalendar

  describe "renderICalendarByteString" $
    it "roundtrips with parseICalendarByteString" $
      forAllValid $ \iCalendar -> do
        let rendered = renderICalendarByteString iCalendar
        iCalendar' <- shouldConformStrict $ parseICalendarByteString rendered
        iCalendar' `shouldBe` iCalendar
