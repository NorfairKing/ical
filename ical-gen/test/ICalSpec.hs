{-# LANGUAGE OverloadedStrings #-}

module ICalSpec where

import qualified Data.ByteString as SB
import qualified Data.Text as T
import ICal
import ICal.Component.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  -- Test based on this part of the spec:
  --
  -- @
  --     The following is a simple example of an iCalendar object:
  --
  --         BEGIN:VCALENDAR
  --         VERSION:2.0
  --         PRODID:-//hacksw/handcal//NONSGML v1.0//EN
  --         BEGIN:VEVENT
  --         UID:19970610T172345Z-AF23B2@example.com
  --         DTSTAMP:19970610T172345Z
  --         DTSTART:19970714T170000Z
  --         DTEND:19970715T040000Z
  --         SUMMARY:Bastille Day Party
  --         END:VEVENT
  --         END:VCALENDAR
  -- @
  pending "above example"

  -- Tests based on example calendars
  scenarioDirRecur "test_resources/calendars" $ \calFile ->
    it "can parse this calendar" $ do
      contents <- SB.readFile calFile
      case parseICalendarByteString contents of
        Left err -> expectationFailure err
        Right contentLines -> shouldBeValid contentLines

  describe "renderVCalendar" $
    it "roundtrips with parseVCalendar" $
      forAllValid $ \vCalendar ->
        let rendered = renderVCalendar vCalendar
            ctx = unlines ["Rendered VCALENDAR:", T.unpack rendered]
         in context ctx $ case parseVCalendar rendered of
              Left err -> expectationFailure err
              Right vCalendar' -> vCalendar' `shouldBe` vCalendar

  describe "renderICalendar" $
    it "roundtrips with parseICalendar" $
      forAllValid $ \iCalendar ->
        let rendered = renderICalendar iCalendar
            ctx = unlines ["Rendered VCALENDAR stream:", T.unpack rendered]
         in context ctx $ case parseICalendar rendered of
              Left err -> expectationFailure err
              Right iCalendar' -> iCalendar' `shouldBe` iCalendar

  describe "renderICalendarByteString" $
    it "roundtrips with parseICalendarByteString" $
      forAllValid $ \iCalendar ->
        case parseICalendarByteString (renderICalendarByteString iCalendar) of
          Left err -> expectationFailure err
          Right iCalendar' -> iCalendar' `shouldBe` iCalendar
