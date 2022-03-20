{-# LANGUAGE OverloadedStrings #-}

module ICalSpec where

import qualified Data.ByteString as SB
import ICal
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
      case parseICalendar contents of
        Left err -> expectationFailure err
        Right contentLines -> shouldBeValid contentLines
