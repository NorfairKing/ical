module ICal where

import Data.ByteString (ByteString)
import ICal.ContentLine
import ICal.UnfoldedLine
import ICal.VCalendar

parseICalendar :: ByteString -> Either String [Calendar]
parseICalendar contents = do
  unfoldedLines <- parseUnfoldedLinesByteString contents
  contentLines <- mapM parseContentLine unfoldedLines
  parseICalendarFromContentLines contentLines
