module ICal where

import Data.ByteString (ByteString)
import ICal.Component
import ICal.ContentLine
import ICal.UnfoldedLine

parseICalendar :: ByteString -> Either String [Calendar]
parseICalendar contents = do
  unfoldedLines <- parseUnfoldedLinesByteString contents
  contentLines <- mapM parseContentLine unfoldedLines
  parseICalendarFromContentLines contentLines
