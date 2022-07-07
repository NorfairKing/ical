module ICal where

import Data.ByteString (ByteString)
import qualified Data.DList as DList
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import ICal.Component
import ICal.ContentLine
import ICal.UnfoldedLine

parseICalendar :: ByteString -> Either String [Calendar]
parseICalendar contents = do
  unfoldedLines <- parseUnfoldedLinesByteString contents
  contentLines <- mapM parseContentLine unfoldedLines
  parseICalendarFromContentLines contentLines

-- TODO which encoding does the spec define? Use that one instead
renderICalendarByteString :: [Calendar] -> ByteString
renderICalendarByteString = TE.encodeUtf8 . renderICalendarText

renderICalendarText :: [Calendar] -> Text
renderICalendarText =
  renderContentLinesText
    . DList.toList
    . DList.concat
    . map componentB
