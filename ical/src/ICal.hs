{-# LANGUAGE OverloadedStrings #-}

module ICal
  ( module ICal,
    module ICal.PropertyType,
    module ICal.Property,
    module ICal.Component,
  )
where

import Control.Arrow (left)
import Data.ByteString (ByteString)
import qualified Data.DList as DList
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import ICal.Component
import ICal.ContentLine
import ICal.Property
import ICal.PropertyType
import ICal.UnfoldedLine

-- | MIME Content type
--
-- @
-- The iCalendar format is suitable as an exchange format between
-- applications or systems.  The format is defined in terms of a MIME
-- content type.
-- @
--
-- > icalContentType = "text/calendar"
icalContentType :: ByteString
icalContentType = "text/calendar"

-- | Parse an ICalendar from a ByteString, assuming UTF8 encoding
--
-- UTF8 is the default character encoding according to the spec:
--
-- @
-- 3.1.4.  Character Set
--
-- There is not a property parameter to declare the charset used in a
-- property value.  The default charset for an iCalendar stream is UTF-8
-- as defined in [RFC3629].
--
-- The "charset" Content-Type parameter MUST be used in MIME transports
-- to specify the charset being used.
-- @
parseICalendarByteString :: ByteString -> Either String [Calendar]
parseICalendarByteString contents = do
  textContents <- left show $ TE.decodeUtf8' contents
  parseICalendar textContents

-- | Parse a VCALENDAR stream
parseICalendar :: Text -> Either String [Calendar]
parseICalendar contents = do
  unfoldedLines <- parseUnfoldedLines contents
  contentLines <- mapM parseContentLineFromUnfoldedLine unfoldedLines
  parseICalendarFromContentLines contentLines

-- | Parse a single VCALENDAR
parseVCalendar :: Text -> Either String Calendar
parseVCalendar contents = do
  unfoldedLines <- parseUnfoldedLines contents
  contentLines <- mapM parseContentLineFromUnfoldedLine unfoldedLines
  parseVCalendarFromContentLines contentLines

-- | Render an ICalendar as a ByteString using the UTF8 encoding.
--
-- UTF8 is the default character encoding according to the spec:
--
-- @
-- 3.1.4.  Character Set
--
-- There is not a property parameter to declare the charset used in a
-- property value.  The default charset for an iCalendar stream is UTF-8
-- as defined in [RFC3629].
--
-- The "charset" Content-Type parameter MUST be used in MIME transports
-- to specify the charset being used.
-- @
renderICalendarByteString :: [Calendar] -> ByteString
renderICalendarByteString = TE.encodeUtf8 . renderICalendar

-- | Render a VCALENDAR stream
renderICalendar :: [Calendar] -> Text
renderICalendar =
  renderContentLines
    . DList.toList
    . iCalendarB

-- | Render a single VCALENDAR
renderVCalendar :: Calendar -> Text
renderVCalendar = renderICalendar . (: [])
