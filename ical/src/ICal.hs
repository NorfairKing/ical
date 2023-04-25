{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ICal
  ( module ICal,
    module ICal.PropertyType,
    module ICal.Property,
    module ICal.Component,
    module ICal.Parameter,
    module ICal.UnfoldedLine,
    module ICal.ContentLine,
  )
where

import Control.Arrow (left)
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.DList as DList
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Void
import ICal.Component
import ICal.Conformance
import ICal.ContentLine
import ICal.Parameter
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

type ICalendar = [Calendar]

data ICalParseError
  = TextDecodingError !TE.UnicodeException
  | UnfoldingError !UnfoldingError
  | ContentLineParseError !String
  | CalendarParseError CalendarParseError
  deriving (Show, Eq)

instance Exception ICalParseError where
  displayException = \case
    TextDecodingError e -> displayException e
    UnfoldingError ue -> displayException ue
    ContentLineParseError s -> s
    CalendarParseError peb -> displayException peb

data ICalParseFixableError = CalendarParseFixableError !CalendarParseFixableError
  deriving (Show, Eq)

instance Exception ICalParseFixableError where
  displayException = \case
    CalendarParseFixableError fe -> displayException fe

data ICalParseWarning = CalendarParseWarning !CalendarParseWarning
  deriving (Show, Eq)

instance Exception ICalParseWarning where
  displayException = \case
    CalendarParseWarning pw -> displayException pw

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
parseICalendarByteString ::
  ByteString ->
  Conform
    ICalParseError
    ICalParseFixableError
    ICalParseWarning
    ICalendar
parseICalendarByteString contents = do
  textContents <- conformFromEither $ left TextDecodingError $ TE.decodeUtf8' contents
  parseICalendar textContents

-- | Parse a VCALENDAR stream
parseICalendar ::
  Text ->
  Conform
    ICalParseError
    ICalParseFixableError
    ICalParseWarning
    ICalendar
parseICalendar contents = do
  unfoldedLines <- conformMapAll UnfoldingError absurd absurd $ parseUnfoldedLines contents
  contentLines <- conformMapAll ContentLineParseError absurd absurd $ conformFromEither $ mapM parseContentLineFromUnfoldedLine unfoldedLines
  conformMapAll CalendarParseError CalendarParseFixableError CalendarParseWarning $ parseICalendarFromContentLines contentLines

-- | Parse a single VCALENDAR
parseVCalendar ::
  Text ->
  Conform
    ICalParseError
    ICalParseFixableError
    ICalParseWarning
    Calendar
parseVCalendar contents = do
  unfoldedLines <- conformMapAll UnfoldingError absurd absurd $ parseUnfoldedLines contents
  contentLines <- conformMapAll ContentLineParseError absurd absurd $ conformFromEither $ mapM parseContentLineFromUnfoldedLine unfoldedLines
  conformMapAll CalendarParseError CalendarParseFixableError CalendarParseWarning $ parseVCalendarFromContentLines contentLines

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
renderICalendarByteString :: ICalendar -> ByteString
renderICalendarByteString = TE.encodeUtf8 . renderICalendar

-- | Render a VCALENDAR stream
renderICalendar :: ICalendar -> Text
renderICalendar =
  renderUnfoldedLines
    . map renderContentLineToUnfoldedLine
    . DList.toList
    . iCalendarB

-- | Render a single VCALENDAR
renderVCalendar :: Calendar -> Text
renderVCalendar = renderICalendar . (: [])

-- | Parse an individual component from Text directly
--
-- You probably don't want to use this.
-- Individual components are not described by the spec as text.
parseComponentFromText ::
  (IsComponent component) =>
  Text ->
  Conform
    ICalParseError
    ICalParseFixableError
    ICalParseWarning
    component
parseComponentFromText contents = do
  unfoldedLines <- conformMapAll UnfoldingError absurd absurd $ parseUnfoldedLines contents
  contentLines <- conformMapAll ContentLineParseError absurd absurd $ conformFromEither $ mapM parseContentLineFromUnfoldedLine unfoldedLines
  conformMapAll CalendarParseError CalendarParseFixableError CalendarParseWarning $ parseComponentFromContentLines contentLines

-- | Render an individual component from Text directly
--
-- You probably don't want to use this.
-- Individual components are not described by the spec as text.
renderComponentText :: IsComponent component => component -> Text
renderComponentText =
  renderUnfoldedLines
    . map renderContentLineToUnfoldedLine
    . DList.toList
    . uncurry renderGeneralComponent
    . namedComponentB

-- | Parse an individual property from Text directly
--
-- You probably don't want to use this.
-- Individual properties are not described by the spec as text.
parsePropertyFromText ::
  IsProperty property =>
  Text ->
  Conform
    ICalParseError
    ICalParseFixableError
    ICalParseWarning
    property
parsePropertyFromText contents = do
  unfoldedLines <- conformMapAll UnfoldingError absurd absurd $ parseUnfoldedLines contents
  case unfoldedLines of
    [] -> unfixableError $ ContentLineParseError "No unfolded lines."
    [unfoldedLine] -> do
      contentLine <- conformMapAll ContentLineParseError absurd absurd $ conformFromEither $ parseContentLineFromUnfoldedLine unfoldedLine
      conformMapAll (ContentLineParseError . show) absurd absurd $ propertyContentLineP contentLine
    _ -> unfixableError $ ContentLineParseError "More than one unfolded line."

-- | Render an individual property from Text directly
--
-- You probably don't want to use this.
-- Individual properties are not described by the spec as text.
renderPropertyText :: IsProperty property => property -> Text
renderPropertyText =
  renderUnfoldedLines
    . (: [])
    . renderContentLineToUnfoldedLine
    . propertyContentLineB
