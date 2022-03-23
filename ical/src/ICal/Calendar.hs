{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Calendar where

import Control.Arrow (left)
import Control.Monad
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.DList (DList (..))
import qualified Data.DList as DList
import Data.Either
import Data.Functor
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import Data.Void
import GHC.Generics (Generic)
import ICal.ContentLine
import ICal.UnfoldedLine
import Text.Megaparsec

parseICalendarFromContentLines :: [ContentLine] -> Either String [Calendar]
parseICalendarFromContentLines contentLines =
  left errorBundlePretty $ parse iCalendarP "" contentLines

type CP = Parsec Void [ContentLine]

instance VisualStream [ContentLine] where
  showTokens :: Proxy [ContentLine] -> NonEmpty ContentLine -> String
  showTokens Proxy =
    T.unpack
      . renderUnfoldedLinesText
      . map renderContentLine
      . NE.toList

-- It would be nice to be able to implement this so we can use 'errorBundlePretty' above.
instance TraversableStream [ContentLine] where
  reachOffset ::
    Int ->
    PosState [ContentLine] ->
    (Maybe String, PosState [ContentLine])
  reachOffset offset posState =
    let newInput = drop offset $ pstateInput posState
        newState =
          posState
            { pstateInput = newInput,
              pstateOffset = offset,
              pstateSourcePos =
                (pstateSourcePos posState)
                  { sourceLine = mkPos (offset + 1)
                  }
            }
     in case newInput of
          [] -> (Nothing, newState)
          (cl : _) -> (Just $ T.unpack $ renderUnfoldedLinesText [renderContentLine cl], newState)

-- [section 3.6](https://datatracker.ietf.org/doc/html/rfc5545#section-3.6)
data Calendar = Calendar
  { calendarProdId :: !ProdId,
    calendarVersion :: !Version,
    calendarEvents :: ![Event],
    calendarTimeZones :: ![TimeZone]
  }
  deriving (Show, Eq, Generic)

instance Validity Calendar

iCalendarP :: CP [Calendar]
iCalendarP = many vCalendarP

vCalendarP :: CP Calendar
vCalendarP = sectionP "VCALENDAR" $ do
  calPropLines <- takeWhileP (Just "calprops") $ \ContentLine {..} ->
    contentLineName /= "BEGIN" && contentLineName /= "END"

  calendarProdId <- parseFirst "PRODID" prodIdP calPropLines
  calendarVersion <- parseFirst "VERSION" versionP calPropLines

  calendarMods <-
    many $
      msum
        [ (\event c -> c {calendarEvents = event : calendarEvents c})
            <$> vEventP,
          (\timeZone c -> c {calendarTimeZones = timeZone : calendarTimeZones c})
            <$> vTimeZoneP
        ]
  let calendarEvents = []
  let calendarTimeZones = []
  let calendarMod :: Calendar -> Calendar
      calendarMod = appEndo $ mconcat $ map Endo calendarMods
  pure $ calendarMod $ Calendar {..}

vCalendarB :: Calendar -> DList ContentLine
vCalendarB = sectionB "VCALENDAR" $ \Calendar {..} ->
  mconcat $
    concat
      [ [ prodIdB calendarProdId,
          versionB calendarVersion
        ],
        map vEventB calendarEvents,
        map vTimeZoneB calendarTimeZones
      ]

-- [section 3.7.3](https://datatracker.ietf.org/doc/html/rfc5545#section-3.7.3)
newtype ProdId = ProdId {unProdId :: Text}
  deriving (Show, Eq, Generic)

instance Validity ProdId

prodIdP :: CP ProdId
prodIdP = do
  ContentLine {..} <- lineWithNameP "PRODID"
  pure $ ProdId {unProdId = contentLineValue}

prodIdB :: ProdId -> DList ContentLine
prodIdB = DList.singleton . mkSimpleContentLine "PRODID" . unProdId

-- [section 3.7.4](https://datatracker.ietf.org/doc/html/rfc5545#section-3.7.4)
newtype Version = Version {unVersion :: Text}
  deriving (Show, Eq, Generic)

instance Validity Version

versionP :: CP Version
versionP = do
  ContentLine {..} <- lineWithNameP "VERSION"
  pure $ Version {unVersion = contentLineValue}

versionB :: Version -> DList ContentLine
versionB = DList.singleton . mkSimpleContentLine "VERSION" . unVersion

-- [section 3.8.4.7](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.4.7)
newtype UID = UID {unUID :: Text}
  deriving (Show, Eq, Generic)

instance Validity UID

uidP :: CP UID
uidP = do
  ContentLine {..} <- lineWithNameP "UID"
  pure $ UID {unUID = contentLineValue}

uidB :: UID -> DList ContentLine
uidB = DList.singleton . mkSimpleContentLine "UID" . unUID

-- [section 3.8.7.2](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.7.2)
newtype DateTimeStamp = DateTimeStamp {unDateTimeStamp :: DateTime}
  deriving (Show, Eq, Generic)

instance Validity DateTimeStamp

dateTimeStampP :: CP DateTimeStamp
dateTimeStampP = do
  ContentLine {..} <- lineWithNameP "DTSTAMP"
  case parseDateTime contentLineValue of
    Left err -> fail err
    Right dateTime -> pure DateTimeStamp {unDateTimeStamp = dateTime}

dateTimeStampB :: DateTimeStamp -> DList ContentLine
dateTimeStampB = DList.singleton . mkSimpleContentLine "DTSTAMP" . renderDateTime . unDateTimeStamp

-- [section 3.3.5](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.5)
data DateTime = DateTime
  deriving (Show, Eq, Generic)

instance Validity DateTime

renderDateTime :: DateTime -> Text
renderDateTime = undefined

parseDateTime :: Text -> Either String DateTime
parseDateTime = undefined

-- [section 3.3.4](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.4)
data Date = Date
  deriving (Show, Eq, Generic)

instance Validity Date

renderDate :: Date -> Text
renderDate = undefined

parseDate :: Text -> Either String Date
parseDate = undefined

-- [section 3.3.12](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.12)
--
-- @
--      Purpose:  This value type is used to identify values that contain a
--         time of day.
--
--      Format Definition:  This value type is defined by the following
--         notation:
--
--          time         = time-hour time-minute time-second [time-utc]
--
--          time-hour    = 2DIGIT        ;00-23
--          time-minute  = 2DIGIT        ;00-59
--          time-second  = 2DIGIT        ;00-60
--          ;The "60" value is used to account for positive "leap" seconds.
--
--          time-utc     = "Z"
--
-- @
data Time
  = TimeFloating !Time.TimeOfDay
  | TimeUTC !Time.TimeOfDay
  -- TODO how do we represent times with a timezone identifier?
  deriving (Show, Eq, Generic)

instance Validity Time

renderTime :: Time -> Text
renderTime =
  T.pack . \case
    TimeFloating tod -> Time.formatTime Time.defaultTimeLocale timeFloatingFormatStr tod
    TimeUTC tod -> Time.formatTime Time.defaultTimeLocale timeUTCFormatStr tod

parseTime :: Text -> Either String Time
parseTime t =
  let s = T.unpack t
   in (TimeFloating <$> parseTimeEither timeFloatingFormatStr s)
        <|> (TimeUTC <$> parseTimeEither timeUTCFormatStr s)

parseTimeEither :: Time.ParseTime t => String -> String -> Either String t
parseTimeEither formatStr s = case Time.parseTime Time.defaultTimeLocale formatStr s of
  Nothing -> Left $ "Could not parse time value: " <> s
  Just t -> Right t

timeFloatingFormatStr :: String
timeFloatingFormatStr = "%H%M%S"

timeUTCFormatStr :: String
timeUTCFormatStr = "%H%M%SZ"

parseFirst :: forall a. CI Text -> CP a -> [ContentLine] -> CP a
parseFirst partName parser = go
  where
    go :: [ContentLine] -> CP a
    go = \case
      [] -> fail $ "Did not find required " <> T.unpack (CI.original partName)
      cls@(_ : rest) -> case parse parser "" cls of
        Right result -> pure result
        Left _ -> go rest

lineWithNameP :: ContentLineName -> CP ContentLine
lineWithNameP name = satisfy $ \ContentLine {..} ->
  contentLineName == name

sectionB :: Text -> (a -> DList ContentLine) -> (a -> DList ContentLine)
sectionB name func =
  (beginB name <>)
    . (<> endB name)
    . func

sectionP :: Text -> CP a -> CP a
sectionP name parser = do
  beginP name
  result <- parser
  endP name
  pure result

beginP :: Text -> CP ()
beginP name = void $ single $ mkSimpleContentLine "BEGIN" name

beginB :: Text -> DList ContentLine
beginB name = DList.singleton $ mkSimpleContentLine "BEGIN" name

endP :: Text -> CP ()
endP name = void $ single $ mkSimpleContentLine "END" name

endB :: Text -> DList ContentLine
endB name = DList.singleton $ mkSimpleContentLine "END" name

-- [section 3.6.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.6.1)
data Event = Event
  { eventUID :: !UID,
    eventDateTimeStamp :: !DateTimeStamp
  }
  deriving (Show, Eq, Generic)

instance Validity Event

vEventP :: CP Event
vEventP = sectionP "VEVENT" $ do
  eventProperties <- takeWhileP (Just "eventProperties") $ \ContentLine {..} ->
    not $ contentLineName == "END" && contentLineValue == "VEVENT"
  eventUID <- parseFirst "UID" uidP eventProperties
  eventDateTimeStamp <- parseFirst "DTSTAMP" dateTimeStampP eventProperties
  pure Event {..}

vEventB :: Event -> DList ContentLine
vEventB = sectionB "VEVENT" $ \Event {..} ->
  mconcat
    [ uidB eventUID,
      dateTimeStampB eventDateTimeStamp
    ]

-- [section 3.6.5](https://datatracker.ietf.org/doc/html/rfc5545#section-3.6.5)
data TimeZone = TimeZone
  deriving (Show, Eq, Generic)

instance Validity TimeZone

vTimeZoneP :: CP TimeZone
vTimeZoneP = sectionP "VTIMEZONE" $ do
  _ <- takeWhileP (Just "timezoneProperties") $ \ContentLine {..} ->
    not $ contentLineName == "END" && contentLineValue == "VTIMEZONE"
  pure TimeZone

vTimeZoneB :: TimeZone -> DList ContentLine
vTimeZoneB = sectionB "VTIMEZONE" $ \_ -> mempty
