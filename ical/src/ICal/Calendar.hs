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
import Data.Map (Map)
import qualified Data.Map as M
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

-- Law for this typeclass: The component roundtrips through '[ContentLine]'.
class IsComponent component where
  -- | Parser for this component
  componentP :: CP component

  -- | Builder for this component
  componentB :: component -> DList ContentLine

-- Law for this typeclass: The property roundtrips through 'ContentLine'.
class IsProperty property where
  -- | Parser for the property
  propertyP :: ContentLine -> Either String property

  -- | Builder for the property
  propertyB :: property -> ContentLine

class IsPropertyType propertyType where
  -- | Parser for the property type
  propertyTypeP :: ContentLineValue -> Either String propertyType

  -- | Builder for the property type
  propertyTypeB :: propertyType -> ContentLineValue

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

instance IsComponent Calendar where
  componentP = vCalendarP
  componentB = vCalendarB

vCalendarP :: CP Calendar
vCalendarP = sectionP "VCALENDAR" $ do
  calPropLines <- takeWhileP (Just "calprops") $ \ContentLine {..} ->
    contentLineName /= "BEGIN" && contentLineName /= "END"

  calendarProdId <- parseFirst "PRODID" calPropLines
  calendarVersion <- parseFirst "VERSION" calPropLines

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
      [ [ DList.fromList
            [ prodIdB calendarProdId,
              versionB calendarVersion
            ]
        ],
        map vEventB calendarEvents,
        map vTimeZoneB calendarTimeZones
      ]

-- [section 3.7.3](https://datatracker.ietf.org/doc/html/rfc5545#section-3.7.3)
newtype ProdId = ProdId {unProdId :: Text}
  deriving (Show, Eq, Generic)

instance Validity ProdId

instance IsProperty ProdId where
  propertyP = prodIdP
  propertyB = prodIdB

prodIdP :: ContentLine -> Either String ProdId
prodIdP = propertyWithNameP "PRODID" $ \ContentLine {..} ->
  Right ProdId {unProdId = contentLineValueRaw contentLineValue}

prodIdB :: ProdId -> ContentLine
prodIdB = mkSimpleContentLine "PRODID" . unProdId

-- [section 3.7.4](https://datatracker.ietf.org/doc/html/rfc5545#section-3.7.4)
newtype Version = Version {unVersion :: Text}
  deriving (Show, Eq, Generic)

instance Validity Version

instance IsProperty Version where
  propertyP = versionP
  propertyB = versionB

versionP :: ContentLine -> Either String Version
versionP = propertyWithNameP "VERSION" $ \ContentLine {..} ->
  Right $ Version {unVersion = contentLineValueRaw contentLineValue}

versionB :: Version -> ContentLine
versionB = mkSimpleContentLine "VERSION" . unVersion

-- [section 3.8.4.7](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.4.7)
newtype UID = UID {unUID :: Text}
  deriving (Show, Eq, Generic)

instance Validity UID

instance IsProperty UID where
  propertyP = uidP
  propertyB = uidB

uidP :: ContentLine -> Either String UID
uidP = propertyWithNameP "UID" $ \ContentLine {..} ->
  Right $ UID {unUID = contentLineValueRaw contentLineValue}

uidB :: UID -> ContentLine
uidB = mkSimpleContentLine "UID" . unUID

-- [section 3.8.7.2](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.7.2)
newtype DateTimeStamp = DateTimeStamp {unDateTimeStamp :: DateTime}
  deriving (Show, Eq, Generic)

instance Validity DateTimeStamp

instance IsProperty DateTimeStamp where
  propertyP = dateTimeStampP
  propertyB = dateTimeStampB

dateTimeStampP :: ContentLine -> Either String DateTimeStamp
dateTimeStampP = propertyWithNameP "DTSTAMP" $ \ContentLine {..} ->
  DateTimeStamp <$> dateTimeP contentLineValue

dateTimeStampB :: DateTimeStamp -> ContentLine
dateTimeStampB = ContentLine "DTSTAMP" . dateTimeB . unDateTimeStamp

-- [section 3.3.5](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.5)
data DateTime
  = DateTimeFloating !Time.LocalTime
  | DateTimeUTC !Time.LocalTime
  | DateTimeZoned !(CI Text) !Time.LocalTime -- TODO make this a timezoneID?
  deriving (Show, Eq, Generic)

instance Validity DateTime

instance IsPropertyType DateTime where
  propertyTypeP = dateTimeP
  propertyTypeB = dateTimeB

dateTimeP :: ContentLineValue -> Either String DateTime
dateTimeP ContentLineValue {..} =
  let s = T.unpack contentLineValueRaw
   in case M.lookup "TZID" contentLineValueParams of
        Just (UnquotedParam tzid :| _) -> DateTimeZoned tzid <$> parseTimeEither dateTimeZonedFormatStr s
        _ ->
          (DateTimeFloating <$> parseTimeEither dateTimeFloatingFormatStr s)
            <|> (DateTimeUTC <$> parseTimeEither dateTimeUTCFormatStr s)

dateTimeB :: DateTime -> ContentLineValue
dateTimeB =
  \case
    DateTimeFloating lt -> mkSimpleContentLineValue $ T.pack $ Time.formatTime Time.defaultTimeLocale dateTimeFloatingFormatStr lt
    DateTimeUTC lt -> mkSimpleContentLineValue $ T.pack $ Time.formatTime Time.defaultTimeLocale dateTimeUTCFormatStr lt
    DateTimeZoned tzid lt ->
      ContentLineValue
        { contentLineValueParams = M.singleton "TZID" (UnquotedParam tzid :| []),
          contentLineValueRaw = T.pack $ Time.formatTime Time.defaultTimeLocale dateTimeZonedFormatStr lt
        }

dateTimeFloatingFormatStr :: String
dateTimeFloatingFormatStr = "%Y%m%dT%H%M%S"

dateTimeUTCFormatStr :: String
dateTimeUTCFormatStr = "%Y%m%dT%H%M%SZ"

dateTimeZonedFormatStr :: String
dateTimeZonedFormatStr = "%Y%m%dT%H%M%S"

-- [section 3.3.4](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.4)
newtype Date = Date {unDate :: Time.Day}
  deriving (Show, Eq, Generic)

instance Validity Date

renderDate :: Date -> Text
renderDate = T.pack . Time.formatTime Time.defaultTimeLocale dateFormatStr . unDate

parseDate :: Text -> Either String Date
parseDate = fmap Date . parseTimeEither dateFormatStr . T.unpack

dateFormatStr :: String
dateFormatStr = "%Y%M%d"

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
parseTimeEither formatStr s = case Time.parseTimeM True Time.defaultTimeLocale formatStr s of
  Nothing -> Left $ "Could not parse time value: " <> s
  Just t -> Right t

timeFloatingFormatStr :: String
timeFloatingFormatStr = "%H%M%S"

timeUTCFormatStr :: String
timeUTCFormatStr = "%H%M%SZ"

parseFirst :: forall a. IsProperty a => Text -> [ContentLine] -> CP a
parseFirst propertyName = go
  where
    go :: [ContentLine] -> CP a
    go = \case
      [] -> fail $ "Did not find required " <> T.unpack propertyName
      (cl : cls) -> case propertyP cl of
        Right result -> pure result
        Left _ -> go cls

lineWithNameP :: ContentLineName -> CP ContentLine
lineWithNameP name = satisfy $ \ContentLine {..} ->
  contentLineName == name

propertyWithNameP :: ContentLineName -> (ContentLine -> Either String a) -> (ContentLine -> Either String a)
propertyWithNameP name func cln =
  if contentLineName cln == name
    then func cln
    else
      Left $
        unwords
          [ "Expected content line with name",
            show name,
            "but got",
            show $ contentLineName cln,
            "instead."
          ]

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

instance IsComponent Event where
  componentP = vEventP
  componentB = vEventB

vEventP :: CP Event
vEventP = sectionP "VEVENT" $ do
  eventProperties <- takeWhileP (Just "eventProperties") $ \ContentLine {..} ->
    not $ contentLineName == "END" && contentLineValue == "VEVENT"
  eventUID <- parseFirst "UID" eventProperties
  eventDateTimeStamp <- parseFirst "DTSTAMP" eventProperties
  pure Event {..}

vEventB :: Event -> DList ContentLine
vEventB = sectionB "VEVENT" $ \Event {..} ->
  DList.fromList
    [ uidB eventUID,
      dateTimeStampB eventDateTimeStamp
    ]

-- [section 3.6.5](https://datatracker.ietf.org/doc/html/rfc5545#section-3.6.5)
data TimeZone = TimeZone
  deriving (Show, Eq, Generic)

instance Validity TimeZone

instance IsComponent TimeZone where
  componentP = vTimeZoneP
  componentB = vTimeZoneB

vTimeZoneP :: CP TimeZone
vTimeZoneP = sectionP "VTIMEZONE" $ do
  _ <- takeWhileP (Just "timezoneProperties") $ \ContentLine {..} ->
    not $ contentLineName == "END" && contentLineValue == "VTIMEZONE"
  pure TimeZone

vTimeZoneB :: TimeZone -> DList ContentLine
vTimeZoneB = sectionB "VTIMEZONE" $ \_ -> mempty
