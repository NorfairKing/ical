{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Data.DList (DList (..))
import qualified Data.DList as DList
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import Data.Void
import GHC.Generics (Generic)
import ICal.ContentLine
import ICal.Property
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

parseFirst :: forall a. IsProperty a => Text -> [ContentLine] -> CP a
parseFirst propertyName = go
  where
    go :: [ContentLine] -> CP a
    go = \case
      [] -> fail $ "Did not find required " <> T.unpack propertyName
      (cl : cls) -> case propertyP cl of
        Right result -> pure result
        Left _ -> go cls

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
    not $ contentLineName == "END" && contentLineValueRaw contentLineValue == "VEVENT"
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
    not $ contentLineName == "END" && contentLineValueRaw contentLineValue == "VTIMEZONE"
  pure TimeZone

vTimeZoneB :: TimeZone -> DList ContentLine
vTimeZoneB = sectionB "VTIMEZONE" $ \_ -> mempty
