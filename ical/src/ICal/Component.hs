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

module ICal.Component where

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
  -- | Name for this component
  componentName :: Proxy component -> Text

  -- | Parser for this component
  componentP :: CP component

  -- | Builder for this component
  componentB :: component -> DList ContentLine

componentSectionP :: forall component. IsComponent component => CP component
componentSectionP = sectionP (componentName (Proxy :: Proxy component)) componentP

sectionP :: Text -> CP a -> CP a
sectionP name parser = do
  parseGivenProperty $ Begin name
  result <- parser
  parseGivenProperty $ End name
  pure result

parseGivenProperty :: IsProperty property => property -> CP ()
parseGivenProperty givenProperty = void $ single $ propertyContentLineB givenProperty

componentSectionB :: forall component. IsComponent component => component -> DList ContentLine
componentSectionB = sectionB (componentName (Proxy :: Proxy component)) componentB

sectionB :: Text -> (a -> DList ContentLine) -> (a -> DList ContentLine)
sectionB name func =
  (DList.singleton (propertyContentLineB (Begin name)) <>)
    . (<> DList.singleton (propertyContentLineB (End name)))
    . func

propertyListB :: IsProperty property => property -> DList ContentLine
propertyListB = DList.singleton . propertyContentLineB

propertyMListB :: IsProperty property => Maybe property -> DList ContentLine
propertyMListB = maybe DList.empty (DList.singleton . propertyContentLineB)

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
iCalendarP = many componentSectionP

instance IsComponent Calendar where
  componentName Proxy = "VCALENDAR"
  componentP = vCalendarP
  componentB = vCalendarB

vCalendarP :: CP Calendar
vCalendarP = do
  calPropLines <- takeWhileP (Just "calprops") $ \ContentLine {..} ->
    contentLineName /= "BEGIN" && contentLineName /= "END"

  calendarProdId <- parseFirst "PRODID" calPropLines
  calendarVersion <- parseFirst "VERSION" calPropLines

  calendarMods <-
    many $
      msum
        [ (\event c -> c {calendarEvents = event : calendarEvents c})
            <$> componentSectionP,
          (\timeZone c -> c {calendarTimeZones = timeZone : calendarTimeZones c})
            <$> componentSectionP
        ]
  let calendarEvents = []
  let calendarTimeZones = []
  let calendarMod :: Calendar -> Calendar
      calendarMod = appEndo $ mconcat $ map Endo calendarMods
  pure $ calendarMod $ Calendar {..}

vCalendarB :: Calendar -> DList ContentLine
vCalendarB Calendar {..} =
  mconcat $
    concat
      [ [ propertyListB calendarProdId,
          propertyListB calendarVersion
        ],
        map componentSectionB calendarEvents,
        map componentSectionB calendarTimeZones
      ]

parseFirst :: forall a. IsProperty a => Text -> [ContentLine] -> CP a
parseFirst propertyName = go
  where
    go :: [ContentLine] -> CP a
    go = \case
      [] -> fail $ "Did not find required " <> T.unpack propertyName
      (cl : cls) -> case propertyContentLineP cl of
        Right result -> pure result
        Left _ -> go cls

parseFirstMaybe :: forall a. IsProperty a => [ContentLine] -> CP (Maybe a)
parseFirstMaybe = go
  where
    go :: [ContentLine] -> CP (Maybe a)
    go = \case
      [] -> pure Nothing
      (cl : cls) -> case propertyContentLineP cl of
        Right result -> pure (Just result)
        Left _ -> go cls

-- [section 3.6.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.6.1)
data Event = Event
  { -- @
    --     ; The following are REQUIRED,
    --     ; but MUST NOT occur more than once.
    -- @
    eventUID :: !UID,
    eventDateTimeStamp :: !DateTimeStamp,
    -- @
    --     ;
    --     ; The following is REQUIRED if the component
    --     ; appears in an iCalendar object that doesn't
    --     ; specify the "METHOD" property; otherwise, it
    --     ; is OPTIONAL; in any case, it MUST NOT occur
    --     ; more than once.
    --     ;
    -- @
    eventDateTimeStart :: !(Maybe DateTimeStart),
    -- @
    --     ;
    --     ; The following are OPTIONAL,
    --     ; but MUST NOT occur more than once.
    --     ;
    -- @
    eventCreated :: !(Maybe Created)
  }
  deriving (Show, Eq, Generic)

instance Validity Event

instance IsComponent Event where
  componentName Proxy = "VEVENT"
  componentP = vEventP
  componentB = vEventB

vEventP :: CP Event
vEventP = do
  eventProperties <- takeWhileP (Just "eventProperties") $ \ContentLine {..} ->
    not $ contentLineName == "END" && contentLineValueRaw contentLineValue == "VEVENT"
  eventUID <- parseFirst "UID" eventProperties
  eventDateTimeStamp <- parseFirst "DTSTAMP" eventProperties
  eventDateTimeStart <- parseFirstMaybe eventProperties
  eventCreated <- parseFirstMaybe eventProperties
  pure Event {..}

vEventB :: Event -> DList ContentLine
vEventB Event {..} =
  mconcat
    [ propertyListB eventUID,
      propertyListB eventDateTimeStamp,
      propertyMListB eventDateTimeStart,
      propertyMListB eventCreated
    ]

-- [section 3.6.5](https://datatracker.ietf.org/doc/html/rfc5545#section-3.6.5)
data TimeZone = TimeZone
  deriving (Show, Eq, Generic)

instance Validity TimeZone

instance IsComponent TimeZone where
  componentName Proxy = "VTIMEZONE"
  componentP = vTimeZoneP
  componentB = vTimeZoneB

vTimeZoneP :: CP TimeZone
vTimeZoneP = do
  _ <- takeWhileP (Just "timezoneProperties") $ \ContentLine {..} ->
    not $ contentLineName == "END" && contentLineValueRaw contentLineValue == "VTIMEZONE"
  pure TimeZone

vTimeZoneB :: TimeZone -> DList ContentLine
vTimeZoneB _ = mempty
