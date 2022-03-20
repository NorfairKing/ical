{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.VCalendar where

import Control.Arrow (left)
import Control.Monad
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.DList (DList (..))
import qualified Data.DList as DList
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Data.Void
import Debug.Trace
import GHC.Generics (Generic)
import ICal.ContentLine
import ICal.UnfoldedLine
import Text.Megaparsec

parseICalendarFromContentLines :: [ContentLine] -> Either String [Calendar]
parseICalendarFromContentLines contentLines =
  left show $ parse iCalendarP "" contentLines

type CP = Parsec Void [ContentLine]

-- instance Stream [ContentLine] where
--   type Token [ContentLine] = ContentLine
--   type Tokens [ContentLine] = [ContentLine]
--   tokensToChunk :: Proxy [ContentLine] -> [ContentLine] -> [ContentLine]
--   tokensToChunk Proxy = id

instance VisualStream [ContentLine] where
  showTokens :: Proxy [ContentLine] -> NonEmpty ContentLine -> String
  showTokens Proxy =
    T.unpack
      . renderUnfoldedLinesText
      . map renderContentLine
      . NE.toList

-- It would be nice to be able to implement this so we can use 'errorBundlePretty' above.
-- instance TraversableStream [ContentLine] where
--   reachOffset ::
--     Int ->
--     PosState [ContentLine] ->
--     (Maybe String, PosState [ContentLine])
--   reachOffset = undefined
--   reachOffsetNoLine ::
--     Int ->
--     PosState [ContentLine] ->
--     PosState [ContentLine]
--   reachOffsetNoLine = undefined

-- [section 3.6](https://datatracker.ietf.org/doc/html/rfc5545#section-3.6)
data Calendar = Calendar
  { calendarProdId :: !ProdId,
    calendarVersion :: !Version,
    calendarEvents :: ![Event]
  }
  deriving (Show, Eq, Generic)

instance Validity Calendar

iCalendarP :: CP [Calendar]
iCalendarP = many vCalendarP

vCalendarP :: CP Calendar
vCalendarP = sectionP "VCALENDAR" $ do
  calPropLines <- takeWhileP (Just "calprops") $ \ContentLine {..} ->
    contentLineName /= "BEGIN"
  traceShowM calPropLines
  calendarProdId <- parseFirst "PRODID" prodIdP calPropLines
  calendarVersion <- parseFirst "VERSION" versionP calPropLines

  calendarEvents <- fmap catMaybes $
    many $ do
      ContentLine {..} <- lineWithNameP "BEGIN"
      case contentLineValue of
        "VEVENT" -> do
          event <- vEventP
          pure $ Just event
        _ -> pure Nothing

  pure Calendar {..}

vCalendarB :: Calendar -> DList ContentLine
vCalendarB = sectionB "VCALENDAR" $ \Calendar {..} ->
  mconcat $
    concat
      [ [ prodIdB calendarProdId,
          versionB calendarVersion
        ],
        map vEventB calendarEvents
      ]

-- [section 3.6.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.6.1)
data Event = Event
  deriving (Show, Eq, Generic)

instance Validity Event

vEventP :: CP Event
vEventP = sectionP "VEVENT" $ pure Event

vEventB :: Event -> DList ContentLine
vEventB = sectionB "VEVENT" $ \_ -> mempty

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
