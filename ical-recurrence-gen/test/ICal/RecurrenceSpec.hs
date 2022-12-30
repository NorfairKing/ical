{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ICal.RecurrenceSpec (spec) where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as SB
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import ICal
import ICal.Conformance
import ICal.Conformance.TestUtils
import ICal.Recurrence
import Path
import Path.IO
import Test.Syd

spec :: Spec
spec = do
  let limit = fromGregorian 2023 01 01
  scenarioDir "test_resources/event" $ \fp -> do
    eventFile <- liftIO $ parseRelFile fp
    when (fileExtension eventFile == Just ".ics") $ do
      it "recurs this file correctly" $ do
        contents <- TE.decodeUtf8 <$> SB.readFile (fromRelFile eventFile)
        event <- shouldConform $ parseComponentFromText contents
        goldenFile <- replaceExtension ".occ" eventFile
        pure $ pureGoldenEventRecurrenceFile goldenFile limit event
  scenarioDir "test_resources/calendar" $ \fp -> do
    eventFile <- liftIO $ parseRelFile fp
    when (fileExtension eventFile == Just ".ics") $ do
      it "recurs this file correctly" $ do
        contents <- TE.decodeUtf8 <$> SB.readFile (fromRelFile eventFile)
        cal <- shouldConform $ parseVCalendar contents
        goldenFile <- replaceExtension ".occ" eventFile
        pure $ pureGoldenCalendarRecurrenceFile goldenFile limit cal
      it "resolves this file correctly" $ do
        contents <- TE.decodeUtf8 <$> SB.readFile (fromRelFile eventFile)
        calendar <- shouldConform $ parseVCalendar contents
        resolvedEvents <- shouldConform $ do
          runR limit (calendarTimeZoneMap calendar) $ do
            occurrences <-
              fmap S.unions $
                mapM
                  (recurEvents limit . getRecurringEvent)
                  (calendarEvents calendar)
            S.fromList <$> mapM resolveEventOccurrence (S.toList occurrences)
        goldenFile <- replaceExtension ".res" eventFile
        pure $ goldenResolvedEventFile goldenFile $ pure resolvedEvents

pureGoldenCalendarRecurrenceFile :: Path Rel File -> Day -> Calendar -> GoldenTest (Set EventOccurrence)
pureGoldenCalendarRecurrenceFile goldenFile limit calendar =
  goldenEventOccurrenceFile goldenFile $
    shouldConform $ do
      runR limit (calendarTimeZoneMap calendar) $
        fmap S.unions $
          mapM
            (recurEvents limit . getRecurringEvent)
            (calendarEvents calendar)

pureGoldenEventRecurrenceFile :: Path Rel File -> Day -> Event -> GoldenTest (Set EventOccurrence)
pureGoldenEventRecurrenceFile goldenFile limit event =
  goldenEventOccurrenceFile goldenFile $ shouldConform $ runRWithoutZones (recurEvents limit (getRecurringEvent event))

goldenEventOccurrenceFile :: Path Rel File -> IO (Set EventOccurrence) -> GoldenTest (Set EventOccurrence)
goldenEventOccurrenceFile goldenFile produceOccurrences =
  GoldenTest
    { goldenTestRead = do
        mGoldenContents <- forgivingAbsence $ TE.decodeUtf8 <$> SB.readFile (fromRelFile goldenFile)
        pure $ parseEventOccurrences <$> mGoldenContents,
      goldenTestProduce = produceOccurrences,
      goldenTestWrite = SB.writeFile (fromRelFile goldenFile) . TE.encodeUtf8 . renderEventOccurrences,
      goldenTestCompare = \actual expected ->
        if actual == expected
          then Nothing
          else
            Just $
              Context
                ( stringsNotEqualButShouldHaveBeenEqual
                    (ppShow (S.toList actual))
                    (ppShow (S.toList expected))
                )
                (goldenContext (fromRelFile goldenFile))
    }

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (ys, zs) = splitAt n xs
   in ys : chunksOf n zs

parseEventOccurrences :: Text -> Set EventOccurrence
parseEventOccurrences =
  S.fromList
    . mapMaybe (parseEventOccurrence . T.intercalate "\r\n")
    . chunksOf 2
    . T.splitOn "\r\n"

parseEventOccurrence :: Text -> Maybe EventOccurrence
parseEventOccurrence t = case T.splitOn "\r\n" t of
  (startLine : endDurationLine : _) -> either (const Nothing) (Just . fst) $
    runConform $ do
      eventOccurrenceStart <- case startLine of
        "" -> pure Nothing
        l -> Just <$> parsePropertyFromText (l <> "\r\n")
      eventOccurrenceEndOrDuration <- case endDurationLine of
        "" -> pure Nothing
        l ->
          Just
            <$> (Left <$> parsePropertyFromText (l <> "\r\n"))
            `altConform` (Right <$> parsePropertyFromText (l <> "\r\n"))
      pure EventOccurrence {..}
  _ -> Nothing

renderEventOccurrences :: Set EventOccurrence -> Text
renderEventOccurrences = foldMap renderEventOccurrence

renderEventOccurrence :: EventOccurrence -> Text
renderEventOccurrence EventOccurrence {..} =
  T.concat
    [ case eventOccurrenceStart of
        Nothing -> ""
        Just dtstart -> renderPropertyText dtstart,
      case eventOccurrenceEndOrDuration of
        Nothing -> ""
        Just (Left end) -> renderPropertyText end
        Just (Right dur) -> renderPropertyText dur
    ]

goldenResolvedEventFile :: Path Rel File -> IO (Set ResolvedEvent) -> GoldenTest (Set ResolvedEvent)
goldenResolvedEventFile goldenFile produceResolvedEvents =
  GoldenTest
    { goldenTestRead = do
        mGoldenContents <- forgivingAbsence $ TE.decodeUtf8 <$> SB.readFile (fromRelFile goldenFile)
        pure $ parseResolvedEvents <$> mGoldenContents,
      goldenTestProduce = produceResolvedEvents,
      goldenTestWrite = SB.writeFile (fromRelFile goldenFile) . TE.encodeUtf8 . renderResolvedEvents,
      goldenTestCompare = \actual expected ->
        if actual == expected
          then Nothing
          else
            Just $
              Context
                ( stringsNotEqualButShouldHaveBeenEqual
                    (ppShow (S.toList actual))
                    (ppShow (S.toList expected))
                )
                (goldenContext (fromRelFile goldenFile))
    }

parseResolvedEvents :: Text -> Set ResolvedEvent
parseResolvedEvents =
  S.fromList
    . mapMaybe (parseResolvedEvent . T.intercalate "\n")
    . chunksOf 2
    . T.splitOn "\n"

parseResolvedEvent :: Text -> Maybe ResolvedEvent
parseResolvedEvent t = case T.splitOn "\n" t of
  (startLine : endDurationLine : _) -> do
    resolvedEventStart <- goM startLine
    resolvedEventEnd <- goM endDurationLine
    pure ResolvedEvent {..}
  _ -> Nothing
  where
    goM :: Text -> Maybe (Maybe Timestamp)
    goM "" = pure Nothing
    goM s = Just <$> go (T.unpack s)
    go :: String -> Maybe Timestamp
    go s =
      (TimestampLocalTime <$> parseTimeM False defaultTimeLocale localTimeFormat s)
        <|> (TimestampUTCTime <$> parseTimeM False defaultTimeLocale utcTimeFormat s)
        <|> (TimestampDay <$> parseTimeM False defaultTimeLocale dayFormat s)

renderResolvedEvents :: Set ResolvedEvent -> Text
renderResolvedEvents = foldMap renderResolvedEvent . S.toAscList

renderResolvedEvent :: ResolvedEvent -> Text
renderResolvedEvent ResolvedEvent {..} =
  T.pack $
    concat
      [ maybe "" go resolvedEventStart <> "\n",
        maybe "" go resolvedEventEnd <> "\n"
      ]
  where
    go = \case
      TimestampDay d -> formatTime defaultTimeLocale dayFormat d
      TimestampLocalTime lt -> formatTime defaultTimeLocale localTimeFormat lt
      TimestampUTCTime lt -> formatTime defaultTimeLocale utcTimeFormat lt

dayFormat :: String
dayFormat = "Day %F"

localTimeFormat :: String
localTimeFormat = "Local %F %T"

utcTimeFormat :: String
utcTimeFormat = "UTC %F %T"
