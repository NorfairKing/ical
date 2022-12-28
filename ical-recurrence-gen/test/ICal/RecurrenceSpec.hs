{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ICal.RecurrenceSpec (spec) where

import Control.Monad
import qualified Data.ByteString as SB
import qualified Data.Map as M
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

spec :: Spec
spec = do
  scenarioDir "test_resources/event" $ \fp -> do
    eventFile <- liftIO $ parseRelFile fp
    when (fileExtension eventFile == Just ".ics") $
      it "recurs this file correctly" $ do
        contents <- TE.decodeUtf8 <$> SB.readFile (fromRelFile eventFile)
        event <- shouldConform $ parseComponentFromText contents
        goldenFile <- replaceExtension ".occ" eventFile
        pure $ pureGoldenEventRecurrenceFile goldenFile (fromGregorian 2023 01 01) event
  scenarioDir "test_resources/calendar" $ \fp -> do
    eventFile <- liftIO $ parseRelFile fp
    when (fileExtension eventFile == Just ".ics") $
      it "recurs this file correctly" $ do
        contents <- TE.decodeUtf8 <$> SB.readFile (fromRelFile eventFile)
        cal <- shouldConform $ parseVCalendar contents
        goldenFile <- replaceExtension ".occ" eventFile
        pure $ pureGoldenCalendarRecurrenceFile goldenFile (fromGregorian 2022 12 27) cal

pureGoldenCalendarRecurrenceFile :: Path Rel File -> Day -> Calendar -> GoldenTest (Set EventOccurrence)
pureGoldenCalendarRecurrenceFile goldenFile day calendar =
  goldenEventOccurrenceFile goldenFile $
    shouldConform $ do
      runR (calendarTimeZoneMap calendar) $
        fmap S.unions $
          mapM
            (recurEvents day . getRecurringEvent)
            (calendarEvents calendar)

pureGoldenEventRecurrenceFile :: Path Rel File -> Day -> Event -> GoldenTest (Set EventOccurrence)
pureGoldenEventRecurrenceFile goldenFile day event =
  goldenEventOccurrenceFile goldenFile $ shouldConform $ runR M.empty (recurEvents day (getRecurringEvent event))

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
