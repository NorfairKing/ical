{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Component.Gen where

import Control.Arrow (left)
import qualified Data.ByteString as SB
import qualified Data.DList as DList
import Data.GenValidity
import Data.GenValidity.CaseInsensitive ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import ICal.Component
import ICal.ContentLine
import ICal.ContentLine.Gen ()
import ICal.Property.Gen ()
import ICal.PropertyType.Duration.Gen ()
import ICal.PropertyType.Gen
import ICal.PropertyType.RecurrenceRule.Gen ()
import ICal.UnfoldedLine
import Test.Syd
import Test.Syd.Validity
import Text.Megaparsec

instance GenValid Calendar where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Event where
  genValid = genValidStructurallyWithoutExtraChecking

  -- Shrink piecewise
  shrinkValid Event {..} =
    Event
      <$> shrinkValid eventDateTimeStamp
      <*> shrinkValid eventUID
      <*> shrinkValid eventDateTimeStart
      <*> shrinkValid eventClassification
      <*> shrinkValid eventCreated
      <*> shrinkValid eventDescription
      <*> shrinkValid eventGeographicPosition
      <*> shrinkValid eventLastModified
      <*> shrinkValid eventLocation
      <*> shrinkValid eventStatus
      <*> shrinkValid eventSummary
      <*> shrinkValid eventTransparency
      <*> shrinkValid eventURL
      <*> shrinkValid eventRecurrenceRules
      <*> shrinkValid eventDateTimeEndDuration

instance GenValid Observance where
  genValid =
    Observance
      <$> genImpreciseLocalTime
      <*> genValid
      <*> genValid
      <*> genValid
      <*> genValid
      <*> genValid

  -- Not shrinking piecewise but that's ok here for now.
  shrinkValid Observance {..} =
    Observance
      <$> shrinkImpreciseLocalTime observanceDateTimeStart
      <*> shrinkValid observanceTimeZoneOffsetTo
      <*> shrinkValid observanceTimeZoneOffsetFrom
      <*> shrinkValid observanceRecurrenceRule
      <*> shrinkValid observanceComment
      <*> shrinkValid observanceTimeZoneName

instance GenValid TimeZoneObservance where
  shrinkValid = \case
    StandardObservance s -> StandardObservance <$> shrinkValid s
    DaylightObservance d@(Daylight o) ->
      StandardObservance (Standard o) :
      (DaylightObservance <$> shrinkValid d)

instance GenValid Standard where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Daylight where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid TimeZone where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

componentScenarioDir ::
  forall a.
  (Show a, Eq a, Validity a, IsComponent a) =>
  FilePath ->
  Spec
componentScenarioDir dir = scenarioDir dir $ \tzFile ->
  it "can parse this file as a timezone and roundtrip it" $ do
    let parseBS bs = do
          textContents <- left show $ TE.decodeUtf8' bs
          unfoldedLines <- parseUnfoldedLines textContents
          contentLines <- mapM parseContentLineFromUnfoldedLine unfoldedLines
          parseComponentFromContentLines contentLines

        renderBS =
          TE.encodeUtf8
            . renderContentLines
            . DList.toList
            . componentSectionB

    contents <- SB.readFile tzFile
    case parseBS contents of
      Left err -> expectationFailure err
      Right result -> do
        shouldBeValid (result :: a)
        let rendered = renderBS result
        case parseBS rendered of
          Left err -> expectationFailure err
          Right result' -> result' `shouldBe` result

componentSpec ::
  forall a.
  (Show a, Eq a, GenValid a, IsComponent a) =>
  Spec
componentSpec = do
  it "renders to a valid list of ContentLines" $
    forAllValid $ \component ->
      shouldBeValid $ DList.toList $ componentSectionB (component :: a)

  it "parses only valid things" $
    forAllValid $ \component ->
      case parse componentSectionP "test input" (DList.toList (componentSectionB (component :: a))) of
        Left _ -> pure ()
        Right a -> shouldBeValid (a :: a)

  it "roundtrips through ContentLines" $
    forAllValid $ \a ->
      let rendered = DList.toList $ componentSectionB (a :: a)
          renderedText = renderUnfoldedLines $ map renderContentLineToUnfoldedLine rendered
          ctx =
            unlines
              [ -- "Internal representation:",
                -- ppShow rendered,
                -- "",
                "Textual representation:",
                T.unpack renderedText
              ]
       in context ctx $
            case parse componentSectionP "test input" rendered of
              Left err -> expectationFailure $ errorBundlePretty err
              Right parsed -> parsed `shouldBe` a
