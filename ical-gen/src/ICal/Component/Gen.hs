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
import Data.GenValidity.Containers
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import ICal.Component
import ICal.ContentLine
import ICal.ContentLine.Gen ()
import ICal.Property
import ICal.Property.Gen ()
import ICal.PropertyType.Duration.Gen ()
import ICal.PropertyType.Gen
import ICal.PropertyType.RecurrenceRule
import ICal.PropertyType.RecurrenceRule.Gen ()
import ICal.UnfoldedLine
import Test.Syd
import Test.Syd.Validity
import Text.Megaparsec

instance GenValid Calendar where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Event where
  genValid = do
    eventDateTimeStamp <- genValid
    eventUID <- genValid
    eventDateTimeStart <- genValid
    eventClassification <- genValid
    eventCreated <- genValid
    eventDescription <- genValid
    eventGeographicPosition <- genValid
    eventLastModified <- genValid
    eventLocation <- genValid
    eventStatus <- genValid
    eventSummary <- genValid
    eventTransparency <- genValid
    eventURL <- genValid
    eventRecurrenceRules <- case eventDateTimeStart of
      Nothing -> pure S.empty
      Just dtstart -> genSetOf $ do
        rrule <- genValid
        case recurrenceRuleUntilCount rrule of
          Just (Left u) -> case (u, dtstart) of
            (UntilDate _, DateTimeStartDate _) -> pure rrule
            (UntilDateTime _, DateTimeStartDateTime _) -> pure rrule
            (_, DateTimeStartDateTime _) -> do
              ut <- genValid
              pure (rrule {recurrenceRuleUntilCount = Just $ Left $ UntilDateTime ut})
            (_, DateTimeStartDate _) -> do
              d <- genValid
              pure (rrule {recurrenceRuleUntilCount = Just $ Left $ UntilDate d})
          _ -> pure rrule

    eventDateTimeEndDuration <- genValid
    pure Event {..}

  shrinkValid (Event mp u rt cn cd d gp lm l ss sy t mu rrs med) = filter isValid $ do
    (mp', (((u', rt'), ((cn', cd'), (d', gp'))), (((lm', l'), (ss', sy')), ((t', mu'), (rrs', med'))))) <-
      shrinkTuple
        shrinkValid
        ( shrinkTuple
            ( shrinkTuple
                ( shrinkTuple
                    shrinkValid
                    shrinkValid
                )
                ( shrinkTuple
                    ( shrinkTuple
                        shrinkValid
                        shrinkValid
                    )
                    ( shrinkTuple
                        shrinkValid
                        shrinkValid
                    )
                )
            )
            ( shrinkTuple
                ( shrinkTuple
                    ( shrinkTuple
                        shrinkValid
                        shrinkValid
                    )
                    ( shrinkTuple
                        shrinkValid
                        shrinkValid
                    )
                )
                ( shrinkTuple
                    ( shrinkTuple
                        shrinkValid
                        shrinkValid
                    )
                    ( shrinkTuple
                        shrinkValid
                        shrinkValid
                    )
                )
            )
        )
        (mp, (((u, rt), ((cn, cd), (d, gp))), (((lm, l), (ss, sy)), ((t, mu), (rrs, med)))))
    pure (Event mp' u' rt' cn' cd' d' gp' lm' l' ss' sy' t' mu' rrs' med')

instance GenValid Observance where
  genValid = do
    observanceDateTimeStart <- genImpreciseLocalTime
    observanceTimeZoneOffsetTo <- genValid
    observanceTimeZoneOffsetFrom <- genValid
    observanceRecurrenceRule <- genSetOf $ do
      rrule <- genValid
      case recurrenceRuleUntilCount rrule of
        Just (Left u) -> case u of
          UntilDateTime _ -> pure rrule
          _ -> do
            ut <- genValid
            pure (rrule {recurrenceRuleUntilCount = Just $ Left $ UntilDateTime ut})
        _ -> pure rrule

    observanceComment <- genValid
    observanceTimeZoneName <- genValid

    pure Observance {..}

  shrinkValid (Observance start to from rules comments name) = filter isValid $ do
    ((start', to'), ((from', rules'), (comments', name'))) <-
      shrinkTuple
        (shrinkTuple shrinkImpreciseLocalTime shrinkValid)
        ( shrinkTuple
            ( shrinkTuple
                shrinkValid
                shrinkValid
            )
            ( shrinkTuple
                shrinkValid
                shrinkValid
            )
        )
        ((start, to), ((from, rules), (comments, name)))
    pure (Observance start' to' from' rules' comments' name')

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
          unfoldedLines <- left show $ parseUnfoldedLines textContents
          contentLines <- mapM parseContentLineFromUnfoldedLine unfoldedLines
          parseComponentFromContentLines contentLines

        renderBS =
          TE.encodeUtf8
            . renderUnfoldedLines
            . map renderContentLineToUnfoldedLine
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
