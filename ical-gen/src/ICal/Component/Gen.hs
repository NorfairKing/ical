{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Component.Gen where

import Control.Arrow (left)
import Control.Exception
import qualified Data.ByteString as SB
import qualified Data.DList as DList
import Data.GenValidity
import Data.GenValidity.CaseInsensitive ()
import Data.GenValidity.Containers
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Void
import ICal
import ICal.Conformance
import ICal.Conformance.TestUtils
import ICal.ContentLine.Gen ()
import ICal.Property.Gen ()
import ICal.PropertyType.Duration.Gen ()
import ICal.PropertyType.Gen
import ICal.PropertyType.RecurrenceRule.Gen ()
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

instance GenValid Component where
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = sized $ \s -> do
    (a, b, c) <- genSplit3 s
    componentProperties <- resize (a + b) genValid
    componentSubcomponents <- resize c genValid
    pure Component {..}

instance GenValid Calendar where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

fixUntilCount :: DateTimeStart -> RecurrenceRule -> Gen RecurrenceRule
fixUntilCount dateTimeStart rrule =
  case recurrenceRuleUntilCount rrule of
    Just (Left u) ->
      let updateUntil u' = rrule {recurrenceRuleUntilCount = Just $ Left u'}
       in case dateTimeStart of
            DateTimeStartDate _ -> case u of
              UntilDate _ -> pure rrule
              _ -> do
                d <- genValid
                pure $ updateUntil $ UntilDate d
            DateTimeStartDateTime dt -> case (dt, u) of
              (DateTimeFloating _, UntilDate _) -> do
                u' <- oneof [UntilDateTimeFloating <$> genImpreciseLocalTime, UntilDateTimeUTC <$> genImpreciseUTCTime]
                pure $ updateUntil u'
              (DateTimeUTC _, UntilDate _) -> do
                ut <- genImpreciseUTCTime
                pure $ updateUntil $ UntilDateTimeUTC ut
              (DateTimeZoned _ _, UntilDate _) -> do
                ut <- genImpreciseUTCTime
                pure $ updateUntil $ UntilDateTimeUTC ut
              (DateTimeUTC _, UntilDateTimeFloating _) -> do
                ut <- genImpreciseUTCTime
                pure $ updateUntil $ UntilDateTimeUTC ut
              (DateTimeZoned _ _, UntilDateTimeFloating _) -> do
                ut <- genImpreciseUTCTime
                pure $ updateUntil $ UntilDateTimeUTC ut
              _ -> pure rrule
    _ -> pure rrule

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
    eventOrganizer <- genValid
    eventStatus <- genValid
    eventSummary <- genValid
    eventTransparency <- genValid
    eventURL <- genValid
    eventRecurrenceID <- case eventDateTimeStart of
      Nothing -> genValid
      Just dtstart ->
        oneof
          [ pure Nothing,
            Just <$> case dtstart of
              DateTimeStartDate _ -> RecurrenceIDDate <$> genValid
              DateTimeStartDateTime _ -> RecurrenceIDDateTime <$> genValid
          ]
    eventRecurrenceRules <- case eventDateTimeStart of
      Nothing -> pure S.empty
      Just dtstart -> genSetOf $ genValid >>= fixUntilCount dtstart

    eventDateTimeEndDuration <- genValid
    eventAttendees <- genValid
    eventExceptionDateTimes <- genValid
    eventRecurrenceDateTimes <- genValid
    pure Event {..}

instance GenValid Observance where
  genValid = do
    observanceDateTimeStart <- genImpreciseLocalTime
    observanceTimeZoneOffsetTo <- genValid
    observanceTimeZoneOffsetFrom <- genValid
    observanceRecurrenceRules <- genSetOf $ genValid >>= fixUntilCount (DateTimeStartDateTime (DateTimeFloating observanceDateTimeStart))

    observanceComment <- genValid
    observanceRecurrenceDateTimes <- genValid
    observanceTimeZoneName <- genValid

    pure Observance {..}

instance GenValid TimeZoneObservance where
  shrinkValid = \case
    StandardObservance s -> StandardObservance <$> shrinkValid s
    DaylightObservance d@(Daylight o) ->
      StandardObservance (Standard o)
        : (DaylightObservance <$> shrinkValid d)

instance GenValid Standard where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Daylight where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid TimeZone where
  genValid = do
    timeZoneId <- genValid
    timeZoneObservances <- S.fromList . NE.toList <$> genValid
    pure TimeZone {..}

componentScenarioDir ::
  forall a.
  (Show a, Eq a, Validity a, IsComponent a) =>
  FilePath ->
  Spec
componentScenarioDir dir = scenarioDir dir $ \tzFile ->
  it "can parse this file as a component strictly and roundtrip it" $ do
    let parseBS bs = runConformStrict $ do
          textContents <- conformFromEither $ left TextDecodingError $ TE.decodeUtf8' bs
          unfoldedLines <- conformMapAll UnfoldingError absurd absurd $ parseUnfoldedLines textContents
          contentLines <- conformMapAll ContentLineParseError absurd absurd $ conformFromEither $ mapM parseContentLineFromUnfoldedLine unfoldedLines
          conformMapAll CalendarParseError CalendarParseFixableError CalendarParseWarning $ parseComponentFromContentLines contentLines

        renderBS =
          TE.encodeUtf8
            . renderUnfoldedLines
            . map renderContentLineToUnfoldedLine
            . DList.toList
            . uncurry renderGeneralComponent
            . namedComponentB

    contents <- SB.readFile tzFile
    case parseBS contents of
      Left err -> expectationFailure $ renderError err
      Right result -> do
        shouldBeValid (result :: a)
        let rendered = renderBS result
        case parseBS rendered of
          Left err -> expectationFailure $ renderError err
          Right result' -> result' `shouldBe` result

renderError :: Either ICalParseError (Notes ICalParseFixableError ICalParseWarning) -> String
renderError = \case
  Left iCalParseError -> displayException iCalParseError
  Right (Notes fes wes) ->
    unlines $
      concat
        [ ["Fixable errors:"],
          map show fes,
          ["Warnings:"],
          map show wes
        ]

componentSpec ::
  forall a.
  (Show a, Eq a, GenValid a, IsComponent a) =>
  Spec
componentSpec = do
  it "renders to a valid Component" $
    forAllValid $ \component ->
      shouldBeValid $ componentB (component :: a)

  it "parses only valid things" $
    forAllValid $ \component ->
      case runConform (componentP (componentB (component :: a))) of
        Left _ -> pure ()
        Right (a, _) -> shouldBeValid (a :: a)

  it "roundtrips through ContentLines" $
    forAllValid $ \a ->
      let (name, component) = namedComponentB (a :: a)
          renderedText :: Text
          renderedText =
            renderUnfoldedLines $
              map renderContentLineToUnfoldedLine $
                DList.toList $
                  renderGeneralComponent name component
          ctx =
            unlines
              [ "Internal representation:",
                ppShow component,
                "",
                "Textual representation:",
                T.unpack renderedText
              ]
       in context ctx $ do
            parsed <- shouldConform $ namedComponentP name component
            parsed `shouldBe` a

  it "roundtrips through Text" $
    forAllValid $ \a -> do
      let renderedText = renderComponentText (a :: a)
      parsed <- shouldConform $ parseComponentFromText renderedText
      parsed `shouldBe` a
