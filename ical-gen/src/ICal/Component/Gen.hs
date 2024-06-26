{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Component.Gen where

import Conformance
import Conformance.TestUtils
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
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
  genValid = sized $ \s -> do
    calendarProductIdentifier <- genValid
    calendarVersion <- genValid
    calendarCalendarScale <- genValid
    calendarMethod <- genValid
    calendarUID <- genValid
    calendarLastModified <- genValid
    calendarURL <- genValid
    calendarDescriptions <- genValid
    calendarImages <- genValid
    (a, b, c, d, e) <- genSplit5 s
    calendarEvents <- resize a genValid
    calendarTodos <- resize b genValid
    calendarJournals <- resize c genValid
    calendarFreeBusys <- resize d genValid
    calendarTimeZones <- resize e genValid
    pure Calendar {..}

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
    eventPriority <- genValid
    eventSequenceNumber <- genValid
    eventStatus <- genValid
    eventSummary <- genValid
    eventTransparency <- genValid
    eventURL <- genValid
    eventRecurrenceIdentifier <- case eventDateTimeStart of
      Nothing -> genValid
      Just dtstart ->
        oneof
          [ pure Nothing,
            Just <$> case dtstart of
              DateTimeStartDate _ -> RecurrenceIdentifierDate <$> genValid <*> genValid
              DateTimeStartDateTime _ -> RecurrenceIdentifierDateTime <$> genValid <*> genValid
          ]
    eventRecurrenceRules <- case eventDateTimeStart of
      Nothing -> pure S.empty
      Just dtstart -> genSetOf $ genValid >>= fixUntilCount dtstart

    eventDateTimeEndDuration <- genValid
    eventAttachments <- genValid
    eventAttendees <- genValid
    eventCategories <- genValid
    eventComments <- genValid
    eventContacts <- genValid
    eventExceptionDateTimes <- genValid
    eventRequestStatuses <- genValid
    eventRelatedTos <- genValid
    eventResources <- genValid
    eventRecurrenceDateTimes <- genValid
    eventImages <- genValid
    eventAlarms <- genValid
    pure Event {..}

instance GenValid Todo where
  genValid = do
    todoDateTimeStamp <- genValid
    todoUID <- genValid

    todoClassification <- genValid
    todoCompleted <- genValid
    todoCreated <- genValid
    todoDescription <- genValid
    todoDateTimeStart <- genValid
    todoGeographicPosition <- genValid
    todoLastModified <- genValid
    todoLocation <- genValid
    todoOrganizer <- genValid
    todoPercentComplete <- genValid
    todoPriority <- genValid
    todoRecurrenceIdentifier <- genValid
    todoSequenceNumber <- genValid
    todoStatus <- genValid
    todoSummary <- genValid
    todoURL <- genValid

    todoRecurrenceRules <- case todoDateTimeStart of
      Nothing -> pure S.empty
      Just dtstart -> genSetOf $ genValid >>= fixUntilCount dtstart

    todoDateTimeDueDuration <- genValid

    todoAttachments <- genValid
    todoAttendees <- genValid
    todoCategories <- genValid
    todoComments <- genValid
    todoContacts <- genValid
    todoExceptionDateTimes <- genValid
    todoRequestStatusses <- genValid
    todoRelatedTos <- genValid
    todoResources <- genValid
    todoRecurrenceDateTimes <- genValid

    pure Todo {..}

instance GenValid Journal where
  genValid = do
    journalDateTimeStamp <- genValid
    journalUID <- genValid

    journalClassification <- genValid
    journalCreated <- genValid
    journalDateTimeStart <- genValid
    journalLastModified <- genValid
    journalOrganizer <- genValid
    journalRecurrenceIdentifier <- genValid
    journalSequenceNumber <- genValid
    journalStatus <- genValid
    journalSummary <- genValid
    journalURL <- genValid

    journalRecurrenceRules <- case journalDateTimeStart of
      Nothing -> pure S.empty
      Just dtstart -> genSetOf $ genValid >>= fixUntilCount dtstart

    journalAttachments <- genValid
    journalAttendees <- genValid
    journalCategories <- genValid
    journalComments <- genValid
    journalContacts <- genValid
    journalDescriptions <- genValid
    journalExceptionDateTimes <- genValid
    journalRelatedTos <- genValid
    journalRecurrenceDateTimes <- genValid
    journalRequestStatusses <- genValid

    pure Journal {..}

instance GenValid FreeBusy where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Alarm where
  genValid = do
    alarmAction <- genValid
    alarmTrigger <- genValid
    alarmRepeatDuration <- genValid
    alarmAttachments <- genValid
    case alarmAction of
      ActionAudio -> do
        alarmDescription <- genValid
        alarmSummary <- genValid
        alarmAttendees <- genValid
        pure Alarm {..}
      ActionDisplay -> do
        alarmDescription <- Just <$> genValid
        alarmSummary <- genValid
        alarmAttendees <- genValid
        pure Alarm {..}
      ActionEmail -> do
        alarmDescription <- Just <$> genValid
        alarmSummary <- Just <$> genValid
        alarmAttendees <- S.insert <$> genValid <*> genValid
        pure Alarm {..}
      ActionOther _ -> do
        alarmDescription <- genValid
        alarmSummary <- genValid
        alarmAttendees <- genValid
        pure Alarm {..}

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
    timeZoneLastModified <- genValid
    timeZoneURL <- genValid
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
          unfoldedLines <- conformMapAll UnfoldingError UnfoldingFixableError absurd $ parseUnfoldedLines textContents
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

  it "roundtrips through a General Component" $
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
