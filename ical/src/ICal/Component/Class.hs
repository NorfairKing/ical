{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Component.Class where

import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Data.DList (DList (..))
import qualified Data.DList as DList
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import Data.Void
import ICal.Conformance
import ICal.ContentLine
import ICal.Property
import ICal.PropertyType
import ICal.UnfoldedLine
import Text.Megaparsec

data CalendarParseError
  = SubcomponentError !(ParseErrorBundle [ContentLine] CalendarParseError)
  | PropertyParseError !PropertyParseError
  | OtherError String
  deriving (Show, Eq, Ord)

instance Exception CalendarParseError where
  displayException = \case
    SubcomponentError pe -> errorBundlePretty pe
    PropertyParseError ppe -> displayException ppe
    OtherError s -> s

instance ShowErrorComponent CalendarParseError where
  showErrorComponent = displayException

data CalendarParseFixableError
  = UntilTypeGuess !DateTimeStart !Until !Until -- Old until new until
  deriving (Show, Eq, Ord)

instance Exception CalendarParseFixableError where
  displayException = \case
    UntilTypeGuess dateTimeStart until1 until2 -> unwords ["UntilTypeGuess", show dateTimeStart, show until1, show until2]

parseComponentFromContentLines ::
  (Validity component, IsComponent component) =>
  [ContentLine] ->
  Conform (ParseErrorBundle [ContentLine] CalendarParseError) CalendarParseFixableError Void component
parseComponentFromContentLines = runCP componentSectionP

runCP ::
  CP a ->
  [ContentLine] ->
  Conform (ParseErrorBundle [ContentLine] CalendarParseError) CalendarParseFixableError Void a
runCP func cls = do
  errOrComponent <- runParserT func "" cls
  case errOrComponent of
    Left err -> unfixableError err
    Right component -> pure component

liftCP :: CP a -> [ContentLine] -> CP a
liftCP parserFunc cls = lift $ runCP parserFunc cls

liftConformToCP :: Conform CalendarParseError CalendarParseFixableError Void a -> CP a
liftConformToCP func = do
  decider <- lift ask
  case runConformFlexible decider func of
    Left hr -> case hr of
      HaltedBecauseOfUnfixableError ue -> customFailure ue
      HaltedBecauseOfStrictness fe -> throwError (HaltedBecauseOfStrictness fe)
    Right (a, notes) -> lift $ do
      tell notes
      pure a

type CP a =
  ParsecT
    CalendarParseError
    [ContentLine]
    (Conform (ParseErrorBundle [ContentLine] CalendarParseError) CalendarParseFixableError Void)
    a

instance VisualStream [ContentLine] where
  showTokens :: Proxy [ContentLine] -> NonEmpty ContentLine -> String
  showTokens Proxy =
    T.unpack
      . renderUnfoldedLines
      . map renderContentLineToUnfoldedLine
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
          (cl : _) -> (Just $ T.unpack $ renderUnfoldedLines [renderContentLineToUnfoldedLine cl], newState)

-- |
--
-- === Laws
--
-- * The '[ContentLine]' that is built is valid:
--
-- >>> forAllValid $ \component -> isValid (componentB component)
--
-- * Anything parsed is valid:
--
-- >>> forAllValid $ \contentLines -> isValid (parse componentP "" contentLines)
--
-- * The property roundtrips through '[ContentLine]'.
--
-- >>> forAllValid $ \component -> parse componentP "" (DList.toList (componentB component)) == Right component
class IsComponent component where
  -- | Name for this component
  componentName :: Proxy component -> Text

  -- | Parser for this component
  componentP :: CP component

  -- | Builder for this component
  componentB :: component -> DList ContentLine

componentSectionP :: forall component. (Validity component, IsComponent component) => CP component
componentSectionP = do
  c <- sectionP (componentName (Proxy :: Proxy component)) componentP
  case prettyValidate c of
    Left err -> fail err
    Right c' -> pure c'

sectionP :: Text -> CP a -> CP a
sectionP name parser = do
  parseGivenProperty $ Begin name
  result <- parser
  parseGivenProperty $ End name
  pure result

parseGivenProperty :: IsProperty property => property -> CP ()
parseGivenProperty givenProperty = void $ single $ propertyContentLineB givenProperty

parseProperty :: IsProperty property => CP property
parseProperty = do
  contentLine <- anySingle
  liftConformToCP $ conformMapErrors PropertyParseError absurd $ propertyContentLineP contentLine

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

propertyDListB :: (Eq property, IsProperty property) => property -> property -> DList ContentLine
propertyDListB defaultValue value =
  if value == defaultValue
    then mempty
    else propertyListB value

propertySetB :: IsProperty property => Set property -> DList ContentLine
propertySetB = DList.fromList . map propertyContentLineB . S.toList

parseFirst :: forall a. IsProperty a => [ContentLine] -> CP a
parseFirst = go
  where
    name = propertyName (Proxy :: Proxy a)
    go :: [ContentLine] -> CP a
    go = \case
      [] -> fail $ "Did not find required " <> show name
      (contentLine : cls) ->
        if contentLineName contentLine == name
          then liftConformToCP $ conformMapErrors PropertyParseError absurd $ propertyContentLineP contentLine
          else go cls

parseFirstMaybe :: forall a. IsProperty a => [ContentLine] -> CP (Maybe a)
parseFirstMaybe = go
  where
    name = propertyName (Proxy :: Proxy a)
    go :: [ContentLine] -> CP (Maybe a)
    go = \case
      [] -> pure Nothing
      -- TODO do better than a linear search?
      (contentLine : cls) ->
        if contentLineName contentLine == name
          then fmap Just $ liftConformToCP $ conformMapErrors PropertyParseError absurd $ propertyContentLineP contentLine
          else go cls

parseList ::
  forall a.
  IsProperty a =>
  [ContentLine] ->
  CP [a]
parseList cls =
  mapM (liftConformToCP . conformMapErrors PropertyParseError absurd . propertyContentLineP) $
    filter ((== name) . contentLineName) cls
  where
    name = propertyName (Proxy :: Proxy a)

parseSet ::
  forall a.
  (Ord a, IsProperty a) =>
  [ContentLine] ->
  CP (Set a)
parseSet = fmap S.fromList . parseList

parseSubcomponent :: forall a. (IsComponent a) => [ContentLine] -> CP a
parseSubcomponent = go1
  where
    go1 :: [ContentLine] -> CP a
    go1 = \case
      [] -> fail "No subcomponent found."
      (cl : rest) ->
        let isBegin ContentLine {..} = contentLineName == "BEGIN" && (contentLineValueRaw contentLineValue == name)
            isEnd ContentLine {..} = contentLineName == "END" && (contentLineValueRaw contentLineValue == name)
         in if isBegin cl
              then go2 $ takeWhile (not . isEnd) rest
              else go1 rest
    go2 :: [ContentLine] -> CP a
    go2 = liftCP componentP

    name = componentName (Proxy :: Proxy a)

parseManySubcomponents :: forall a. (IsComponent a) => [ContentLine] -> CP [a]
parseManySubcomponents = go1
  where
    go1 :: [ContentLine] -> CP [a]
    go1 = \case
      [] -> pure []
      (cl : rest) ->
        let isBegin ContentLine {..} = contentLineName == "BEGIN" && (contentLineValueRaw contentLineValue == name)
            isEnd ContentLine {..} = contentLineName == "END" && (contentLineValueRaw contentLineValue == name)
         in if isBegin cl
              then
                let (subComponentLines, restAfterSubcomponent) = break isEnd rest
                 in (:) <$> go2 subComponentLines <*> go1 restAfterSubcomponent
              else go1 rest
    go2 :: [ContentLine] -> CP a
    go2 = liftCP componentP

    name = componentName (Proxy :: Proxy a)

parseManySubcomponents2 :: forall a b. (IsComponent a, IsComponent b) => [ContentLine] -> CP [Either a b]
parseManySubcomponents2 = go1
  where
    go1 :: [ContentLine] -> CP [Either a b]
    go1 = \case
      [] -> pure []
      (cl : rest) ->
        let isBegin ContentLine {..} =
              contentLineName == "BEGIN"
                && ( contentLineValueRaw contentLineValue == nameA
                       || contentLineValueRaw contentLineValue == nameB
                   )
            isEnd :: forall c. IsComponent c => ContentLine -> Bool
            isEnd ContentLine {..} = contentLineName == "END" && (contentLineValueRaw contentLineValue == componentName (Proxy :: Proxy c))
         in if isBegin cl
              then
                if contentLineValueRaw (contentLineValue cl) == nameA
                  then
                    let (subComponentLines, restAfterSubcomponent) = break (isEnd @a) rest
                     in (:) <$> (Left <$> go2 subComponentLines) <*> go1 restAfterSubcomponent
                  else
                    let (subComponentLines, restAfterSubcomponent) = break (isEnd @b) rest
                     in (:) <$> (Right <$> go2 subComponentLines) <*> go1 restAfterSubcomponent
              else go1 rest
    go2 :: forall c. IsComponent c => [ContentLine] -> CP c
    go2 = liftCP componentP

    nameA = componentName (Proxy :: Proxy a)
    nameB = componentName (Proxy :: Proxy b)

parseSomeSubcomponents :: forall a. (IsComponent a) => [ContentLine] -> CP (NonEmpty a)
parseSomeSubcomponents cls = do
  cs <- parseManySubcomponents cls
  case NE.nonEmpty cs of
    Nothing -> fail "expected at least one subcompent"
    Just ne -> pure ne

validateMDateTimeStartRRule :: Maybe DateTimeStart -> Set RecurrenceRule -> Validation
validateMDateTimeStartRRule mDateTimeStart recurrenceRules =
  case mDateTimeStart of
    Nothing ->
      -- [section 3.8.2.4. Date-Time Start](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.2.4)
      --
      -- @
      -- This property is
      -- REQUIRED in all types of recurring calendar components that
      -- specify the "RRULE" property.
      -- @
      declare "If there is no DTSTART, then there are no recurrence rules" $
        S.null recurrenceRules
    Just dateTimeStart -> validateDateTimeStartRRule dateTimeStart recurrenceRules

validateDateTimeStartRRule :: DateTimeStart -> Set RecurrenceRule -> Validation
validateDateTimeStartRRule dateTimeStart recurrenceRules =
  decorateList (S.toList recurrenceRules) $ \recurrenceRule ->
    case recurrenceRuleUntilCount recurrenceRule of
      Just (Left u) ->
        let msg =
              unlines
                [ "The value type of the UNTIL rule part has the same value type as the DTSTART property.",
                  show dateTimeStart,
                  show u
                ]
         in declare msg $
              -- [section 3.3.10.  Recurrence Rule](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.10)
              -- @
              -- The value of the UNTIL rule part MUST have the same
              -- value type as the "DTSTART" property.
              -- @
              case dateTimeStart of
                DateTimeStartDate _ -> case u of
                  UntilDate _ -> True
                  _ -> False
                DateTimeStartDateTime dateTime -> case (dateTime, u) of
                  -- [section 3.3.10.  Recurrence Rule](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.10)
                  -- @
                  -- "DTSTART" property is specified as a date with local time, then
                  -- the UNTIL rule part MUST also be specified as a date with local
                  -- time.
                  -- @
                  (_, UntilDate _) -> False
                  -- [section 3.3.10.  Recurrence Rule](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.10)
                  -- @
                  -- If the "DTSTART" property is specified as a date with UTC
                  -- time or a date with local time and time zone reference, then the
                  -- UNTIL rule part MUST be specified as a date with UTC time.
                  -- @
                  (DateTimeFloating _, UntilDateTimeFloating _) -> True
                  (DateTimeUTC _, UntilDateTimeFloating _) -> False
                  (DateTimeZoned _ _, UntilDateTimeFloating _) -> False
                  (DateTimeFloating _, UntilDateTimeUTC _) -> True
                  (DateTimeUTC _, UntilDateTimeUTC _) -> True
                  (DateTimeZoned _ _, UntilDateTimeUTC _) -> True
      _ -> mempty

-- It turns out that certain ical providers such as Google Calendar may output invalid
-- ICal that we still have to be able to deal with somehow.
-- For example, on 2022-06-26, google outputted an event with these properties:
--
-- @
-- BEGIN:VEVENT
-- UID:18jktp1kl13aov1ku35sf8i40b_R20220705T150000@google.com
-- DTSTART;TZID=Europe/Kiev:20220705T180000
-- DTEND;TZID=Europe/Kiev:20220705T183000
-- RRULE:FREQ=WEEKLY;WKST=SU;UNTIL=20220717;BYDAY=TU
-- DTSTAMP:20220726T130525Z
-- @
--
-- However, the spec says:
--
-- @
-- The value of the UNTIL rule part MUST have the same
-- value type as the "DTSTART" property.
-- @
--
-- This means that the UNTIL part must be specified as date WITH TIME.
-- The spec also says:
--
-- @
-- If the value
-- specified by UNTIL is synchronized with the specified recurrence,
-- this DATE or DATE-TIME becomes the last instance of the
-- recurrence.
-- @
--
-- So when the UNTIL has a date without time, we will guess the time that is
-- specified in DTSTART.
fixUntil :: Maybe DateTimeStart -> RecurrenceRule -> CP RecurrenceRule
fixUntil mDateTimeStart rrule =
  case mDateTimeStart of
    Nothing -> pure rrule
    Just dateTimeStart ->
      case recurrenceRuleUntilCount rrule of
        Just (Left u) -> case (dateTimeStart, u) of
          (DateTimeStartDateTime dt, UntilDate (Date ud)) -> do
            let newUntil = case dt of
                  -- This guess is somewhat sensible.
                  DateTimeUTC (Time.UTCTime _ sdt) ->
                    UntilDateTimeUTC (Time.UTCTime ud sdt)
                  -- This guess is fine as well.
                  DateTimeFloating (Time.LocalTime _ tod) ->
                    UntilDateTimeFloating (Time.LocalTime ud tod)
                  -- This guess is bad
                  DateTimeZoned _ (Time.LocalTime _ tod) ->
                    UntilDateTimeUTC (Time.UTCTime ud (Time.timeOfDayToTime tod))
            lift $ emitFixableError $ UntilTypeGuess dateTimeStart u newUntil
            pure $ rrule {recurrenceRuleUntilCount = Just $ Left newUntil}
          _ -> pure rrule
        _ -> pure rrule
