{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Component.Class where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Data.DList (DList (..))
import qualified Data.DList as DList
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
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
import GHC.Generics (Generic)
import ICal.Conformance
import ICal.ContentLine
import ICal.Property
import ICal.PropertyType
import ICal.UnfoldedLine
import Text.Megaparsec

data Component = Component
  { -- TODO rename
    componentName' :: !Text,
    -- TODO consider if
    -- 1. order of the values matters
    -- 2. duplicates matter
    --
    -- If both don't matter then use a set.
    componentProperties :: !(Map ContentLineName (NonEmpty ContentLineValue)),
    componentSubcomponents :: ![Component]
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Component

instance NFData Component

-- TODO rename
renderGeneralComponents :: [Component] -> DList ContentLine
renderGeneralComponents = foldMap renderGeneralComponent

renderGeneralComponent :: Component -> DList ContentLine
renderGeneralComponent Component {..} =
  mconcat
    [ DList.singleton $ propertyContentLineB (Begin componentName'),
      DList.fromList $
        concatMap
          ( \(name, values) ->
              map (ContentLine name) (NE.toList values)
          )
          (M.toList componentProperties),
      foldMap renderGeneralComponent componentSubcomponents,
      DList.singleton $ propertyContentLineB (End componentName')
    ]

-- TODO rename
parseGeneralComponents ::
  [ContentLine] ->
  Conform CalendarParseError CalendarParseFixableError CalendarParseWarning [Component]
parseGeneralComponents = goComponents
  where
    goComponents ::
      [ContentLine] ->
      Conform CalendarParseError CalendarParseFixableError CalendarParseWarning [Component]
    goComponents = \case
      [] -> pure []
      cls -> do
        (component, leftovers) <- parseGeneralComponentHelper cls
        restComponents <- goComponents leftovers
        pure (component : restComponents)

parseGeneralComponent ::
  [ContentLine] ->
  Conform CalendarParseError CalendarParseFixableError CalendarParseWarning Component
parseGeneralComponent = fmap fst . parseGeneralComponentHelper

-- TODO rename
parseGeneralComponentHelper ::
  [ContentLine] ->
  Conform CalendarParseError CalendarParseFixableError CalendarParseWarning (Component, [ContentLine])
parseGeneralComponentHelper = \case
  [] -> error "fail: no begin for a component"
  (firstCL : restCLs) -> do
    Begin name <-
      conformMapAll
        PropertyParseError
        absurd
        absurd
        $ propertyContentLineP firstCL
    goComponent name M.empty [] restCLs
  where
    goComponent ::
      Text ->
      Map ContentLineName (NonEmpty ContentLineValue) ->
      [Component] ->
      -- TODO use a DList
      [ContentLine] ->
      Conform CalendarParseError CalendarParseFixableError CalendarParseWarning (Component, [ContentLine])
    goComponent name properties subComponents = \case
      [] -> error "fail: only a begin, but nothing else."
      [lastCL] -> do
        End name' <-
          conformMapAll
            PropertyParseError
            absurd
            absurd
            $ propertyContentLineP
              lastCL
        if name' == name
          then
            pure
              ( Component
                  { componentName' = name,
                    componentProperties = properties,
                    componentSubcomponents = subComponents
                  },
                []
              )
          else error "fail: end had the wrong name"
      (cl : rest) ->
        case contentLineName cl of
          "END" -> do
            End name' <-
              conformMapAll
                PropertyParseError
                absurd
                absurd
                $ propertyContentLineP
                  cl
            if name' == name
              then
                pure
                  ( Component
                      { componentName' = name,
                        componentProperties = properties,
                        componentSubcomponents = subComponents
                      },
                    rest
                  )
              else error "fail: end had the wrong name"
          "BEGIN" -> do
            Begin name' <-
              conformMapAll
                PropertyParseError
                absurd
                absurd
                $ propertyContentLineP
                  cl
            (subcomponent, leftovers) <- goComponent name' M.empty [] rest
            goComponent name properties (subComponents <> [subcomponent]) leftovers
          _ ->
            goComponent
              name
              (M.insertWith (flip (<>)) (contentLineName cl) (contentLineValue cl :| []) properties)
              subComponents
              rest

data CalendarParseError
  = SubcomponentError !CalendarParseError
  | PropertyParseError !PropertyParseError
  deriving (Show, Eq, Ord)

instance Exception CalendarParseError where
  displayException = \case
    SubcomponentError cpe -> displayException cpe
    PropertyParseError ppe -> displayException ppe

instance ShowErrorComponent CalendarParseError where
  showErrorComponent = displayException

data CalendarParseFixableError
  = UntilTypeGuess !DateTimeStart !Until !Until -- Old until new until
  deriving (Show, Eq, Ord)

instance Exception CalendarParseFixableError where
  displayException = \case
    UntilTypeGuess dateTimeStart until1 until2 -> unwords ["UntilTypeGuess", show dateTimeStart, show until1, show until2]

data CalendarParseWarning
  = WarnMultipleRecurrenceRules !(Set RecurrenceRule)
  deriving (Show, Eq, Ord)

instance Exception CalendarParseWarning where
  displayException = \case
    WarnMultipleRecurrenceRules rrs -> unwords ["Component has multiple recurrence rules:", show rrs]

parseComponentFromContentLines ::
  (Validity component, IsComponent component) =>
  [ContentLine] ->
  CP component
parseComponentFromContentLines cls = do
  parseGeneralComponent cls >>= componentP

type CP a =
  Conform
    CalendarParseError
    CalendarParseFixableError
    CalendarParseWarning
    a

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
  componentP :: Component -> CP component

  -- | Builder for this component
  componentB :: component -> Component

sectionB :: Text -> (a -> DList ContentLine) -> (a -> DList ContentLine)
sectionB name func =
  (DList.singleton (propertyContentLineB (Begin name)) <>)
    . (<> DList.singleton (propertyContentLineB (End name)))
    . func

requiredProperty :: forall a. IsProperty a => Map ContentLineName (NonEmpty ContentLineValue) -> CP a
requiredProperty m = case M.lookup name m of
  Nothing -> error $ "fail: Did not find required property " <> show name
  Just values -> case values of
    (value :| _) ->
      conformMapAll PropertyParseError absurd absurd $
        propertyContentLineP (ContentLine name value)
        -- TODO warning when there are multiple.
  where
    name = propertyName (Proxy :: Proxy a)

requiredPropertyB :: IsProperty property => property -> Map ContentLineName (NonEmpty ContentLineValue)
requiredPropertyB property =
  let cl = propertyContentLineB property
   in M.singleton (contentLineName cl) (contentLineValue cl :| [])

parseFirst :: forall a. IsProperty a => [ContentLine] -> CP a
parseFirst = go
  where
    name = propertyName (Proxy :: Proxy a)
    go :: [ContentLine] -> CP a
    go = \case
      [] -> error $ "fail: Did not find required " <> show name
      (contentLine : cls) ->
        if contentLineName contentLine == name
          then conformMapAll PropertyParseError absurd absurd $ propertyContentLineP contentLine
          else go cls

optionalPropertyB :: IsProperty property => Maybe property -> Map ContentLineName (NonEmpty ContentLineValue)
optionalPropertyB = maybe M.empty requiredPropertyB

optionalPropertyWithDefaultB ::
  (Eq property, IsProperty property) =>
  property ->
  property ->
  Map ContentLineName (NonEmpty ContentLineValue)
optionalPropertyWithDefaultB defaultValue value =
  if value == defaultValue
    then mempty
    else requiredPropertyB value

optionalProperty ::
  forall a.
  IsProperty a =>
  Map ContentLineName (NonEmpty ContentLineValue) ->
  CP (Maybe a)
optionalProperty m = case M.lookup name m of
  Nothing -> pure Nothing
  Just values -> case values of
    (value :| _) ->
      fmap Just $
        conformMapAll PropertyParseError absurd absurd $
          propertyContentLineP (ContentLine name value)
          -- TODO warning when there are multiple.
  where
    name = propertyName (Proxy :: Proxy a)

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
          then fmap Just $ conformMapAll PropertyParseError absurd absurd $ propertyContentLineP contentLine
          else go cls

listOfPropertiesB ::
  IsProperty property =>
  [property] ->
  Map ContentLineName (NonEmpty ContentLineValue)
listOfPropertiesB = M.unionsWith (<>) . map requiredPropertyB

listOfProperties ::
  forall a.
  IsProperty a =>
  Map ContentLineName (NonEmpty ContentLineValue) ->
  CP [a]
listOfProperties m = do
  let values = maybe [] NE.toList $ M.lookup name m
  mapM (conformMapAll PropertyParseError absurd absurd . propertyContentLineP) (map (ContentLine name) values)
  where
    name = propertyName (Proxy :: Proxy a)

parseList ::
  forall a.
  IsProperty a =>
  [ContentLine] ->
  CP [a]
parseList cls =
  mapM (conformMapAll PropertyParseError absurd absurd . propertyContentLineP) $
    filter ((== name) . contentLineName) cls
  where
    name = propertyName (Proxy :: Proxy a)

propertySetB :: IsProperty property => Set property -> DList ContentLine
propertySetB = DList.fromList . map propertyContentLineB . S.toList

setOfPropertiesB ::
  IsProperty property =>
  Set property ->
  Map ContentLineName (NonEmpty ContentLineValue)
setOfPropertiesB = listOfPropertiesB . S.toList

setOfProperties ::
  forall a.
  (Ord a, IsProperty a) =>
  Map ContentLineName (NonEmpty ContentLineValue) ->
  CP (Set a)
setOfProperties = fmap S.fromList . listOfProperties

parseSet ::
  forall a.
  (Ord a, IsProperty a) =>
  [ContentLine] ->
  CP (Set a)
parseSet = fmap S.fromList . parseList

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
            emitFixableError $ UntilTypeGuess dateTimeStart u newUntil
            pure $ rrule {recurrenceRuleUntilCount = Just $ Left newUntil}
          _ -> pure rrule
        _ -> pure rrule
