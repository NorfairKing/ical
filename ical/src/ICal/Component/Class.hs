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

module ICal.Component.Class
  ( ComponentName,
    Component (..),
    IsComponent (..),
    CP,
    CalendarParseError (..),
    ComponentParseError (..),
    TimeZoneParseError (..),
    CalendarParseFixableError (..),
    CalendarParseWarning (..),
    parseGeneralComponent,
    parseGeneralComponents,
    renderGeneralComponent,
    renderGeneralComponents,
    parseComponentFromContentLines,
    namedComponentP,
    namedComponentB,
    namedComponentMapB,

    -- * Helper functions for writing the parser
    requiredPropertyP,
    optionalPropertyP,
    optionalPropertyWithDefaultP,
    listOfPropertiesP,
    setOfPropertiesP,
    subComponentsP,

    -- * Helper functions for writing the builder
    requiredPropertyB,
    optionalPropertyB,
    optionalPropertyWithDefaultB,
    listOfPropertiesB,
    setOfPropertiesB,
    subComponentsB,

    -- * General fixers
    fixUntil,

    -- * Helper functions for validation
    validateMDateTimeStartRRule,
    validateDateTimeStartRRule,
  )
where

import Control.DeepSeq
import Control.Exception
import Data.DList (DList (..))
import qualified Data.DList as DList
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
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

type ComponentName = Text

data Component = Component
  { componentProperties :: !(Map ContentLineName (NonEmpty ContentLineValue)),
    componentSubcomponents :: !(Map ComponentName (NonEmpty Component))
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Component

instance NFData Component

renderGeneralComponents :: Map ComponentName (NonEmpty Component) -> DList ContentLine
renderGeneralComponents =
  foldMap
    ( \(name, components) ->
        foldMap (renderGeneralComponent name) (NE.toList components)
    )
    . M.toList

renderGeneralComponent :: Text -> Component -> DList ContentLine
renderGeneralComponent name Component {..} =
  mconcat
    [ DList.singleton $ propertyContentLineB (Begin name),
      DList.fromList $
        concatMap
          ( \(n, values) ->
              map (ContentLine n) (NE.toList values)
          )
          (M.toList componentProperties),
      renderGeneralComponents componentSubcomponents,
      DList.singleton $ propertyContentLineB (End name)
    ]

parseGeneralComponents ::
  [ContentLine] ->
  Conform
    CalendarParseError
    CalendarParseFixableError
    CalendarParseWarning
    (Map ComponentName (NonEmpty Component))
parseGeneralComponents = go
  where
    go ::
      [ContentLine] ->
      Conform
        CalendarParseError
        CalendarParseFixableError
        CalendarParseWarning
        (Map ComponentName (NonEmpty Component))
    go = \case
      [] -> pure M.empty
      cls -> do
        ((name, component), leftovers) <- parseGeneralComponentHelper cls
        restComponents <- go leftovers
        pure (M.insertWith (<>) name (component :| []) restComponents)

parseGeneralComponent ::
  [ContentLine] ->
  Conform
    CalendarParseError
    CalendarParseFixableError
    CalendarParseWarning
    (Text, Component)
parseGeneralComponent =
  -- TODO check that there were no other lines after this.
  fmap fst . parseGeneralComponentHelper

parseGeneralComponentHelper ::
  [ContentLine] ->
  Conform
    CalendarParseError
    CalendarParseFixableError
    CalendarParseWarning
    ((ComponentName, Component), [ContentLine])
parseGeneralComponentHelper = \case
  [] -> unfixableError $ GeneralComponentError ComponentParseErrorMissingBegin
  (firstCL : restCLs) -> do
    Begin name <-
      conformMapAll
        PropertyParseError
        absurd
        absurd
        $ propertyContentLineP firstCL
    go name M.empty M.empty restCLs
  where
    go ::
      Text ->
      Map ContentLineName (NonEmpty ContentLineValue) ->
      Map ComponentName (NonEmpty Component) ->
      [ContentLine] ->
      Conform
        CalendarParseError
        CalendarParseFixableError
        CalendarParseWarning
        ((ComponentName, Component), [ContentLine])
    go name properties subComponents = \case
      [] -> unfixableError $ GeneralComponentError $ ComponentParseErrorMissingEnd name
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
                  ( ( name,
                      Component
                        { componentProperties = properties,
                          componentSubcomponents = subComponents
                        }
                    ),
                    rest
                  )
              else unfixableError $ GeneralComponentError $ ComponentParseErrorIncorrectEnd name name'
          "BEGIN" -> do
            ((name', subComponent), leftovers) <- parseGeneralComponentHelper (cl : rest)
            go
              name
              properties
              (M.insertWith (flip (<>)) name' (subComponent :| []) subComponents)
              leftovers
          _ ->
            go
              name
              (M.insertWith (flip (<>)) (contentLineName cl) (contentLineValue cl :| []) properties)
              subComponents
              rest

data CalendarParseError
  = CalendarParseErrorComponentIncorrectName !Text !Text
  | CalendarParseErrorMissingRequiredProperty !ContentLineName
  | GeneralComponentError !ComponentParseError
  | TimeZoneParseError !TimeZoneParseError
  | PropertyParseError !PropertyParseError
  deriving (Show, Eq, Ord)

instance Exception CalendarParseError where
  displayException = \case
    CalendarParseErrorComponentIncorrectName actual expected ->
      unwords
        [ "Tried to parse a component with name",
          show expected,
          "but found a component with name",
          show actual,
          "instead"
        ]
    CalendarParseErrorMissingRequiredProperty name ->
      unwords
        [ "Missing required property:",
          show (renderContentLineName name)
        ]
    GeneralComponentError cpe -> displayException cpe
    TimeZoneParseError tzpe -> displayException tzpe
    PropertyParseError ppe -> displayException ppe

data ComponentParseError
  = ComponentParseErrorMissingBegin
  | ComponentParseErrorMissingEnd !Text
  | ComponentParseErrorIncorrectEnd !Text !Text
  deriving (Show, Eq, Ord)

instance Exception ComponentParseError where
  displayException = \case
    ComponentParseErrorMissingBegin -> "Tried to parse a component, but didn't find a BEGIN property."
    ComponentParseErrorMissingEnd n ->
      unwords
        [ "Missing END property for component with name",
          show n
        ]
    ComponentParseErrorIncorrectEnd expected actual ->
      unwords
        [ unwords ["Missing END property for component with name", show expected],
          unwords ["found an END property for component with name", show actual, "instead."]
        ]

data TimeZoneParseError
  = TimeZoneParseErrorNoObservances
  | TimeZoneParseErrorDTStartNotDateTime !DateTimeStart
  deriving (Show, Eq, Ord, Generic)

instance Exception TimeZoneParseError where
  displayException = \case
    TimeZoneParseErrorNoObservances -> "Time zone had no observances but must have at least one standard or daylight observance."
    TimeZoneParseErrorDTStartNotDateTime dtstart ->
      unwords
        [ "Time zone DTSTART must be specified as a datetime, but found:",
          show dtstart
        ]

data CalendarParseFixableError
  = MissingProdId !ProdId
  | UntilTypeGuess
      !DateTimeStart
      !Until
      -- ^ Old 'UNTIL'
      !Until
      -- ^ New guessed 'UNTIL'
  deriving (Show, Eq, Ord)

instance Exception CalendarParseFixableError where
  displayException = \case
    MissingProdId prodid -> unwords ["Missing PRODID, added", show prodid]
    UntilTypeGuess dateTimeStart until1 until2 -> unwords ["UntilTypeGuess", show dateTimeStart, show until1, show until2]

data CalendarParseWarning
  = WarnMultipleRecurrenceRules !(Set RecurrenceRule)
  deriving (Show, Eq, Ord)

instance Exception CalendarParseWarning where
  displayException = \case
    WarnMultipleRecurrenceRules rrs -> unwords ["Component has multiple recurrence rules:", show rrs]

parseComponentFromContentLines ::
  IsComponent component =>
  [ContentLine] ->
  CP component
parseComponentFromContentLines cls = do
  parseGeneralComponent cls >>= uncurry namedComponentP

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

namedComponentP ::
  forall component.
  IsComponent component =>
  Text ->
  Component ->
  CP component
namedComponentP actualName component =
  let expectedName = componentName (Proxy :: Proxy component)
   in if actualName == expectedName
        then componentP component
        else unfixableError $ CalendarParseErrorComponentIncorrectName actualName expectedName

namedComponentB ::
  forall component.
  IsComponent component =>
  component ->
  (ComponentName, Component)
namedComponentB component =
  ( componentName (Proxy :: Proxy component),
    componentB component
  )

namedComponentMapB ::
  forall component.
  IsComponent component =>
  component ->
  Map ComponentName (NonEmpty Component)
namedComponentMapB component =
  let (name, generalComponent) = namedComponentB component
   in M.singleton name (generalComponent :| [])

requiredPropertyP :: forall a. IsProperty a => Map ContentLineName (NonEmpty ContentLineValue) -> CP a
requiredPropertyP m = case M.lookup name m of
  Nothing -> unfixableError $ CalendarParseErrorMissingRequiredProperty name
  Just values -> case values of
    (value :| _) ->
      conformMapAll PropertyParseError absurd absurd $
        propertyContentLineP (ContentLine name value)
        -- TODO fixable error when there are multiple.
  where
    name = propertyName (Proxy :: Proxy a)

requiredPropertyB :: IsProperty property => property -> Map ContentLineName (NonEmpty ContentLineValue)
requiredPropertyB property =
  let cl = propertyContentLineB property
   in M.singleton (contentLineName cl) (contentLineValue cl :| [])

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

optionalPropertyP ::
  forall a.
  IsProperty a =>
  Map ContentLineName (NonEmpty ContentLineValue) ->
  CP (Maybe a)
optionalPropertyP m = case M.lookup name m of
  Nothing -> pure Nothing
  Just values -> case values of
    (value :| _) ->
      fmap Just $
        conformMapAll PropertyParseError absurd absurd $
          propertyContentLineP (ContentLine name value)
          -- TODO warning when there are multiple.
  where
    name = propertyName (Proxy :: Proxy a)

optionalPropertyWithDefaultP ::
  forall a.
  IsProperty a =>
  a ->
  Map ContentLineName (NonEmpty ContentLineValue) ->
  CP a
optionalPropertyWithDefaultP defaultValue m = fromMaybe defaultValue <$> optionalPropertyP m

listOfPropertiesB ::
  IsProperty property =>
  [property] ->
  Map ContentLineName (NonEmpty ContentLineValue)
listOfPropertiesB = M.unionsWith (<>) . map requiredPropertyB

listOfPropertiesP ::
  forall a.
  IsProperty a =>
  Map ContentLineName (NonEmpty ContentLineValue) ->
  CP [a]
listOfPropertiesP m = do
  let values = maybe [] NE.toList $ M.lookup name m
  mapM (conformMapAll PropertyParseError absurd absurd . propertyContentLineP) (map (ContentLine name) values)
  where
    name = propertyName (Proxy :: Proxy a)

setOfPropertiesB ::
  IsProperty property =>
  Set property ->
  Map ContentLineName (NonEmpty ContentLineValue)
setOfPropertiesB = listOfPropertiesB . S.toList

setOfPropertiesP ::
  forall a.
  (Ord a, IsProperty a) =>
  Map ContentLineName (NonEmpty ContentLineValue) ->
  CP (Set a)
setOfPropertiesP = fmap S.fromList . listOfPropertiesP

subComponentsP ::
  forall component.
  IsComponent component =>
  Map ComponentName (NonEmpty Component) ->
  CP [component]
subComponentsP =
  mapM componentP
    . maybe [] NE.toList
    . M.lookup (componentName (Proxy :: Proxy component))

subComponentsB ::
  forall component.
  IsComponent component =>
  [component] ->
  Map ComponentName (NonEmpty Component)
subComponentsB = M.unionsWith (<>) . map namedComponentMapB

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
