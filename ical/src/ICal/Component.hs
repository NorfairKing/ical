{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Component where

import Control.Applicative.Permutations
import Control.Arrow (left)
import Control.DeepSeq
import Control.Monad
import Data.DList (DList (..))
import qualified Data.DList as DList
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
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
import ICal.ContentLine
import ICal.Property
import ICal.PropertyType.Class
import ICal.PropertyType.Date
import ICal.PropertyType.DateTime
import ICal.PropertyType.Duration
import ICal.PropertyType.RecurrenceRule
import ICal.UnfoldedLine
import Text.Megaparsec

parseICalendarFromContentLines :: [ContentLine] -> Either String [Calendar]
parseICalendarFromContentLines contentLines =
  left errorBundlePretty $ parse iCalendarP "" contentLines

parseVCalendarFromContentLines :: [ContentLine] -> Either String Calendar
parseVCalendarFromContentLines = parseComponentFromContentLines

parseComponentFromContentLines :: (Validity component, IsComponent component) => [ContentLine] -> Either String component
parseComponentFromContentLines = left errorBundlePretty . parse componentSectionP ""

type CP = Parsec Void [ContentLine]

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

iCalendarP :: CP [Calendar]
iCalendarP = many componentSectionP

iCalendarB :: [Calendar] -> DList ContentLine
iCalendarB = foldMap componentSectionB

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
  case propertyContentLineP contentLine of
    Left err -> fail err
    Right p -> pure p

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

-- |
--
-- === [section 3.6](https://datatracker.ietf.org/doc/html/rfc5545#section-3.6)
--
-- @
-- The body of the iCalendar object consists of a sequence of calendar
-- properties and one or more calendar components.  The calendar
-- properties are attributes that apply to the calendar object as a
-- whole.  The calendar components are collections of properties that
-- express a particular calendar semantic.  For example, the calendar
-- component can specify an event, a to-do, a journal entry, time zone
-- information, free/busy time information, or an alarm.
--
-- The body of the iCalendar object is defined by the following
-- notation:
--
--     icalbody   = calprops component
--
--     calprops   = *(
--                ;
--                ; The following are REQUIRED,
--                ; but MUST NOT occur more than once.
--                ;
--                prodid / version /
--                ;
--                ; The following are OPTIONAL,
--                ; but MUST NOT occur more than once.
--                ;
--                calscale / method /
--                ;
--                ; The following are OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                x-prop / iana-prop
--                ;
--                )
--
--     component  = 1*(eventc / todoc / journalc / freebusyc /
--                  timezonec / iana-comp / x-comp)
--
--     iana-comp  = "BEGIN" ":" iana-token CRLF
--                  1*contentline
--                  "END" ":" iana-token CRLF
--
--     x-comp     = "BEGIN" ":" x-name CRLF
--                  1*contentline
--                  "END" ":" x-name CRLF
--
-- An iCalendar object MUST include the "PRODID" and "VERSION" calendar
-- properties.  In addition, it MUST include at least one calendar
-- component.  Special forms of iCalendar objects are possible to
-- publish just busy time (i.e., only a "VFREEBUSY" calendar component)
-- or time zone (i.e., only a "VTIMEZONE" calendar component)
-- information.  In addition, a complex iCalendar object that is used to
-- capture a complete snapshot of the contents of a calendar is possible
-- (e.g., composite of many different calendar components).  More
-- commonly, an iCalendar object will consist of just a single "VEVENT",
-- "VTODO", or "VJOURNAL" calendar component.  Applications MUST ignore
-- x-comp and iana-comp values they don't recognize.  Applications that
-- support importing iCalendar objects SHOULD support all of the
-- component types defined in this document, and SHOULD NOT silently
-- drop any components as that can lead to user data loss.
-- @
data Calendar = Calendar
  { calendarVersion :: !Version,
    calendarProdId :: !ProdId,
    calendarCalendarScale :: !CalendarScale,
    calendarEvents :: ![Event],
    calendarTimeZones :: ![TimeZone]
  }
  deriving (Show, Eq, Generic)

instance Validity Calendar

instance NFData Calendar

instance IsComponent Calendar where
  componentName Proxy = "VCALENDAR"
  componentP :: CP Calendar
  componentP = do
    calPropLines <- takeWhileP (Just "calprops") $ \ContentLine {..} ->
      not $ contentLineName == "END" && contentLineValueRaw contentLineValue == "VCALENDAR"

    calendarVersion <- parseFirst calPropLines
    calendarProdId <- parseFirst calPropLines

    -- @
    -- The default value is "GREGORIAN".
    -- @
    calendarCalendarScale <- fromMaybe CalendarScaleGregorian <$> parseFirstMaybe calPropLines

    calendarTimeZones <- parseManySubcomponents calPropLines
    calendarEvents <- parseManySubcomponents calPropLines

    pure $ Calendar {..}

  componentB :: Calendar -> DList ContentLine
  componentB Calendar {..} =
    mconcat $
      concat
        [ [ propertyListB calendarVersion,
            propertyListB calendarProdId,
            -- @
            -- The default value is "GREGORIAN".
            -- @
            propertyDListB CalendarScaleGregorian calendarCalendarScale
          ],
          map componentSectionB calendarEvents,
          map componentSectionB calendarTimeZones
        ]

makeCalendar :: ProdId -> Calendar
makeCalendar prodId =
  Calendar
    { calendarProdId = prodId,
      calendarVersion = version20,
      -- @
      -- The default value is "GREGORIAN".
      -- @
      calendarCalendarScale = CalendarScaleGregorian,
      calendarEvents = [],
      calendarTimeZones = []
    }

parseFirst :: forall a. IsProperty a => [ContentLine] -> CP a
parseFirst = go
  where
    name = propertyName (Proxy :: Proxy a)
    go :: [ContentLine] -> CP a
    go = \case
      [] -> fail $ "Did not find required " <> show name
      (cl : cls) ->
        if contentLineName cl == name
          then case propertyContentLineP cl of
            Right result -> pure result
            Left err -> fail err
          else go cls

parseFirstMaybe :: forall a. IsProperty a => [ContentLine] -> CP (Maybe a)
parseFirstMaybe = go
  where
    name = propertyName (Proxy :: Proxy a)
    go :: [ContentLine] -> CP (Maybe a)
    go = \case
      [] -> pure Nothing
      -- TODO do better than a linear search?
      (cl : cls) ->
        if contentLineName cl == name
          then case propertyContentLineP cl of
            Right result -> pure (Just result)
            Left err -> fail err
          else go cls

parseSet ::
  forall a.
  (Ord a, IsProperty a) =>
  [ContentLine] ->
  CP (Set a)
parseSet cls =
  fmap S.fromList $
    mapM (either fail pure . propertyContentLineP) $
      filter ((== name) . contentLineName) cls
  where
    name = propertyName (Proxy :: Proxy a)

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
    go2 cls = case parse componentP "subcomponent" cls of
      Left err -> fail $ show err
      Right a -> pure a

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
    go2 cls = case parse componentP "subcomponent" cls of
      Left err -> fail $ show err
      Right a -> pure a

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
    go2 cls = case parse componentP "subcomponent" cls of
      Left err -> fail $ show err
      Right a -> pure a

    nameA = componentName (Proxy :: Proxy a)
    nameB = componentName (Proxy :: Proxy b)

parseSomeSubcomponents :: forall a. (IsComponent a) => [ContentLine] -> CP (NonEmpty a)
parseSomeSubcomponents cls = do
  cs <- parseManySubcomponents cls
  case NE.nonEmpty cs of
    Nothing -> fail "expected at least one subcompent"
    Just ne -> pure ne

-- |
--
-- === [section 3.6.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.6.1)
--
-- @
-- Component Name:  VEVENT
--
-- Purpose:  Provide a grouping of component properties that describe an
--    event.
--
-- Format Definition:  A "VEVENT" calendar component is defined by the
--    following notation:
--
--     eventc     = "BEGIN" ":" "VEVENT" CRLF
--                  eventprop *alarmc
--                  "END" ":" "VEVENT" CRLF
--
--     eventprop  = *(
--                ;
--                ; The following are REQUIRED,
--                ; but MUST NOT occur more than once.
--
--                ;
--                dtstamp / uid /
--                ;
--                ; The following is REQUIRED if the component
--                ; appears in an iCalendar object that doesn't
--                ; specify the "METHOD" property; otherwise, it
--                ; is OPTIONAL; in any case, it MUST NOT occur
--                ; more than once.
--                ;
--                dtstart /
--                ;
--                ; The following are OPTIONAL,
--                ; but MUST NOT occur more than once.
--                ;
--                class / created / description / geo /
--                last-mod / location / organizer / priority /
--                seq / status / summary / transp /
--                url / recurid /
--                ;
--                ; The following is OPTIONAL,
--                ; but SHOULD NOT occur more than once.
--                ;
--                rrule /
--                ;
--                ; Either 'dtend' or 'duration' MAY appear in
--                ; a 'eventprop', but 'dtend' and 'duration'
--                ; MUST NOT occur in the same 'eventprop'.
--                ;
--                dtend / duration /
--                ;
--                ; The following are OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                attach / attendee / categories / comment /
--                contact / exdate / rstatus / related /
--                resources / rdate / x-prop / iana-prop
--                ;
--                )
--
-- Description:  A "VEVENT" calendar component is a grouping of
--    component properties, possibly including "VALARM" calendar
--    components, that represents a scheduled amount of time on a
--    calendar.  For example, it can be an activity; such as a one-hour
--    long, department meeting from 8:00 AM to 9:00 AM, tomorrow.
--    Generally, an event will take up time on an individual calendar.
--    Hence, the event will appear as an opaque interval in a search for
--    busy time.  Alternately, the event can have its Time Transparency
--
--
--
--    set to "TRANSPARENT" in order to prevent blocking of the event in
--    searches for busy time.
--
--    The "VEVENT" is also the calendar component used to specify an
--    anniversary or daily reminder within a calendar.  These events
--    have a DATE value type for the "DTSTART" property instead of the
--    default value type of DATE-TIME.  If such a "VEVENT" has a "DTEND"
--    property, it MUST be specified as a DATE value also.  The
--    anniversary type of "VEVENT" can span more than one date (i.e.,
--    "DTEND" property value is set to a calendar date after the
--    "DTSTART" property value).  If such a "VEVENT" has a "DURATION"
--    property, it MUST be specified as a "dur-day" or "dur-week" value.
--
--    The "DTSTART" property for a "VEVENT" specifies the inclusive
--    start of the event.  For recurring events, it also specifies the
--    very first instance in the recurrence set.  The "DTEND" property
--    for a "VEVENT" calendar component specifies the non-inclusive end
--    of the event.  For cases where a "VEVENT" calendar component
--    specifies a "DTSTART" property with a DATE value type but no
--    "DTEND" nor "DURATION" property, the event's duration is taken to
--    be one day.  For cases where a "VEVENT" calendar component
--    specifies a "DTSTART" property with a DATE-TIME value type but no
--    "DTEND" property, the event ends on the same calendar date and
--    time of day specified by the "DTSTART" property.
--
--    The "VEVENT" calendar component cannot be nested within another
--    calendar component.  However, "VEVENT" calendar components can be
--    related to each other or to a "VTODO" or to a "VJOURNAL" calendar
--    component with the "RELATED-TO" property.
--
-- Example:  The following is an example of the "VEVENT" calendar
--    component used to represent a meeting that will also be opaque to
--    searches for busy time:
--
--     BEGIN:VEVENT
--     UID:19970901T130000Z-123401@example.com
--     DTSTAMP:19970901T130000Z
--     DTSTART:19970903T163000Z
--     DTEND:19970903T190000Z
--     SUMMARY:Annual Employee Review
--     CLASS:PRIVATE
--     CATEGORIES:BUSINESS,HUMAN RESOURCES
--     END:VEVENT
--
--    The following is an example of the "VEVENT" calendar component
--    used to represent a reminder that will not be opaque, but rather
--    transparent, to searches for busy time:
--
--     BEGIN:VEVENT
--     UID:19970901T130000Z-123402@example.com
--     DTSTAMP:19970901T130000Z
--     DTSTART:19970401T163000Z
--     DTEND:19970402T010000Z
--     SUMMARY:Laurel is in sensitivity awareness class.
--     CLASS:PUBLIC
--     CATEGORIES:BUSINESS,HUMAN RESOURCES
--     TRANSP:TRANSPARENT
--     END:VEVENT
--
--    The following is an example of the "VEVENT" calendar component
--    used to represent an anniversary that will occur annually:
--
--     BEGIN:VEVENT
--     UID:19970901T130000Z-123403@example.com
--     DTSTAMP:19970901T130000Z
--     DTSTART;VALUE=DATE:19971102
--     SUMMARY:Our Blissful Anniversary
--     TRANSP:TRANSPARENT
--     CLASS:CONFIDENTIAL
--     CATEGORIES:ANNIVERSARY,PERSONAL,SPECIAL OCCASION
--     RRULE:FREQ=YEARLY
--     END:VEVENT
--
--    The following is an example of the "VEVENT" calendar component
--    used to represent a multi-day event scheduled from June 28th, 2007
--    to July 8th, 2007 inclusively.  Note that the "DTEND" property is
--    set to July 9th, 2007, since the "DTEND" property specifies the
--    non-inclusive end of the event.
--
--     BEGIN:VEVENT
--     UID:20070423T123432Z-541111@example.com
--     DTSTAMP:20070423T123432Z
--     DTSTART;VALUE=DATE:20070628
--     DTEND;VALUE=DATE:20070709
--     SUMMARY:Festival International de Jazz de Montreal
--     TRANSP:TRANSPARENT
--     END:VEVENT
-- @
data Event = Event
  { -- @
    -- ; The following are REQUIRED,
    -- ; but MUST NOT occur more than once.
    -- dtstamp / uid /
    -- @
    eventDateTimeStamp :: !DateTimeStamp,
    eventUID :: !UID,
    -- @
    -- ;
    -- ; The following is REQUIRED if the component
    -- ; appears in an iCalendar object that doesn't
    -- ; specify the "METHOD" property; otherwise, it
    -- ; is OPTIONAL; in any case, it MUST NOT occur
    -- ; more than once.
    -- ;
    -- dtstart /
    -- @
    eventDateTimeStart :: !(Maybe DateTimeStart),
    -- @
    -- ;
    -- ; The following are OPTIONAL,
    -- ; but MUST NOT occur more than once.
    -- ;
    -- class / created / description / geo /
    -- last-mod / location / organizer / priority /
    -- seq / status / summary / transp /
    -- url / recurid /
    -- @
    eventClassification :: !Classification,
    eventCreated :: !(Maybe Created),
    eventDescription :: !(Maybe Description),
    eventGeographicPosition :: !(Maybe GeographicPosition),
    eventLastModified :: !(Maybe LastModified),
    eventLocation :: !(Maybe Location),
    eventStatus :: !(Maybe Status),
    eventSummary :: !(Maybe Summary),
    eventTransparency :: !Transparency,
    eventURL :: !(Maybe URL),
    -- @
    -- ;
    -- ; The following is OPTIONAL,
    -- ; but SHOULD NOT occur more than once.
    -- ;
    -- rrule /
    -- @
    eventRecurrenceRules :: !(Set RecurrenceRule),
    -- @
    -- ;
    -- ; Either 'dtend' or 'duration' MAY appear in
    -- ; a 'eventprop', but 'dtend' and 'duration'
    -- ; MUST NOT occur in the same 'eventprop'.
    -- ;
    -- dtend / duration /
    -- @
    eventDateTimeEndDuration :: !(Maybe (Either DateTimeEnd Duration))
    -- @
    -- ;
    -- ; The following are OPTIONAL,
    -- ; and MAY occur more than once.
    -- ;
    -- attach / attendee / categories / comment /
    -- contact / exdate / rstatus / related /
    -- resources / rdate / x-prop / iana-prop
    -- ;
    -- @
  }
  deriving (Show, Eq, Generic)

instance Validity Event where
  validate e@Event {..} =
    mconcat
      [ genericValidate e,
        validateMDateTimeStartRRule eventDateTimeStart eventRecurrenceRules
      ]

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
        -- [section 3.3.10.  Recurrence Rule](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.10)
        -- @
        -- The value of the UNTIL rule part MUST have the same
        -- value type as the "DTSTART" property.
        -- Furthermore, if the
        -- "DTSTART" property is specified as a date with local time, then
        -- the UNTIL rule part MUST also be specified as a date with local
        -- time.
        -- If the "DTSTART" property is specified as a date with UTC
        -- time or a date with local time and time zone reference, then the
        -- UNTIL rule part MUST be specified as a date with UTC time.
        -- @
        let msg =
              unlines
                [ "The value type of the UNTIL rule part has the same value type as the DTSTART property.",
                  show dateTimeStart,
                  show u
                ]
         in declare msg $
              case (dateTimeStart, u) of
                (DateTimeStartDate _, UntilDate _) -> True
                (DateTimeStartDateTime _, UntilDateTime _) -> True
                _ -> False
      _ -> mempty

instance NFData Event

instance IsComponent Event where
  componentName Proxy = "VEVENT"
  componentP = vEventP
  componentB = vEventB

vEventP :: CP Event
vEventP = do
  eventProperties <- takeWhileP (Just "eventProperties") $ \ContentLine {..} ->
    not $ contentLineName == "END" && contentLineValueRaw contentLineValue == "VEVENT"
  eventDateTimeStamp <- parseFirst eventProperties
  eventUID <- parseFirst eventProperties
  eventDateTimeStart <- parseFirstMaybe eventProperties
  -- @
  -- ;Default is PUBLIC
  -- @
  eventClassification <- fromMaybe ClassificationPublic <$> parseFirstMaybe eventProperties
  eventCreated <- parseFirstMaybe eventProperties
  eventDescription <- parseFirstMaybe eventProperties
  eventGeographicPosition <- parseFirstMaybe eventProperties
  eventLastModified <- parseFirstMaybe eventProperties
  eventLocation <- parseFirstMaybe eventProperties
  eventStatus <- parseFirstMaybe eventProperties
  eventSummary <- parseFirstMaybe eventProperties
  -- @
  -- ;Default value is OPAQUE
  -- @
  eventTransparency <- fromMaybe TransparencyOpaque <$> parseFirstMaybe eventProperties
  eventURL <- parseFirstMaybe eventProperties

  -- It turns out that certain ical providers such as Google may output invalid
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
  let fixUntil :: RecurrenceRule -> RecurrenceRule
      fixUntil rrule =
        case eventDateTimeStart of
          Nothing -> rrule
          Just dateTimeStart ->
            case recurrenceRuleUntilCount rrule of
              Just (Left u) -> case (dateTimeStart, u) of
                (DateTimeStartDateTime dt, UntilDate (Date ud)) ->
                  let newUntil = case dt of
                        -- This guess is somewhat sensible.
                        DateTimeUTC (Time.UTCTime _ sdt) ->
                          UntilDateTime (Time.UTCTime ud sdt)
                        -- This guess is bad.
                        DateTimeFloating (Time.LocalTime _ tod) ->
                          UntilDateTime (Time.UTCTime ud (Time.timeOfDayToTime tod))
                        -- This guess is even worse.
                        DateTimeZoned _ (Time.LocalTime _ tod) ->
                          UntilDateTime (Time.UTCTime ud (Time.timeOfDayToTime tod))
                   in rrule {recurrenceRuleUntilCount = Just $ Left newUntil}
                _ -> rrule
              _ -> rrule
  eventRecurrenceRules <- S.map fixUntil <$> parseSet eventProperties

  mEnd <- parseFirstMaybe eventProperties
  mDuration <- parseFirstMaybe eventProperties
  let eventDateTimeEndDuration = case (mEnd, mDuration) of
        (Nothing, Nothing) -> Nothing
        (Nothing, Just d) -> Just (Right d)
        (Just e, _) -> Just (Left e) -- Not failing to parse if both are present.
  pure Event {..}

permutationList :: MonadPlus m => m a -> Permutation m [a]
permutationList p = toPermutationWithDefault [] ((:) <$> p <*> some p)

vEventB :: Event -> DList ContentLine
vEventB Event {..} =
  mconcat
    [ propertyListB eventDateTimeStamp,
      propertyListB eventUID,
      propertyMListB eventDateTimeStart,
      -- @
      -- ;Default is PUBLIC
      -- @
      propertyDListB ClassificationPublic eventClassification,
      propertyMListB eventCreated,
      propertyMListB eventDescription,
      propertyMListB eventGeographicPosition,
      propertyMListB eventLastModified,
      propertyMListB eventLocation,
      propertyMListB eventStatus,
      propertyMListB eventSummary,
      -- @
      -- ;Default value is OPAQUE
      -- @
      propertyDListB TransparencyOpaque eventTransparency,
      propertyMListB eventURL,
      propertySetB eventRecurrenceRules,
      case eventDateTimeEndDuration of
        Nothing -> mempty
        Just endOrDuration -> case endOrDuration of
          Left e -> propertyListB e
          Right d -> propertyListB d
    ]

makeEvent :: UID -> DateTimeStamp -> Event
makeEvent uid dateTimeStamp =
  Event
    { eventUID = uid,
      eventDateTimeStamp = dateTimeStamp,
      eventDateTimeStart = Nothing,
      -- @
      -- ;Default is PUBLIC
      -- @
      eventClassification = ClassificationPublic,
      eventCreated = Nothing,
      eventDescription = Nothing,
      eventGeographicPosition = Nothing,
      eventLastModified = Nothing,
      eventLocation = Nothing,
      eventStatus = Nothing,
      eventSummary = Nothing,
      -- @
      -- ;Default value is OPAQUE
      -- @
      eventTransparency = TransparencyOpaque,
      eventURL = Nothing,
      eventRecurrenceRules = S.empty,
      eventDateTimeEndDuration = Nothing
    }

-- |
--
-- === [section 3.6.5](https://datatracker.ietf.org/doc/html/rfc5545#section-3.6.5)
--
-- @
-- Component Name:  VTIMEZONE
--
-- Purpose:  Provide a grouping of component properties that defines a
--    time zone.
--
-- Format Definition:  A "VTIMEZONE" calendar component is defined by
--    the following notation:
--
--     timezonec  = "BEGIN" ":" "VTIMEZONE" CRLF
--                  *(
--                  ;
--                  ; 'tzid' is REQUIRED, but MUST NOT occur more
--                  ; than once.
--                  ;
--                  tzid /
--                  ;
--                  ; 'last-mod' and 'tzurl' are OPTIONAL,
--                  ; but MUST NOT occur more than once.
--                  ;
--                  last-mod / tzurl /
--                  ;
--                  ; One of 'standardc' or 'daylightc' MUST occur
--                  ; and each MAY occur more than once.
--                  ;
--                  standardc / daylightc /
--                  ;
--                  ; The following are OPTIONAL,
--                  ; and MAY occur more than once.
--                  ;
--                  x-prop / iana-prop
--                  ;
--                  )
--                  "END" ":" "VTIMEZONE" CRLF
--
--     standardc  = "BEGIN" ":" "STANDARD" CRLF
--                  tzprop
--                  "END" ":" "STANDARD" CRLF
--
--     daylightc  = "BEGIN" ":" "DAYLIGHT" CRLF
--                  tzprop
--                  "END" ":" "DAYLIGHT" CRLF
--
--     tzprop     = *(
--                  ;
--                  ; The following are REQUIRED,
--                  ; but MUST NOT occur more than once.
--                  ;
--                  dtstart / tzoffsetto / tzoffsetfrom /
--                  ;
--                  ; The following is OPTIONAL,
--                  ; but SHOULD NOT occur more than once.
--                  ;
--                  rrule /
--                  ;
--                  ; The following are OPTIONAL,
--                  ; and MAY occur more than once.
--                  ;
--                  comment / rdate / tzname / x-prop / iana-prop
--                  ;
--                  )
--
-- Description:  A time zone is unambiguously defined by the set of time
--    measurement rules determined by the governing body for a given
--    geographic area.  These rules describe, at a minimum, the base
--
--    offset from UTC for the time zone, often referred to as the
--    Standard Time offset.  Many locations adjust their Standard Time
--    forward or backward by one hour, in order to accommodate seasonal
--    changes in number of daylight hours, often referred to as Daylight
--    Saving Time.  Some locations adjust their time by a fraction of an
--    hour.  Standard Time is also known as Winter Time.  Daylight
--    Saving Time is also known as Advanced Time, Summer Time, or Legal
--    Time in certain countries.  The following table shows the changes
--    in time zone rules in effect for New York City starting from 1967.
--    Each line represents a description or rule for a particular
--    observance.
--
--                       Effective Observance Rule
--
--   +-----------+--------------------------+--------+--------------+
--   | Date      | (Date-Time)              | Offset | Abbreviation |
--   +-----------+--------------------------+--------+--------------+
--   | 1967-1973 | last Sun in Apr, 02:00   | -0400  | EDT          |
--   |           |                          |        |              |
--   | 1967-2006 | last Sun in Oct, 02:00   | -0500  | EST          |
--   |           |                          |        |              |
--   | 1974-1974 | Jan 6, 02:00             | -0400  | EDT          |
--   |           |                          |        |              |
--   | 1975-1975 | Feb 23, 02:00            | -0400  | EDT          |
--   |           |                          |        |              |
--   | 1976-1986 | last Sun in Apr, 02:00   | -0400  | EDT          |
--   |           |                          |        |              |
--   | 1987-2006 | first Sun in Apr, 02:00  | -0400  | EDT          |
--   |           |                          |        |              |
--   | 2007-*    | second Sun in Mar, 02:00 | -0400  | EDT          |
--   |           |                          |        |              |
--   | 2007-*    | first Sun in Nov, 02:00  | -0500  | EST          |
--   +-----------+--------------------------+--------+--------------+
--
-- Note: The specification of a global time zone registry is not
--       addressed by this document and is left for future study.
--       However, implementers may find the TZ database [TZDB] a useful
--       reference.  It is an informal, public-domain collection of time
--       zone information, which is currently being maintained by
--       volunteer Internet participants, and is used in several
--       operating systems.  This database contains current and
--       historical time zone information for a wide variety of
--       locations around the globe; it provides a time zone identifier
--       for every unique time zone rule set in actual use since 1970,
--       with historical data going back to the introduction of standard
--       time.
--
--    Interoperability between two calendaring and scheduling
--    applications, especially for recurring events, to-dos or journal
--    entries, is dependent on the ability to capture and convey date
--    and time information in an unambiguous format.  The specification
--    of current time zone information is integral to this behavior.
--
--    If present, the "VTIMEZONE" calendar component defines the set of
--    Standard Time and Daylight Saving Time observances (or rules) for
--    a particular time zone for a given interval of time.  The
--    "VTIMEZONE" calendar component cannot be nested within other
--    calendar components.  Multiple "VTIMEZONE" calendar components can
--    exist in an iCalendar object.  In this situation, each "VTIMEZONE"
--    MUST represent a unique time zone definition.  This is necessary
--    for some classes of events, such as airline flights, that start in
--    one time zone and end in another.
--
--    The "VTIMEZONE" calendar component MUST include the "TZID"
--    property and at least one definition of a "STANDARD" or "DAYLIGHT"
--    sub-component.  The "STANDARD" or "DAYLIGHT" sub-component MUST
--    include the "DTSTART", "TZOFFSETFROM", and "TZOFFSETTO"
--    properties.
--
--    An individual "VTIMEZONE" calendar component MUST be specified for
--    each unique "TZID" parameter value specified in the iCalendar
--    object.  In addition, a "VTIMEZONE" calendar component, referred
--    to by a recurring calendar component, MUST provide valid time zone
--    information for all recurrence instances.
--
--    Each "VTIMEZONE" calendar component consists of a collection of
--    one or more sub-components that describe the rule for a particular
--    observance (either a Standard Time or a Daylight Saving Time
--    observance).  The "STANDARD" sub-component consists of a
--    collection of properties that describe Standard Time.  The
--    "DAYLIGHT" sub-component consists of a collection of properties
--    that describe Daylight Saving Time.  In general, this collection
--    of properties consists of:
--
--    *  the first onset DATE-TIME for the observance;
--
--    *  the last onset DATE-TIME for the observance, if a last onset is
--       known;
--
--    *  the offset to be applied for the observance;
--
--    *  a rule that describes the day and time when the observance
--       takes effect;
--
--    *  an optional name for the observance.
--
--    For a given time zone, there may be multiple unique definitions of
--    the observances over a period of time.  Each observance is
--    described using either a "STANDARD" or "DAYLIGHT" sub-component.
--    The collection of these sub-components is used to describe the
--    time zone for a given period of time.  The offset to apply at any
--    given time is found by locating the observance that has the last
--    onset date and time before the time in question, and using the
--    offset value from that observance.
--
--    The top-level properties in a "VTIMEZONE" calendar component are:
--
--    The mandatory "TZID" property is a text value that uniquely
--    identifies the "VTIMEZONE" calendar component within the scope of
--    an iCalendar object.
--
--    The optional "LAST-MODIFIED" property is a UTC value that
--    specifies the date and time that this time zone definition was
--    last updated.
--
--    The optional "TZURL" property is a url value that points to a
--    published "VTIMEZONE" definition.  "TZURL" SHOULD refer to a
--    resource that is accessible by anyone who might need to interpret
--    the object.  This SHOULD NOT normally be a "file" URL or other URL
--    that is not widely accessible.
--
--    The collection of properties that are used to define the
--    "STANDARD" and "DAYLIGHT" sub-components include:
--
--    The mandatory "DTSTART" property gives the effective onset date
--    and local time for the time zone sub-component definition.
--    "DTSTART" in this usage MUST be specified as a date with a local
--    time value.
--
--    The mandatory "TZOFFSETFROM" property gives the UTC offset that is
--    in use when the onset of this time zone observance begins.
--    "TZOFFSETFROM" is combined with "DTSTART" to define the effective
--    onset for the time zone sub-component definition.  For example,
--    the following represents the time at which the observance of
--    Standard Time took effect in Fall 1967 for New York City:
--
--     DTSTART:19671029T020000
--
--     TZOFFSETFROM:-0400
--
--    The mandatory "TZOFFSETTO" property gives the UTC offset for the
--    time zone sub-component (Standard Time or Daylight Saving Time)
--    when this observance is in use.
--
--    The optional "TZNAME" property is the customary name for the time
--    zone.  This could be used for displaying dates.
--
--    The onset DATE-TIME values for the observance defined by the time
--    zone sub-component is defined by the "DTSTART", "RRULE", and
--    "RDATE" properties.
--
--    The "RRULE" property defines the recurrence rule for the onset of
--    the observance defined by this time zone sub-component.  Some
--    specific requirements for the usage of "RRULE" for this purpose
--    include:
--
--    *  If observance is known to have an effective end date, the
--       "UNTIL" recurrence rule parameter MUST be used to specify the
--       last valid onset of this observance (i.e., the UNTIL DATE-TIME
--       will be equal to the last instance generated by the recurrence
--       pattern).  It MUST be specified in UTC time.
--
--    *  The "DTSTART" and the "TZOFFSETFROM" properties MUST be used
--       when generating the onset DATE-TIME values (instances) from the
--       "RRULE".
--
--    The "RDATE" property can also be used to define the onset of the
--    observance by giving the individual onset date and times.  "RDATE"
--    in this usage MUST be specified as a date with local time value,
--    relative to the UTC offset specified in the "TZOFFSETFROM"
--    property.
--
--    The optional "COMMENT" property is also allowed for descriptive
--    explanatory text.
--
-- Example:  The following are examples of the "VTIMEZONE" calendar
--    component:
--
--    This is an example showing all the time zone rules for New York
--    City since April 30, 1967 at 03:00:00 EDT.
--
--     BEGIN:VTIMEZONE
--     TZID:America/New_York
--     LAST-MODIFIED:20050809T050000Z
--     BEGIN:DAYLIGHT
--     DTSTART:19670430T020000
--     RRULE:FREQ=YEARLY;BYMONTH=4;BYDAY=-1SU;UNTIL=19730429T070000Z
--     TZOFFSETFROM:-0500
--     TZOFFSETTO:-0400
--     TZNAME:EDT
--     END:DAYLIGHT
--     BEGIN:STANDARD
--
--     DTSTART:19671029T020000
--     RRULE:FREQ=YEARLY;BYMONTH=10;BYDAY=-1SU;UNTIL=20061029T060000Z
--     TZOFFSETFROM:-0400
--     TZOFFSETTO:-0500
--     TZNAME:EST
--     END:STANDARD
--     BEGIN:DAYLIGHT
--     DTSTART:19740106T020000
--     RDATE:19750223T020000
--     TZOFFSETFROM:-0500
--     TZOFFSETTO:-0400
--     TZNAME:EDT
--     END:DAYLIGHT
--     BEGIN:DAYLIGHT
--     DTSTART:19760425T020000
--     RRULE:FREQ=YEARLY;BYMONTH=4;BYDAY=-1SU;UNTIL=19860427T070000Z
--     TZOFFSETFROM:-0500
--     TZOFFSETTO:-0400
--     TZNAME:EDT
--     END:DAYLIGHT
--     BEGIN:DAYLIGHT
--     DTSTART:19870405T020000
--     RRULE:FREQ=YEARLY;BYMONTH=4;BYDAY=1SU;UNTIL=20060402T070000Z
--     TZOFFSETFROM:-0500
--     TZOFFSETTO:-0400
--     TZNAME:EDT
--     END:DAYLIGHT
--     BEGIN:DAYLIGHT
--     DTSTART:20070311T020000
--     RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=2SU
--     TZOFFSETFROM:-0500
--     TZOFFSETTO:-0400
--     TZNAME:EDT
--     END:DAYLIGHT
--     BEGIN:STANDARD
--     DTSTART:20071104T020000
--     RRULE:FREQ=YEARLY;BYMONTH=11;BYDAY=1SU
--     TZOFFSETFROM:-0400
--     TZOFFSETTO:-0500
--     TZNAME:EST
--     END:STANDARD
--     END:VTIMEZONE
--
--    This is an example showing time zone information for New York City
--    using only the "DTSTART" property.  Note that this is only
--    suitable for a recurring event that starts on or later than March
--    11, 2007 at 03:00:00 EDT (i.e., the earliest effective transition
--    date and time) and ends no later than March 9, 2008 at 01:59:59
--    EST (i.e., latest valid date and time for EST in this scenario).
--    For example, this can be used for a recurring event that occurs
--    every Friday, 8:00 A.M.-9:00 A.M., starting June 1, 2007, ending
--    December 31, 2007,
--
--     BEGIN:VTIMEZONE
--     TZID:America/New_York
--     LAST-MODIFIED:20050809T050000Z
--     BEGIN:STANDARD
--     DTSTART:20071104T020000
--     TZOFFSETFROM:-0400
--     TZOFFSETTO:-0500
--     TZNAME:EST
--     END:STANDARD
--     BEGIN:DAYLIGHT
--     DTSTART:20070311T020000
--     TZOFFSETFROM:-0500
--     TZOFFSETTO:-0400
--     TZNAME:EDT
--     END:DAYLIGHT
--     END:VTIMEZONE
--
--    This is a simple example showing the current time zone rules for
--    New York City using a "RRULE" recurrence pattern.  Note that there
--    is no effective end date to either of the Standard Time or
--    Daylight Time rules.  This information would be valid for a
--    recurring event starting today and continuing indefinitely.
--
--     BEGIN:VTIMEZONE
--     TZID:America/New_York
--     LAST-MODIFIED:20050809T050000Z
--     TZURL:http://zones.example.com/tz/America-New_York.ics
--     BEGIN:STANDARD
--     DTSTART:20071104T020000
--     RRULE:FREQ=YEARLY;BYMONTH=11;BYDAY=1SU
--     TZOFFSETFROM:-0400
--     TZOFFSETTO:-0500
--     TZNAME:EST
--     END:STANDARD
--     BEGIN:DAYLIGHT
--     DTSTART:20070311T020000
--     RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=2SU
--     TZOFFSETFROM:-0500
--     TZOFFSETTO:-0400
--     TZNAME:EDT
--     END:DAYLIGHT
--     END:VTIMEZONE
--
--    This is an example showing a set of rules for a fictitious time
--    zone where the Daylight Time rule has an effective end date (i.e.,
--    after that date, Daylight Time is no longer observed).
--
--     BEGIN:VTIMEZONE
--     TZID:Fictitious
--     LAST-MODIFIED:19870101T000000Z
--     BEGIN:STANDARD
--     DTSTART:19671029T020000
--     RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10
--     TZOFFSETFROM:-0400
--     TZOFFSETTO:-0500
--     TZNAME:EST
--     END:STANDARD
--     BEGIN:DAYLIGHT
--     DTSTART:19870405T020000
--     RRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4;UNTIL=19980404T070000Z
--     TZOFFSETFROM:-0500
--     TZOFFSETTO:-0400
--     TZNAME:EDT
--     END:DAYLIGHT
--     END:VTIMEZONE
--
--    This is an example showing a set of rules for a fictitious time
--    zone where the first Daylight Time rule has an effective end date.
--    There is a second Daylight Time rule that picks up where the other
--    left off.
--
--     BEGIN:VTIMEZONE
--     TZID:Fictitious
--     LAST-MODIFIED:19870101T000000Z
--     BEGIN:STANDARD
--     DTSTART:19671029T020000
--     RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10
--     TZOFFSETFROM:-0400
--     TZOFFSETTO:-0500
--     TZNAME:EST
--     END:STANDARD
--     BEGIN:DAYLIGHT
--     DTSTART:19870405T020000
--     RRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4;UNTIL=19980404T070000Z
--     TZOFFSETFROM:-0500
--     TZOFFSETTO:-0400
--     TZNAME:EDT
--     END:DAYLIGHT
--     BEGIN:DAYLIGHT
--     DTSTART:19990424T020000
--     RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=4
--     TZOFFSETFROM:-0500
--     TZOFFSETTO:-0400
--     TZNAME:EDT
--     END:DAYLIGHT
--     END:VTIMEZONE
-- @
data TimeZone = TimeZone
  { -- @
    -- ; 'tzid' is REQUIRED, but MUST NOT occur more
    -- ; than once.
    -- ;
    -- tzid /
    -- @
    timeZoneId :: !TZID,
    -- @
    -- ; 'last-mod' and 'tzurl' are OPTIONAL,
    -- ; but MUST NOT occur more than once.
    -- ;
    -- last-mod / tzurl /
    -- @
    -- TODO
    -- @
    -- ; One of 'standardc' or 'daylightc' MUST occur
    -- ; and each MAY occur more than once.
    -- ;
    -- standardc / daylightc /
    -- @
    -- API Note: The order does not matter but duplicates are allowed so we cannot use a set.
    timeZoneObservances :: !(NonEmpty TimeZoneObservance)
    -- @
    -- ; The following are OPTIONAL,
    -- ; and MAY occur more than once.
    -- ;
    -- x-prop / iana-prop
    -- @
  }
  deriving (Show, Eq, Generic)

instance Validity TimeZone

instance NFData TimeZone

instance IsComponent TimeZone where
  componentName Proxy = "VTIMEZONE"
  componentP = vTimeZoneP
  componentB = vTimeZoneB

vTimeZoneP :: CP TimeZone
vTimeZoneP = do
  timeZoneProperties <- takeWhileP (Just "timeZoneProperties") $ \ContentLine {..} ->
    not $
      contentLineName == "END" && (contentLineValueRaw contentLineValue == "VTIMEZONE")
  timeZoneId <- parseFirst timeZoneProperties
  eithers <- parseManySubcomponents2 timeZoneProperties
  let os = map (either StandardObservance DaylightObservance) eithers
  timeZoneObservances <- case NE.nonEmpty os of
    Nothing -> fail "Must have at least one standardc or daylightc"
    Just ne -> pure ne
  pure TimeZone {..}

vTimeZoneB :: TimeZone -> DList ContentLine
vTimeZoneB TimeZone {..} =
  mconcat
    [ propertyListB timeZoneId,
      foldMap timeZoneObservanceB timeZoneObservances
    ]

data TimeZoneObservance
  = StandardObservance !Standard
  | DaylightObservance !Daylight
  deriving (Show, Eq, Generic)

instance Validity TimeZoneObservance

instance NFData TimeZoneObservance

timeZoneObservanceP :: CP TimeZoneObservance
timeZoneObservanceP =
  StandardObservance <$> componentSectionP
    <|> DaylightObservance <$> componentSectionP

timeZoneObservanceB :: TimeZoneObservance -> DList ContentLine
timeZoneObservanceB = \case
  StandardObservance s -> componentSectionB s
  DaylightObservance d -> componentSectionB d

newtype Standard = Standard {unStandard :: Observance}
  deriving (Show, Eq, Generic)

instance Validity Standard

instance NFData Standard

instance IsComponent Standard where
  componentName Proxy = "STANDARD"
  componentP = Standard <$> observanceP
  componentB = observanceB . unStandard

newtype Daylight = Daylight {unDaylight :: Observance}
  deriving (Show, Eq, Generic)

instance Validity Daylight

instance NFData Daylight

instance IsComponent Daylight where
  componentName Proxy = "DAYLIGHT"
  componentP = Daylight <$> observanceP
  componentB = observanceB . unDaylight

data Observance = Observance
  { -- @
    -- ; The following are REQUIRED,
    -- ; but MUST NOT occur more than once.
    -- ;
    -- dtstart / tzoffsetto / tzoffsetfrom /
    -- @
    -- @
    -- The mandatory "DTSTART" property gives the effective onset date
    -- and local time for the time zone sub-component definition.
    -- "DTSTART" in this usage MUST be specified as a date with a local
    -- time value.
    -- @
    observanceDateTimeStart :: !Time.LocalTime,
    observanceTimeZoneOffsetTo :: !TimeZoneOffsetTo,
    observanceTimeZoneOffsetFrom :: !TimeZoneOffsetFrom,
    -- @
    -- ; The following is OPTIONAL,
    -- ; but SHOULD NOT occur more than once.
    -- ;
    -- rrule /
    -- @
    observanceRecurrenceRule :: !(Set RecurrenceRule),
    -- @
    -- ; The following are OPTIONAL,
    -- ; and MAY occur more than once.
    -- ;
    -- comment / rdate / tzname / x-prop / iana-prop
    -- @
    observanceComment :: !(Set Comment),
    observanceTimeZoneName :: !(Set TimeZoneName)
  }
  deriving (Show, Eq, Generic)

instance Validity Observance where
  validate o@Observance {..} =
    mconcat
      [ genericValidate o,
        validateImpreciseLocalTime observanceDateTimeStart,
        validateDateTimeStartRRule (DateTimeStartDateTime (DateTimeFloating observanceDateTimeStart)) observanceRecurrenceRule
      ]

instance NFData Observance

observanceP :: CP Observance
observanceP = do
  observanceProperties <- takeWhileP (Just "observanceProperties") $ \ContentLine {..} ->
    not $
      contentLineName == "END"
        && ( contentLineValueRaw contentLineValue == "STANDARD"
               || contentLineValueRaw contentLineValue == "DAYLIGHT"
           )
  -- @
  -- The mandatory "DTSTART" property gives the effective onset date
  -- and local time for the time zone sub-component definition.
  -- "DTSTART" in this usage MUST be specified as a date with a local
  -- time value.
  -- @
  dtstart <- parseFirst observanceProperties
  observanceDateTimeStart <- case dtstart of
    DateTimeStartDate _ -> fail "DTSTART must be specified as a datetime, not a date."
    DateTimeStartDateTime dt -> case dt of
      DateTimeFloating lt -> pure lt
      _ -> fail "DTSTART must be specified as a date with a local time value."

  observanceTimeZoneOffsetTo <- parseFirst observanceProperties
  observanceTimeZoneOffsetFrom <- parseFirst observanceProperties
  observanceRecurrenceRule <- parseSet observanceProperties
  observanceComment <- parseSet observanceProperties
  observanceTimeZoneName <- parseSet observanceProperties
  pure Observance {..}

observanceB :: Observance -> DList ContentLine
observanceB Observance {..} =
  mconcat
    [ propertyListB (DateTimeStartDateTime (DateTimeFloating observanceDateTimeStart)),
      propertyListB observanceTimeZoneOffsetTo,
      propertyListB observanceTimeZoneOffsetFrom,
      propertySetB observanceRecurrenceRule,
      propertySetB observanceComment,
      propertySetB observanceTimeZoneName
    ]
