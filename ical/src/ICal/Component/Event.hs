{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Component.Event where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans
import Data.DList (DList (..))
import Data.Maybe
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import GHC.Generics (Generic)
import ICal.Component.Class
import ICal.Conformance
import ICal.ContentLine
import ICal.Property
import ICal.PropertyType
import Text.Megaparsec

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

  eventRecurrenceRules <- S.fromList <$> (parseList eventProperties >>= traverse (fixUntil eventDateTimeStart))
  when (S.size eventRecurrenceRules > 1) $ lift $ emitWarning $ WarnMultipleRecurrenceRules eventRecurrenceRules

  mEnd <- parseFirstMaybe eventProperties
  mDuration <- parseFirstMaybe eventProperties
  let eventDateTimeEndDuration = case (mEnd, mDuration) of
        (Nothing, Nothing) -> Nothing
        (Nothing, Just d) -> Just (Right d)
        (Just e, _) -> Just (Left e) -- Not failing to parse if both are present.
  pure Event {..}

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
