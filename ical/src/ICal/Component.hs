{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Component
  ( module ICal.Component,
    module ICal.Component.Class,
    module ICal.Component.Event,
    module ICal.Component.TimeZone,
  )
where

import Control.DeepSeq
import Control.Monad
import Data.DList (DList (..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Proxy
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import GHC.Generics (Generic)
import ICal.Component.Class
import ICal.Component.Event
import ICal.Component.TimeZone
import ICal.Conformance
import ICal.ContentLine
import ICal.Parameter
import ICal.Property
import Text.Megaparsec

parseICalendarFromContentLines ::
  [ContentLine] ->
  Conform
    CalendarParseError
    CalendarParseFixableError
    CalendarParseWarning
    [Calendar]
parseICalendarFromContentLines =
  parseGeneralComponents >=> mapM (uncurry namedComponentP) . M.toList

parseVCalendarFromContentLines ::
  [ContentLine] ->
  Conform
    CalendarParseError
    CalendarParseFixableError
    CalendarParseWarning
    Calendar
parseVCalendarFromContentLines =
  parseGeneralComponent >=> uncurry namedComponentP

iCalendarB :: [Calendar] -> DList ContentLine
iCalendarB = foldMap renderGeneralComponents . map namedComponentB

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
--
-- TODO fulfill this SHOULD:
-- @
-- Applications that
-- support importing iCalendar objects SHOULD support all of the
-- component types defined in this document, and SHOULD NOT silently
-- drop any components as that can lead to user data loss.
-- @
data Calendar = Calendar
  { -- @
    --                ; The following are REQUIRED,
    --                ; but MUST NOT occur more than once.
    --                ;
    --                prodid / version /
    -- @
    calendarProdId :: !ProdId,
    calendarVersion :: !Version,
    -- @
    --                ;
    --                ; The following are OPTIONAL,
    --                ; but MUST NOT occur more than once.
    --                ;
    --                calscale / method /
    -- @
    calendarCalendarScale :: !CalendarScale,
    calendarMethod :: !(Maybe Method),
    -- TODO:
    -- @
    --                ;
    --                ; The following are OPTIONAL,
    --                ; and MAY occur more than once.
    --                ;
    --                x-prop / iana-prop
    --                ;
    --                )
    -- @
    -- @
    --     component  = 1*(eventc / todoc / journalc / freebusyc /
    --                  timezonec / iana-comp / x-comp)
    -- @
    calendarEvents :: ![Event],
    calendarTimeZones :: ![TimeZone]
  }
  deriving (Show, Eq, Generic)

instance Validity Calendar

instance NFData Calendar

instance IsComponent Calendar where
  componentName Proxy = "VCALENDAR"
  componentP Component {..} = do
    -- TODO implement a warning for this SHOULD:
    -- @
    -- The Calendar Properties are attributes that apply to the iCalendar
    -- object, as a whole.  These properties do not appear within a calendar
    -- component.  They SHOULD be specified after the "BEGIN:VCALENDAR"
    -- delimiter string and prior to any calendar component.
    -- @

    calendarProdId <- requiredProperty componentProperties
    calendarVersion <- requiredProperty componentProperties

    calendarCalendarScale <- fromMaybe defaultCalendarScale <$> optionalProperty componentProperties
    calendarMethod <- optionalProperty componentProperties

    calendarTimeZones <- subComponentsP componentSubcomponents
    calendarEvents <- subComponentsP componentSubcomponents

    pure $ Calendar {..}

  componentB Calendar {..} =
    Component
      { componentProperties =
          M.unionsWith
            (<>)
            [ requiredPropertyB calendarProdId,
              requiredPropertyB calendarVersion,
              optionalPropertyWithDefaultB defaultCalendarScale calendarCalendarScale,
              optionalPropertyB calendarMethod
            ],
        componentSubcomponents =
          M.unionsWith
            (<>)
            [ subComponentsB calendarEvents,
              subComponentsB calendarTimeZones
            ]
      }

makeCalendar :: ProdId -> Calendar
makeCalendar prodId =
  Calendar
    { calendarProdId = prodId,
      calendarVersion = version20,
      calendarCalendarScale = defaultCalendarScale,
      calendarMethod = Nothing,
      calendarEvents = [],
      calendarTimeZones = []
    }

calendarTimeZoneMap :: Calendar -> Map TZIDParam TimeZone
calendarTimeZoneMap = makeTimeZoneMap . calendarTimeZones

makeTimeZoneMap :: [TimeZone] -> Map TZIDParam TimeZone
makeTimeZoneMap = M.fromList . map (\tz -> (tzidParam $ timeZoneId tz, tz))
