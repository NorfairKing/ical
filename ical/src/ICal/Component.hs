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
import Data.DList (DList (..))
import Data.Maybe
import Data.Proxy
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import Data.Void
import GHC.Generics (Generic)
import ICal.Component.Class
import ICal.Component.Event
import ICal.Component.TimeZone
import ICal.Conformance
import ICal.ContentLine
import ICal.Property
import Text.Megaparsec

parseICalendarFromContentLines :: [ContentLine] -> Conform (ParseErrorBundle [ContentLine] CalendarParseError) CalendarParseFixableError Void [Calendar]
parseICalendarFromContentLines = runCP iCalendarP

parseVCalendarFromContentLines :: [ContentLine] -> Conform (ParseErrorBundle [ContentLine] CalendarParseError) CalendarParseFixableError Void Calendar
parseVCalendarFromContentLines = parseComponentFromContentLines

iCalendarP :: CP [Calendar]
iCalendarP = many componentSectionP

iCalendarB :: [Calendar] -> DList ContentLine
iCalendarB = foldMap componentSectionB

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
