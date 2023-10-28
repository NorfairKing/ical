{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Component.FreeBusy
  ( FreeBusy (..),
    makeFreeBusy,
  )
where

import Control.DeepSeq
import qualified Data.Map.Strict as M
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import GHC.Generics (Generic)
import ICal.Component.Class
import ICal.Property

-- | Free/Busy component
--
-- === [section 3.6.4](https://datatracker.ietf.org/doc/html/rfc5545#section-3.6.4)
--
-- @
-- Component Name:  VFREEBUSY
--
-- Purpose:  Provide a grouping of component properties that describe
--    either a request for free/busy time, describe a response to a
--    request for free/busy time, or describe a published set of busy
--    time.
--
-- Format Definition:  A "VFREEBUSY" calendar component is defined by
--    the following notation:
--
--     freebusyc  = "BEGIN" ":" "VFREEBUSY" CRLF
--                  fbprop
--                  "END" ":" "VFREEBUSY" CRLF
--
--     fbprop     = *(
--                ;
--                ; The following are REQUIRED,
--                ; but MUST NOT occur more than once.
--                ;
--                dtstamp / uid /
--                ;
--                ; The following are OPTIONAL,
--                ; but MUST NOT occur more than once.
--                ;
--                contact / dtstart / dtend /
--                organizer / url /
--                ;
--                ; The following are OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                attendee / comment / freebusy / rstatus / x-prop /
--                iana-prop
--                ;
--                )
--
-- Description:  A "VFREEBUSY" calendar component is a grouping of
--    component properties that represents either a request for free or
--    busy time information, a reply to a request for free or busy time
--    information, or a published set of busy time information.
--
--    When used to request free/busy time information, the "ATTENDEE"
--    property specifies the calendar users whose free/busy time is
--    being requested; the "ORGANIZER" property specifies the calendar
--    user who is requesting the free/busy time; the "DTSTART" and
--    "DTEND" properties specify the window of time for which the free/
--    busy time is being requested; the "UID" and "DTSTAMP" properties
--    are specified to assist in proper sequencing of multiple free/busy
--    time requests.
--
--    When used to reply to a request for free/busy time, the "ATTENDEE"
--    property specifies the calendar user responding to the free/busy
--    time request; the "ORGANIZER" property specifies the calendar user
--    that originally requested the free/busy time; the "FREEBUSY"
--    property specifies the free/busy time information (if it exists);
--    and the "UID" and "DTSTAMP" properties are specified to assist in
--    proper sequencing of multiple free/busy time replies.
--
--    When used to publish busy time, the "ORGANIZER" property specifies
--    the calendar user associated with the published busy time; the
--    "DTSTART" and "DTEND" properties specify an inclusive time window
--    that surrounds the busy time information; the "FREEBUSY" property
--    specifies the published busy time information; and the "DTSTAMP"
--    property specifies the DATE-TIME that iCalendar object was
--    created.
--
--    The "VFREEBUSY" calendar component cannot be nested within another
--    calendar component.  Multiple "VFREEBUSY" calendar components can
--    be specified within an iCalendar object.  This permits the
--    grouping of free/busy information into logical collections, such
--    as monthly groups of busy time information.
--
--    The "VFREEBUSY" calendar component is intended for use in
--    iCalendar object methods involving requests for free time,
--    requests for busy time, requests for both free and busy, and the
--    associated replies.
--
--    Free/Busy information is represented with the "FREEBUSY" property.
--    This property provides a terse representation of time periods.
--    One or more "FREEBUSY" properties can be specified in the
--    "VFREEBUSY" calendar component.
--
--    When present in a "VFREEBUSY" calendar component, the "DTSTART"
--    and "DTEND" properties SHOULD be specified prior to any "FREEBUSY"
--    properties.
--
--    The recurrence properties ("RRULE", "RDATE", "EXDATE") are not
--    permitted within a "VFREEBUSY" calendar component.  Any recurring
--    events are resolved into their individual busy time periods using
--    the "FREEBUSY" property.
--
-- Example:  The following is an example of a "VFREEBUSY" calendar
--    component used to request free or busy time information:
--
--     BEGIN:VFREEBUSY
--     UID:19970901T082949Z-FA43EF@example.com
--     ORGANIZER:mailto:jane_doe@example.com
--     ATTENDEE:mailto:john_public@example.com
--     DTSTART:19971015T050000Z
--     DTEND:19971016T050000Z
--     DTSTAMP:19970901T083000Z
--     END:VFREEBUSY
--
--    The following is an example of a "VFREEBUSY" calendar component
--    used to reply to the request with busy time information:
--
--     BEGIN:VFREEBUSY
--     UID:19970901T095957Z-76A912@example.com
--     ORGANIZER:mailto:jane_doe@example.com
--     ATTENDEE:mailto:john_public@example.com
--     DTSTAMP:19970901T100000Z
--     FREEBUSY:19971015T050000Z/PT8H30M,
--      19971015T160000Z/PT5H30M,19971015T223000Z/PT6H30M
--     URL:http://example.com/pub/busy/jpublic-01.ifb
--     COMMENT:This iCalendar file contains busy time information for
--       the next three months.
--     END:VFREEBUSY
--
--    The following is an example of a "VFREEBUSY" calendar component
--    used to publish busy time information:
--
--     BEGIN:VFREEBUSY
--     UID:19970901T115957Z-76A912@example.com
--     DTSTAMP:19970901T120000Z
--     ORGANIZER:jsmith@example.com
--     DTSTART:19980313T141711Z
--     DTEND:19980410T141711Z
--     FREEBUSY:19980314T233000Z/19980315T003000Z
--     FREEBUSY:19980316T153000Z/19980316T163000Z
--     FREEBUSY:19980318T030000Z/19980318T040000Z
--     URL:http://www.example.com/calendar/busytime/jsmith.ifb
--     END:VFREEBUSY
-- @
data FreeBusy = FreeBusy
  { -- @
    --                ;
    --                ; The following are REQUIRED,
    --                ; but MUST NOT occur more than once.
    --                ;
    --                dtstamp / uid /
    -- @
    freebusyDateTimeStamp :: !DateTimeStamp,
    freebusyUID :: !UID,
    -- @
    --                ;
    --                ; The following are OPTIONAL,
    --                ; but MUST NOT occur more than once.
    --                ;
    --                contact / dtstart / dtend /
    --                organizer / url /
    -- @
    freebusyContact :: !(Maybe Contact),
    freebusyDateTimeStart :: !(Maybe DateTimeStart),
    freebusyDateTimeEnd :: !(Maybe DateTimeEnd),
    freebusyOrganizer :: !(Maybe Organizer),
    freebusyURL :: !(Maybe URL),
    -- @
    --                ;
    --                ; The following are OPTIONAL,
    --                ; and MAY occur more than once.
    --                ;
    --                attendee / comment / freebusy / rstatus / x-prop /
    --                iana-prop
    -- @
    freebusyAttendees :: !(Set Attendee),
    freebusyComments :: !(Set Comment),
    freebusyFreeBusyIntervals :: !(Set FreeBusyIntervals),
    freebusyRequestStatuses :: !(Set RequestStatus)
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity FreeBusy

instance NFData FreeBusy

instance IsComponent FreeBusy where
  componentName Proxy = "VFREEBUSY"
  componentP = vFreeBusyP
  componentB = vFreeBusyB

vFreeBusyP :: Component -> CP FreeBusy
vFreeBusyP Component {..} = do
  freebusyDateTimeStamp <- requiredPropertyP componentProperties
  freebusyUID <- requiredPropertyP componentProperties

  freebusyContact <- optionalPropertyP componentProperties
  freebusyDateTimeStart <- optionalPropertyP componentProperties
  freebusyDateTimeEnd <- optionalPropertyP componentProperties
  freebusyOrganizer <- optionalPropertyP componentProperties
  freebusyURL <- optionalPropertyP componentProperties

  freebusyAttendees <- setOfPropertiesP componentProperties
  freebusyComments <- setOfPropertiesP componentProperties
  freebusyFreeBusyIntervals <- setOfPropertiesP componentProperties
  freebusyRequestStatuses <- setOfPropertiesP componentProperties

  pure FreeBusy {..}

vFreeBusyB :: FreeBusy -> Component
vFreeBusyB FreeBusy {..} =
  Component
    { componentProperties =
        M.unionsWith
          (<>)
          [ requiredPropertyB freebusyDateTimeStamp,
            requiredPropertyB freebusyUID,
            optionalPropertyB freebusyContact,
            optionalPropertyB freebusyDateTimeStart,
            optionalPropertyB freebusyDateTimeEnd,
            optionalPropertyB freebusyOrganizer,
            optionalPropertyB freebusyURL,
            setOfPropertiesB freebusyAttendees,
            setOfPropertiesB freebusyComments,
            setOfPropertiesB freebusyFreeBusyIntervals,
            setOfPropertiesB freebusyRequestStatuses
          ],
      componentSubcomponents = M.empty
    }

makeFreeBusy :: UID -> DateTimeStamp -> FreeBusy
makeFreeBusy uid dateTimeStamp =
  FreeBusy
    { freebusyUID = uid,
      freebusyDateTimeStamp = dateTimeStamp,
      freebusyContact = Nothing,
      freebusyDateTimeStart = Nothing,
      freebusyDateTimeEnd = Nothing,
      freebusyOrganizer = Nothing,
      freebusyURL = Nothing,
      freebusyAttendees = S.empty,
      freebusyComments = S.empty,
      freebusyFreeBusyIntervals = S.empty,
      freebusyRequestStatuses = S.empty
    }
