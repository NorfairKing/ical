{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Component.Journal
  ( Journal (..),
    makeJournal,
  )
where

import Conformance
import Control.DeepSeq
import Control.Monad
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
import ICal.PropertyType

-- | To-Do Component
--
-- === [section 3.6.2](https://datatracker.ietf.org/doc/html/rfc5545#section-3.6.2)
--
-- @
-- Component Name:  VJOURNAL
--
-- Purpose:  Provide a grouping of component properties that describe a
--    journal entry.
--
-- Format Definition:  A "VJOURNAL" calendar component is defined by the
--    following notation:
--
--     journalc   = "BEGIN" ":" "VJOURNAL" CRLF
--                  jourprop
--                  "END" ":" "VJOURNAL" CRLF
--
--     jourprop   = *(
--                ;
--                ; The following are REQUIRED,
--                ; but MUST NOT occur more than once.
--                ;
--                dtstamp / uid /
--                ;
--                ; The following are OPTIONAL,
--                ; but MUST NOT occur more than once.
--                ;
--                class / created / dtstart /
--                last-mod / organizer / recurid / seq /
--                status / summary / url /
--                ;
--                ; The following is OPTIONAL,
--                ; but SHOULD NOT occur more than once.
--                ;
--                rrule /
--                ;
--                ; The following are OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                attach / attendee / categories / comment /
--                contact / description / exdate / related / rdate /
--                rstatus / x-prop / iana-prop
--                ;
--                )
--
-- Description:  A "VJOURNAL" calendar component is a grouping of
--    component properties that represent one or more descriptive text
--    notes associated with a particular calendar date.  The "DTSTART"
--    property is used to specify the calendar date with which the
--    journal entry is associated.  Generally, it will have a DATE value
--    data type, but it can also be used to specify a DATE-TIME value
--    data type.  Examples of a journal entry include a daily record of
--    a legislative body or a journal entry of individual telephone
--    contacts for the day or an ordered list of accomplishments for the
--    day.  The "VJOURNAL" calendar component can also be used to
--    associate a document with a calendar date.
--
--    The "VJOURNAL" calendar component does not take up time on a
--    calendar.  Hence, it does not play a role in free or busy time
--    searches -- it is as though it has a time transparency value of
--    TRANSPARENT.  It is transparent to any such searches.
--
--    The "VJOURNAL" calendar component cannot be nested within another
--    calendar component.  However, "VJOURNAL" calendar components can
--    be related to each other or to a "VEVENT" or to a "VTODO" calendar
--    component, with the "RELATED-TO" property.
--
-- Example:  The following is an example of the "VJOURNAL" calendar
--    component:
--
--     BEGIN:VJOURNAL
--     UID:19970901T130000Z-123405@example.com
--     DTSTAMP:19970901T130000Z
--     DTSTART;VALUE=DATE:19970317
--     SUMMARY:Staff meeting minutes
--     DESCRIPTION:1. Staff meeting: Participants include Joe\,
--       Lisa\, and Bob. Aurora project plans were reviewed.
--       There is currently no budget reserves for this project.
--       Lisa will escalate to management. Next meeting on Tuesday.\n
--      2. Telephone Conference: ABC Corp. sales representative
--       called to discuss new printer. Promised to get us a demo by
--       Friday.\n3. Henry Miller (Handsoff Insurance): Car was
--       totaled by tree. Is looking into a loaner car. 555-2323
--       (tel).
--     END:VJOURNAL
-- @
data Journal = Journal
  { -- @
    --                ;
    --                ; The following are REQUIRED,
    --                ; but MUST NOT occur more than once.
    --                ;
    --                dtstamp / uid /
    -- @
    journalDateTimeStamp :: DateTimeStamp,
    journalUID :: UID,
    -- @
    --                ;
    --                ; The following are OPTIONAL,
    --                ; but MUST NOT occur more than once.
    --                ;
    --                class / created / dtstart /
    -- @
    journalClassification :: !Classification,
    journalCreated :: !(Maybe Created),
    journalDateTimeStart :: !(Maybe DateTimeStart),
    -- @
    --                last-mod / organizer / recurid / seq /
    -- @
    journalLastModified :: !(Maybe LastModified),
    journalOrganizer :: !(Maybe Organizer),
    journalRecurrenceIdentifier :: !(Maybe RecurrenceIdentifier),
    journalSequenceNumber :: !SequenceNumber,
    -- @
    --                status / summary / url /
    -- @
    journalStatus :: !(Maybe Status),
    journalSummary :: !(Maybe Summary),
    journalURL :: !(Maybe URL),
    -- @
    --                ;
    --                ; The following is OPTIONAL,
    --                ; but SHOULD NOT occur more than once.
    --                ;
    --                rrule /
    -- @
    journalRecurrenceRules :: !(Set RecurrenceRule),
    -- @
    --                ;
    --                ; The following are OPTIONAL,
    --                ; and MAY occur more than once.
    --                ;
    --                attach / attendee / categories / comment /
    -- @
    journalAttachments :: !(Set Attachment),
    journalAttendees :: !(Set Attendee),
    journalCategories :: !(Set Categories),
    journalComments :: !(Set Comment),
    -- @
    --                contact / description / exdate / related / rdate /
    -- @
    journalContacts :: !(Set Contact),
    journalDescriptions :: !(Set Description),
    journalExceptionDateTimes :: !(Set ExceptionDateTimes),
    journalRelatedTos :: !(Set RelatedTo),
    journalRecurrenceDateTimes :: !(Set RecurrenceDateTimes),
    -- @
    --                rstatus / x-prop / iana-prop
    -- @
    journalRequestStatusses :: !(Set RequestStatus)
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Journal where
  validate t@Journal {..} =
    mconcat
      [ genericValidate t,
        validateMDateTimeStartRRule journalDateTimeStart journalRecurrenceRules
      ]

instance NFData Journal

instance IsComponent Journal where
  componentName Proxy = "VJOURNAL"
  componentP = vJournalP
  componentB = vJournalB

vJournalP :: Component -> CP Journal
vJournalP Component {..} = do
  journalDateTimeStamp <- requiredPropertyP componentProperties
  journalUID <- requiredPropertyP componentProperties

  journalClassification <- optionalPropertyWithDefaultP defaultClassification componentProperties
  journalCreated <- optionalPropertyP componentProperties
  journalDateTimeStart <- optionalPropertyP componentProperties
  journalLastModified <- optionalPropertyP componentProperties
  journalOrganizer <- optionalPropertyP componentProperties
  journalRecurrenceIdentifier <- optionalPropertyP componentProperties
  journalSequenceNumber <- optionalPropertyWithDefaultP defaultSequenceNumber componentProperties
  journalStatus <- optionalPropertyP componentProperties
  journalSummary <- optionalPropertyP componentProperties
  journalURL <- optionalPropertyP componentProperties

  journalRecurrenceRules <-
    S.fromList
      <$> (listOfPropertiesP componentProperties >>= traverse (fixUntil journalDateTimeStart))
  when (S.size journalRecurrenceRules > 1) $ emitWarning $ WarnMultipleRecurrenceRules journalRecurrenceRules

  journalAttachments <- setOfPropertiesP componentProperties
  journalAttendees <- setOfPropertiesP componentProperties
  journalCategories <- setOfPropertiesP componentProperties
  journalComments <- setOfPropertiesP componentProperties
  journalContacts <- setOfPropertiesP componentProperties
  journalDescriptions <- setOfPropertiesP componentProperties
  journalExceptionDateTimes <- setOfPropertiesP componentProperties
  journalRequestStatusses <- setOfPropertiesP componentProperties
  journalRelatedTos <- setOfPropertiesP componentProperties
  journalRecurrenceDateTimes <- setOfPropertiesP componentProperties

  pure Journal {..}

vJournalB :: Journal -> Component
vJournalB Journal {..} =
  Component
    { componentProperties =
        M.unionsWith
          (<>)
          [ requiredPropertyB journalDateTimeStamp,
            requiredPropertyB journalUID,
            optionalPropertyWithDefaultB defaultClassification journalClassification,
            optionalPropertyB journalCreated,
            optionalPropertyB journalDateTimeStart,
            optionalPropertyB journalLastModified,
            optionalPropertyB journalOrganizer,
            optionalPropertyB journalRecurrenceIdentifier,
            optionalPropertyWithDefaultB defaultSequenceNumber journalSequenceNumber,
            optionalPropertyB journalStatus,
            optionalPropertyB journalSummary,
            optionalPropertyB journalURL,
            setOfPropertiesB journalRecurrenceRules,
            setOfPropertiesB journalAttachments,
            setOfPropertiesB journalAttendees,
            setOfPropertiesB journalCategories,
            setOfPropertiesB journalComments,
            setOfPropertiesB journalContacts,
            setOfPropertiesB journalDescriptions,
            setOfPropertiesB journalExceptionDateTimes,
            setOfPropertiesB journalRequestStatusses,
            setOfPropertiesB journalRelatedTos,
            setOfPropertiesB journalRecurrenceDateTimes
          ],
      componentSubcomponents = mempty
    }

makeJournal :: UID -> DateTimeStamp -> Journal
makeJournal uid dateTimeStamp =
  Journal
    { journalUID = uid,
      journalDateTimeStamp = dateTimeStamp,
      journalClassification = defaultClassification,
      journalCreated = Nothing,
      journalDateTimeStart = Nothing,
      journalLastModified = Nothing,
      journalOrganizer = Nothing,
      journalRecurrenceIdentifier = Nothing,
      journalSequenceNumber = defaultSequenceNumber,
      journalStatus = Nothing,
      journalSummary = Nothing,
      journalURL = Nothing,
      journalRecurrenceRules = S.empty,
      journalAttachments = S.empty,
      journalAttendees = S.empty,
      journalCategories = S.empty,
      journalComments = S.empty,
      journalContacts = S.empty,
      journalDescriptions = S.empty,
      journalExceptionDateTimes = S.empty,
      journalRequestStatusses = S.empty,
      journalRelatedTos = S.empty,
      journalRecurrenceDateTimes = S.empty
    }
