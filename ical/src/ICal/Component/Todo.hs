{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Component.Todo
  ( Todo (..),
    makeTodo,
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
-- Component Name:  VTODO
--
-- Purpose:  Provide a grouping of calendar properties that describe a
--    to-do.
-- Format Definition:  A "VTODO" calendar component is defined by the
--    following notation:
--
--     todoc      = "BEGIN" ":" "VTODO" CRLF
--                  todoprop *alarmc
--                  "END" ":" "VTODO" CRLF
--
--     todoprop   = *(
--                ;
--                ; The following are REQUIRED,
--                ; but MUST NOT occur more than once.
--                ;
--                dtstamp / uid /
--                ;
--                ; The following are OPTIONAL,
--                ; but MUST NOT occur more than once.
--                ;
--                class / completed / created / description /
--                dtstart / geo / last-mod / location / organizer /
--                percent / priority / recurid / seq / status /
--                summary / url /
--                ;
--                ; The following is OPTIONAL,
--                ; but SHOULD NOT occur more than once.
--                ;
--                rrule /
--                ;
--                ; Either 'due' or 'duration' MAY appear in
--                ; a 'todoprop', but 'due' and 'duration'
--                ; MUST NOT occur in the same 'todoprop'.
--                ; If 'duration' appear in a 'todoprop',
--                ; then 'dtstart' MUST also appear in
--                ; the same 'todoprop'.
--                ;
--                due / duration /
--                ;
--                ; The following are OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                attach / attendee / categories / comment / contact /
--                exdate / rstatus / related / resources /
--                rdate / x-prop / iana-prop
--                ;
--                )
--
-- Description:  A "VTODO" calendar component is a grouping of component
--    properties and possibly "VALARM" calendar components that
--    represent an action-item or assignment.  For example, it can be
--    used to represent an item of work assigned to an individual; such
--    as "turn in travel expense today".
--
--    The "VTODO" calendar component cannot be nested within another
--    calendar component.  However, "VTODO" calendar components can be
--    related to each other or to a "VEVENT" or to a "VJOURNAL" calendar
--    component with the "RELATED-TO" property.
--
--    A "VTODO" calendar component without the "DTSTART" and "DUE" (or
--    "DURATION") properties specifies a to-do that will be associated
--    with each successive calendar date, until it is completed.
--
-- Examples:  The following is an example of a "VTODO" calendar
--    component that needs to be completed before May 1st, 2007.  On
--    midnight May 1st, 2007 this to-do would be considered overdue.
--
--     BEGIN:VTODO
--     UID:20070313T123432Z-456553@example.com
--     DTSTAMP:20070313T123432Z
--     DUE;VALUE=DATE:20070501
--     SUMMARY:Submit Quebec Income Tax Return for 2006
--     CLASS:CONFIDENTIAL
--     CATEGORIES:FAMILY,FINANCE
--     STATUS:NEEDS-ACTION
--     END:VTODO
--
--    The following is an example of a "VTODO" calendar component that
--    was due before 1:00 P.M. UTC on July 9th, 2007 and was completed
--    on July 7th, 2007 at 10:00 A.M. UTC.
--
--     BEGIN:VTODO
--     UID:20070514T103211Z-123404@example.com
--     DTSTAMP:20070514T103211Z
--     DTSTART:20070514T110000Z
--     DUE:20070709T130000Z
--     COMPLETED:20070707T100000Z
--     SUMMARY:Submit Revised Internet-Draft
--     PRIORITY:1
--     STATUS:NEEDS-ACTION
--     END:VTODO
-- @
data Todo = Todo
  { -- @
    --                ;
    --                ; The following are REQUIRED,
    --                ; but MUST NOT occur more than once.
    --                ;
    --                dtstamp / uid /
    -- @
    todoDateTimeStamp :: DateTimeStamp,
    todoUID :: UID,
    -- @
    --                ;
    --                ; The following are OPTIONAL,
    --                ; but MUST NOT occur more than once.
    --                ;
    --                class / completed / created / description /
    -- @
    todoClassification :: !Classification,
    todoCompleted :: !(Maybe DateTimeCompleted),
    todoCreated :: !(Maybe Created),
    todoDescription :: !(Maybe Description),
    -- @
    --                dtstart / geo / last-mod / location / organizer /
    -- @
    todoDateTimeStart :: !(Maybe DateTimeStart),
    todoGeographicPosition :: !(Maybe GeographicPosition),
    todoLastModified :: !(Maybe LastModified),
    todoLocation :: !(Maybe Location),
    todoOrganizer :: !(Maybe Organizer),
    todoPercentComplete :: !(Maybe PercentComplete),
    -- @
    --                percent / priority / recurid / seq / status /
    -- @
    todoPriority :: !Priority,
    todoRecurrenceIdentifier :: !(Maybe RecurrenceIdentifier),
    todoSequenceNumber :: !SequenceNumber,
    todoStatus :: !(Maybe Status),
    -- @
    --                summary / url /
    -- @
    todoSummary :: !(Maybe Summary),
    todoURL :: !(Maybe URL),
    -- @
    --                ;
    --                ; The following is OPTIONAL,
    --                ; but SHOULD NOT occur more than once.
    --                ;
    --                rrule /
    -- @
    todoRecurrenceRules :: !(Set RecurrenceRule),
    -- @
    --                ;
    --                ; Either 'due' or 'duration' MAY appear in
    --                ; a 'todoprop', but 'due' and 'duration'
    --                ; MUST NOT occur in the same 'todoprop'.
    --                ; If 'duration' appear in a 'todoprop',
    --                ; then 'dtstart' MUST also appear in
    --                ; the same 'todoprop'.
    --                ;
    --                due / duration /
    -- @
    todoDateTimeDueDuration :: !(Maybe (Either DateTimeDue Duration)),
    -- @
    --                ;
    --                ; The following are OPTIONAL,
    --                ; and MAY occur more than once.
    --                ;
    --                attach / attendee / categories / comment / contact /
    --                exdate / rstatus / related / resources /
    --                rdate / x-prop / iana-prop
    -- @
    todoAttachments :: !(Set Attachment),
    todoAttendees :: !(Set Attendee),
    todoCategories :: !(Set Categories),
    todoComments :: !(Set Comment),
    todoContacts :: !(Set Contact),
    todoExceptionDateTimes :: !(Set ExceptionDateTimes),
    todoRequestStatusses :: !(Set RequestStatus),
    todoRelatedTos :: !(Set RelatedTo),
    todoResources :: !(Set Resources),
    todoRecurrenceDateTimes :: !(Set RecurrenceDateTimes)
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Todo where
  validate t@Todo {..} =
    mconcat
      [ genericValidate t,
        validateMDateTimeStartRRule todoDateTimeStart todoRecurrenceRules
      ]

instance NFData Todo

instance IsComponent Todo where
  componentName Proxy = "VTODO"
  componentP = vTodoP
  componentB = vTodoB

vTodoP :: Component -> CP Todo
vTodoP Component {..} = do
  todoDateTimeStamp <- requiredPropertyP componentProperties
  todoUID <- requiredPropertyP componentProperties

  todoClassification <- optionalPropertyWithDefaultP defaultClassification componentProperties
  todoCompleted <- optionalPropertyP componentProperties
  todoCreated <- optionalPropertyP componentProperties
  todoDescription <- optionalPropertyP componentProperties
  todoDateTimeStart <- optionalPropertyP componentProperties
  todoGeographicPosition <- optionalPropertyP componentProperties
  todoLastModified <- optionalPropertyP componentProperties
  todoLocation <- optionalPropertyP componentProperties
  todoOrganizer <- optionalPropertyP componentProperties
  todoPercentComplete <- optionalPropertyP componentProperties
  todoPriority <- optionalPropertyWithDefaultP defaultPriority componentProperties
  todoRecurrenceIdentifier <- optionalPropertyP componentProperties
  todoSequenceNumber <- optionalPropertyWithDefaultP defaultSequenceNumber componentProperties
  todoStatus <- optionalPropertyP componentProperties
  todoSummary <- optionalPropertyP componentProperties
  todoURL <- optionalPropertyP componentProperties

  todoRecurrenceRules <-
    S.fromList
      <$> (listOfPropertiesP componentProperties >>= traverse (fixUntil todoDateTimeStart))
  when (S.size todoRecurrenceRules > 1) $ emitWarning $ WarnMultipleRecurrenceRules todoRecurrenceRules

  mDue <- optionalPropertyP componentProperties
  mDuration <- optionalPropertyP componentProperties
  let todoDateTimeDueDuration = case (mDue, mDuration) of
        (Nothing, Nothing) -> Nothing
        (Nothing, Just d) -> Just (Right d)
        -- Not failing to parse if both are present. TODO emit a warning or fixable error
        (Just e, _) -> Just (Left e)

  todoAttachments <- setOfPropertiesP componentProperties
  todoAttendees <- setOfPropertiesP componentProperties
  todoCategories <- setOfPropertiesP componentProperties
  todoComments <- setOfPropertiesP componentProperties
  todoContacts <- setOfPropertiesP componentProperties
  todoExceptionDateTimes <- setOfPropertiesP componentProperties
  todoRequestStatusses <- setOfPropertiesP componentProperties
  todoRelatedTos <- setOfPropertiesP componentProperties
  todoResources <- setOfPropertiesP componentProperties
  todoRecurrenceDateTimes <- setOfPropertiesP componentProperties

  pure Todo {..}

vTodoB :: Todo -> Component
vTodoB Todo {..} =
  Component
    { componentProperties =
        M.unionsWith
          (<>)
          [ requiredPropertyB todoDateTimeStamp,
            requiredPropertyB todoUID,
            optionalPropertyWithDefaultB defaultClassification todoClassification,
            optionalPropertyB todoCompleted,
            optionalPropertyB todoCreated,
            optionalPropertyB todoDescription,
            optionalPropertyB todoDateTimeStart,
            optionalPropertyB todoGeographicPosition,
            optionalPropertyB todoLastModified,
            optionalPropertyB todoLocation,
            optionalPropertyB todoOrganizer,
            optionalPropertyB todoPercentComplete,
            optionalPropertyWithDefaultB defaultPriority todoPriority,
            optionalPropertyB todoRecurrenceIdentifier,
            optionalPropertyWithDefaultB defaultSequenceNumber todoSequenceNumber,
            optionalPropertyB todoStatus,
            optionalPropertyB todoSummary,
            optionalPropertyB todoURL,
            setOfPropertiesB todoRecurrenceRules,
            case todoDateTimeDueDuration of
              Nothing -> mempty
              Just (Left due) -> requiredPropertyB due
              Just (Right duration) -> requiredPropertyB duration,
            setOfPropertiesB todoAttachments,
            setOfPropertiesB todoAttendees,
            setOfPropertiesB todoCategories,
            setOfPropertiesB todoComments,
            setOfPropertiesB todoContacts,
            setOfPropertiesB todoExceptionDateTimes,
            setOfPropertiesB todoRequestStatusses,
            setOfPropertiesB todoRelatedTos,
            setOfPropertiesB todoResources,
            setOfPropertiesB todoRecurrenceDateTimes
          ],
      componentSubcomponents = mempty
    }

makeTodo :: UID -> DateTimeStamp -> Todo
makeTodo uid dateTimeStamp =
  Todo
    { todoUID = uid,
      todoDateTimeStamp = dateTimeStamp,
      todoClassification = defaultClassification,
      todoCompleted = Nothing,
      todoCreated = Nothing,
      todoDescription = Nothing,
      todoDateTimeStart = Nothing,
      todoGeographicPosition = Nothing,
      todoLastModified = Nothing,
      todoLocation = Nothing,
      todoOrganizer = Nothing,
      todoPercentComplete = Nothing,
      todoPriority = defaultPriority,
      todoRecurrenceIdentifier = Nothing,
      todoSequenceNumber = defaultSequenceNumber,
      todoStatus = Nothing,
      todoSummary = Nothing,
      todoURL = Nothing,
      todoRecurrenceRules = S.empty,
      todoDateTimeDueDuration = Nothing,
      todoAttachments = S.empty,
      todoAttendees = S.empty,
      todoCategories = S.empty,
      todoComments = S.empty,
      todoContacts = S.empty,
      todoExceptionDateTimes = S.empty,
      todoRequestStatusses = S.empty,
      todoRelatedTos = S.empty,
      todoResources = S.empty,
      todoRecurrenceDateTimes = S.empty
    }
