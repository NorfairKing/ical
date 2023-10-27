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

import Control.DeepSeq
import qualified Data.Map.Strict as M
import Data.Proxy
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import GHC.Generics (Generic)
import ICal.Component.Class
import ICal.Property

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
    todoLocation :: !(Maybe LastModified),
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
    todoURL :: !(Maybe URL)
    -- @
    --                ;
    --                ; The following is OPTIONAL,
    --                ; but SHOULD NOT occur more than once.
    --                ;
    --                rrule /
    -- @
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
    -- @
    --                ;
    --                ; The following are OPTIONAL,
    --                ; and MAY occur more than once.
    --                ;
    --                attach / attendee / categories / comment / contact /
    --                exdate / rstatus / related / resources /
    --                rdate / x-prop / iana-prop
    -- @
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Todo where
  validate t@Todo {} =
    mconcat
      [ genericValidate t
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
  todoLocation <- optionalPropertyP componentProperties
  todoOrganizer <- optionalPropertyP componentProperties
  todoPercentComplete <- optionalPropertyP componentProperties
  todoPriority <- optionalPropertyWithDefaultP defaultPriority componentProperties
  todoRecurrenceIdentifier <- optionalPropertyP componentProperties
  todoSequenceNumber <- optionalPropertyWithDefaultP defaultSequenceNumber componentProperties
  todoStatus <- optionalPropertyP componentProperties
  todoSummary <- optionalPropertyP componentProperties
  todoURL <- optionalPropertyP componentProperties

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
            optionalPropertyB todoLocation,
            optionalPropertyB todoOrganizer,
            optionalPropertyB todoPercentComplete,
            optionalPropertyWithDefaultB defaultPriority todoPriority,
            optionalPropertyB todoRecurrenceIdentifier,
            optionalPropertyWithDefaultB defaultSequenceNumber todoSequenceNumber,
            optionalPropertyB todoStatus,
            optionalPropertyB todoSummary,
            optionalPropertyB todoURL
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
      todoLocation = Nothing,
      todoOrganizer = Nothing,
      todoPercentComplete = Nothing,
      todoPriority = defaultPriority,
      todoRecurrenceIdentifier = Nothing,
      todoSequenceNumber = defaultSequenceNumber,
      todoStatus = Nothing,
      todoSummary = Nothing,
      todoURL = Nothing
    }
