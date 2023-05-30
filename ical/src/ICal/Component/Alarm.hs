{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Component.Alarm
  ( Alarm (..),
    makeAudioAlarm,
    makeDisplayAlarm,
    makeEmailAlarm,
  )
where

import Control.DeepSeq
import Control.Monad
import qualified Data.Map.Strict as M
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
import ICal.Property
import ICal.PropertyType

-- | Alarm
--
-- === [section 3.6.6](https://datatracker.ietf.org/doc/html/rfc5545#section-3.6.6)
--
-- @
-- Component Name:  VALARM
--
-- Purpose:  Provide a grouping of component properties that define an
--    alarm.
--
-- Format Definition:  A "VALARM" calendar component is defined by the
--    following notation:
--
--     alarmc     = "BEGIN" ":" "VALARM" CRLF
--                  (audioprop / dispprop / emailprop)
--                  "END" ":" "VALARM" CRLF
--
--     audioprop  = *(
--                ;
--                ; 'action' and 'trigger' are both REQUIRED,
--
--
--                ; but MUST NOT occur more than once.
--                ;
--                action / trigger /
--                ;
--                ; 'duration' and 'repeat' are both OPTIONAL,
--                ; and MUST NOT occur more than once each;
--                ; but if one occurs, so MUST the other.
--                ;
--                duration / repeat /
--                ;
--                ; The following is OPTIONAL,
--                ; but MUST NOT occur more than once.
--                ;
--                attach /
--                ;
--                ; The following is OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                x-prop / iana-prop
--                ;
--                )
--
--     dispprop   = *(
--                ;
--                ; The following are REQUIRED,
--                ; but MUST NOT occur more than once.
--                ;
--                action / description / trigger /
--                ;
--                ; 'duration' and 'repeat' are both OPTIONAL,
--                ; and MUST NOT occur more than once each;
--                ; but if one occurs, so MUST the other.
--                ;
--                duration / repeat /
--                ;
--                ; The following is OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                x-prop / iana-prop
--                ;
--                )
--
--     emailprop  = *(
--                ;
--                ; The following are all REQUIRED,
--                ; but MUST NOT occur more than once.
--                ;
--                action / description / trigger / summary /
--
--                ;
--                ; The following is REQUIRED,
--                ; and MAY occur more than once.
--                ;
--                attendee /
--                ;
--                ; 'duration' and 'repeat' are both OPTIONAL,
--                ; and MUST NOT occur more than once each;
--                ; but if one occurs, so MUST the other.
--                ;
--                duration / repeat /
--                ;
--                ; The following are OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                attach / x-prop / iana-prop
--                ;
--                )
--
-- Description:  A "VALARM" calendar component is a grouping of
--    component properties that is a reminder or alarm for an event or a
--    to-do.  For example, it may be used to define a reminder for a
--    pending event or an overdue to-do.
--
--    The "VALARM" calendar component MUST include the "ACTION" and
--    "TRIGGER" properties.  The "ACTION" property further constrains
--    the "VALARM" calendar component in the following ways:
--
--    When the action is "AUDIO", the alarm can also include one and
--    only one "ATTACH" property, which MUST point to a sound resource,
--    which is rendered when the alarm is triggered.
--
--    When the action is "DISPLAY", the alarm MUST also include a
--    "DESCRIPTION" property, which contains the text to be displayed
--    when the alarm is triggered.
--
--    When the action is "EMAIL", the alarm MUST include a "DESCRIPTION"
--    property, which contains the text to be used as the message body,
--    a "SUMMARY" property, which contains the text to be used as the
--    message subject, and one or more "ATTENDEE" properties, which
--    contain the email address of attendees to receive the message.  It
--    can also include one or more "ATTACH" properties, which are
--    intended to be sent as message attachments.  When the alarm is
--    triggered, the email message is sent.
--
--    The "VALARM" calendar component MUST only appear within either a
--    "VEVENT" or "VTODO" calendar component.  "VALARM" calendar
--    components cannot be nested.  Multiple mutually independent
--
--    "VALARM" calendar components can be specified for a single
--    "VEVENT" or "VTODO" calendar component.
--
--    The "TRIGGER" property specifies when the alarm will be triggered.
--    The "TRIGGER" property specifies a duration prior to the start of
--    an event or a to-do.  The "TRIGGER" edge may be explicitly set to
--    be relative to the "START" or "END" of the event or to-do with the
--    "RELATED" parameter of the "TRIGGER" property.  The "TRIGGER"
--    property value type can alternatively be set to an absolute
--    calendar date with UTC time.
--
--    In an alarm set to trigger on the "START" of an event or to-do,
--    the "DTSTART" property MUST be present in the associated event or
--    to-do.  In an alarm in a "VEVENT" calendar component set to
--    trigger on the "END" of the event, either the "DTEND" property
--    MUST be present, or the "DTSTART" and "DURATION" properties MUST
--    both be present.  In an alarm in a "VTODO" calendar component set
--    to trigger on the "END" of the to-do, either the "DUE" property
--    MUST be present, or the "DTSTART" and "DURATION" properties MUST
--    both be present.
--
--    The alarm can be defined such that it triggers repeatedly.  A
--    definition of an alarm with a repeating trigger MUST include both
--    the "DURATION" and "REPEAT" properties.  The "DURATION" property
--    specifies the delay period, after which the alarm will repeat.
--    The "REPEAT" property specifies the number of additional
--    repetitions that the alarm will be triggered.  This repetition
--    count is in addition to the initial triggering of the alarm.  Both
--    of these properties MUST be present in order to specify a
--    repeating alarm.  If one of these two properties is absent, then
--    the alarm will not repeat beyond the initial trigger.
--
--    The "ACTION" property is used within the "VALARM" calendar
--    component to specify the type of action invoked when the alarm is
--    triggered.  The "VALARM" properties provide enough information for
--    a specific action to be invoked.  It is typically the
--    responsibility of a "Calendar User Agent" (CUA) to deliver the
--    alarm in the specified fashion.  An "ACTION" property value of
--    AUDIO specifies an alarm that causes a sound to be played to alert
--    the user; DISPLAY specifies an alarm that causes a text message to
--    be displayed to the user; and EMAIL specifies an alarm that causes
--    an electronic email message to be delivered to one or more email
--    addresses.
--
--    In an AUDIO alarm, if the optional "ATTACH" property is included,
--    it MUST specify an audio sound resource.  The intention is that
--    the sound will be played as the alarm effect.  If an "ATTACH"
--    property is specified that does not refer to a sound resource, or
--
--    if the specified sound resource cannot be rendered (because its
--    format is unsupported, or because it cannot be retrieved), then
--    the CUA or other entity responsible for playing the sound may
--    choose a fallback action, such as playing a built-in default
--    sound, or playing no sound at all.
--
--    In a DISPLAY alarm, the intended alarm effect is for the text
--    value of the "DESCRIPTION" property to be displayed to the user.
--
--    In an EMAIL alarm, the intended alarm effect is for an email
--    message to be composed and delivered to all the addresses
--    specified by the "ATTENDEE" properties in the "VALARM" calendar
--    component.  The "DESCRIPTION" property of the "VALARM" calendar
--    component MUST be used as the body text of the message, and the
--    "SUMMARY" property MUST be used as the subject text.  Any "ATTACH"
--    properties in the "VALARM" calendar component SHOULD be sent as
--    attachments to the message.
--
--       Note: Implementations should carefully consider whether they
--       accept alarm components from untrusted sources, e.g., when
--       importing calendar objects from external sources.  One
--       reasonable policy is to always ignore alarm components that the
--       calendar user has not set herself, or at least ask for
--       confirmation in such a case.
--
-- Example:  The following example is for a "VALARM" calendar component
--    that specifies an audio alarm that will sound at a precise time
--    and repeat 4 more times at 15-minute intervals:
--
--     BEGIN:VALARM
--     TRIGGER;VALUE=DATE-TIME:19970317T133000Z
--     REPEAT:4
--     DURATION:PT15M
--     ACTION:AUDIO
--     ATTACH;FMTTYPE=audio/basic:ftp://example.com/pub/
--      sounds/bell-01.aud
--     END:VALARM
--
--    The following example is for a "VALARM" calendar component that
--    specifies a display alarm that will trigger 30 minutes before the
--    scheduled start of the event or of the to-do it is associated with
--    and will repeat 2 more times at 15-minute intervals:
--
--     BEGIN:VALARM
--     TRIGGER:-PT30M
--     REPEAT:2
--     DURATION:PT15M
--     ACTION:DISPLAY
--     DESCRIPTION:Breakfast meeting with executive\n
--      team at 8:30 AM EST.
--     END:VALARM
--
--    The following example is for a "VALARM" calendar component that
--    specifies an email alarm that will trigger 2 days before the
--    scheduled due DATE-TIME of a to-do with which it is associated.
--    It does not repeat.  The email has a subject, body, and attachment
--    link.
--
--     BEGIN:VALARM
--     TRIGGER;RELATED=END:-P2D
--     ACTION:EMAIL
--     ATTENDEE:mailto:john_doe@example.com
--     SUMMARY:*** REMINDER: SEND AGENDA FOR WEEKLY STAFF MEETING ***
--     DESCRIPTION:A draft agenda needs to be sent out to the attendees
--       to the weekly managers meeting (MGR-LIST). Attached is a
--       pointer the document template for the agenda file.
--     ATTACH;FMTTYPE=application/msword:http://example.com/
--      templates/agenda.doc
--     END:VALARM
-- @
data Alarm = Alarm
  { alarmAction :: !Action,
    alarmTrigger :: !Trigger,
    alarmDescription :: !(Maybe Description),
    alarmSummary :: !(Maybe Summary),
    --                ; 'duration' and 'repeat' are both OPTIONAL,
    --                ; and MUST NOT occur more than once each;
    --                ; but if one occurs, so MUST the other.
    alarmRepeatDuration :: !(Maybe (Repeat, Duration))
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Alarm where
  validate alarm@Alarm {..} =
    mconcat
      [ genericValidate alarm,
        case alarmAction of
          -- @
          --     audioprop  = *(
          --                ;
          --                ; 'action' and 'trigger' are both REQUIRED,
          --                ; but MUST NOT occur more than once.
          --                ;
          --                action / trigger /
          --                ;
          --                ; 'duration' and 'repeat' are both OPTIONAL,
          --                ; and MUST NOT occur more than once each;
          --                ; but if one occurs, so MUST the other.
          --                ;
          --                duration / repeat /
          --                ;
          --                ; The following is OPTIONAL,
          --                ; but MUST NOT occur more than once.
          --                ;
          --                attach /
          --                ;
          --                ; The following is OPTIONAL,
          --                ; and MAY occur more than once.
          --                ;
          --                x-prop / iana-prop
          --                ;
          --                )
          -- @
          ActionAudio -> mempty
          -- @
          --     dispprop   = *(
          --                ;
          --                ; The following are REQUIRED,
          --                ; but MUST NOT occur more than once.
          --                ;
          --                action / description / trigger /
          --                ;
          --                ; 'duration' and 'repeat' are both OPTIONAL,
          --                ; and MUST NOT occur more than once each;
          --                ; but if one occurs, so MUST the other.
          --                ;
          --                duration / repeat /
          --                ;
          --                ; The following is OPTIONAL,
          --                ; and MAY occur more than once.
          --                ;
          --                x-prop / iana-prop
          --                ;
          --                )
          -- @
          ActionDisplay ->
            mconcat
              [ declare "Has a description" $ isJust alarmDescription
              ]
          -- @
          --     emailprop  = *(
          --                ;
          --                ; The following are all REQUIRED,
          --                ; but MUST NOT occur more than once.
          --                ;
          --                action / description / trigger / summary /
          --
          --                ;
          --                ; The following is REQUIRED,
          --                ; and MAY occur more than once.
          --                ;
          --                attendee /
          --                ;
          --                ; 'duration' and 'repeat' are both OPTIONAL,
          --                ; and MUST NOT occur more than once each;
          --                ; but if one occurs, so MUST the other.
          --                ;
          --                duration / repeat /
          --                ;
          --                ; The following are OPTIONAL,
          --                ; and MAY occur more than once.
          --                ;
          --                attach / x-prop / iana-prop
          --                ;
          --                )
          -- @
          ActionEmail ->
            mconcat
              [ declare "Has a description" $ isJust alarmDescription,
                declare "Has a summary" $ isJust alarmSummary
              ]
          ActionOther _ -> mempty
      ]

instance NFData Alarm

instance IsComponent Alarm where
  componentName Proxy = "VALARM"
  componentP = vAlarmP
  componentB = vAlarmB

vAlarmP :: Component -> CP Alarm
vAlarmP Component {..} = do
  alarmAction <- requiredPropertyP componentProperties
  alarmTrigger <- requiredPropertyP componentProperties
  alarmDescription <- optionalPropertyP componentProperties
  alarmSummary <- optionalPropertyP componentProperties
  mRepeat <- optionalPropertyP componentProperties
  mDuration <- optionalPropertyP componentProperties
  alarmRepeatDuration <- case (mRepeat, mDuration) of
    (Just repeat, Just duration) -> pure $ Just (repeat, duration)
    (Nothing, Nothing) -> pure Nothing
    _ -> pure Nothing -- TODO emit a warning or fixable error
  pure Alarm {..}

vAlarmB :: Alarm -> Component
vAlarmB Alarm {..} =
  Component
    { componentProperties =
        M.unionsWith
          (<>)
          [ requiredPropertyB alarmAction,
            requiredPropertyB alarmTrigger,
            optionalPropertyB alarmDescription,
            optionalPropertyB alarmSummary,
            case alarmRepeatDuration of
              Nothing -> M.empty
              Just (repeat, duration) ->
                M.unionsWith
                  (<>)
                  [ requiredPropertyB repeat,
                    requiredPropertyB duration
                  ]
          ],
      componentSubcomponents = M.empty
    }

-- @
--     audioprop  = *(
--                ;
--                ; 'action' and 'trigger' are both REQUIRED,
--                ; but MUST NOT occur more than once.
--                ;
--                action / trigger /
--                ;
--                ; 'duration' and 'repeat' are both OPTIONAL,
--                ; and MUST NOT occur more than once each;
--                ; but if one occurs, so MUST the other.
--                ;
--                duration / repeat /
--                ;
--                ; The following is OPTIONAL,
--                ; but MUST NOT occur more than once.
--                ;
--                attach /
--                ;
--                ; The following is OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                x-prop / iana-prop
--                ;
--                )
-- @
makeAudioAlarm :: Trigger -> Alarm
makeAudioAlarm alarmTrigger =
  let alarmAction = ActionAudio
      alarmDescription = Nothing
      alarmSummary = Nothing
      alarmRepeatDuration = Nothing
   in Alarm {..}

-- @
--     dispprop   = *(
--                ;
--                ; The following are REQUIRED,
--                ; but MUST NOT occur more than once.
--                ;
--                action / description / trigger /
--                ;
--                ; 'duration' and 'repeat' are both OPTIONAL,
--                ; and MUST NOT occur more than once each;
--                ; but if one occurs, so MUST the other.
--                ;
--                duration / repeat /
--                ;
--                ; The following is OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                x-prop / iana-prop
--                ;
--                )
-- @
makeDisplayAlarm :: Description -> Trigger -> Alarm
makeDisplayAlarm description alarmTrigger =
  let alarmAction = ActionDisplay
      alarmDescription = Just description
      alarmSummary = Nothing
      alarmRepeatDuration = Nothing
   in Alarm {..}

-- @
--     emailprop  = *(
--                ;
--                ; The following are all REQUIRED,
--                ; but MUST NOT occur more than once.
--                ;
--                action / description / trigger / summary /
--
--                ;
--                ; The following is REQUIRED,
--                ; and MAY occur more than once.
--                ;
--                attendee /
--                ;
--                ; 'duration' and 'repeat' are both OPTIONAL,
--                ; and MUST NOT occur more than once each;
--                ; but if one occurs, so MUST the other.
--                ;
--                duration / repeat /
--                ;
--                ; The following are OPTIONAL,
--                ; and MAY occur more than once.
--                ;
--                attach / x-prop / iana-prop
--                ;
--                )
-- @
makeEmailAlarm :: Description -> Trigger -> Summary -> Alarm
makeEmailAlarm description alarmTrigger summary =
  let alarmAction = ActionEmail
      alarmDescription = Just description
      alarmSummary = Just summary
      alarmRepeatDuration = Nothing
   in Alarm {..}
