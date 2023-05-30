{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Parameter where

import Control.DeepSeq
import Control.Exception
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Proxy
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import Data.String
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text
import Data.Validity.Time ()
import Data.Void
import GHC.Generics (Generic)
import ICal.Conformance
import ICal.ContentLine
import Text.Megaparsec

deriving instance Ord s => Ord (PosState s)

deriving instance (Ord s, Ord (Token s), Ord e) => Ord (ParseError s e)

deriving instance (Ord s, Ord (Token s), Ord e) => Ord (ParseErrorBundle s e)

data ParameterParseError
  = ParameterNotFound !ParamName !(Map ParamName (NonEmpty ParamValue))
  | MultipleParametersfound !(NonEmpty ParamValue)
  | UnknownRSVPExpectation !ParamValue -- TODO we can turn this into a fixable error by guessing the default value.
  | UnknownAlarmTriggerRelationship !ParamValue -- TODO we can turn this into a fixable error by guessing the default value.
  deriving (Show, Eq, Ord)

instance Exception ParameterParseError where
  displayException = \case
    ParameterNotFound name m ->
      unlines
        [ "Parameter not found: " <> show name,
          "while looking through these parameters:",
          show m
        ]
    MultipleParametersfound values ->
      unlines
        [ "Multiple parameter values found where one was expected.",
          "values:",
          show values
        ]
    UnknownRSVPExpectation pv ->
      unlines
        [ "Unknown RSVP Value:",
          show pv
        ]
    UnknownAlarmTriggerRelationship pv ->
      unlines
        [ "Unknown RELATED Value:",
          show pv
        ]

-- | Parameters
--
-- === Laws
--
-- * The 'NonEmpty ParamValue' that is built is valid:
--
-- >>> forAllValid $ \parameter -> isValid (parameterB parameter)
--
-- * Anything parsed is valid:
--
-- >>> forAllValid $ \paramValues -> isValid (parameterP paramValues)
--
-- * The parameter roundtrips through 'ContentLineValue'.
--
-- >>> forAllValid $ \parameter -> parameterP (parameterB parameter) == Right parameter
class IsParameter param where
  -- Name of the parameter
  parameterName :: Proxy param -> ParamName

  -- | Parser for the parameter
  parameterP :: NonEmpty ParamValue -> Conform ParameterParseError Void Void param

  -- | Builder for the parameter
  parameterB :: param -> NonEmpty ParamValue

lookupParam :: forall param. IsParameter param => Map ParamName (NonEmpty ParamValue) -> Maybe (Conform ParameterParseError Void Void param)
lookupParam m = do
  let name = parameterName (Proxy :: Proxy param)
  pvs <- M.lookup name m
  pure $ parameterP pvs

optionalParam :: forall param. IsParameter param => Map ParamName (NonEmpty ParamValue) -> Conform ParameterParseError Void Void (Maybe param)
optionalParam m =
  let name = parameterName (Proxy :: Proxy param)
   in mapM parameterP (M.lookup name m)

optionalParamSet ::
  forall param.
  (Ord param, IsParameter param) =>
  Map ParamName (NonEmpty ParamValue) ->
  Conform ParameterParseError Void Void (Maybe (Set param))
optionalParamSet m =
  let name = parameterName (Proxy :: Proxy param)
   in mapM
        (fmap S.fromList . mapM (parameterP . (:| [])) . NE.toList)
        (M.lookup name m)

requireParam :: forall param. IsParameter param => Map ParamName (NonEmpty ParamValue) -> Conform ParameterParseError Void Void param
requireParam m = case lookupParam m of
  Just errOrResult -> errOrResult
  Nothing -> unfixableError $ ParameterNotFound (parameterName (Proxy :: Proxy param)) m

paramMap :: forall param. IsParameter param => param -> Map ParamName (NonEmpty ParamValue)
paramMap param = M.singleton (parameterName (Proxy :: Proxy param)) (parameterB param)

setParamMap :: forall param. IsParameter param => Set param -> Map ParamName (NonEmpty ParamValue)
setParamMap params = case NE.nonEmpty (map parameterB (S.toList params)) of
  Nothing -> M.empty
  Just ne -> M.singleton (parameterName (Proxy :: Proxy param)) (sconcat ne)

insertParam :: forall param. IsParameter param => param -> ContentLineValue -> ContentLineValue
insertParam param clv = clv {contentLineValueParams = M.insert (parameterName (Proxy :: Proxy param)) (parameterB param) (contentLineValueParams clv)}

insertParamWithDefault :: forall param. (Eq param, IsParameter param) => param -> param -> ContentLineValue -> ContentLineValue
insertParamWithDefault defaultParam param clv =
  if param == defaultParam
    then clv
    else insertParam param clv

singleParamP :: (ParamValue -> Conform ParameterParseError Void Void a) -> NonEmpty ParamValue -> Conform ParameterParseError Void Void a
singleParamP func = \case
  value :| [] -> func value
  ne -> unfixableError $ MultipleParametersfound ne

-- TODO figure out if this text should be case-insensitive
anySingleParamP :: (CI Text -> Conform ParameterParseError Void Void a) -> NonEmpty ParamValue -> Conform ParameterParseError Void Void a
anySingleParamP func = singleParamP $ \case
  UnquotedParam c -> func c
  QuotedParam t -> func (CI.mk t)

singleParamB :: (a -> ParamValue) -> a -> NonEmpty ParamValue
singleParamB func = (:| []) . func

-- TODO figure out if this text should be case-insensitive
anySingleParamB :: (a -> CI Text) -> a -> NonEmpty ParamValue
anySingleParamB func = singleParamB $ \a ->
  let ci = func a
      o = CI.original ci
   in if haveToQuoteText o
        then QuotedParam o
        else UnquotedParam ci

-- | Common name
--
-- [section 3.2.2](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.2)
-- @
-- Parameter Name:  CN
--
-- Purpose:  To specify the common name to be associated with the
--    calendar user specified by the property.
--
-- Format Definition:  This property parameter is defined by the
--    following notation:
--
--   cnparam    = "CN" "=" param-value
--
-- Description:  This parameter can be specified on properties with a
--    CAL-ADDRESS value type.  The parameter specifies the common name
--    to be associated with the calendar user specified by the property.
--    The parameter value is text.  The parameter value can be used for
--    display text to be associated with the calendar address specified
--    by the property.
--
-- Example:
--
--     ORGANIZER;CN="John Smith":mailto:jsmith@example.com
-- @
newtype CommonName = CommonName {unCommonName :: ParamValue}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString)

instance Validity CommonName

instance NFData CommonName

instance IsParameter CommonName where
  parameterName Proxy = "CN"
  parameterP = singleParamP $ pure . CommonName
  parameterB = singleParamB unCommonName

-- | Participation status
--
-- [section 3.2.12](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.12)
--
-- @
-- Parameter Name:  PARTSTAT
--
-- Purpose:  To specify the participation status for the calendar user
--    specified by the property.
--
-- Format Definition:  This property parameter is defined by the
--    following notation:
--
--     partstatparam    = "PARTSTAT" "="
--                       (partstat-event
--                      / partstat-todo
--                      / partstat-jour)
--
--     partstat-event   = ("NEEDS-ACTION"    ; Event needs action
--                      / "ACCEPTED"         ; Event accepted
--                      / "DECLINED"         ; Event declined
--                      / "TENTATIVE"        ; Event tentatively
--                                           ; accepted
--                      / "DELEGATED"        ; Event delegated
--                      / x-name             ; Experimental status
--                      / iana-token)        ; Other IANA-registered
--                                           ; status
--     ; These are the participation statuses for a "VEVENT".
--     ; Default is NEEDS-ACTION.
--
--     partstat-todo    = ("NEEDS-ACTION"    ; To-do needs action
--                      / "ACCEPTED"         ; To-do accepted
--                      / "DECLINED"         ; To-do declined
--                      / "TENTATIVE"        ; To-do tentatively
--                                           ; accepted
--
--
--
-- ruisseaux                Standards Track                    [Page 22]
--
--
--  5545                       iCalendar                  September 2009
--
--
--                      / "DELEGATED"        ; To-do delegated
--                      / "COMPLETED"        ; To-do completed
--                                           ; COMPLETED property has
--                                           ; DATE-TIME completed
--                      / "IN-PROCESS"       ; To-do in process of
--                                           ; being completed
--                      / x-name             ; Experimental status
--                      / iana-token)        ; Other IANA-registered
--                                           ; status
--     ; These are the participation statuses for a "VTODO".
--     ; Default is NEEDS-ACTION.
--
--
--
--     partstat-jour    = ("NEEDS-ACTION"    ; Journal needs action
--                      / "ACCEPTED"         ; Journal accepted
--                      / "DECLINED"         ; Journal declined
--                      / x-name             ; Experimental status
--                      / iana-token)        ; Other IANA-registered
--                                           ; status
--     ; These are the participation statuses for a "VJOURNAL".
--     ; Default is NEEDS-ACTION.
--
-- Description:  This parameter can be specified on properties with a
--    CAL-ADDRESS value type.  The parameter identifies the
--    participation status for the calendar user specified by the
--    property value.  The parameter values differ depending on whether
--    they are associated with a group-scheduled "VEVENT", "VTODO", or
--    "VJOURNAL".  The values MUST match one of the values allowed for
--    the given calendar component.  If not specified on a property that
--    allows this parameter, the default value is NEEDS-ACTION.
--    Applications MUST treat x-name and iana-token values they don't
--    recognize the same way as they would the NEEDS-ACTION value.
--
-- Example:
--
--     ATTENDEE;PARTSTAT=DECLINED:mailto:jsmith@example.com
-- @
data ParticipationStatus
  = ParticipationStatusNeedsAction
  | ParticipationStatusAccepted
  | ParticipationStatusDeclined
  | ParticipationStatusTentative
  | ParticipationStatusDelegated
  | ParticipationStatusCompleted
  | ParticipationStatusInProcess
  | ParticipationStatusOther !ParamValue
  deriving stock (Show, Eq, Ord, Generic)

instance Validity ParticipationStatus

instance NFData ParticipationStatus

instance IsParameter ParticipationStatus where
  parameterName Proxy = "PARTSTAT"
  parameterP =
    singleParamP $
      pure
        . ( \pv -> case paramValueCI pv of
              "NEEDS-ACTION" -> ParticipationStatusNeedsAction
              "ACCEPTED" -> ParticipationStatusAccepted
              "DECLINED" -> ParticipationStatusDeclined
              "TENTATIVE" -> ParticipationStatusTentative
              "DELEGATED" -> ParticipationStatusDelegated
              "COMPLETED" -> ParticipationStatusCompleted
              "IN-PROCESS" -> ParticipationStatusInProcess
              _ -> ParticipationStatusOther pv
          )
  parameterB = singleParamB $ \case
    ParticipationStatusNeedsAction -> "NEEDS-ACTION"
    ParticipationStatusAccepted -> "ACCEPTED"
    ParticipationStatusDeclined -> "DECLINED"
    ParticipationStatusTentative -> "TENTATIVE"
    ParticipationStatusDelegated -> "DELEGATED"
    ParticipationStatusCompleted -> "COMPLETED"
    ParticipationStatusInProcess -> "IN-PROCESS"
    ParticipationStatusOther pv -> pv

-- | Default participation status
--
-- @
--     ; Default is NEEDS-ACTION.
-- @
defaultParticipationStatus :: ParticipationStatus
defaultParticipationStatus = ParticipationStatusNeedsAction

-- | RSVP Expectation
--
-- [section 3.2.17](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.17)
--
-- @
-- Parameter Name:  RSVP
--
-- Purpose:  To specify whether there is an expectation of a favor of a
--    reply from the calendar user specified by the property value.
--
-- Format Definition:  This property parameter is defined by the
--    following notation:
--
--     rsvpparam = "RSVP" "=" ("TRUE" / "FALSE")
--     ; Default is FALSE
--
-- Description:  This parameter can be specified on properties with a
--    CAL-ADDRESS value type.  The parameter identifies the expectation
--    of a reply from the calendar user specified by the property value.
--    This parameter is used by the "Organizer" to request a
--    participation status reply from an "Attendee" of a group-scheduled
--    event or to-do.  If not specified on a property that allows this
--    parameter, the default value is FALSE.
--
-- Example:
--
--     ATTENDEE;RSVP=TRUE:mailto:jsmith@example.com
-- @
data RSVPExpectation
  = RSVPExpectationTrue
  | RSVPExpectationFalse
  deriving stock (Show, Eq, Ord, Generic)

instance Validity RSVPExpectation

instance NFData RSVPExpectation

instance IsParameter RSVPExpectation where
  parameterName Proxy = "RSVP"
  parameterP = singleParamP $ \pv -> case paramValueCI pv of
    "TRUE" -> pure RSVPExpectationTrue
    "FALSE" -> pure RSVPExpectationFalse
    _ -> unfixableError $ UnknownRSVPExpectation pv
  parameterB = singleParamB $ \case
    RSVPExpectationTrue -> "TRUE"
    RSVPExpectationFalse -> "FALSE"

-- | Default RSVP Expectation
--
-- @
--     ; Default is FALSE
-- @
defaultRSVPExpectation :: RSVPExpectation
defaultRSVPExpectation = RSVPExpectationFalse

-- | Participation role
--
-- [section 3.2.16](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.16)
--
-- @
-- Parameter Name:  ROLE
--
-- Purpose:  To specify the participation role for the calendar user
--    specified by the property.
--
-- Format Definition:  This property parameter is defined by the
--    following notation:
--
--     roleparam  = "ROLE" "="
--                 ("CHAIR"             ; Indicates chair of the
--                                      ; calendar entity
--                / "REQ-PARTICIPANT"   ; Indicates a participant whose
--                                      ; participation is required
--                / "OPT-PARTICIPANT"   ; Indicates a participant whose
--                                      ; participation is optional
--                / "NON-PARTICIPANT"   ; Indicates a participant who
--                                      ; is copied for information
--                                      ; purposes only
--                / x-name              ; Experimental role
--                / iana-token)         ; Other IANA role
--     ; Default is REQ-PARTICIPANT
--
-- Description:  This parameter can be specified on properties with a
--    CAL-ADDRESS value type.  The parameter specifies the participation
--    role for the calendar user specified by the property in the group
--    schedule calendar component.  If not specified on a property that
--    allows this parameter, the default value is REQ-PARTICIPANT.
--    Applications MUST treat x-name and iana-token values they don't
--    recognize the same way as they would the REQ-PARTICIPANT value.
--
-- Example:
--
--     ATTENDEE;ROLE=CHAIR:mailto:mrbig@example.com
-- @
data ParticipationRole
  = ParticipationRoleChair
  | ParticipationRoleRequiredParticipant
  | ParticipationRoleOptionalParticipant
  | ParticipationRoleNonParticipant
  | ParticipationRoleOther !ParamValue
  deriving stock (Show, Eq, Ord, Generic)

instance Validity ParticipationRole

instance NFData ParticipationRole

instance IsParameter ParticipationRole where
  parameterName Proxy = "ROLE"
  parameterP =
    singleParamP $
      pure
        . ( \pv -> case paramValueCI pv of
              "CHAIR" -> ParticipationRoleChair
              "REQ-PARTICIPANT" -> ParticipationRoleRequiredParticipant
              "OPT-PARTICIPANT" -> ParticipationRoleOptionalParticipant
              "NON-PARTICIPANT" -> ParticipationRoleNonParticipant
              _ -> ParticipationRoleOther pv
          )
  parameterB = singleParamB $ \case
    ParticipationRoleChair -> "CHAIR"
    ParticipationRoleRequiredParticipant -> "REQ-PARTICIPANT"
    ParticipationRoleOptionalParticipant -> "OPT-PARTICIPANT"
    ParticipationRoleNonParticipant -> "NON-PARTICIPANT"
    ParticipationRoleOther pv -> pv

-- | Default participation role
--
-- @
--     ; Default is REQ-PARTICIPANT
-- @
defaultParticipationRole :: ParticipationRole
defaultParticipationRole = ParticipationRoleRequiredParticipant

-- | Time Zone Identifier
--
-- [section 3.2.19](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.19)
--
-- @
-- Parameter Name:  TZID
--
-- Purpose:  To specify the identifier for the time zone definition for
--    a time component in the property value.
--
-- Format Definition:  This property parameter is defined by the
--    following notation:
--
--     tzidparam  = "TZID" "=" [tzidprefix] paramtext
--
--     tzidprefix = "/"
--
-- Description:  This parameter MUST be specified on the "DTSTART",
--    "DTEND", "DUE", "EXDATE", and "RDATE" properties when either a
--    DATE-TIME or TIME value type is specified and when the value is
--    neither a UTC or a "floating" time.  Refer to the DATE-TIME or
--    TIME value type definition for a description of UTC and "floating
--    time" formats.  This property parameter specifies a text value
--
--    that uniquely identifies the "VTIMEZONE" calendar component to be
--    used when evaluating the time portion of the property.  The value
--    of the "TZID" property parameter will be equal to the value of the
--    "TZID" property for the matching time zone definition.  An
--    individual "VTIMEZONE" calendar component MUST be specified for
--    each unique "TZID" parameter value specified in the iCalendar
--    object.
--
--    The parameter MUST be specified on properties with a DATE-TIME
--    value if the DATE-TIME is not either a UTC or a "floating" time.
--    Failure to include and follow VTIMEZONE definitions in iCalendar
--    objects may lead to inconsistent understanding of the local time
--    at any given location.
--
--    The presence of the SOLIDUS character as a prefix, indicates that
--    this "TZID" represents a unique ID in a globally defined time zone
--    registry (when such registry is defined).
--
--       Note: This document does not define a naming convention for
--       time zone identifiers.  Implementers may want to use the naming
--       conventions defined in existing time zone specifications such
--       as the public-domain TZ database [TZDB].  The specification of
--       globally unique time zone identifiers is not addressed by this
--       document and is left for future study.
--
--    The following are examples of this property parameter:
--
--     DTSTART;TZID=America/New_York:19980119T020000
--
--     DTEND;TZID=America/New_York:19980119T030000
--
--    The "TZID" property parameter MUST NOT be applied to DATE
--    properties and DATE-TIME or TIME properties whose time values are
--    specified in UTC.
--
--    The use of local time in a DATE-TIME or TIME value without the
--    "TZID" property parameter is to be interpreted as floating time,
--    regardless of the existence of "VTIMEZONE" calendar components in
--    the iCalendar object.
--
--    For more information, see the sections on the value types DATE-
--    TIME and TIME.
-- @
newtype TimeZoneIdentifierParam = TimeZoneIdentifierParam
  { unTimeZoneIdentifierParam ::
      -- We assume that TimeZoneIdentifier parameters are case-insensitive because the examples in the spec are not quoted and the spec says:
      -- @
      -- Property parameter values that are not in quoted-strings are case-
      -- insensitive.
      -- @
      CI Text
  }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, IsString, Read)

instance Validity TimeZoneIdentifierParam where
  validate p@(TimeZoneIdentifierParam ci) =
    mconcat
      [ genericValidate p,
        decorateText (CI.original ci) validateSafeChar
      ]

instance NFData TimeZoneIdentifierParam

instance IsParameter TimeZoneIdentifierParam where
  parameterName Proxy = "TZID"
  parameterP = anySingleParamP $ pure . TimeZoneIdentifierParam
  parameterB = anySingleParamB unTimeZoneIdentifierParam

-- | Value Data Type
--
-- [section 3.2.20](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.20)
--
-- @
-- Parameter Name:  VALUE
--
-- Purpose:  To explicitly specify the value type format for a property
--    value.
--
-- Format Definition:  This property parameter is defined by the
--    following notation:
--
--     valuetypeparam = "VALUE" "=" valuetype
--
--     valuetype  = ("BINARY"
--                / "BOOLEAN"
--                / "CAL-ADDRESS"
--                / "DATE"
--                / "DATE-TIME"
--                / "DURATION"
--                / "FLOAT"
--                / "INTEGER"
--                / "PERIOD"
--                / "RECUR"
--                / "TEXT"
--                / "TIME"
--                / "URI"
--                / "UTC-OFFSET"
--                / x-name
--                ; Some experimental iCalendar value type.
--                / iana-token)
--                ; Some other IANA-registered iCalendar value type.
--
-- Description:  This parameter specifies the value type and format of
--    the property value.  The property values MUST be of a single value
--    type.  For example, a "RDATE" property cannot have a combination
--    of DATE-TIME and TIME value types.
--
--    If the property's value is the default value type, then this
--    parameter need not be specified.  However, if the property's
--    default value type is overridden by some other allowable value
--    type, then this parameter MUST be specified.
--
--    Applications MUST preserve the value data for x-name and iana-
--    token values that they don't recognize without attempting to
--    interpret or parse the value data.
-- @
data ValueDataType
  = TypeBinary
  | TypeBoolean
  | TypeCalendarAddress
  | TypeDate
  | TypeDateTime
  | TypeDuration
  | TypeFloat
  | TypeInteger
  | TypePeriod
  | TypeRecur
  | TypeText
  | TypeTime
  | TypeURI
  | TypeUTCOffset
  | -- | Other value type
    --
    -- @
    -- Applications MUST preserve the value data for x-name and iana-
    -- token values that they don't recognize without attempting to
    -- interpret or parse the value data.
    -- @
    TypeOther !ParamValue
  deriving stock (Show, Eq, Ord, Generic)

instance Validity ValueDataType

instance NFData ValueDataType

instance IsParameter ValueDataType where
  parameterName Proxy = "VALUE"
  parameterP =
    singleParamP $
      pure
        . ( \pv -> case paramValueCI pv of
              "BINARY" -> TypeBinary
              "BOOLEAN" -> TypeBoolean
              "CAL-ADDRESS" -> TypeCalendarAddress
              "DATE" -> TypeDate
              "DATE-TIME" -> TypeDateTime
              "DURATION" -> TypeDuration
              "FLOAT" -> TypeFloat
              "INTEGER" -> TypeInteger
              "PERIOD" -> TypePeriod
              "RECUR" -> TypeRecur
              "TEXT" -> TypeText
              "TIME" -> TypeTime
              "URI" -> TypeURI
              "UTC-OFFSET" -> TypeUTCOffset
              _ -> TypeOther pv
          )
  parameterB = singleParamB $ \case
    TypeBinary -> "BINARY"
    TypeBoolean -> "BOOLEAN"
    TypeCalendarAddress -> "CAL-ADDRESS"
    TypeDate -> "DATE"
    TypeDateTime -> "DATE-TIME"
    TypeDuration -> "DURATION"
    TypeFloat -> "FLOAT"
    TypeInteger -> "INTEGER"
    TypePeriod -> "PERIOD"
    TypeRecur -> "RECUR"
    TypeText -> "TEXT"
    TypeTime -> "TIME"
    TypeURI -> "URI"
    TypeUTCOffset -> "UTC-OFFSET"
    TypeOther pv -> pv

-- | Alarm Trigger Relationship
--
-- [section 3.8.4.5](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.4.5)
--
-- @
-- Parameter Name:  RELATED
--
-- Purpose:  To specify the relationship of the alarm trigger with
--    respect to the start or end of the calendar component.
--
-- Format Definition:  This property parameter is defined by the
--    following notation:
--
--     trigrelparam       = "RELATED" "="
--                         ("START"       ; Trigger off of start
--                        / "END")        ; Trigger off of end
--
-- Description:  This parameter can be specified on properties that
--    specify an alarm trigger with a "DURATION" value type.  The
--    parameter specifies whether the alarm will trigger relative to the
--    start or end of the calendar component.  The parameter value START
--    will set the alarm to trigger off the start of the calendar
--    component; the parameter value END will set the alarm to trigger
--    off the end of the calendar component.  If the parameter is not
--    specified on an allowable property, then the default is START.
--
-- Example:
--
--     TRIGGER;RELATED=END:PT5M
--
-- @
data AlarmTriggerRelationship
  = AlarmTriggerRelationshipStart
  | AlarmTriggerRelationshipEnd
  deriving stock (Show, Eq, Ord, Generic)

instance Validity AlarmTriggerRelationship

instance NFData AlarmTriggerRelationship

instance IsParameter AlarmTriggerRelationship where
  parameterName Proxy = "RELATED"
  parameterP =
    singleParamP $ \pv -> case paramValueCI pv of
      "START" -> pure AlarmTriggerRelationshipStart
      "END" -> pure AlarmTriggerRelationshipEnd
      _ -> unfixableError $ UnknownAlarmTriggerRelationship pv
  parameterB = singleParamB $ \case
    AlarmTriggerRelationshipStart -> "START"
    AlarmTriggerRelationshipEnd -> "END"

-- @
-- If the parameter is not
-- specified on an allowable property, then the default is START.
-- @
defaultAlarmTriggerRelationship :: AlarmTriggerRelationship
defaultAlarmTriggerRelationship = AlarmTriggerRelationshipStart
