{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Parameter
  ( module ICal.Parameter,
    module ICal.Parameter.Class,
    module ICal.Parameter.ValueDataType,
  )
where

import Control.DeepSeq
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import Data.String
import Data.Text (Text)
import Data.Validity
import Data.Validity.Text
import Data.Validity.Time ()
import GHC.Generics (Generic)
import ICal.Conformance
import ICal.ContentLine
import ICal.Parameter.Class
import ICal.Parameter.ValueDataType
import ICal.PropertyType.CalAddress
import ICal.PropertyType.URI

-- | Alternate Text Representation
--
-- [section 3.2.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.1)
--
-- @
-- Parameter Name:  ALTREP
--
-- Purpose:  To specify an alternate text representation for the
--    property value.
--
-- Format Definition:  This property parameter is defined by the
--    following notation:
--
--   altrepparam = "ALTREP" "=" DQUOTE uri DQUOTE
--
-- Description:  This parameter specifies a URI that points to an
--    alternate representation for a textual property value.  A property
--    specifying this parameter MUST also include a value that reflects
--
--
--    the default representation of the text value.  The URI parameter
--    value MUST be specified in a quoted-string.
--
--       Note: While there is no restriction imposed on the URI schemes
--       allowed for this parameter, Content Identifier (CID) [RFC2392],
--       HTTP [RFC2616], and HTTPS [RFC2818] are the URI schemes most
--       commonly used by current implementations.
--
-- Example:
--
--     DESCRIPTION;ALTREP="CID:part3.msg.970415T083000@example.com":
--      Project XYZ Review Meeting will include the following agenda
--       items: (a) Market Overview\, (b) Finances\, (c) Project Man
--      agement
--
--    The "ALTREP" property parameter value might point to a "text/html"
--    content portion.
--
--     Content-Type:text/html
--     Content-Id:<part3.msg.970415T083000@example.com>
--
--     <html>
--       <head>
--        <title></title>
--       </head>
--       <body>
--         <p>
--           <b>Project XYZ Review Meeting</b> will include
--           the following agenda items:
--           <ol>
--             <li>Market Overview</li>
--             <li>Finances</li>
--             <li>Project Management</li>
--           </ol>
--         </p>
--       </body>
--     </html>
-- @
newtype AlternateTextRepresentation = AlternateTextRepresentation {unAlternateTextRepresentation :: URI}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString)

instance Validity AlternateTextRepresentation

instance NFData AlternateTextRepresentation

instance IsParameter AlternateTextRepresentation where
  parameterName Proxy = "ALTREP"
  parameterP = quotedParamP $ \t -> case parseURI t of
    Nothing -> unfixableError $ InvalidURI t
    Just uri -> pure $ AlternateTextRepresentation uri
  parameterB = quotedParamB $ renderURI . unAlternateTextRepresentation

-- | Common name
--
-- [section 3.2.2](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.2)
--
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
  parameterP = pure . CommonName
  parameterB = unCommonName

-- | Calendar User Type
--
-- [section 3.2.3](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.3)
--
-- @
-- Parameter Name:  CUTYPE
--
-- Purpose:  To identify the type of calendar user specified by the
--    property.
--
-- Format Definition:  This property parameter is defined by the
--    following notation:
--
--     cutypeparam        = "CUTYPE" "="
--                        ("INDIVIDUAL"   ; An individual
--                       / "GROUP"        ; A group of individuals
--                       / "RESOURCE"     ; A physical resource
--                       / "ROOM"         ; A room resource
--                       / "UNKNOWN"      ; Otherwise not known
--                       / x-name         ; Experimental type
--                       / iana-token)    ; Other IANA-registered
--                                        ; type
--     ; Default is INDIVIDUAL
--
-- Description:  This parameter can be specified on properties with a
--    CAL-ADDRESS value type.  The parameter identifies the type of
--    calendar user specified by the property.  If not specified on a
--    property that allows this parameter, the default is INDIVIDUAL.
--    Applications MUST treat x-name and iana-token values they don't
--    recognize the same way as they would the UNKNOWN value.
--
-- Example:
--
--     ATTENDEE;CUTYPE=GROUP:mailto:ietf-calsch@example.org
-- @
data CalendarUserType
  = CalendarUserTypeIndividual
  | CalendarUserTypeGroup
  | CalendarUserTypeResource
  | CalendarUserTypeRoom
  | CalendarUserTypeUnknown
  deriving stock (Show, Eq, Ord, Generic)

instance Validity CalendarUserType

instance NFData CalendarUserType

instance IsParameter CalendarUserType where
  parameterName Proxy = "CUTYPE"
  parameterP =
    pure . \case
      "INDIVIDUAL" -> CalendarUserTypeIndividual
      "GROUP" -> CalendarUserTypeGroup
      "RESOURCE" -> CalendarUserTypeResource
      "ROOM" -> CalendarUserTypeRoom
      "UNKNOWN" -> CalendarUserTypeUnknown
      -- @
      --    Applications MUST treat x-name and iana-token values they don't
      --    recognize the same way as they would the UNKNOWN value.
      -- @
      _ -> CalendarUserTypeUnknown
  parameterB = \case
    CalendarUserTypeIndividual -> "INDIVIDUAL"
    CalendarUserTypeGroup -> "GROUP"
    CalendarUserTypeResource -> "RESOURCE"
    CalendarUserTypeRoom -> "ROOM"
    CalendarUserTypeUnknown -> "UNKNOWN"

-- @
--     ; Default is INDIVIDUAL
-- @
defaultCalendarUserType :: CalendarUserType
defaultCalendarUserType = CalendarUserTypeIndividual

-- | Delegator
--
-- [section 3.2.4](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.4)
--
-- @
-- Parameter Name:  DELEGATED-FROM
--
-- Purpose:  To specify the calendar users that have delegated their
--    participation to the calendar user specified by the property.
--
-- Format Definition:  This property parameter is defined by the
--    following notation:
--
--     delfromparam       = "DELEGATED-FROM" "=" DQUOTE cal-address
--                           DQUOTE *("," DQUOTE cal-address DQUOTE)
--
-- Description:  This parameter can be specified on properties with a
--    CAL-ADDRESS value type.  This parameter specifies those calendar
--    users that have delegated their participation in a group-scheduled
--    event or to-do to the calendar user specified by the property.
--    The individual calendar address parameter values MUST each be
--    specified in a quoted-string.
--
-- Example:
--
--     ATTENDEE;DELEGATED-FROM="mailto:jsmith@example.com":mailto:
--      jdoe@example.com
-- @
newtype Delegator = Delegator {unDelegator :: CalAddress}
  deriving stock (Show, Eq, Ord, Generic)

instance Validity Delegator

instance NFData Delegator

instance IsParameter Delegator where
  parameterName Proxy = "DELEGATED-FROM"
  parameterP = quotedParamP $ \t -> case parseCalAddress t of
    Nothing -> unfixableError $ InvalidCalAddress t
    Just ca -> pure $ Delegator ca
  parameterB = quotedParamB $ renderCalAddress . unDelegator

-- | Delegatee
--
-- [section 3.2.4](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.4)
--
-- @
-- Parameter Name:  DELEGATED-TO
--
-- Purpose:  To specify the calendar users to whom the calendar user
--    specified by the property has delegated participation.
--
-- Format Definition:  This property parameter is defined by the
--    following notation:
--
--     deltoparam = "DELEGATED-TO" "=" DQUOTE cal-address DQUOTE
--                  *("," DQUOTE cal-address DQUOTE)
--
-- Description:  This parameter can be specified on properties with a
--    CAL-ADDRESS value type.  This parameter specifies those calendar
--    users whom have been delegated participation in a group-scheduled
--    event or to-do by the calendar user specified by the property.
--    The individual calendar address parameter values MUST each be
--    specified in a quoted-string.
--
-- Example:
--
--     ATTENDEE;DELEGATED-TO="mailto:jdoe@example.com","mailto:jqpublic
--      @example.com":mailto:jsmith@example.com
-- @
newtype Delegatee = Delegatee {unDelegatee :: CalAddress}
  deriving stock (Show, Eq, Ord, Generic)

instance Validity Delegatee

instance NFData Delegatee

instance IsParameter Delegatee where
  parameterName Proxy = "DELEGATED-TO"
  parameterP = quotedParamP $ \t -> case parseCalAddress t of
    Nothing -> unfixableError $ InvalidCalAddress t
    Just ca -> pure $ Delegatee ca
  parameterB = quotedParamB $ renderCalAddress . unDelegatee

-- | Directory Entry Reference
--
-- [section 3.2.4](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.4)
--
-- @
-- Parameter Name:  DIR
--
-- Purpose:  To specify reference to a directory entry associated with
--    the calendar user specified by the property.
--
-- Format Definition:  This property parameter is defined by the
--    following notation:
--
--     dirparam   = "DIR" "=" DQUOTE uri DQUOTE
--
-- Description:  This parameter can be specified on properties with a
--    CAL-ADDRESS value type.  The parameter specifies a reference to
--    the directory entry associated with the calendar user specified by
--    the property.  The parameter value is a URI.  The URI parameter
--    value MUST be specified in a quoted-string.
--
--       Note: While there is no restriction imposed on the URI schemes
--       allowed for this parameter, CID [RFC2392], DATA [RFC2397], FILE
--       [RFC1738], FTP [RFC1738], HTTP [RFC2616], HTTPS [RFC2818], LDAP
--       [RFC4516], and MID [RFC2392] are the URI schemes most commonly
--       used by current implementations.
--
-- Example:
--
--     ORGANIZER;DIR="ldap://example.com:6666/o=ABC%20Industries,
--      c=US???(cn=Jim%20Dolittle)":mailto:jimdo@example.com
-- @
newtype DirectoryEntryReference = DirectoryEntryReference {unDirectoryEntryReference :: URI}
  deriving stock (Show, Eq, Ord, Generic)

instance Validity DirectoryEntryReference

instance NFData DirectoryEntryReference

instance IsParameter DirectoryEntryReference where
  parameterName Proxy = "DIR"
  parameterP = quotedParamP $ \t -> case parseURI t of
    Nothing -> unfixableError $ InvalidURI t
    Just ca -> pure $ DirectoryEntryReference ca
  parameterB = quotedParamB $ renderURI . unDirectoryEntryReference

-- | Inline Encoding
--
-- [section 3.2.7](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.7)
--
-- @
-- Parameter Name:  ENCODING
--
-- Purpose:  To specify an alternate inline encoding for the property
--    value.
--
-- Format Definition:  This property parameter is defined by the
--    following notation:
--
--
--     encodingparam      = "ENCODING" "="
--                        ( "8BIT"
--        ; "8bit" text encoding is defined in [RFC2045]
--                        / "BASE64"
--        ; "BASE64" binary encoding format is defined in [RFC4648]
--                        )
--
-- Description:  This property parameter identifies the inline encoding
--    used in a property value.  The default encoding is "8BIT",
--    corresponding to a property value consisting of text.  The
--    "BASE64" encoding type corresponds to a property value encoded
--    using the "BASE64" encoding defined in [RFC2045].
--
--    If the value type parameter is ";VALUE=BINARY", then the inline
--    encoding parameter MUST be specified with the value
--    ";ENCODING=BASE64".
--
-- Example:
--
--   ATTACH;FMTTYPE=text/plain;ENCODING=BASE64;VALUE=BINARY:TG9yZW
--    0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2ljaW
--    5nIGVsaXQsIHNlZCBkbyBlaXVzbW9kIHRlbXBvciBpbmNpZGlkdW50IHV0IG
--    xhYm9yZSBldCBkb2xvcmUgbWFnbmEgYWxpcXVhLiBVdCBlbmltIGFkIG1pbm
--    ltIHZlbmlhbSwgcXVpcyBub3N0cnVkIGV4ZXJjaXRhdGlvbiB1bGxhbWNvIG
--    xhYm9yaXMgbmlzaSB1dCBhbGlxdWlwIGV4IGVhIGNvbW1vZG8gY29uc2VxdW
--    F0LiBEdWlzIGF1dGUgaXJ1cmUgZG9sb3IgaW4gcmVwcmVoZW5kZXJpdCBpbi
--    B2b2x1cHRhdGUgdmVsaXQgZXNzZSBjaWxsdW0gZG9sb3JlIGV1IGZ1Z2lhdC
--    BudWxsYSBwYXJpYXR1ci4gRXhjZXB0ZXVyIHNpbnQgb2NjYWVjYXQgY3VwaW
--    RhdGF0IG5vbiBwcm9pZGVudCwgc3VudCBpbiBjdWxwYSBxdWkgb2ZmaWNpYS
--    BkZXNlcnVudCBtb2xsaXQgYW5pbSBpZCBlc3QgbGFib3J1bS4=
-- @
data Encoding = Encoding8Bit | EncodingBase64
  deriving stock (Show, Eq, Ord, Generic)

instance Validity Encoding

instance NFData Encoding

instance IsParameter Encoding where
  parameterName Proxy = "ENCODING"
  parameterP = \case
    "8BIT" -> pure Encoding8Bit
    "BASE64" -> pure EncodingBase64
    pv -> unfixableError $ UnknownEncoding pv
  parameterB = \case
    Encoding8Bit -> "8BIT"
    EncodingBase64 -> "BASE64"

defaultEncoding :: Encoding
defaultEncoding = Encoding8Bit

-- | Format Type
--
-- [section 3.2.8](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.8)
--
-- @
-- Parameter Name:  FMTTYPE
--
-- Purpose:  To specify the content type of a referenced object.
--
-- Format Definition:  This property parameter is defined by the
--    following notation:
--
--     fmttypeparam = "FMTTYPE" "=" type-name "/" subtype-name
--                    ; Where "type-name" and "subtype-name" are
--                    ; defined in Section 4.2 of [RFC4288].
--
-- Description:  This parameter can be specified on properties that are
--    used to reference an object.  The parameter specifies the media
--    type [RFC4288] of the referenced object.  For example, on the
--    "ATTACH" property, an FTP type URI value does not, by itself,
--    necessarily convey the type of content associated with the
--    resource.  The parameter value MUST be the text for either an
--    IANA-registered media type or a non-standard media type.
--
-- Example:
--
--     ATTACH;FMTTYPE=application/msword:ftp://example.com/pub/docs/
--      agenda.doc
-- @
newtype FormatType = FormatType {unFormatType :: ParamValue} -- TODO something more specific than a ParamValue
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString)

instance Validity FormatType

instance NFData FormatType

instance IsParameter FormatType where
  parameterName Proxy = "FMTTYPE"
  parameterP = pure . FormatType
  parameterB = unFormatType

-- | Free/Busy Time Type
--
-- [section 3.2.9](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.9)
--
-- @
-- Parameter Name:  FBTYPE
--
-- Purpose:  To specify the free or busy time type.
--
-- Format Definition:  This property parameter is defined by the
--    following notation:
--
--     fbtypeparam        = "FBTYPE" "=" ("FREE" / "BUSY"
--                        / "BUSY-UNAVAILABLE" / "BUSY-TENTATIVE"
--                        / x-name
--              ; Some experimental iCalendar free/busy type.
--                        / iana-token)
--              ; Some other IANA-registered iCalendar free/busy type.
--
-- Description:  This parameter specifies the free or busy time type.
--    The value FREE indicates that the time interval is free for
--    scheduling.  The value BUSY indicates that the time interval is
--    busy because one or more events have been scheduled for that
--    interval.  The value BUSY-UNAVAILABLE indicates that the time
--    interval is busy and that the interval can not be scheduled.  The
--    value BUSY-TENTATIVE indicates that the time interval is busy
--    because one or more events have been tentatively scheduled for
--    that interval.  If not specified on a property that allows this
--    parameter, the default is BUSY.  Applications MUST treat x-name
--    and iana-token values they don't recognize the same way as they
--    would the BUSY value.
--
-- Example:  The following is an example of this parameter on a
--    "FREEBUSY" property.
--
--     FREEBUSY;FBTYPE=BUSY:19980415T133000Z/19980415T170000Z
-- @
data FreeBusyTimeType
  = FreeBusyTimeTypeFree
  | FreeBusyTimeTypeBusy
  | FreeBusyTimeTypeBusyUnavailable
  | FreeBusyTimeTypeBusyTentative
  deriving stock (Show, Eq, Ord, Generic)

instance Validity FreeBusyTimeType

instance NFData FreeBusyTimeType

instance IsParameter FreeBusyTimeType where
  parameterName Proxy = "FBTYPE"
  parameterP =
    pure . \case
      "FREE" -> FreeBusyTimeTypeFree
      "BUSY" -> FreeBusyTimeTypeBusy
      "BUSY-UNAVAILABLE" -> FreeBusyTimeTypeBusyUnavailable
      "BUSY-TENTATIVE" -> FreeBusyTimeTypeBusyTentative
      -- @
      --    Applications MUST treat x-name
      --    and iana-token values they don't recognize the same way as they
      --    would the BUSY value.
      -- @
      _ -> FreeBusyTimeTypeBusy
  parameterB = \case
    FreeBusyTimeTypeFree -> "FREE"
    FreeBusyTimeTypeBusy -> "BUSY"
    FreeBusyTimeTypeBusyUnavailable -> "BUSY-UNAVAILABLE"
    FreeBusyTimeTypeBusyTentative -> "BUSY-TENTATIVE"

-- @
-- If not specified on a property that allows this parameter, the default is BUSY.
-- @
defaultFreeBusyTimeType :: FreeBusyTimeType
defaultFreeBusyTimeType = FreeBusyTimeTypeBusy

-- | Language
--
-- [section 3.2.10](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.10)
--
-- @
--
-- Parameter Name:  LANGUAGE
--
-- Purpose:  To specify the language for text values in a property or
--    property parameter.
--
-- Format Definition:  This property parameter is defined by the
--    following notation:
--
--     languageparam = "LANGUAGE" "=" language
--
--     language = Language-Tag
--                ; As defined in [RFC5646].
--
-- Description:  This parameter identifies the language of the text in
--    the property value and of all property parameter values of the
--    property.  The value of the "LANGUAGE" property parameter is that
--    defined in [RFC5646].
--
--    For transport in a MIME entity, the Content-Language header field
--    can be used to set the default language for the entire body part.
--    Otherwise, no default language is assumed.
--
-- Example:  The following are examples of this parameter on the
--    "SUMMARY" and "LOCATION" properties:
--
--     SUMMARY;LANGUAGE=en-US:Company Holiday Party
--
--     LOCATION;LANGUAGE=en:Germany
--
--     LOCATION;LANGUAGE=no:Tyskland
-- @
newtype Language =
  -- TODO consider actually implementing the tag from RFC5646 here.
  Language {unLanguage :: ParamValue}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString)

instance Validity Language

instance NFData Language

instance IsParameter Language where
  parameterName Proxy = "LANGUAGE"
  parameterP = pure . Language
  parameterB = unLanguage

-- | Group or List Membership
--
-- [section 3.2.11](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.11)
--
-- @
-- Parameter Name:  MEMBER
--
-- Purpose:  To specify the group or list membership of the calendar
--    user specified by the property.
--
-- Format Definition:  This property parameter is defined by the
--    following notation:
--
--     memberparam        = "MEMBER" "=" DQUOTE cal-address DQUOTE
--                          *("," DQUOTE cal-address DQUOTE)
--
-- Description:  This parameter can be specified on properties with a
--    CAL-ADDRESS value type.  The parameter identifies the groups or
--    list membership for the calendar user specified by the property.
--    The parameter value is either a single calendar address in a
--    quoted-string or a COMMA-separated list of calendar addresses,
--    each in a quoted-string.  The individual calendar address
--    parameter values MUST each be specified in a quoted-string.
--
-- Example:
--
--     ATTENDEE;MEMBER="mailto:ietf-calsch@example.org":mailto:
--      jsmith@example.com
--
--     ATTENDEE;MEMBER="mailto:projectA@example.com","mailto:pr
--      ojectB@example.com":mailto:janedoe@example.com
-- @
newtype Membership = Membership {unMembership :: CalAddress}
  deriving stock (Show, Eq, Ord, Generic)

instance Validity Membership

instance NFData Membership

instance IsParameter Membership where
  parameterName Proxy = "MEMBER"
  parameterP = quotedParamP $ \t -> case parseCalAddress t of
    Nothing -> unfixableError $ InvalidCalAddress t
    Just ca -> pure $ Membership ca
  parameterB = quotedParamB $ renderCalAddress . unMembership

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
  parameterB = \case
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

-- | Recurrence Identifier Range
--
-- [section 3.2.13](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.13)
--
-- @
-- Parameter Name:  RANGE
--
-- Purpose:  To specify the effective range of recurrence instances from
--    the instance specified by the recurrence identifier specified by
--    the property.
--
-- Format Definition:  This property parameter is defined by the
--    following notation:
--
--     rangeparam = "RANGE" "=" "THISANDFUTURE"
--     ; To specify the instance specified by the recurrence identifier
--     ; and all subsequent recurrence instances.
--
-- Description:  This parameter can be specified on a property that
--    specifies a recurrence identifier.  The parameter specifies the
--    effective range of recurrence instances that is specified by the
--    property.  The effective range is from the recurrence identifier
--    specified by the property.  If this parameter is not specified on
--    an allowed property, then the default range is the single instance
--    specified by the recurrence identifier value of the property.  The
--    parameter value can only be "THISANDFUTURE" to indicate a range
--    defined by the recurrence identifier and all subsequent instances.
--    The value "THISANDPRIOR" is deprecated by this revision of
--    iCalendar and MUST NOT be generated by applications.
--
-- Example:
--
--     RECURRENCE-ID;RANGE=THISANDFUTURE:19980401T133000Z
-- @
data RecurrenceIdentifierRange
  = RecurrenceIdentifierRangeThisAndFuture
  deriving stock (Show, Eq, Ord, Generic)

instance Validity RecurrenceIdentifierRange

instance NFData RecurrenceIdentifierRange

instance IsParameter RecurrenceIdentifierRange where
  parameterName Proxy = "RANGE"
  parameterP = \case
    "THISANDFUTURE" -> pure RecurrenceIdentifierRangeThisAndFuture
    pv -> unfixableError $ UnknownRecurrenceIdentifierRange pv
  parameterB = \case
    RecurrenceIdentifierRangeThisAndFuture -> "THISANDFUTURE"

-- | Alarm Trigger Relationship
--
-- [section 3.2.14](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.14)
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
  parameterP = \case
    "START" -> pure AlarmTriggerRelationshipStart
    "END" -> pure AlarmTriggerRelationshipEnd
    pv -> unfixableError $ UnknownAlarmTriggerRelationship pv
  parameterB = \case
    AlarmTriggerRelationshipStart -> "START"
    AlarmTriggerRelationshipEnd -> "END"

-- @
-- If the parameter is not
-- specified on an allowable property, then the default is START.
-- @
defaultAlarmTriggerRelationship :: AlarmTriggerRelationship
defaultAlarmTriggerRelationship = AlarmTriggerRelationshipStart

-- | Relationship type
--
-- [section 3.2.15](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.15)
--
-- @
-- Parameter Name:  RELTYPE
--
-- Purpose:  To specify the type of hierarchical relationship associated
--    with the calendar component specified by the property.
--
-- Format Definition:  This property parameter is defined by the
--    following notation:
--
--     reltypeparam       = "RELTYPE" "="
--                         ("PARENT"    ; Parent relationship - Default
--                        / "CHILD"     ; Child relationship
--                        / "SIBLING"   ; Sibling relationship
--                        / iana-token  ; Some other IANA-registered
--                                      ; iCalendar relationship type
--                        / x-name)     ; A non-standard, experimental
--                                      ; relationship type
--
-- Description:  This parameter can be specified on a property that
--    references another related calendar.  The parameter specifies the
--    hierarchical relationship type of the calendar component
--    referenced by the property.  The parameter value can be PARENT, to
--    indicate that the referenced calendar component is a superior of
--    calendar component; CHILD to indicate that the referenced calendar
--    component is a subordinate of the calendar component; or SIBLING
--    to indicate that the referenced calendar component is a peer of
--    the calendar component.  If this parameter is not specified on an
--    allowable property, the default relationship type is PARENT.
--    Applications MUST treat x-name and iana-token values they don't
--    recognize the same way as they would the PARENT value.
--
-- Example:
--
--     RELATED-TO;RELTYPE=SIBLING:19960401-080045-4000F192713@
--      example.com
-- @
data RelationshipType
  = RelationshipTypeParent
  | RelationshipTypeChild
  | RelationshipTypeSibling
  deriving stock (Show, Eq, Ord, Generic)

instance Validity RelationshipType

instance NFData RelationshipType

instance IsParameter RelationshipType where
  parameterName Proxy = "RELTYPE"
  parameterP =
    pure . \case
      "PARENT" -> RelationshipTypeParent
      "CHILD" -> RelationshipTypeChild
      "SIBLING" -> RelationshipTypeSibling
      -- @
      --    Applications MUST treat x-name and iana-token values they don't
      --    recognize the same way as they would the PARENT value.
      -- @
      _ -> RelationshipTypeParent
  parameterB = \case
    RelationshipTypeParent -> "PARENT"
    RelationshipTypeChild -> "CHILD"
    RelationshipTypeSibling -> "SIBLING"

-- @
--    If this parameter is not specified on an
--    allowable property, the default relationship type is PARENT.
-- @
defaultRelationshipType :: RelationshipType
defaultRelationshipType = RelationshipTypeParent

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
    pure
      . ( \case
            "CHAIR" -> ParticipationRoleChair
            "REQ-PARTICIPANT" -> ParticipationRoleRequiredParticipant
            "OPT-PARTICIPANT" -> ParticipationRoleOptionalParticipant
            "NON-PARTICIPANT" -> ParticipationRoleNonParticipant
            pv -> ParticipationRoleOther pv
        )
  parameterB = \case
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
  parameterP = \case
    "TRUE" -> pure RSVPExpectationTrue
    "FALSE" -> pure RSVPExpectationFalse
    pv -> unfixableError $ UnknownRSVPExpectation pv
  parameterB = \case
    RSVPExpectationTrue -> "TRUE"
    RSVPExpectationFalse -> "FALSE"

-- | Default RSVP Expectation
--
-- @
--     ; Default is FALSE
-- @
defaultRSVPExpectation :: RSVPExpectation
defaultRSVPExpectation = RSVPExpectationFalse

-- | Sent By
--
-- [section 3.2.18](https://datatracker.ietf.org/doc/html/rfc5545#section-3.2.18)
--
-- @
-- Parameter Name:  SENT-BY
--
-- Purpose:  To specify the calendar user that is acting on behalf of
--    the calendar user specified by the property.
--
-- Format Definition:  This property parameter is defined by the
--    following notation:
--
--     sentbyparam        = "SENT-BY" "=" DQUOTE cal-address DQUOTE
--
-- Description:  This parameter can be specified on properties with a
--    CAL-ADDRESS value type.  The parameter specifies the calendar user
--    that is acting on behalf of the calendar user specified by the
--    property.  The parameter value MUST be a mailto URI as defined in
--    [RFC2368].  The individual calendar address parameter values MUST
--    each be specified in a quoted-string.
--
-- Example:
--
--     ORGANIZER;SENT-BY="mailto:sray@example.com":mailto:
--      jsmith@example.com
-- @
newtype SentBy = SentBy {unSentBy :: CalAddress}
  deriving stock (Show, Eq, Ord, Generic)

instance Validity SentBy

instance NFData SentBy

instance IsParameter SentBy where
  parameterName Proxy = "SENT-BY"
  parameterP = quotedParamP $ \t -> case parseCalAddress t of
    Nothing -> unfixableError $ InvalidCalAddress t
    Just ca -> pure $ SentBy ca
  parameterB = quotedParamB $ renderCalAddress . unSentBy

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
--
-- However, [Erratum 5505](https://www.rfc-editor.org/errata/eid5505) says:
--
-- @
-- Section 3.2.19 says:
--
--        tzidparam  = "TZID" "=" [tzidprefix] paramtext
--
-- It should say:
--
--        tzidparam  = "TZID" "=" [tzidprefix] param-value
--
-- Notes:
--
-- TZID appears to be the only parameter defined 5545 whose value can not be a
-- quoted string. This is problematic in that time zone IDs such as "GMT-03:00"
-- are beginning to appear (note the embedded colon). RFC 6868 has no mechanism
-- to quote a colon character, as it relies on such characters appearing within
-- a quoted string. I see no technical reason why a TZID parameter can not be
-- quoted, and existing implementations already accept quoted TZIDs.
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

-- | Display
--
-- [section 6.1 of RFC 7986](https://datatracker.ietf.org/doc/html/rfc7986#section-6.1)
--
-- @
-- Parameter Name:  DISPLAY
--
-- Purpose:  To specify different ways in which an image for a calendar
--    or component can be displayed.
-- Format Definition:  This property parameter is defined by the
--    following notation:
--
-- displayparam = "DISPLAY" "=" displayval *("," displayval)
--
-- displayval =  ("BADGE" /     ; image inline with the title of the
--                              ; even
--                "GRAPHIC" /   ; a full image replacement for the event
--                              ; itself
--                "FULLSIZE" /  ; an image that is used to enhance the
--                              ; event
--                "THUMBNAIL" / ; a smaller variant of "FULLSIZE" to be
--                              ; used when space for the image is
--                              ; constrained
--                x-name /      ; Experimental type
--                iana-token)   ; Other IANA-registered type
--                              ;
--                              ; Default is BADGE
--
-- Description:  This property parameter MAY be specified on "IMAGE"
--    properties.  In the absence of this parameter, the default value
--    "BADGE" MUST be used.  The value determines how a client ought to
--    present an image supplied in iCalendar data to the user.
--
--    Values for this parameter are registered with IANA as per
--    Section 9.3.1.  New values can be added to this registry following
--    the procedure outlined in Section 8.2.1 of [RFC5545].
--
--    Servers and clients MUST handle x-name and iana-token values they
--    don't recognize by not displaying any image at all.
--
-- Example:
--
-- IMAGE;VALUE=URI;DISPLAY=BADGE,THUMBNAIL;FMTTYPE=image/png:https://exa
--  mple.com/images/weather-cloudy.png
-- @
data Display
  = DisplayBadge
  | DisplayGraphic
  | DisplayFullSize
  | DisplayThumbnail
  | DisplayOther !ParamValue
  deriving stock (Show, Eq, Ord, Generic)

instance Validity Display

instance NFData Display

instance IsParameter Display where
  parameterName Proxy = "DISPLAY"
  parameterP =
    pure . \case
      "BADGE" -> DisplayBadge
      "GRAPHIC" -> DisplayGraphic
      "FULLSIZE" -> DisplayFullSize
      "THUMBNAIL" -> DisplayThumbnail
      pv -> DisplayOther pv
  parameterB = \case
    DisplayBadge -> "BADGE"
    DisplayGraphic -> "GRAPHIC"
    DisplayFullSize -> "FULLSIZE"
    DisplayThumbnail -> "THUMBNAIL"
    DisplayOther pv -> pv

-- @
-- In the absence of this parameter, the default value
-- "BADGE" MUST be used.
-- @
defaultDisplay :: NonEmpty Display
defaultDisplay = DisplayBadge :| []
