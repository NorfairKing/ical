{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.ParameterSpec where

import ICal.Parameter
import ICal.Parameter.Gen
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "CommonName" $ do
    genValidSpec @CommonName
    parameterSpec @CommonName
    -- From the spec:
    -- @
    -- Example:
    --
    --     ORGANIZER;CN="John Smith":mailto:jsmith@example.com
    -- @
    parameterExampleSpec ["John Smith"] (CommonName "John Smith")

    -- @
    -- ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=TENTATIVE;CN=Henry
    --  Cabot:mailto:hcabot@example.com
    -- ATTENDEE;ROLE=REQ-PARTICIPANT;DELEGATED-FROM="mailto:bob@
    --  example.com";PARTSTAT=ACCEPTED;CN=Jane Doe:mailto:jdoe@
    --  example.com
    -- @
    parameterExampleSpec ["Henry Cabot"] (CommonName "Henry Cabot")
    parameterExampleSpec ["Jane Doe"] (CommonName "Jane Doe")
    -- @
    -- ATTENDEE;CN=John Smith;DIR="ldap://example.com:6666/o=ABC%
    --  20Industries,c=US???(cn=Jim%20Dolittle)":mailto:jimdo@
    --  example.com
    -- @
    parameterExampleSpec ["John Smith"] (CommonName "John Smith")

    -- @
    -- ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=TENTATIVE;DELEGATED-FROM=
    --  "mailto:iamboss@example.com";CN=Henry Cabot:mailto:hcabot@
    --  example.com
    -- ATTENDEE;ROLE=NON-PARTICIPANT;PARTSTAT=DELEGATED;DELEGATED-TO=
    --  "mailto:hcabot@example.com";CN=The Big Cheese:mailto:iamboss
    --  @example.com
    -- ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=ACCEPTED;CN=Jane Doe
    --  :mailto:jdoe@example.com
    -- @
    parameterExampleSpec ["The Big Cheese"] (CommonName "The Big Cheese")
    -- @
    --     ORGANIZER;CN=John Smith:mailto:jsmith@example.com
    --
    --     ORGANIZER;CN=JohnSmith;DIR="ldap://example.com:6666/o=DC%20Ass
    --      ociates,c=US???(cn=John%20Smith)":mailto:jsmith@example.com
    -- @
    parameterExampleSpec ["JohnSmith"] (CommonName "JohnSmith")

  describe "Encoding" $ do
    genValidSpec @Encoding
    parameterSpec @Encoding
    -- From the spec:
    -- @
    --   ATTACH;FMTTYPE=text/plain;ENCODING=BASE64;VALUE=BINARY:TG9yZW
    -- @
    parameterExampleSpec ["BASE64"] EncodingBase64
    parameterExampleSpec ["8BIT"] Encoding8Bit

  describe "FormatType" $ do
    genValidSpec @FormatType
    parameterSpec @FormatType
    -- From the spec:
    -- @
    -- Example:
    --
    --     ATTACH;FMTTYPE=application/msword:ftp://example.com/pub/docs/
    --      agenda.doc
    -- @
    parameterExampleSpec ["application/msword"] (FormatType "application/msword")

    -- @
    -- ATTACH;FMTTYPE=application/postscript:ftp://example.com/pub/
    --  reports/r-960812.ps
    -- @
    parameterExampleSpec ["application/postscript"] (FormatType "application/postscript")

    -- @
    -- X-ABC-MMSUBJ;VALUE=URI;FMTTYPE=audio/basic:http://www.example.
    --  org/mysubj.au
    -- @
    parameterExampleSpec ["audio/basic"] (FormatType "audio/basic")

    -- @
    -- ATTACH;FMTTYPE=text/plain;ENCODING=BASE64;VALUE=BINARY:VGhlIH
    --  F1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4
    -- @
    parameterExampleSpec ["text/plain"] (FormatType "text/plain")
    -- @
    -- ATTACH;FMTTYPE=image/vnd.microsoft.icon;ENCODING=BASE64;VALUE
    --  =BINARY:AAABAAEAEBAQAAEABAAoAQAAFgAAACgAAAAQAAAAIAAAAAEABAAA
    --  AAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAAAAgIAAAICAgADAwMAA////AAAA
    --  AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    --  AAAAAAAAAAAAAAAAAAAAAAMwAAAAAAABNEMQAAAAAAAkQgAAAAAAJEREQgAA
    --  ACECQ0QgEgAAQxQzM0E0AABERCRCREQAADRDJEJEQwAAAhA0QwEQAAAAAERE
    --  AAAAAAAAREQAAAAAAAAkQgAAAAAAAAMgAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    --  AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    --  AAAAAAAAAAAA
    -- @
    parameterExampleSpec ["image/vnd.microsoft.icon"] (FormatType "image/vnd.microsoft.icon")

  describe "ParticipationStatus" $ do
    genValidSpec @ParticipationStatus
    parameterSpec @ParticipationStatus
    -- From the spec:
    -- @
    -- Example:
    --
    --     ATTENDEE;PARTSTAT=DECLINED:mailto:jsmith@example.com
    -- @
    parameterExampleSpec ["ACCEPTED"] ParticipationStatusAccepted
    parameterExampleSpec ["DECLINED"] ParticipationStatusDeclined
    parameterExampleSpec ["TENTATIVE"] ParticipationStatusTentative
    parameterExampleSpec ["DELEGATED"] ParticipationStatusDelegated

  describe "RSVPExpectation" $ do
    genValidSpec @RSVPExpectation
    parameterSpec @RSVPExpectation
    -- From the spec:
    -- @
    -- Example:
    --
    --     ATTENDEE;RSVP=TRUE:mailto:jsmith@example.com
    -- @
    parameterExampleSpec ["TRUE"] RSVPExpectationTrue
    parameterExampleSpec ["FALSE"] RSVPExpectationFalse

  describe "ParticipationRole" $ do
    genValidSpec @ParticipationRole
    parameterSpec @ParticipationRole
    -- From the spec:
    -- @
    -- Example:
    --
    --     ATTENDEE;ROLE=CHAIR:mailto:mrbig@example.com
    -- @
    parameterExampleSpec ["CHAIR"] ParticipationRoleChair
    parameterExampleSpec ["REQ-PARTICIPANT"] ParticipationRoleRequiredParticipant
    parameterExampleSpec ["OPT-PARTICIPANT"] ParticipationRoleOptionalParticipant

  describe "TimeZoneIdentifierParam" $ do
    genValidSpec @TimeZoneIdentifierParam
    parameterSpec @TimeZoneIdentifierParam
    -- From the spec:
    -- @
    --    The following are examples of this property parameter:
    --
    --     DTSTART;TZID=America/New_York:19980119T020000
    --
    --     DTEND;TZID=America/New_York:19980119T030000
    -- @
    parameterExampleSpec ["America/New_York"] (TimeZoneIdentifierParam "America/New_York")
    parameterExampleSpec ["/example.org/America/New_York"] (TimeZoneIdentifierParam "/example.org/America/New_York")

  describe "ValueDataType" $ do
    genValidSpec @ValueDataType
    parameterSpec @ValueDataType
    parameterExampleSpec ["DATE"] TypeDate
    parameterExampleSpec ["DATE-TIME"] TypeDateTime

  describe "AlarmTriggerRelationship" $ do
    genValidSpec @AlarmTriggerRelationship
    parameterSpec @AlarmTriggerRelationship
    parameterExampleSpec ["START"] AlarmTriggerRelationshipStart
    parameterExampleSpec ["END"] AlarmTriggerRelationshipEnd
