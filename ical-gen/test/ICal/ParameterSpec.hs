{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.ParameterSpec where

import ICal.ContentLine
import ICal.Parameter
import ICal.Parameter.Gen
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "AlternateTextRepresentation" $ do
    genValidSpec @AlternateTextRepresentation
    parameterSpec @AlternateTextRepresentation
    -- @
    --     DESCRIPTION;ALTREP="CID:part3.msg.970415T083000@example.com":
    --      Project XYZ Review Meeting will include the following agenda
    --       items: (a) Market Overview\, (b) Finances\, (c) Project Man
    --      agement
    -- @
    parameterExampleSpec
      "CID:part3.msg.970415T083000@example.com"
      (AlternateTextRepresentation "CID:part3.msg.970415T083000@example.com")
    -- @
    -- LOCATION;ALTREP="http://xyzcorp.com/conf-rooms/f123.vcf":
    --   Conference Room - F123\, Bldg. 002
    -- @
    parameterExampleSpec
      "http://xyzcorp.com/conf-rooms/f123.vcf"
      (AlternateTextRepresentation "http://xyzcorp.com/conf-rooms/f123.vcf")
    -- @
    -- CONTACT;ALTREP="ldap://example.com:6666/o=ABC%20Industries\,
    --  c=US???(cn=Jim%20Dolittle)":Jim Dolittle\, ABC Industries\,
    --  +1-919-555-1234
    -- @
    parameterExampleSpec
      "ldap://example.com:6666/o=ABC%20Industriesc=US???(cn=Jim%20Dolittle)"
      (AlternateTextRepresentation "ldap://example.com:6666/o=ABC%20Industriesc=US???(cn=Jim%20Dolittle)")

    -- @
    -- CONTACT;ALTREP="CID:part3.msg970930T083000SILVER@example.com":
    --  Jim Dolittle\, ABC Industries\, +1-919-555-1234
    -- @
    parameterExampleSpec
      "CID:part3.msg970930T083000SILVER@example.com"
      (AlternateTextRepresentation "CID:part3.msg970930T083000SILVER@example.com")

    -- @
    -- CONTACT;ALTREP="http://example.com/pdi/jdoe.vcf":Jim
    --   Dolittle\, ABC Industries\, +1-919-555-1234
    -- @
    parameterExampleSpec
      "http://example.com/pdi/jdoe.vcf"
      (AlternateTextRepresentation "http://example.com/pdi/jdoe.vcf")

    -- @
    -- DESCRIPTION;ALTREP="cid:part1.0001@example.org":The Fall'98 Wild
    --   Wizards Conference - - Las Vegas\, NV\, USA
    -- @
    parameterExampleSpec
      "cid:part1.0001@example.org"
      (AlternateTextRepresentation "cid:part1.0001@example.org")

  describe "CommonName" $ do
    genValidSpec @CommonName
    parameterSpec @CommonName
    -- From the spec:
    -- @
    -- Example:
    --
    --     ORGANIZER;CN="John Smith":mailto:jsmith@example.com
    -- @
    parameterExampleSpec "John Smith" (CommonName "John Smith")

    -- @
    -- ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=TENTATIVE;CN=Henry
    --  Cabot:mailto:hcabot@example.com
    -- ATTENDEE;ROLE=REQ-PARTICIPANT;DELEGATED-FROM="mailto:bob@
    --  example.com";PARTSTAT=ACCEPTED;CN=Jane Doe:mailto:jdoe@
    --  example.com
    -- @
    parameterExampleSpec "Henry Cabot" (CommonName "Henry Cabot")
    parameterExampleSpec "Jane Doe" (CommonName "Jane Doe")

    -- @
    -- ATTENDEE;CN=John Smith;DIR="ldap://example.com:6666/o=ABC%
    --  20Industries,c=US???(cn=Jim%20Dolittle)":mailto:jimdo@
    --  example.com
    -- @
    parameterExampleSpec "John Smith" (CommonName "John Smith")

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
    parameterExampleSpec "The Big Cheese" (CommonName "The Big Cheese")

    -- @
    --     ORGANIZER;CN=John Smith:mailto:jsmith@example.com
    --
    --     ORGANIZER;CN=JohnSmith;DIR="ldap://example.com:6666/o=DC%20Ass
    --      ociates,c=US???(cn=John%20Smith)":mailto:jsmith@example.com
    -- @
    parameterExampleSpec "JohnSmith" (CommonName "JohnSmith")

  describe "CalendarUserType" $ do
    genValidSpec @CalendarUserType
    parameterSpec @CalendarUserType

    -- @
    -- Example:
    --
    --     ATTENDEE;CUTYPE=GROUP:mailto:ietf-calsch@example.org
    -- @
    parameterExampleSpec "GROUP" CalendarUserTypeGroup

  describe "Delegator" $ do
    genValidSpec @Delegator
    parameterSpec @Delegator

    -- @
    --  ATTENDEE;DELEGATED-FROM="mailto:jsmith@example.com":mailto:
    --   jdoe@example.com
    -- @
    parameterExampleSpec (QuotedParam "mailto:jsmith@example.com") (Delegator "mailto:jsmith@example.com")

  describe "Delegatee" $ do
    genValidSpec @Delegatee
    parameterSpec @Delegatee

    -- @
    --     ATTENDEE;DELEGATED-TO="mailto:jdoe@example.com","mailto:jqpublic
    --      @example.com":mailto:jsmith@example.com
    -- @
    parameterExampleSpec (QuotedParam "mailto:jdoe@example.com") (Delegatee "mailto:jdoe@example.com")

  describe "DirectoryEntryReference" $ do
    genValidSpec @DirectoryEntryReference
    parameterSpec @DirectoryEntryReference

    -- @
    --     ORGANIZER;DIR="ldap://example.com:6666/o=ABC%20Industries,
    --      c=US???(cn=Jim%20Dolittle)":mailto:jimdo@example.com
    -- @
    parameterExampleSpec (QuotedParam "ldap://example.com:6666/o=ABC%20Industries,c=US???(cn=Jim%20Dolittle)") (DirectoryEntryReference "ldap://example.com:6666/o=ABC%20Industries,c=US???(cn=Jim%20Dolittle)")

  describe "Encoding" $ do
    genValidSpec @Encoding
    parameterSpec @Encoding
    -- From the spec:
    -- @
    --   ATTACH;FMTTYPE=text/plain;ENCODING=BASE64;VALUE=BINARY:TG9yZW
    -- @
    parameterExampleSpec "BASE64" EncodingBase64
    parameterExampleSpec "8BIT" Encoding8Bit

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
    parameterExampleSpec "application/msword" (FormatType "application/msword")

    -- @
    -- ATTACH;FMTTYPE=application/postscript:ftp://example.com/pub/
    --  reports/r-960812.ps
    -- @
    parameterExampleSpec "application/postscript" (FormatType "application/postscript")

    -- @
    -- X-ABC-MMSUBJ;VALUE=URI;FMTTYPE=audio/basic:http://www.example.
    --  org/mysubj.au
    -- @
    parameterExampleSpec "audio/basic" (FormatType "audio/basic")

    -- @
    -- ATTACH;FMTTYPE=text/plain;ENCODING=BASE64;VALUE=BINARY:VGhlIH
    --  F1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4
    -- @
    parameterExampleSpec "text/plain" (FormatType "text/plain")
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
    parameterExampleSpec "image/vnd.microsoft.icon" (FormatType "image/vnd.microsoft.icon")

  describe "Language" $ do
    genValidSpec @Language
    parameterSpec @Language

    -- @
    -- SUMMARY;LANGUAGE=en-US:Company Holiday Party
    --
    -- LOCATION;LANGUAGE=en:Germany
    --
    -- LOCATION;LANGUAGE=no:Tyskland
    -- @
    parameterExampleSpec "en-US" (Language "en-US")
    parameterExampleSpec "en" (Language "en")
    parameterExampleSpec "no" (Language "no")

    -- @
    -- RESOURCES;LANGUAGE=fr:Nettoyeur haute pression
    -- @
    parameterExampleSpec "fr" (Language "fr")

    -- @
    -- TZNAME;LANGUAGE=fr-CA:HNE
    -- @
    parameterExampleSpec "fr-CA" (Language "fr-CA")

  describe "ParticipationStatus" $ do
    genValidSpec @ParticipationStatus
    parameterSpec @ParticipationStatus

    -- @
    -- Example:
    --
    --     ATTENDEE;PARTSTAT=DECLINED:mailto:jsmith@example.com
    -- @
    parameterExampleSpec "ACCEPTED" ParticipationStatusAccepted
    parameterExampleSpec "DECLINED" ParticipationStatusDeclined
    parameterExampleSpec "TENTATIVE" ParticipationStatusTentative
    parameterExampleSpec "DELEGATED" ParticipationStatusDelegated

  describe "RecurrenceIdentifierRange" $ do
    genValidSpec @RecurrenceIdentifierRange
    parameterSpec @RecurrenceIdentifierRange

    -- @
    -- Example:
    --
    --     RECURRENCE-ID;RANGE=THISANDFUTURE:19980401T133000Z
    -- @
    parameterExampleSpec "THISANDFUTURE" RecurrenceIdentifierRangeThisAndFuture

  describe "RSVPExpectation" $ do
    genValidSpec @RSVPExpectation
    parameterSpec @RSVPExpectation

    -- @
    -- Example:
    --
    --     ATTENDEE;RSVP=TRUE:mailto:jsmith@example.com
    -- @
    parameterExampleSpec "TRUE" RSVPExpectationTrue
    parameterExampleSpec "FALSE" RSVPExpectationFalse

  describe "ParticipationRole" $ do
    genValidSpec @ParticipationRole
    parameterSpec @ParticipationRole

    -- @
    -- Example:
    --
    --     ATTENDEE;ROLE=CHAIR:mailto:mrbig@example.com
    -- @
    parameterExampleSpec "CHAIR" ParticipationRoleChair
    parameterExampleSpec "REQ-PARTICIPANT" ParticipationRoleRequiredParticipant
    parameterExampleSpec "OPT-PARTICIPANT" ParticipationRoleOptionalParticipant

  describe "TimeZoneIdentifierParam" $ do
    genValidSpec @TimeZoneIdentifierParam
    parameterSpec @TimeZoneIdentifierParam

    -- @
    --    The following are examples of this property parameter:
    --
    --     DTSTART;TZID=America/New_York:19980119T020000
    --
    --     DTEND;TZID=America/New_York:19980119T030000
    -- @
    parameterExampleSpec "America/New_York" (TimeZoneIdentifierParam "America/New_York")
    parameterExampleSpec "/example.org/America/New_York" (TimeZoneIdentifierParam "/example.org/America/New_York")

  describe "ValueDataType" $ do
    genValidSpec @ValueDataType
    parameterSpec @ValueDataType

    parameterExampleSpec "DATE" TypeDate
    parameterExampleSpec "DATE-TIME" TypeDateTime

  describe "AlarmTriggerRelationship" $ do
    genValidSpec @AlarmTriggerRelationship
    parameterSpec @AlarmTriggerRelationship
    parameterExampleSpec "START" AlarmTriggerRelationshipStart
    parameterExampleSpec "END" AlarmTriggerRelationshipEnd

  describe "Display" $ do
    genValidSpec @Display
    parameterSpec @Display

    -- RFC 7986:
    -- @
    -- Example:
    --
    -- IMAGE;VALUE=URI;DISPLAY=BADGE,THUMBNAIL;FMTTYPE=image/png:https://exa
    --  mple.com/images/weather-cloudy.png
    -- @
    parameterExampleSpec "BADGE" DisplayBadge
    parameterExampleSpec "THUMBNAIL" DisplayThumbnail
