{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertySpec where

import qualified Data.Set as S
import Data.Time
import ICal.Parameter
import ICal.Property
import ICal.Property.Gen
import ICal.PropertyType
import ICal.PropertyType.Duration.Gen ()
import Test.Syd
import Test.Syd.Validity hiding (Location)

spec :: Spec
spec = do
  describe "Method" $ do
    genValidSpec @Method
    propertySpec @Method
    -- @
    -- Example:  The following is a hypothetical example of this property to
    --    convey that the iCalendar object is a scheduling request:
    --
    --     METHOD:REQUEST
    -- @
    propertyExampleSpec "METHOD:REQUEST" (Method "REQUEST")

  describe "ProductIdentifier" $ do
    genValidSpec @ProductIdentifier
    propertySpec @ProductIdentifier
    propertyExampleSpec "PRODID:Example" (ProductIdentifier "Example")

  describe "Version" $ do
    genValidSpec @Version
    propertySpec @Version
    propertyExampleSpec "VERSION:2.0" (Version "2.0")

  describe "RecurrenceIdentifier" $ do
    genValidSpec @RecurrenceIdentifier
    propertySpec @RecurrenceIdentifier
    -- @
    -- Example:  The following are examples of this property:
    --
    --     RECURRENCE-ID;VALUE=DATE:19960401
    --
    --     RECURRENCE-ID;RANGE=THISANDFUTURE:19960120T120000Z
    -- @
    propertyExampleSpec
      "RECURRENCE-ID;VALUE=DATE:19960401"
      (RecurrenceIdentifierDate (Date (fromGregorian 1996 04 01)))
    -- TODO make this roundtrip
    propertyParseExampleSpec
      "RECURRENCE-ID;RANGE=THISANDFUTURE:19960120T120000Z"
      ( RecurrenceIdentifierDateTime
          ( DateTimeUTC
              ( UTCTime
                  (fromGregorian 1996 01 20)
                  (timeOfDayToTime (TimeOfDay 12 00 00))
              )
          )
      )

  describe "UID" $ do
    genValidSpec @UID
    propertySpec @UID
    propertyExampleSpec
      "UID:19960401T080045Z-4000F192713-0052@example.com"
      (UID "19960401T080045Z-4000F192713-0052@example.com")

  describe "TimeZoneIdentifier" $ do
    genValidSpec @TimeZoneIdentifier
    propertySpec @TimeZoneIdentifier
    propertyExampleSpec "TZID:America/New_York" (TimeZoneIdentifier "America/New_York")
    propertyExampleSpec "TZID:America/Los_Angeles" (TimeZoneIdentifier "America/Los_Angeles")
    propertyExampleSpec "TZID:/example.org/America/New_York" (TimeZoneIdentifier "/example.org/America/New_York")

  describe "DateTimeStamp" $ do
    genValidSpec @DateTimeStamp
    propertySpec @DateTimeStamp
    propertyParseExampleSpec
      "DTSTAMP;VALUE=DATE-TIME:19971210T080000Z"
      (DateTimeStamp (DateTimeUTC (localTimeToUTC utc (LocalTime (fromGregorian 1997 12 10) (TimeOfDay 08 00 00)))))
    propertyExampleSpec
      "DTSTAMP:19971210T080000Z"
      (DateTimeStamp (DateTimeUTC (localTimeToUTC utc (LocalTime (fromGregorian 1997 12 10) (TimeOfDay 08 00 00)))))
    propertyParseExampleSpec
      "DTSTAMP;VALUE=DATE-TIME:18581117T000000Z"
      (DateTimeStamp (DateTimeUTC (localTimeToUTC utc (LocalTime (fromGregorian 1858 11 17) (TimeOfDay 00 00 00)))))
    propertyExampleSpec
      "DTSTAMP:18581117T000000Z"
      (DateTimeStamp (DateTimeUTC (localTimeToUTC utc (LocalTime (fromGregorian 1858 11 17) (TimeOfDay 00 00 00)))))

  describe "DateTimeStart" $ do
    genValidSpec @DateTimeStart
    propertySpec @DateTimeStart
    propertyExampleSpec
      "DTSTART:19980118T073000Z"
      (DateTimeStartDateTime (DateTimeUTC (localTimeToUTC utc (LocalTime (fromGregorian 1998 01 18) (TimeOfDay 07 30 00)))))
    propertyParseExampleSpec
      "DTSTART;VALUE=DATE-TIME:19980118T073000Z"
      (DateTimeStartDateTime (DateTimeUTC (localTimeToUTC utc (LocalTime (fromGregorian 1998 01 18) (TimeOfDay 07 30 00)))))

  describe "Attachment" $ do
    genValidSpec @Attachment
    propertySpec @Attachment
    -- @
    -- Example:  The following are examples of this property:
    --
    --     ATTACH:CID:jsmith.part3.960817T083000.xyzMail@example.com
    --
    --     ATTACH;FMTTYPE=application/postscript:ftp://example.com/pub/
    --      reports/r-960812.ps
    -- @
    propertyExampleSpec
      "ATTACH:CID:jsmith.part3.960817T083000.xyzMail@example.com"
      (AttachmentURI "CID:jsmith.part3.960817T083000.xyzMail@example.com")
    propertyExampleSpec
      "ATTACH;FMTTYPE=application/postscript:ftp://example.com/pub/reports/r-960812.ps"
      (AttachmentURI "ftp://example.com/pub/reports/r-960812.ps")
    -- @
    -- Example:  The following is an example of a "BASE64" encoded binary
    --    value data:
    --
    --   ATTACH;FMTTYPE=image/vnd.microsoft.icon;ENCODING=BASE64;VALUE
    --    =BINARY:AAABAAEAEBAQAAEABAAoAQAAFgAAACgAAAAQAAAAIAAAAAEABAAA
    --    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAAAAgIAAAICAgADAwMAA////AAAA
    --    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    --    AAAAAAAAAAAAAAAAAAAAAAMwAAAAAAABNEMQAAAAAAAkQgAAAAAAJEREQgAA
    --    ACECQ0QgEgAAQxQzM0E0AABERCRCREQAADRDJEJEQwAAAhA0QwEQAAAAAERE
    --    AAAAAAAAREQAAAAAAAAkQgAAAAAAAAMgAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    --    AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
    --    AAAAAAAAAAAA
    -- @
    propertyExampleSpec
      "ATTACH;FMTTYPE=image/vnd.microsoft.icon;ENCODING=BASE64;VALUE=BINARY:AAABAAEAEBAQAAEABAAoAQAAFgAAACgAAAAQAAAAIAAAAAEABAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAAAAgIAAAICAgADAwMAA////AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMwAAAAAAABNEMQAAAAAAAkQgAAAAAAJEREQgAAACECQ0QgEgAAQxQzM0E0AABERCRCREQAADRDJEJEQwAAAhA0QwEQAAAAAEREAAAAAAAAREQAAAAAAAAkQgAAAAAAAAMgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
      (AttachmentBinary (Binary ""))

  describe "Classification" $ do
    genValidSpec @Classification
    propertySpec @Classification
    propertyExampleSpec
      "CLASS:PUBLIC"
      ClassificationPublic

  describe "Organizer" $ do
    genValidSpec @Organizer
    propertySpec @Organizer
    -- @
    -- Example:  The following is an example of this property:
    --
    --     ORGANIZER;CN=John Smith:mailto:jsmith@example.com
    --
    --    The following is an example of this property with a pointer to the
    --    directory information associated with the organizer:
    --
    --     ORGANIZER;CN=JohnSmith;DIR="ldap://example.com:6666/o=DC%20Ass
    --      ociates,c=US???(cn=John%20Smith)":mailto:jsmith@example.com
    --
    --    The following is an example of this property used by another
    --    calendar user who is acting on behalf of the organizer, with
    --    responses intended to be sent back to the organizer, not the other
    --    calendar user:
    --
    --     ORGANIZER;SENT-BY="mailto:jane_doe@example.com":
    --      mailto:jsmith@example.com
    -- @
    propertyExampleSpec
      "ORGANIZER;CN=John Smith:mailto:jsmith@example.com"
      ((mkOrganizer "mailto:jsmith@example.com") {organizerCommonName = Just "John Smith"})
    propertyParseExampleSpec
      "ORGANIZER;CN=JohnSmith;DIR=\"ldap://example.com:6666/o=DC%20Associates,c=US???(cn=John%20Smith)\":mailto:jsmith@example.com"
      ((mkOrganizer "mailto:jsmith@example.com") {organizerCommonName = Just "JohnSmith"})
    propertyParseExampleSpec
      "ORGANIZER;SENT-BY=\"mailto:jane_doe@example.com\":mailto:jsmith@example.com"
      (mkOrganizer "mailto:jsmith@example.com")

  describe "Created" $ do
    genValidSpec @Created
    propertySpec @Created
    propertyExampleSpec
      "CREATED:19960329T133000Z"
      (Created (localTimeToUTC utc (LocalTime (fromGregorian 1996 03 29) (TimeOfDay 13 30 00))))

  describe "Summary" $ do
    genValidSpec @Summary
    propertySpec @Summary
    propertyExampleSpec
      "SUMMARY:Department Party"
      (Summary "Department Party")

  describe "Description" $ do
    genValidSpec @Description
    propertySpec @Description
    propertyExampleSpec
      "DESCRIPTION:Meeting to provide technical review for \"Phoenix\" design.\\nHappy Face Conference Room. Phoenix design team MUST attend this meeting.\\nRSVP to team leader."
      (Description "Meeting to provide technical review for \"Phoenix\" design.\nHappy Face Conference Room. Phoenix design team MUST attend this meeting.\nRSVP to team leader.")

  describe "GeographicPosition" $ do
    genValidSpec @GeographicPosition
    propertySpec @GeographicPosition
    propertyExampleSpec
      "GEO:37.386013;-122.082932"
      ( GeographicPosition
          { geographicPositionLat = 37.386013,
            geographicPositionLon = -122.082932
          }
      )

  describe "LastModified" $ do
    genValidSpec @LastModified
    propertySpec @LastModified
    -- From the spec:
    -- @
    -- Example:  The following is an example of this property:
    --
    --     LAST-MODIFIED:19960817T133000Z
    -- @
    propertyExampleSpec
      "LAST-MODIFIED:19960817T133000Z"
      (LastModified (localTimeToUTC utc (LocalTime (fromGregorian 1996 08 17) (TimeOfDay 13 30 00))))

  describe "Location" $ do
    genValidSpec @Location
    propertySpec @Location
    -- From the spec:
    -- @
    -- Example:  The following are some examples of this property:
    --
    --     LOCATION:Conference Room - F123\, Bldg. 002
    --
    --     LOCATION;ALTREP="http://xyzcorp.com/conf-rooms/f123.vcf":
    --      Conference Room - F123\, Bldg. 002
    -- @
    propertyExampleSpec
      "LOCATION:Conference Room - F123\\, Bldg. 002"
      (Location "Conference Room - F123, Bldg. 002")
    xdescribe "not implemented yet" $
      propertyExampleSpec
        "LOCATION;ALTREP=\"http://xyzcorp.com/conf-rooms/f123.vcf\":Conference Room - F123\\, Bldg. 002"
        (Location "Conference Room - F123, Bldg. 002")

  describe "Status" $ do
    genValidSpec @Status
    propertySpec @Status
    -- From the spec:
    -- @
    -- Example:  The following is an example of this property for a "VEVENT"
    --    calendar component:
    --
    --     STATUS:TENTATIVE
    -- @
    propertyExampleSpec "STATUS:TENTATIVE" StatusTentative

  describe "DateTimeEnd" $ do
    genValidSpec @DateTimeEnd
    propertySpec @DateTimeEnd
    -- From the spec:
    -- @
    -- Example:  The following is an example of this property:
    --
    --     DTEND:19960401T150000Z
    --
    --     DTEND;VALUE=DATE:19980704
    -- @
    propertyParseExampleSpec
      "DTEND;VALUE=DATE-TIME:19960401T150000Z"
      (DateTimeEndDateTime (DateTimeUTC (localTimeToUTC utc (LocalTime (fromGregorian 1996 04 01) (TimeOfDay 15 00 00)))))
    propertyExampleSpec
      "DTEND:19960401T150000Z"
      (DateTimeEndDateTime (DateTimeUTC (localTimeToUTC utc (LocalTime (fromGregorian 1996 04 01) (TimeOfDay 15 00 00)))))
    propertyExampleSpec
      "DTEND;VALUE=DATE:19980704"
      (DateTimeEndDate (Date (fromGregorian 1998 07 04)))

  describe "Duration" $ do
    genValidSpec @Duration
    propertySpec @Duration
    -- From the spec:
    -- @
    -- Example:  The following is an example of this property that specifies
    --    an interval of time of one hour and zero minutes and zero seconds:
    --
    --     DURATION:PT1H0M0S
    --
    --    The following is an example of this property that specifies an
    --    interval of time of 15 minutes.
    --
    --     DURATION:PT15M
    -- @
    propertyParseExampleSpec
      "DURATION:PT1H0M0S"
      ( DurationTime
          ( DurTime
              { durTimeSign = Positive,
                durTimeHour = 1,
                durTimeMinute = 0,
                durTimeSecond = 0
              }
          )
      )
    propertyExampleSpec
      "DURATION:PT15M"
      ( DurationTime
          ( DurTime
              { durTimeSign = Positive,
                durTimeHour = 0,
                durTimeMinute = 15,
                durTimeSecond = 0
              }
          )
      )

  describe "URL" $ do
    genValidSpec @URL
    propertySpec @URL
    -- From the spec:
    -- @
    -- Example:  The following is an example of this property:
    --
    --     URL:http://example.com/pub/calendars/jsmith/mytime.ics
    -- @
    propertyExampleSpec
      "URL:http://example.com/pub/calendars/jsmith/mytime.ics"
      (URL "http://example.com/pub/calendars/jsmith/mytime.ics")

  describe "TimeZoneName" $ do
    xdescribe "already in DurationSpec" $ genValidSpec @TimeZoneName
    propertySpec @TimeZoneName
    pending "works for this example"

  describe "TimeZoneOffsetFrom" $ do
    genValidSpec @TimeZoneOffsetFrom
    propertySpec @TimeZoneOffsetFrom
    -- @
    -- Example:  The following are examples of this property:
    --
    --     TZOFFSETFROM:-0500
    --
    --     TZOFFSETFROM:+1345
    -- @
    -- @
    propertyExampleSpec "TZOFFSETFROM:-0500" (TimeZoneOffsetFrom (UTCOffset (-18000)))
    propertyExampleSpec "TZOFFSETFROM:+1345" (TimeZoneOffsetFrom (UTCOffset 49500))

  describe "TimeZoneOffsetTo" $ do
    genValidSpec @TimeZoneOffsetTo
    propertySpec @TimeZoneOffsetTo
    -- Example:  The following are examples of this property:
    --
    --     TZOFFSETTO:-0400
    --
    --     TZOFFSETTO:+1245
    -- @
    propertyExampleSpec "TZOFFSETTO:-0400" (TimeZoneOffsetTo (UTCOffset (-14400)))
    propertyExampleSpec "TZOFFSETTO:+1245" (TimeZoneOffsetTo (UTCOffset 45900))

  describe "Comment" $ do
    genValidSpec @Comment
    propertySpec @Comment
    propertyExampleSpec
      "COMMENT:The meeting really needs to include both ourselves and the customer. We can't hold this meeting without them. As a matter of fact\\, the venue for the meeting ought to be at their site. - - John"
      (Comment "The meeting really needs to include both ourselves and the customer. We can't hold this meeting without them. As a matter of fact, the venue for the meeting ought to be at their site. - - John")

  describe "Transparency" $ do
    genValidSpec @Transparency
    propertySpec @Transparency
    -- From the spec:
    -- @
    -- Example:  The following is an example of this property for an event
    --    that is transparent or does not block on free/busy time searches:
    --
    --     TRANSP:TRANSPARENT
    --
    --    The following is an example of this property for an event that is
    --    opaque or blocks on free/busy time searches:
    --
    --     TRANSP:OPAQUE
    -- @
    propertyExampleSpec
      "TRANSP:TRANSPARENT"
      TransparencyTransparent
    propertyExampleSpec
      "TRANSP:OPAQUE"
      TransparencyOpaque

  describe "Attendee" $ do
    genValidSpec @Attendee
    propertySpec @Attendee
    -- From the spec:
    -- @
    -- Example:  The following are examples of this property's use for a
    --    to-do:
    --
    --     ATTENDEE;MEMBER="mailto:DEV-GROUP@example.com":
    --      mailto:joecool@example.com
    --     ATTENDEE;DELEGATED-FROM="mailto:immud@example.com":
    --      mailto:ildoit@example.com
    --    The following is an example of this property used for specifying
    --    multiple attendees to an event:
    --
    --     ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=TENTATIVE;CN=Henry
    --      Cabot:mailto:hcabot@example.com
    --     ATTENDEE;ROLE=REQ-PARTICIPANT;DELEGATED-FROM="mailto:bob@
    --      example.com";PARTSTAT=ACCEPTED;CN=Jane Doe:mailto:jdoe@
    --      example.com
    --
    -- @
    propertyParseExampleSpec
      "ATTENDEE;MEMBER=\"mailto:DEV-GROUP@example.com\":mailto:joecool@example.com"
      (mkAttendee "mailto:joecool@example.com")
    propertyParseExampleSpec
      "ATTENDEE;DELEGATED-FROM=\"mailto:immud@example.com\":mailto:ildoit@example.com"
      (mkAttendee "mailto:ildoit@example.com")
    propertyParseExampleSpec
      "ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=TENTATIVE;CN=HenryCabot:mailto:hcabot@example.com"
      ( (mkAttendee "mailto:hcabot@example.com")
          { attendeeParticipationRole = ParticipationRoleRequiredParticipant,
            attendeeParticipationStatus = ParticipationStatusTentative,
            attendeeCommonName = Just "HenryCabot"
          }
      )
    propertyRenderExampleSpec
      "ATTENDEE;PARTSTAT=TENTATIVE;CN=HenryCabot:mailto:hcabot@example.com"
      ( (mkAttendee "mailto:hcabot@example.com")
          { attendeeParticipationRole = ParticipationRoleRequiredParticipant,
            attendeeParticipationStatus = ParticipationStatusTentative,
            attendeeCommonName = Just "HenryCabot"
          }
      )
    propertyParseExampleSpec
      "ATTENDEE;ROLE=REQ-PARTICIPANT;DELEGATED-FROM=\"mailto:bob@example.com\";PARTSTAT=ACCEPTED;CN=Jane Doe:mailto:jdoe@example.com"
      ( (mkAttendee "mailto:jdoe@example.com")
          { attendeeParticipationRole = ParticipationRoleRequiredParticipant,
            attendeeParticipationStatus = ParticipationStatusAccepted,
            attendeeCommonName = Just "Jane Doe"
          }
      )

    -- @
    --    The following is an example of this property with a URI to the
    --    directory information associated with the attendee:
    --
    --     ATTENDEE;CN=John Smith;DIR="ldap://example.com:6666/o=ABC%
    --      20Industries,c=US???(cn=Jim%20Dolittle)":mailto:jimdo@
    --      example.com
    --
    -- @
    propertyParseExampleSpec
      "ATTENDEE;CN=John Smith;DIR=\"ldap://example.com:6666/o=ABC%20Industries,c=US???(cn=Jim%20Dolittle)\":mailto:jimdo@example.com"
      ((mkAttendee "mailto:jimdo@example.com") {attendeeCommonName = Just "John Smith"})

    -- @
    --    The following is an example of this property with "delegatee" and
    --    "delegator" information for an event:
    --
    --     ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=TENTATIVE;DELEGATED-FROM=
    --      "mailto:iamboss@example.com";CN=Henry Cabot:mailto:hcabot@
    --      example.com
    --     ATTENDEE;ROLE=NON-PARTICIPANT;PARTSTAT=DELEGATED;DELEGATED-TO=
    --      "mailto:hcabot@example.com";CN=The Big Cheese:mailto:iamboss
    --      @example.com
    --     ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=ACCEPTED;CN=Jane Doe
    --      :mailto:jdoe@example.com
    -- @
    propertyParseExampleSpec
      "ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=TENTATIVE;DELEGATED-FROM=\"mailto:iamboss@example.com\";CN=Henry Cabot:mailto:hcabot@example.com"
      ( (mkAttendee "mailto:hcabot@example.com")
          { attendeeParticipationRole = ParticipationRoleRequiredParticipant,
            attendeeParticipationStatus = ParticipationStatusTentative,
            attendeeCommonName = Just "Henry Cabot"
          }
      )
    propertyParseExampleSpec
      "ATTENDEE;ROLE=NON-PARTICIPANT;PARTSTAT=DELEGATED;DELEGATED-TO=\"mailto:hcabot@example.com\";CN=The Big Cheese:mailto:iamboss@example.com"
      ( (mkAttendee "mailto:iamboss@example.com")
          { attendeeParticipationRole = ParticipationRoleNonParticipant,
            attendeeParticipationStatus = ParticipationStatusDelegated,
            attendeeCommonName = Just "The Big Cheese"
          }
      )
    propertyParseExampleSpec
      "ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=ACCEPTED;CN=Jane Doe:mailto:jdoe@example.com"
      ( (mkAttendee "mailto:jdoe@example.com")
          { attendeeParticipationRole = ParticipationRoleRequiredParticipant,
            attendeeParticipationStatus = ParticipationStatusAccepted,
            attendeeCommonName = Just "Jane Doe"
          }
      )
    propertyRenderExampleSpec
      "ATTENDEE;PARTSTAT=ACCEPTED;CN=Jane Doe:mailto:jdoe@example.com"
      ( (mkAttendee "mailto:jdoe@example.com")
          { attendeeParticipationRole = ParticipationRoleRequiredParticipant,
            attendeeParticipationStatus = ParticipationStatusAccepted,
            attendeeCommonName = Just "Jane Doe"
          }
      )

    -- @
    -- Example:  The following is an example of this property's use when
    --    another calendar user is acting on behalf of the "Attendee":
    --
    --     ATTENDEE;SENT-BY=mailto:jan_doe@example.com;CN=John Smith:
    --      mailto:jsmith@example.com
    -- @
    --
    -- with the erratum:
    --
    -- @
    -- Section 3.8.4.1 says:
    --
    -- Example:  The following is an example of this property's use when
    --       another calendar user is acting on behalf of the "Attendee":
    --
    --        ATTENDEE;SENT-BY=mailto:jan_doe@example.com;CN=John Smith:
    --         mailto:jsmith@example.com
    --
    -- It should say:
    --
    -- Example:  The following is an example of this property's use when
    --       another calendar user is acting on behalf of the "Attendee":
    --
    --        ATTENDEE;SENT-BY="mailto:jan_doe@example.com";CN=John Smith:
    --                         ^                          ^
    --         mailto:jsmith@example.com
    --
    -- Notes:
    --
    -- In the specification for SENT-BY (3.2.18), the r-value MUST be explicitly bound by DQUOTEs (ABNF follows)
    --
    -- sentbyparam = "SENT-BY" "=" DQUOTE cal-address DQUOTE
    -- @
    propertyParseExampleSpec
      "ATTENDEE;SENT-BY=\"mailto:jan_doe@example.com\";CN=John Smith:mailto:jsmith@example.com"
      ((mkAttendee "mailto:jsmith@example.com") {attendeeCommonName = Just "John Smith"})

  describe "ExceptionDateTimes" $ do
    genValidSpec @ExceptionDateTimes
    propertySpec @ExceptionDateTimes
    -- From the spec:
    -- @
    -- Example:  The following is an example of this property:
    --
    --     EXDATE:19960402T010000Z,19960403T010000Z,19960404T010000Z
    -- @
    let val =
          ExceptionDateTimes
            ( DateTimesUTC
                ( S.fromList
                    [ UTCTime
                        (fromGregorian 1996 4 2)
                        (timeOfDayToTime (TimeOfDay 01 00 00)),
                      UTCTime
                        (fromGregorian 1996 4 3)
                        (timeOfDayToTime (TimeOfDay 01 00 00)),
                      UTCTime
                        (fromGregorian 1996 4 4)
                        (timeOfDayToTime (TimeOfDay 01 00 00))
                    ]
                )
            )
    propertyExampleSpec
      "EXDATE:19960402T010000Z,19960403T010000Z,19960404T010000Z"
      val
    propertyParseExampleSpec
      "EXDATE;VALUE=DATE-TIME:19960402T010000Z,19960403T010000Z,19960404T010000Z"
      val

  describe "RecurrenceDateTimes" $ do
    genValidSpec @RecurrenceDateTimes
    propertySpec @RecurrenceDateTimes

    -- From the spec:
    -- @
    -- Example:  The following are examples of this property:
    --
    --     RDATE:19970714T123000Z
    --     RDATE;TZID=America/New_York:19970714T083000
    --
    --     RDATE;VALUE=PERIOD:19960403T020000Z/19960403T040000Z,
    --      19960404T010000Z/PT3H
    --
    --     RDATE;VALUE=DATE:19970101,19970120,19970217,19970421
    --      19970526,19970704,19970901,19971014,19971128,19971129,19971225
    -- @
    --     RDATE:19970714T123000Z
    let ex1 =
          RecurrenceDateTimes $
            DateTimesUTC
              ( S.fromList
                  [ UTCTime
                      (fromGregorian 1997 7 14)
                      (timeOfDayToTime (TimeOfDay 12 30 00))
                  ]
              )
    propertyExampleSpec
      "RDATE:19970714T123000Z"
      ex1
    propertyParseExampleSpec
      "RDATE;VALUE=DATE-TIME:19970714T123000Z"
      ex1
    --     RDATE;TZID=America/New_York:19970714T083000
    let ex2 =
          RecurrenceDateTimes $
            DateTimesZoned
              "America/New_York"
              ( S.fromList
                  [LocalTime (fromGregorian 1997 7 14) (TimeOfDay 08 30 00)]
              )

    propertyExampleSpec
      "RDATE;TZID=America/New_York:19970714T083000"
      ex2
    propertyParseExampleSpec
      "RDATE;VALUE=DATE-TIME;TZID=America/New_York:19970714T083000"
      ex2

    --     RDATE;VALUE=PERIOD:19960403T020000Z/19960403T040000Z,
    --      19960404T010000Z/PT3H
    let ex3 =
          RecurrencePeriods
            ( S.fromList
                [ PeriodStartEnd (UTCTime (fromGregorian 1996 04 03) (timeOfDayToTime (TimeOfDay 02 00 00))) (UTCTime (fromGregorian 1996 04 03) (timeOfDayToTime (TimeOfDay 04 00 00))),
                  PeriodStartDuration
                    (UTCTime (fromGregorian 1996 04 04) (timeOfDayToTime (TimeOfDay 01 00 00)))
                    ( DurationTime
                        DurTime
                          { durTimeSign = Positive,
                            durTimeHour = 3,
                            durTimeMinute = 0,
                            durTimeSecond = 0
                          }
                    )
                ]
            )
    propertyExampleSpec
      "RDATE;VALUE=PERIOD:19960403T020000Z/19960403T040000Z,19960404T010000Z/PT3H"
      ex3

    --     RDATE;VALUE=DATE:19970101,19970120,19970217,19970421
    --      19970526,19970704,19970901,19971014,19971128,19971129,19971225
    let ex4 =
          RecurrenceDates $
            S.fromList
              [ Date (fromGregorian 1997 1 1),
                Date (fromGregorian 1997 1 20),
                Date (fromGregorian 1997 2 17),
                Date (fromGregorian 1997 4 21),
                Date (fromGregorian 1997 5 26),
                Date (fromGregorian 1997 7 4),
                Date (fromGregorian 1997 9 1),
                Date (fromGregorian 1997 10 14),
                Date (fromGregorian 1997 11 28),
                Date (fromGregorian 1997 11 29),
                Date (fromGregorian 1997 12 25)
              ]

    propertyExampleSpec
      "RDATE;VALUE=DATE:19970101,19970120,19970217,19970421,19970526,19970704,19970901,19971014,19971128,19971129,19971225"
      ex4

  describe "Action" $ do
    genValidSpec @Action
    propertySpec @Action
    -- @
    -- Example:  The following are examples of this property in a "VALARM"
    --    calendar component:
    --
    --     ACTION:AUDIO
    --
    --     ACTION:DISPLAY
    -- @
    propertyExampleSpec
      "ACTION:AUDIO"
      ActionAudio
    propertyExampleSpec
      "ACTION:DISPLAY"
      ActionDisplay
    propertyExampleSpec
      "ACTION:EMAIL"
      ActionEmail

  describe "Repeat" $ do
    genValidSpec @Repeat
    propertySpec @Repeat
    propertyExampleSpec
      "REPEAT:4"
      (Repeat 4)

  describe "Trigger" $ do
    genValidSpec @Trigger
    propertySpec @Trigger
    -- @
    -- Example:  A trigger set 15 minutes prior to the start of the event or
    --    to-do.
    --
    --     TRIGGER:-PT15M
    --
    --    A trigger set five minutes after the end of an event or the due
    --    date of a to-do.
    --
    --     TRIGGER;RELATED=END:PT5M
    --
    --    A trigger set to an absolute DATE-TIME.
    --
    --     TRIGGER;VALUE=DATE-TIME:19980101T050000Z
    -- @
    propertyExampleSpec
      "TRIGGER:-PT15M"
      (TriggerDuration AlarmTriggerRelationshipStart (DurationTime (DurTime {durTimeSign = Negative, durTimeHour = 0, durTimeMinute = 15, durTimeSecond = 0})))
    propertyExampleSpec
      "TRIGGER;RELATED=END:PT5M"
      (TriggerDuration AlarmTriggerRelationshipEnd (DurationTime (DurTime {durTimeSign = Positive, durTimeHour = 0, durTimeMinute = 5, durTimeSecond = 0})))
    propertyExampleSpec
      "TRIGGER;VALUE=DATE-TIME:19980101T050000Z"
      (TriggerDateTime (UTCTime (fromGregorian 1998 01 01) (timeOfDayToTime (TimeOfDay 05 00 00))))
