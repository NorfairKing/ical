{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
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
  describe "CalendarScale" $ do
    genValidSpec @CalendarScale
    propertySpec @CalendarScale

    -- @
    -- Example:  The following is an example of this property:
    --
    --     CALSCALE:GREGORIAN
    -- @
    propertyExampleSpec "CALSCALE:GREGORIAN" CalendarScaleGregorian

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
    propertyExampleSpec "METHOD:PUBLISH" (Method "PUBLISH")
    propertyExampleSpec "METHOD:REPLY" (Method "REPLY")
    propertyExampleSpec "METHOD:ADD" (Method "ADD")
    propertyExampleSpec "METHOD:CANCEL" (Method "CANCEL")
    propertyExampleSpec "METHOD:REFRESH" (Method "REFRESH")
    propertyExampleSpec "METHOD:COUNTER" (Method "COUNTER")
    propertyExampleSpec "METHOD:DECLINECOUNTER" (Method "DECLINECOUNTER")

  describe "ProductIdentifier" $ do
    genValidSpec @ProductIdentifier
    propertySpec @ProductIdentifier
    propertyExampleSpec "PRODID:Example" (ProductIdentifier "Example")
    propertyExampleSpec "PRODID:-//Example/ExampleCalendarClient//EN" (ProductIdentifier "-//Example/ExampleCalendarClient//EN")

  describe "Version" $ do
    genValidSpec @Version
    propertySpec @Version
    propertyExampleSpec "VERSION:2.0" (Version "2.0")

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
      (AttachmentURI Nothing "CID:jsmith.part3.960817T083000.xyzMail@example.com")
    propertyExampleSpec
      "ATTACH;FMTTYPE=application/postscript:ftp://example.com/pub/reports/r-960812.ps"
      (AttachmentURI (Just "application/postscript") "ftp://example.com/pub/reports/r-960812.ps")

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
      ( AttachmentBinary
          (Just "image/vnd.microsoft.icon")
          "AAABAAEAEBAQAAEABAAoAQAAFgAAACgAAAAQAAAAIAAAAAEABAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAAAAAgIAAAICAgADAwMAA////AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMwAAAAAAABNEMQAAAAAAAkQgAAAAAAJEREQgAAACECQ0QgEgAAQxQzM0E0AABERCRCREQAADRDJEJEQwAAAhA0QwEQAAAAAEREAAAAAAAAREQAAAAAAAAkQgAAAAAAAAMgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
      )

  describe "Categories" $ do
    genValidSpec @Categories
    propertySpec @Categories

    -- @
    --    Example:  The following are examples of this property:
    --
    --        CATEGORIES:APPOINTMENT,EDUCATION
    --
    --        CATEGORIES:MEETING
    -- @
    propertyExampleSpec "CATEGORIES:APPOINTMENT,EDUCATION" (makeCategories ["APPOINTMENT", "EDUCATION"])
    propertyExampleSpec "CATEGORIES:MEETING" (makeCategories ["MEETING"])

    -- @
    -- CATEGORIES:BUSINESS,HUMAN RESOURCES
    -- @
    propertyExampleSpec "CATEGORIES:BUSINESS,HUMAN RESOURCES" (makeCategories ["BUSINESS", "HUMAN RESOURCES"])

    -- @
    -- CATEGORIES:ANNIVERSARY,PERSONAL,SPECIAL OCCASION
    -- @
    propertyExampleSpec "CATEGORIES:ANNIVERSARY,PERSONAL,SPECIAL OCCASION" (makeCategories ["ANNIVERSARY", "PERSONAL", "SPECIAL OCCASION"])

    -- @
    -- CATEGORIES:FAMILY,FINANCE
    -- @
    propertyExampleSpec "CATEGORIES:FAMILY,FINANCE" (makeCategories ["FAMILY", "FINANCE"])

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
      (RecurrenceIdentifierDate Nothing (Date (fromGregorian 1996 04 01)))
    propertyExampleSpec
      "RECURRENCE-ID;RANGE=THISANDFUTURE:19960120T120000Z"
      ( RecurrenceIdentifierDateTime
          (Just RecurrenceIdentifierRangeThisAndFuture)
          ( DateTimeUTC
              ( UTCTime
                  (fromGregorian 1996 01 20)
                  (timeOfDayToTime (TimeOfDay 12 00 00))
              )
          )
      )
    -- @
    -- RECURRENCE-ID;RANGE=THISANDFUTURE:19980401T133000Z
    -- @
    propertyExampleSpec
      "RECURRENCE-ID;RANGE=THISANDFUTURE:19980401T133000Z"
      ( RecurrenceIdentifierDateTime
          (Just RecurrenceIdentifierRangeThisAndFuture)
          ( DateTimeUTC
              ( UTCTime
                  (fromGregorian 1998 04 01)
                  (timeOfDayToTime (TimeOfDay 13 30 00))
              )
          )
      )

  describe "UID" $ do
    genValidSpec @UID
    propertySpec @UID

    -- @
    -- Example:  The following is an example of this property:
    --
    --     UID:19960401T080045Z-4000F192713-0052@example.com
    -- @
    propertyExampleSpec
      "UID:19960401T080045Z-4000F192713-0052@example.com"
      (UID "19960401T080045Z-4000F192713-0052@example.com")

    -- @
    -- UID:19970610T172345Z-AF23B2@example.com
    -- @
    propertyExampleSpec
      "UID:19970610T172345Z-AF23B2@example.com"
      (UID "19970610T172345Z-AF23B2@example.com")

    -- @
    -- The following is an example of such a property value:
    --
    -- UID:5FC53010-1267-4F8E-BC28-1D7AE55A7C99
    -- @
    propertyExampleSpec
      "UID:5FC53010-1267-4F8E-BC28-1D7AE55A7C99"
      (UID "5FC53010-1267-4F8E-BC28-1D7AE55A7C99")

  describe "TimeZoneIdentifier" $ do
    genValidSpec @TimeZoneIdentifier
    propertySpec @TimeZoneIdentifier
    propertyExampleSpec "TZID:America/New_York" (TimeZoneIdentifier "America/New_York")
    propertyExampleSpec "TZID:America/Los_Angeles" (TimeZoneIdentifier "America/Los_Angeles")
    propertyExampleSpec "TZID:/example.org/America/New_York" (TimeZoneIdentifier "/example.org/America/New_York")

  describe "DateTimeCompleted" $ do
    genValidSpec @DateTimeCompleted
    propertySpec @DateTimeCompleted

    -- @
    -- Example:  The following is an example of this property:
    --
    --  COMPLETED:19960401T150000Z
    -- @
    propertyExampleSpec "COMPLETED:19960401T150000Z" (DateTimeCompleted (DateTimeUTC (localTimeToUTC utc (LocalTime (fromGregorian 1996 04 01) (TimeOfDay 15 00 00)))))

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

  describe "DateTimeDue" $ do
    genValidSpec @DateTimeDue
    propertySpec @DateTimeDue

    -- @
    -- Example:  The following is an example of this property:
    --
    --     DUE:19980430T000000Z
    -- @
    propertyExampleSpec "DUE:19980430T000000Z" (DateTimeDueDateTime (DateTimeUTC (localTimeToUTC utc (LocalTime (fromGregorian 1998 04 30) (TimeOfDay 00 00 00)))))

  describe "DateTimeStart" $ do
    genValidSpec @DateTimeStart
    propertySpec @DateTimeStart
    propertyExampleSpec
      "DTSTART:19980118T073000Z"
      (DateTimeStartDateTime (DateTimeUTC (localTimeToUTC utc (LocalTime (fromGregorian 1998 01 18) (TimeOfDay 07 30 00)))))
    propertyParseExampleSpec
      "DTSTART;VALUE=DATE-TIME:19980118T073000Z"
      (DateTimeStartDateTime (DateTimeUTC (localTimeToUTC utc (LocalTime (fromGregorian 1998 01 18) (TimeOfDay 07 30 00)))))

  describe "Resources" $ do
    genValidSpec @Resources
    propertySpec @Resources

    -- @
    -- Example:  The following is an example of this property:
    --
    --     RESOURCES:EASEL,PROJECTOR,VCR
    --
    --     RESOURCES;LANGUAGE=fr:Nettoyeur haute pression
    -- @
    propertyExampleSpec
      "RESOURCES:EASEL,PROJECTOR,VCR"
      (makeResources ["EASEL", "PROJECTOR", "VCR"])
    propertyExampleSpec
      "RESOURCES;LANGUAGE=fr:Nettoyeur haute pression"
      ( (makeResources ["Nettoyeur haute pression"])
          { resourcesLanguage = Just "fr"
          }
      )

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
      ((makeOrganizer "mailto:jsmith@example.com") {organizerCommonName = Just "John Smith"})
    propertyExampleSpec
      "ORGANIZER;CN=JohnSmith;DIR=\"ldap://example.com:6666/o=DC%20Associates,c=US???(cn=John%20Smith)\":mailto:jsmith@example.com"
      ( (makeOrganizer "mailto:jsmith@example.com")
          { organizerCommonName = Just "JohnSmith",
            organizerDirectoryEntryReference = Just (DirectoryEntryReference "ldap://example.com:6666/o=DC%20Associates,c=US???(cn=John%20Smith)")
          }
      )
    propertyExampleSpec
      "ORGANIZER;SENT-BY=\"mailto:jane_doe@example.com\":mailto:jsmith@example.com"
      ( (makeOrganizer "mailto:jsmith@example.com")
          { organizerSentBy = Just (SentBy "mailto:jane_doe@example.com")
          }
      )

  describe "Created" $ do
    genValidSpec @Created
    propertySpec @Created
    propertyExampleSpec
      "CREATED:19960329T133000Z"
      (Created (localTimeToUTC utc (LocalTime (fromGregorian 1996 03 29) (TimeOfDay 13 30 00))))

  describe "Summary" $ do
    genValidSpec @Summary
    propertySpec @Summary
    -- @
    -- Example:  The following is an example of this property:
    --
    --     SUMMARY:Department Party
    -- @

    propertyExampleSpec
      "SUMMARY:Department Party"
      Summary
        { summaryContents = "Department Party",
          summaryAlternateTextRepresentation = Nothing,
          summaryLanguage = Nothing
        }

  describe "Description" $ do
    genValidSpec @Description
    propertySpec @Description

    propertyExampleSpec
      "DESCRIPTION:Meeting to provide technical review for \"Phoenix\" design.\\nHappy Face Conference Room. Phoenix design team MUST attend this meeting.\\nRSVP to team leader."
      Description
        { descriptionContents = "Meeting to provide technical review for \"Phoenix\" design.\nHappy Face Conference Room. Phoenix design team MUST attend this meeting.\nRSVP to team leader.",
          descriptionAlternateTextRepresentation = Nothing,
          descriptionLanguage = Nothing
        }

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
      Location
        { locationContents = "Conference Room - F123, Bldg. 002",
          locationAlternateTextRepresentation = Nothing,
          locationLanguage = Nothing
        }
    propertyExampleSpec
      "LOCATION;ALTREP=\"http://xyzcorp.com/conf-rooms/f123.vcf\":Conference Room - F123\\, Bldg. 002"
      Location
        { locationContents = "Conference Room - F123, Bldg. 002",
          locationAlternateTextRepresentation = Just "http://xyzcorp.com/conf-rooms/f123.vcf",
          locationLanguage = Nothing
        }

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

  describe "PercentComplete" $ do
    genValidSpec @PercentComplete
    propertySpec @PercentComplete

    -- @
    --   Example:  The following is an example of this property to show 39%
    --      completion:
    --
    --       PERCENT-COMPLETE:39
    -- @
    propertyExampleSpec "PERCENT-COMPLETE:39" (PercentComplete 39)

  describe "Priority" $ do
    genValidSpec @Priority
    propertySpec @Priority

    -- @
    -- Example:  The following is an example of a property with the highest
    --    priority:
    --
    --     PRIORITY:1
    --
    --    The following is an example of a property with a next highest
    --    priority:
    --
    --     PRIORITY:2
    --
    --    The following is an example of a property with no priority.  This
    --    is equivalent to not specifying the "PRIORITY" property:
    --
    --     PRIORITY:0
    -- @
    propertyExampleSpec "PRIORITY:1" (Priority 1)
    propertyExampleSpec "PRIORITY:2" (Priority 2)
    propertyExampleSpec "PRIORITY:0" (Priority 0)

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
  describe "RelatedTo" $ do
    genValidSpec @RelatedTo
    propertySpec @RelatedTo

    -- @
    --
    --     RELATED-TO:jsmith.part7.19960817T083000.xyzMail@example.com
    --
    --     RELATED-TO:19960401-080045-4000F192713-0052@example.com
    -- @
    propertyExampleSpec
      "RELATED-TO:jsmith.part7.19960817T083000.xyzMail@example.com"
      (makeRelatedTo "jsmith.part7.19960817T083000.xyzMail@example.com")
    propertyExampleSpec
      "RELATED-TO:19960401-080045-4000F192713-0052@example.com"
      (makeRelatedTo "19960401-080045-4000F192713-0052@example.com")

  describe "URL" $ do
    genValidSpec @URL
    propertySpec @URL

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
    -- @
    --     TZNAME:EST
    --
    --     TZNAME;LANGUAGE=fr-CA:HNE
    -- @
    propertyExampleSpec
      "TZNAME:EST"
      TimeZoneName
        { timeZoneNameContents = "EST",
          timeZoneNameLanguage = Nothing
        }
    propertyExampleSpec
      "TZNAME;LANGUAGE=fr-CA:HNE"
      TimeZoneName
        { timeZoneNameContents = "HNE",
          timeZoneNameLanguage = Just (Language "fr-CA")
        }

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
    propertyExampleSpec "TZOFFSETFROM:-0500" (TimeZoneOffsetFrom (UTCOffset (-18000)))
    propertyExampleSpec "TZOFFSETFROM:+1345" (TimeZoneOffsetFrom (UTCOffset 49500))

  describe "TimeZoneOffsetTo" $ do
    genValidSpec @TimeZoneOffsetTo
    propertySpec @TimeZoneOffsetTo

    -- @
    -- Example:  The following are examples of this property:
    --
    --     TZOFFSETTO:-0400
    --
    --     TZOFFSETTO:+1245
    -- @
    propertyExampleSpec "TZOFFSETTO:-0400" (TimeZoneOffsetTo (UTCOffset (-14400)))
    propertyExampleSpec "TZOFFSETTO:+1245" (TimeZoneOffsetTo (UTCOffset 45900))

  describe "TimeZoneURL" $ do
    genValidSpec @TimeZoneURL
    propertySpec @TimeZoneURL

    -- @
    -- Example:  The following is an example of this property:
    --
    --  TZURL:http://timezones.example.org/tz/America-Los_Angeles.ics
    -- @
    propertyExampleSpec "TZURL:http://timezones.example.org/tz/America-Los_Angeles.ics" (TimeZoneURL "http://timezones.example.org/tz/America-Los_Angeles.ics")

  describe "Comment" $ do
    genValidSpec @Comment
    propertySpec @Comment
    propertyExampleSpec
      "COMMENT:The meeting really needs to include both ourselves and the customer. We can't hold this meeting without them. As a matter of fact\\, the venue for the meeting ought to be at their site. - - John"
      Comment
        { commentContents = "The meeting really needs to include both ourselves and the customer. We can't hold this meeting without them. As a matter of fact, the venue for the meeting ought to be at their site. - - John",
          commentAlternateTextRepresentation = Nothing,
          commentLanguage = Nothing
        }

  describe "FreeBusyIntervals" $ do
    genValidSpec @FreeBusyIntervals
    propertySpec @FreeBusyIntervals

    -- @
    -- Example:  The following are some examples of this property:
    --
    --     FREEBUSY;FBTYPE=BUSY-UNAVAILABLE:19970308T160000Z/PT8H30M
    --
    --     FREEBUSY;FBTYPE=FREE:19970308T160000Z/PT3H,19970308T200000Z/PT1H
    --
    --     FREEBUSY;FBTYPE=FREE:19970308T160000Z/PT3H,19970308T200000Z/PT1H
    --      ,19970308T230000Z/19970309T000000Z
    -- @
    propertyExampleSpec
      "FREEBUSY;FBTYPE=BUSY-UNAVAILABLE:19970308T160000Z/PT8H30M"
      ( ( makeFreeBusyIntervals
            [ PeriodStartDuration
                (UTCTime (fromGregorian 1997 03 08) (timeOfDayToTime (TimeOfDay 16 00 00)))
                ( DurationTime
                    DurTime
                      { durTimeSign = Positive,
                        durTimeHour = 8,
                        durTimeMinute = 30,
                        durTimeSecond = 0
                      }
                )
            ]
        )
          { freeBusyIntervalsType = FreeBusyTimeTypeBusyUnavailable
          }
      )
    propertyExampleSpec
      "FREEBUSY;FBTYPE=FREE:19970308T160000Z/PT3H,19970308T200000Z/PT1H"
      ( ( makeFreeBusyIntervals
            [ PeriodStartDuration
                (UTCTime (fromGregorian 1997 03 08) (timeOfDayToTime (TimeOfDay 16 00 00)))
                ( DurationTime
                    DurTime
                      { durTimeSign = Positive,
                        durTimeHour = 3,
                        durTimeMinute = 0,
                        durTimeSecond = 0
                      }
                ),
              PeriodStartDuration
                (UTCTime (fromGregorian 1997 03 08) (timeOfDayToTime (TimeOfDay 20 00 00)))
                ( DurationTime
                    DurTime
                      { durTimeSign = Positive,
                        durTimeHour = 1,
                        durTimeMinute = 0,
                        durTimeSecond = 0
                      }
                )
            ]
        )
          { freeBusyIntervalsType = FreeBusyTimeTypeFree
          }
      )
    propertyExampleSpec
      "FREEBUSY;FBTYPE=FREE:19970308T160000Z/PT3H,19970308T200000Z/PT1H,19970308T230000Z/19970309T000000Z"
      ( ( makeFreeBusyIntervals
            [ PeriodStartDuration
                (UTCTime (fromGregorian 1997 03 08) (timeOfDayToTime (TimeOfDay 16 00 00)))
                ( DurationTime
                    DurTime
                      { durTimeSign = Positive,
                        durTimeHour = 3,
                        durTimeMinute = 0,
                        durTimeSecond = 0
                      }
                ),
              PeriodStartDuration
                (UTCTime (fromGregorian 1997 03 08) (timeOfDayToTime (TimeOfDay 20 00 00)))
                ( DurationTime
                    DurTime
                      { durTimeSign = Positive,
                        durTimeHour = 1,
                        durTimeMinute = 0,
                        durTimeSecond = 0
                      }
                ),
              PeriodStartEnd
                (UTCTime (fromGregorian 1997 03 08) (timeOfDayToTime (TimeOfDay 23 00 00)))
                (UTCTime (fromGregorian 1997 03 09) (timeOfDayToTime (TimeOfDay 00 00 00)))
            ]
        )
          { freeBusyIntervalsType = FreeBusyTimeTypeFree
          }
      )

  describe "Transparency" $ do
    genValidSpec @Transparency
    propertySpec @Transparency

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
    propertyExampleSpec
      "ATTENDEE;MEMBER=\"mailto:DEV-GROUP@example.com\":mailto:joecool@example.com"
      ( (makeAttendee "mailto:joecool@example.com")
          { attendeeMemberships = [Membership "mailto:DEV-GROUP@example.com"]
          }
      )
    propertyExampleSpec
      "ATTENDEE;DELEGATED-FROM=\"mailto:immud@example.com\":mailto:ildoit@example.com"
      ( (makeAttendee "mailto:ildoit@example.com")
          { attendeeDelegators = [Delegator "mailto:immud@example.com"]
          }
      )
    propertyParseExampleSpec
      "ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=TENTATIVE;CN=HenryCabot:mailto:hcabot@example.com"
      ( (makeAttendee "mailto:hcabot@example.com")
          { attendeeParticipationRole = ParticipationRoleRequiredParticipant,
            attendeeParticipationStatus = ParticipationStatusTentative,
            attendeeCommonName = Just "HenryCabot"
          }
      )
    propertyRenderExampleSpec
      "ATTENDEE;PARTSTAT=TENTATIVE;CN=HenryCabot:mailto:hcabot@example.com"
      ( (makeAttendee "mailto:hcabot@example.com")
          { attendeeParticipationRole = ParticipationRoleRequiredParticipant,
            attendeeParticipationStatus = ParticipationStatusTentative,
            attendeeCommonName = Just "HenryCabot"
          }
      )
    propertyParseExampleSpec
      "ATTENDEE;ROLE=REQ-PARTICIPANT;DELEGATED-FROM=\"mailto:bob@example.com\";PARTSTAT=ACCEPTED;CN=Jane Doe:mailto:jdoe@example.com"
      ( (makeAttendee "mailto:jdoe@example.com")
          { attendeeParticipationRole = ParticipationRoleRequiredParticipant,
            attendeeParticipationStatus = ParticipationStatusAccepted,
            attendeeDelegators = [Delegator "mailto:bob@example.com"],
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
    propertyExampleSpec
      "ATTENDEE;CN=John Smith;DIR=\"ldap://example.com:6666/o=ABC%20Industries,c=US???(cn=Jim%20Dolittle)\":mailto:jimdo@example.com"
      ( (makeAttendee "mailto:jimdo@example.com")
          { attendeeCommonName = Just "John Smith",
            attendeeDirectoryEntryReference = Just (DirectoryEntryReference "ldap://example.com:6666/o=ABC%20Industries,c=US???(cn=Jim%20Dolittle)")
          }
      )

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
      ( (makeAttendee "mailto:hcabot@example.com")
          { attendeeParticipationRole = ParticipationRoleRequiredParticipant,
            attendeeParticipationStatus = ParticipationStatusTentative,
            attendeeDelegators = [Delegator "mailto:iamboss@example.com"],
            attendeeCommonName = Just "Henry Cabot"
          }
      )
    propertyExampleSpec
      "ATTENDEE;ROLE=NON-PARTICIPANT;PARTSTAT=DELEGATED;DELEGATED-TO=\"mailto:hcabot@example.com\";CN=The Big Cheese:mailto:iamboss@example.com"
      ( (makeAttendee "mailto:iamboss@example.com")
          { attendeeParticipationRole = ParticipationRoleNonParticipant,
            attendeeParticipationStatus = ParticipationStatusDelegated,
            attendeeDelegatees = [Delegatee "mailto:hcabot@example.com"],
            attendeeCommonName = Just "The Big Cheese"
          }
      )
    propertyParseExampleSpec
      "ATTENDEE;ROLE=REQ-PARTICIPANT;PARTSTAT=ACCEPTED;CN=Jane Doe:mailto:jdoe@example.com"
      ( (makeAttendee "mailto:jdoe@example.com")
          { attendeeParticipationRole = ParticipationRoleRequiredParticipant,
            attendeeParticipationStatus = ParticipationStatusAccepted,
            attendeeCommonName = Just "Jane Doe"
          }
      )
    propertyExampleSpec
      "ATTENDEE;PARTSTAT=ACCEPTED;CN=Jane Doe:mailto:jdoe@example.com"
      ( (makeAttendee "mailto:jdoe@example.com")
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
    propertyExampleSpec
      "ATTENDEE;SENT-BY=\"mailto:jan_doe@example.com\";CN=John Smith:mailto:jsmith@example.com"
      ( (makeAttendee "mailto:jsmith@example.com")
          { attendeeCommonName = Just "John Smith",
            attendeeSentBy = Just (SentBy "mailto:jan_doe@example.com")
          }
      )

    -- @
    -- Example:
    --
    --     ATTENDEE;MEMBER="mailto:ietf-calsch@example.org":mailto:
    --      jsmith@example.com
    --
    --     ATTENDEE;MEMBER="mailto:projectA@example.com","mailto:pr
    --      ojectB@example.com":mailto:janedoe@example.com
    -- @
    propertyExampleSpec
      "ATTENDEE;MEMBER=\"mailto:ietf-calsch@example.org\":mailto:jsmith@example.com"
      ( (makeAttendee "mailto:jsmith@example.com")
          { attendeeMemberships = [Membership "mailto:ietf-calsch@example.org"]
          }
      )
    propertyExampleSpec
      "ATTENDEE;MEMBER=\"mailto:projectA@example.com\",\"mailto:projectB@example.com\":mailto:janedoe@example.com"
      ( (makeAttendee "mailto:janedoe@example.com")
          { attendeeMemberships =
              [ Membership "mailto:projectA@example.com",
                Membership "mailto:projectB@example.com"
              ]
          }
      )

  describe "Contact" $ do
    genValidSpec @Contact
    propertySpec @Contact

    -- @
    -- Example:  The following is an example of this property referencing
    --    textual contact information:
    --
    --     CONTACT:Jim Dolittle\, ABC Industries\, +1-919-555-1234
    --
    --    The following is an example of this property with an alternate
    --    representation of an LDAP URI to a directory entry containing the
    --    contact information:
    --
    --     CONTACT;ALTREP="ldap://example.com:6666/o=ABC%20Industries\,
    --      c=US???(cn=Jim%20Dolittle)":Jim Dolittle\, ABC Industries\,
    --      +1-919-555-1234
    --
    --    The following is an example of this property with an alternate
    --    representation of a MIME body part containing the contact
    --    information, such as a vCard [RFC2426] embedded in a text/
    --    directory media type [RFC2425]:
    --
    --     CONTACT;ALTREP="CID:part3.msg970930T083000SILVER@example.com":
    --      Jim Dolittle\, ABC Industries\, +1-919-555-1234
    --
    --    The following is an example of this property referencing a network
    --    resource, such as a vCard [RFC2426] object containing the contact
    --    information:
    --
    --     CONTACT;ALTREP="http://example.com/pdi/jdoe.vcf":Jim
    --       Dolittle\, ABC Industries\, +1-919-555-1234
    -- @
    propertyExampleSpec
      "CONTACT:Jim Dolittle\\, ABC Industries\\, +1-919-555-1234"
      (makeContact "Jim Dolittle, ABC Industries, +1-919-555-1234")
    -- WARNING: DEVIATION FROM THE SPEC
    -- This example from the spec makes no sense.
    -- On the one hand there is an escaped comma in the parameter value of ALTREP.
    -- This is allowed and should not be unescaped in such a situation.
    -- HOWEVER, if we don't unescape it then the result is not a valid URI.
    -- There are no errata about this so I assume that this is an unfound
    -- mistake in the spec and the comma after Industries, should not be
    -- escaped.
    propertyExampleSpec
      "CONTACT;ALTREP=\"ldap://example.com:6666/o=ABC%20Industries,c=US???(cn=Jim%20Dolittle)\":Jim Dolittle\\, ABC Industries\\, +1-919-555-1234"
      ( (makeContact "Jim Dolittle, ABC Industries, +1-919-555-1234")
          { contactAlternateTextRepresentation = Just (AlternateTextRepresentation "ldap://example.com:6666/o=ABC%20Industries,c=US???(cn=Jim%20Dolittle)")
          }
      )
    propertyExampleSpec
      "CONTACT;ALTREP=\"CID:part3.msg970930T083000SILVER@example.com\":Jim Dolittle\\, ABC Industries\\, +1-919-555-1234"
      ( (makeContact "Jim Dolittle, ABC Industries, +1-919-555-1234")
          { contactAlternateTextRepresentation = Just (AlternateTextRepresentation "CID:part3.msg970930T083000SILVER@example.com")
          }
      )
    propertyExampleSpec
      "CONTACT;ALTREP=\"http://example.com/pdi/jdoe.vcf\":Jim Dolittle\\, ABC Industries\\, +1-919-555-1234"
      ( (makeContact "Jim Dolittle, ABC Industries, +1-919-555-1234")
          { contactAlternateTextRepresentation = Just (AlternateTextRepresentation "http://example.com/pdi/jdoe.vcf")
          }
      )

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

  describe "Image" $ do
    genValidSpec @Image
    propertySpec @Image
    -- @
    -- Example:  The following are examples of this property:
    --
    -- IMAGE;VALUE=URI;DISPLAY=BADGE;FMTTYPE=image/png:h
    --  ttp://example.com/images/party.png
    -- @
    propertyParseExampleSpec
      "IMAGE;VALUE=URI;DISPLAY=BADGE;FMTTYPE=image/png:http://example.com/images/party.png"
      ( Image
          { imageContents = Left "http://example.com/images/party.png",
            imageFormatType = Just "image/png",
            imageAlternateTextRepresentation = Nothing,
            imageDisplay = [DisplayBadge]
          }
      )
    propertyRenderExampleSpec
      "IMAGE;VALUE=URI;FMTTYPE=image/png:http://example.com/images/party.png"
      ( Image
          { imageContents = Left "http://example.com/images/party.png",
            imageFormatType = Just "image/png",
            imageAlternateTextRepresentation = Nothing,
            imageDisplay = [DisplayBadge]
          }
      )

    describe "makeURIImage" $
      it "produces valid images" $
        producesValid makeURIImage
    describe "makeBinaryImage" $
      it "produces valid images" $
        producesValid makeBinaryImage

    -- @
    -- Example:
    --
    -- IMAGE;VALUE=URI;DISPLAY=BADGE,THUMBNAIL;FMTTYPE=image/png:https://exa
    --  mple.com/images/weather-cloudy.png
    -- @
    propertyParseExampleSpec
      "IMAGE;VALUE=URI;DISPLAY=BADGE,THUMBNAIL;FMTTYPE=image/png:https://example.com/images/weather-cloudy.png"
      ( Image
          { imageContents = Left "https://example.com/images/weather-cloudy.png",
            imageFormatType = Just "image/png",
            imageAlternateTextRepresentation = Nothing,
            imageDisplay = [DisplayBadge, DisplayThumbnail]
          }
      )
    propertyRenderExampleSpec
      "IMAGE;VALUE=URI;DISPLAY=BADGE,THUMBNAIL;FMTTYPE=image/png:https://example.com/images/weather-cloudy.png"
      ( Image
          { imageContents = Left "https://example.com/images/weather-cloudy.png",
            imageFormatType = Just "image/png",
            imageAlternateTextRepresentation = Nothing,
            imageDisplay = [DisplayBadge, DisplayThumbnail]
          }
      )
