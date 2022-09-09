{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertySpec where

import Data.Time
import ICal.Property
import ICal.Property.Gen
import ICal.PropertyType.Date
import ICal.PropertyType.DateTime
import ICal.PropertyType.Duration
import ICal.PropertyType.Duration.Gen ()
import ICal.PropertyType.URI
import ICal.PropertyType.UTCOffset
import qualified Network.URI as Network
import Test.Syd
import Test.Syd.Validity hiding (Location)

spec :: Spec
spec = do
  describe "ProdId" $ do
    genValidSpec @ProdId
    propertySpec @ProdId
    propertyExampleSpec "PRODID:Example" (ProdId "Example")

  describe "Version" $ do
    genValidSpec @Version
    propertySpec @Version
    propertyExampleSpec "VERSION:2.0" (Version "2.0")

  describe "UID" $ do
    genValidSpec @UID
    propertySpec @UID
    propertyExampleSpec
      "UID:19960401T080045Z-4000F192713-0052@example.com"
      (UID "19960401T080045Z-4000F192713-0052@example.com")

  describe "TZID" $ do
    genValidSpec @TZID
    propertySpec @TZID
    propertyExampleSpec "TZID:America/New_York" (TZID "America/New_York")
    propertyExampleSpec "TZID:America/Los_Angeles" (TZID "America/Los_Angeles")
    propertyExampleSpec "TZID:/example.org/America/New_York" (TZID "/example.org/America/New_York")

  describe "DateTimeStamp" $ do
    genValidSpec @DateTimeStamp
    propertySpec @DateTimeStamp
    propertyRenderExampleSpec
      "DTSTAMP;VALUE=DATE-TIME:19971210T080000Z"
      (DateTimeStamp (DateTimeUTC (localTimeToUTC utc (LocalTime (fromGregorian 1997 12 10) (TimeOfDay 08 00 00)))))
    propertyParseExampleSpec
      "DTSTAMP:19971210T080000Z"
      (DateTimeStamp (DateTimeUTC (localTimeToUTC utc (LocalTime (fromGregorian 1997 12 10) (TimeOfDay 08 00 00)))))
    propertyRenderExampleSpec
      "DTSTAMP;VALUE=DATE-TIME:18581117T000000Z"
      (DateTimeStamp (DateTimeUTC (localTimeToUTC utc (LocalTime (fromGregorian 1858 11 17) (TimeOfDay 00 00 00)))))
    propertyParseExampleSpec
      "DTSTAMP:18581117T000000Z"
      (DateTimeStamp (DateTimeUTC (localTimeToUTC utc (LocalTime (fromGregorian 1858 11 17) (TimeOfDay 00 00 00)))))

  describe "DateTimeStart" $ do
    genValidSpec @DateTimeStart
    propertySpec @DateTimeStart
    propertyParseExampleSpec
      "DTSTART:19980118T073000Z"
      (DateTimeStartDateTime (DateTimeUTC (localTimeToUTC utc (LocalTime (fromGregorian 1998 01 18) (TimeOfDay 07 30 00)))))
    propertyRenderExampleSpec
      "DTSTART;VALUE=DATE-TIME:19980118T073000Z"
      (DateTimeStartDateTime (DateTimeUTC (localTimeToUTC utc (LocalTime (fromGregorian 1998 01 18) (TimeOfDay 07 30 00)))))

  describe "Classification" $ do
    genValidSpec @Classification
    propertySpec @Classification
    propertyExampleSpec
      "CLASS:PUBLIC"
      ClassificationPublic

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
    propertyRenderExampleSpec
      "DTEND;VALUE=DATE-TIME:19960401T150000Z"
      (DateTimeEndDateTime (DateTimeUTC (localTimeToUTC utc (LocalTime (fromGregorian 1996 04 01) (TimeOfDay 15 00 00)))))
    propertyParseExampleSpec
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
    uri <- liftIO $ maybe (expectationFailure "could not parse URI") pure $ Network.parseURI "http://example.com/pub/calendars/jsmith/mytime.ics"
    propertyExampleSpec
      "URL:http://example.com/pub/calendars/jsmith/mytime.ics"
      (URL (URI uri))

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

  describe "ExceptionDateTimes" $ do
    genValidSpec @ExceptionDateTimes
    propertySpec @ExceptionDateTimes

-- -- From the spec:
-- -- @
-- -- Example:  The following is an example of this property:
-- --
-- --     EXDATE:19960402T010000Z,19960403T010000Z,19960404T010000Z
-- -- @
-- propertyExampleSpec
--   "EXDATE:19960402T010000Z,19960403T010000Z,19960404T010000Z"
--   (undefined :: ExceptionDateTimes)
