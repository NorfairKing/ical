{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertySpec where

import Data.Time
import GHC.Stack (HasCallStack, withFrozenCallStack)
import ICal.ContentLine
import ICal.Property
import ICal.Property.Gen
import ICal.PropertyType.Date
import ICal.PropertyType.DateTime
import ICal.PropertyType.Duration
import ICal.PropertyType.Duration.Gen ()
import ICal.PropertyType.URI
import qualified Network.URI as Network
import Test.Syd
import Test.Syd.Validity hiding (Location)

spec :: Spec
spec = do
  let parseSpec :: (HasCallStack, Show a, Eq a, IsProperty a) => ContentLine -> a -> Spec
      parseSpec cl v =
        it ("works for the example of " <> show cl) $
          case propertyContentLineP cl of
            Left err -> expectationFailure err
            Right v' -> v' `shouldBe` v
      renderSpec :: (HasCallStack, Show a, IsProperty a) => ContentLine -> a -> Spec
      renderSpec cl v =
        it ("can render the example of " <> show v) $
          propertyContentLineB v `shouldBe` cl
      exampleSpec :: (HasCallStack, Show a, Eq a, IsProperty a) => ContentLine -> a -> Spec
      exampleSpec cl v = withFrozenCallStack $ do
        parseSpec cl v
        renderSpec cl v

  describe "ProdId" $ do
    genValidSpec @ProdId
    propertySpec @ProdId
    exampleSpec "PRODID:Example" (ProdId "Example")

  describe "Version" $ do
    genValidSpec @Version
    propertySpec @Version
    exampleSpec "VERSION:2.0" (Version "2.0")

  describe "UID" $ do
    genValidSpec @UID
    propertySpec @UID
    exampleSpec
      "UID:19960401T080045Z-4000F192713-0052@example.com"
      (UID "19960401T080045Z-4000F192713-0052@example.com")

  describe "TZID" $ do
    genValidSpec @TZID
    propertySpec @TZID
    exampleSpec "TZID:America/New_York" (TZID "America/New_York")
    exampleSpec "TZID:America/Los_Angeles" (TZID "America/Los_Angeles")
    exampleSpec "TZID:/example.org/America/New_York" (TZID "/example.org/America/New_York")

  describe "DateTimeStamp" $ do
    genValidSpec @DateTimeStamp
    propertySpec @DateTimeStamp
    exampleSpec
      "DTSTAMP:19971210T080000Z"
      (DateTimeStamp (DateTimeUTC (LocalTime (fromGregorian 1997 12 10) (TimeOfDay 08 00 00))))
    exampleSpec
      "DTSTAMP:18581117T000000Z"
      (DateTimeStamp (DateTimeUTC (LocalTime (fromGregorian 1858 11 17) (TimeOfDay 00 00 00))))

  describe "DateTimeStart" $ do
    genValidSpec @DateTimeStart
    propertySpec @DateTimeStart
    exampleSpec
      "DTSTART:19980118T073000Z"
      (DateTimeStartDateTime (DateTimeUTC (LocalTime (fromGregorian 1998 01 18) (TimeOfDay 07 30 00))))

  describe "Classification" $ do
    genValidSpec @Classification
    propertySpec @Classification
    exampleSpec
      "CLASS:PUBLIC"
      ClassificationPublic

  describe "Created" $ do
    genValidSpec @Created
    propertySpec @Created
    exampleSpec
      "CREATED:19960329T133000Z"
      (Created (LocalTime (fromGregorian 1996 03 29) (TimeOfDay 13 30 00)))

  describe "Summary" $ do
    genValidSpec @Summary
    propertySpec @Summary
    exampleSpec
      "SUMMARY:Department Party"
      (Summary "Department Party")

  describe "Description" $ do
    genValidSpec @Description
    propertySpec @Description
    exampleSpec
      "DESCRIPTION:Meeting to provide technical review for \"Phoenix\" design.\\nHappy Face Conference Room. Phoenix design team MUST attend this meeting.\\nRSVP to team leader."
      (Description "Meeting to provide technical review for \"Phoenix\" design.\nHappy Face Conference Room. Phoenix design team MUST attend this meeting.\nRSVP to team leader.")

  describe "GeographicPosition" $ do
    genValidSpec @GeographicPosition
    propertySpec @GeographicPosition
    exampleSpec
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
    exampleSpec
      "LAST-MODIFIED:19960817T133000Z"
      (LastModified (LocalTime (fromGregorian 1996 08 17) (TimeOfDay 13 30 00)))

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
    exampleSpec
      "LOCATION:Conference Room - F123\\, Bldg. 002"
      (Location "Conference Room - F123, Bldg. 002")
    xdescribe "not implemented yet" $
      exampleSpec
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
    exampleSpec "STATUS:TENTATIVE" StatusTentative

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
    exampleSpec
      "DTEND:19960401T150000Z"
      (DateTimeEndDateTime (DateTimeUTC (LocalTime (fromGregorian 1996 04 01) (TimeOfDay 15 00 00))))
    xdescribe "not implemented yet" $
      exampleSpec
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
    parseSpec
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
    exampleSpec
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
    exampleSpec
      "URL:http://example.com/pub/calendars/jsmith/mytime.ics"
      (URL (URI uri))

  describe "TimeZoneName" $ do
    xdescribe "already in DurationSpec" $ genValidSpec @TimeZoneName
    propertySpec @TimeZoneName
    pending "works for this example"
