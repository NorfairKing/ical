{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Property where

import Control.Applicative
import Data.Proxy
import Data.Text (Text)
import qualified Data.Time as Time
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import GHC.Generics (Generic)
import ICal.ContentLine
import ICal.PropertyType

-- Law for this typeclass: The property roundtrips through 'ContentLine'.
class IsProperty property where
  -- Name of the property
  propertyName :: Proxy property -> ContentLineName

  -- | Parser for the property
  propertyP :: ContentLineValue -> Either String property

  -- | Builder for the property
  propertyB :: property -> ContentLineValue

propertyContentLineP ::
  forall property.
  IsProperty property =>
  ContentLine ->
  Either String property
propertyContentLineP ContentLine {..} =
  let name = propertyName (Proxy :: Proxy property)
   in if contentLineName == name
        then propertyP contentLineValue
        else
          Left $
            unwords
              [ "Expected content line with name",
                show name,
                "but got",
                show contentLineName,
                "instead."
              ]

propertyContentLineB :: forall property. IsProperty property => property -> ContentLine
propertyContentLineB = ContentLine (propertyName (Proxy :: Proxy property)) . propertyB

newtype Begin = Begin {unBegin :: Text}
  deriving (Show, Eq, Generic)

instance Validity Begin

instance IsProperty Begin where
  propertyName Proxy = "BEGIN"
  propertyP = beginP
  propertyB = beginB

beginP :: ContentLineValue -> Either String Begin
beginP = fmap Begin . propertyTypeP

beginB :: Begin -> ContentLineValue
beginB = propertyTypeB . unBegin

newtype End = End {unEnd :: Text}
  deriving (Show, Eq, Generic)

instance Validity End

instance IsProperty End where
  propertyName Proxy = "END"
  propertyP = endP
  propertyB = endB

endP :: ContentLineValue -> Either String End
endP = fmap End . propertyTypeP

endB :: End -> ContentLineValue
endB = propertyTypeB . unEnd

-- [section 3.7.3](https://datatracker.ietf.org/doc/html/rfc5545#section-3.7.3)
newtype ProdId = ProdId {unProdId :: Text}
  deriving (Show, Eq, Generic)

instance Validity ProdId

instance IsProperty ProdId where
  propertyName Proxy = "PRODID"
  propertyP = prodIdP
  propertyB = prodIdB

prodIdP :: ContentLineValue -> Either String ProdId
prodIdP = fmap ProdId . propertyTypeP

prodIdB :: ProdId -> ContentLineValue
prodIdB = propertyTypeB . unProdId

-- [section 3.7.4](https://datatracker.ietf.org/doc/html/rfc5545#section-3.7.4)
newtype Version = Version {unVersion :: Text}
  deriving (Show, Eq, Generic)

instance Validity Version

instance IsProperty Version where
  propertyName Proxy = "VERSION"
  propertyP = versionP
  propertyB = versionB

versionP :: ContentLineValue -> Either String Version
versionP = fmap Version . propertyTypeP

versionB :: Version -> ContentLineValue
versionB = propertyTypeB . unVersion

-- [section 3.8.4.7](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.4.7)
newtype UID = UID {unUID :: Text}
  deriving (Show, Eq, Generic)

instance Validity UID

instance IsProperty UID where
  propertyName Proxy = "UID"
  propertyP = uidP
  propertyB = uidB

uidP :: ContentLineValue -> Either String UID
uidP = fmap UID . propertyTypeP

uidB :: UID -> ContentLineValue
uidB = propertyTypeB . unUID

-- [section 3.8.7.2](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.7.2)
newtype DateTimeStamp = DateTimeStamp {unDateTimeStamp :: DateTime}
  deriving (Show, Eq, Generic)

instance Validity DateTimeStamp

instance IsProperty DateTimeStamp where
  propertyName Proxy = "DTSTAMP"
  propertyP = dateTimeStampP
  propertyB = dateTimeStampB

dateTimeStampP :: ContentLineValue -> Either String DateTimeStamp
dateTimeStampP = fmap DateTimeStamp . propertyTypeP

dateTimeStampB :: DateTimeStamp -> ContentLineValue
dateTimeStampB = propertyTypeB . unDateTimeStamp

-- [section 3.8.3.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.3.1)
newtype TZID = TZID {unTZID :: Text}
  deriving (Show, Eq, Generic)

instance Validity TZID

instance IsProperty TZID where
  propertyName Proxy = "TZID"
  propertyP = tzIDP
  propertyB = tzIDB

tzIDP :: ContentLineValue -> Either String TZID
tzIDP = fmap TZID . propertyTypeP

tzIDB :: TZID -> ContentLineValue
tzIDB = propertyTypeB . unTZID

-- [section 3.8.2.4](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.2.4)
--
-- @
--     Property Name:  DTSTART
--
--     Purpose:  This property specifies when the calendar component begins.
--
--     Value Type:  The default value type is DATE-TIME.  The time value
--        MUST be one of the forms defined for the DATE-TIME value type.
--        The value type can be set to a DATE value type.
-- @
data DateTimeStart
  = DateTimeStartDate !Date
  | DateTimeStartDateTime !DateTime
  deriving (Show, Eq, Generic)

instance Validity DateTimeStart

instance IsProperty DateTimeStart where
  propertyName Proxy = "DTSTART"
  propertyP = dateTimeStartP
  propertyB = dateTimeStartB

dateTimeStartP :: ContentLineValue -> Either String DateTimeStart
dateTimeStartP cl =
  (DateTimeStartDate <$> propertyTypeP cl)
    <|> (DateTimeStartDateTime <$> propertyTypeP cl)

dateTimeStartB :: DateTimeStart -> ContentLineValue
dateTimeStartB = \case
  DateTimeStartDate date -> propertyTypeB date
  DateTimeStartDateTime dateTime -> propertyTypeB dateTime

-- | [section 3.8.7.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.7.1)
--
-- @
--     Property Name:  CREATED
--
--     Purpose:  This property specifies the date and time that the calendar
--        information was created by the calendar user agent in the calendar
--        store.
--
--           Note: This is analogous to the creation date and time for a
--           file in the file system.
--
--     Value Type:  DATE-TIME
--
--     Property Parameters:  IANA and non-standard property parameters can
--        be specified on this property.
--
--     Conformance:  The property can be specified once in "VEVENT",
--        "VTODO", or "VJOURNAL" calendar components.  The value MUST be
--        specified as a date with UTC time.
--
--     Description:  This property specifies the date and time that the
--        calendar information was created by the calendar user agent in the
--        calendar store.
--
--     Format Definition:  This property is defined by the following
--        notation:
--
--         created    = "CREATED" creaparam ":" date-time CRLF
--
--         creaparam  = *(";" other-param)
--
--     Example:  The following is an example of this property:
--
--         CREATED:19960329T133000Z
-- @
--
-- Because the spec says "The value MUST bespecified as a date with UTC time.",
-- we will just store the 'LocalTime' (in the utc timezone) instead of a
-- 'DateTime'
newtype Created = Created {unCreated :: Time.LocalTime}
  deriving (Show, Eq, Generic)

instance Validity Created where
  validate c@Created {..} =
    mconcat
      [ genericValidate c,
        validateImpreciseLocalTime unCreated
      ]

instance IsProperty Created where
  propertyName Proxy = "CREATED"
  propertyP = createdP
  propertyB = createdB

createdP :: ContentLineValue -> Either String Created
createdP = fmap Created . dateTimeUTCP

createdB :: Created -> ContentLineValue
createdB = dateTimeUTCB . unCreated

-- [section 3.8.1.5](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.1.5)
--
-- @
--     Property Name:  DESCRIPTION
--
--     Purpose:  This property provides a more complete description of the
--        calendar component than that provided by the "SUMMARY" property.
--
--     Value Type:  TEXT
--
--     Property Parameters:  IANA, non-standard, alternate text
--        representation, and language property parameters can be specified
--        on this property.
--     Conformance:  The property can be specified in the "VEVENT", "VTODO",
--        "VJOURNAL", or "VALARM" calendar components.  The property can be
--        specified multiple times only within a "VJOURNAL" calendar
--        component.
--
--     Description:  This property is used in the "VEVENT" and "VTODO" to
--        capture lengthy textual descriptions associated with the activity.
--
--        This property is used in the "VJOURNAL" calendar component to
--        capture one or more textual journal entries.
--
--        This property is used in the "VALARM" calendar component to
--        capture the display text for a DISPLAY category of alarm, and to
--        capture the body text for an EMAIL category of alarm.
--
--     Format Definition:  This property is defined by the following
--        notation:
--
--         description = "DESCRIPTION" descparam ":" text CRLF
--
--         descparam   = *(
--                     ;
--                     ; The following are OPTIONAL,
--                     ; but MUST NOT occur more than once.
--                     ;
--                     (";" altrepparam) / (";" languageparam) /
--                     ;
--                     ; The following is OPTIONAL,
--                     ; and MAY occur more than once.
--                     ;
--                     (";" other-param)
--                     ;
--                     )
--
--     Example:  The following is an example of this property with formatted
--        line breaks in the property value:
--
--         DESCRIPTION:Meeting to provide technical review for "Phoenix"
--           design.\nHappy Face Conference Room. Phoenix design team
--           MUST attend this meeting.\nRSVP to team leader.
-- @
newtype Description = Description {unDescription :: Text}
  deriving (Show, Eq, Generic)

instance Validity Description

instance IsProperty Description where
  propertyName Proxy = "DESCRIPTION"
  propertyP = descriptionP
  propertyB = descriptionB

descriptionP :: ContentLineValue -> Either String Description
descriptionP = fmap Description . propertyTypeP

descriptionB :: Description -> ContentLineValue
descriptionB = propertyTypeB . unDescription
