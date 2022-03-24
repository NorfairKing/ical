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
  -- | Parser for the property
  propertyP :: ContentLine -> Either String property

  -- | Builder for the property
  propertyB :: property -> ContentLine

parsePropertyWithName :: ContentLineName -> (ContentLine -> Either String a) -> (ContentLine -> Either String a)
parsePropertyWithName name func cln =
  if contentLineName cln == name
    then func cln
    else
      Left $
        unwords
          [ "Expected content line with name",
            show name,
            "but got",
            show $ contentLineName cln,
            "instead."
          ]

propertyWithNameP :: IsPropertyType propertyType => ContentLineName -> (ContentLine -> Either String propertyType)
propertyWithNameP cln = parsePropertyWithName cln $ \ContentLine {..} ->
  propertyTypeP contentLineValue

propertyWithNameB :: IsPropertyType propertyType => ContentLineName -> (propertyType -> ContentLine)
propertyWithNameB cln = ContentLine cln . propertyTypeB

newtype Begin = Begin {unBegin :: Text}
  deriving (Show, Eq, Generic)

instance Validity Begin

instance IsProperty Begin where
  propertyP = beginP
  propertyB = beginB

beginP :: ContentLine -> Either String Begin
beginP = fmap Begin . propertyWithNameP "BEGIN"

beginB :: Begin -> ContentLine
beginB = propertyWithNameB "BEGIN" . unBegin

newtype End = End {unEnd :: Text}
  deriving (Show, Eq, Generic)

instance Validity End

instance IsProperty End where
  propertyP = endP
  propertyB = endB

endP :: ContentLine -> Either String End
endP = fmap End . propertyWithNameP "END"

endB :: End -> ContentLine
endB = propertyWithNameB "END" . unEnd

-- [section 3.7.3](https://datatracker.ietf.org/doc/html/rfc5545#section-3.7.3)
newtype ProdId = ProdId {unProdId :: Text}
  deriving (Show, Eq, Generic)

instance Validity ProdId

instance IsProperty ProdId where
  propertyP = prodIdP
  propertyB = prodIdB

prodIdP :: ContentLine -> Either String ProdId
prodIdP = fmap ProdId . propertyWithNameP "PRODID"

prodIdB :: ProdId -> ContentLine
prodIdB = propertyWithNameB "PRODID" . unProdId

-- [section 3.7.4](https://datatracker.ietf.org/doc/html/rfc5545#section-3.7.4)
newtype Version = Version {unVersion :: Text}
  deriving (Show, Eq, Generic)

instance Validity Version

instance IsProperty Version where
  propertyP = versionP
  propertyB = versionB

versionP :: ContentLine -> Either String Version
versionP = fmap Version . propertyWithNameP "VERSION"

versionB :: Version -> ContentLine
versionB = propertyWithNameB "VERSION" . unVersion

-- [section 3.8.4.7](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.4.7)
newtype UID = UID {unUID :: Text}
  deriving (Show, Eq, Generic)

instance Validity UID

instance IsProperty UID where
  propertyP = uidP
  propertyB = uidB

uidP :: ContentLine -> Either String UID
uidP = fmap UID . propertyWithNameP "UID"

uidB :: UID -> ContentLine
uidB = propertyWithNameB "UID" . unUID

-- [section 3.8.7.2](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.7.2)
newtype DateTimeStamp = DateTimeStamp {unDateTimeStamp :: DateTime}
  deriving (Show, Eq, Generic)

instance Validity DateTimeStamp

instance IsProperty DateTimeStamp where
  propertyP = dateTimeStampP
  propertyB = dateTimeStampB

dateTimeStampP :: ContentLine -> Either String DateTimeStamp
dateTimeStampP = fmap DateTimeStamp . propertyWithNameP "DTSTAMP"

dateTimeStampB :: DateTimeStamp -> ContentLine
dateTimeStampB = propertyWithNameB "DTSTAMP" . unDateTimeStamp

-- [section 3.8.3.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.3.1)
newtype TZID = TZID {unTZID :: Text}
  deriving (Show, Eq, Generic)

instance Validity TZID

instance IsProperty TZID where
  propertyP = tzIDP
  propertyB = tzIDB

tzIDP :: ContentLine -> Either String TZID
tzIDP = fmap TZID . propertyWithNameP "TZID"

tzIDB :: TZID -> ContentLine
tzIDB = propertyWithNameB "TZID" . unTZID

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
  propertyP = dateTimeStartP
  propertyB = dateTimeStartB

dateTimeStartP :: ContentLine -> Either String DateTimeStart
dateTimeStartP cl =
  (DateTimeStartDate <$> propertyWithNameP "DTSTART" cl)
    <|> (DateTimeStartDateTime <$> propertyWithNameP "DTSTART" cl)

dateTimeStartB :: DateTimeStart -> ContentLine
dateTimeStartB = \case
  DateTimeStartDate date -> propertyWithNameB "DTSTART" date
  DateTimeStartDateTime dateTime -> propertyWithNameB "DTSTART" dateTime

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
  propertyP = createdP
  propertyB = createdB

createdP :: ContentLine -> Either String Created
createdP = parsePropertyWithName "CREATED" (fmap Created . dateTimeUTCP . contentLineValue)

createdB :: Created -> ContentLine
createdB = ContentLine "CREATED" . dateTimeUTCB . unCreated
