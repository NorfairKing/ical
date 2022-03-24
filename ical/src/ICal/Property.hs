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

import Data.Text (Text)
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

propertyWithNameP :: ContentLineName -> (ContentLine -> Either String a) -> (ContentLine -> Either String a)
propertyWithNameP name func cln =
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

newtype Begin = Begin {unBegin :: Text}
  deriving (Show, Eq, Generic)

instance Validity Begin

instance IsProperty Begin where
  propertyP = beginP
  propertyB = beginB

beginP :: ContentLine -> Either String Begin
beginP = propertyWithNameP "BEGIN" $ \ContentLine {..} ->
  Right Begin {unBegin = contentLineValueRaw contentLineValue}

beginB :: Begin -> ContentLine
beginB = mkSimpleContentLine "BEGIN" . unBegin

newtype End = End {unEnd :: Text}
  deriving (Show, Eq, Generic)

instance Validity End

instance IsProperty End where
  propertyP = endP
  propertyB = endB

endP :: ContentLine -> Either String End
endP = propertyWithNameP "END" $ \ContentLine {..} ->
  Right End {unEnd = contentLineValueRaw contentLineValue}

endB :: End -> ContentLine
endB = mkSimpleContentLine "END" . unEnd

-- [section 3.7.3](https://datatracker.ietf.org/doc/html/rfc5545#section-3.7.3)
newtype ProdId = ProdId {unProdId :: Text}
  deriving (Show, Eq, Generic)

instance Validity ProdId

instance IsProperty ProdId where
  propertyP = prodIdP
  propertyB = prodIdB

prodIdP :: ContentLine -> Either String ProdId
prodIdP = propertyWithNameP "PRODID" $ \ContentLine {..} ->
  Right ProdId {unProdId = contentLineValueRaw contentLineValue}

prodIdB :: ProdId -> ContentLine
prodIdB = mkSimpleContentLine "PRODID" . unProdId

-- [section 3.7.4](https://datatracker.ietf.org/doc/html/rfc5545#section-3.7.4)
newtype Version = Version {unVersion :: Text}
  deriving (Show, Eq, Generic)

instance Validity Version

instance IsProperty Version where
  propertyP = versionP
  propertyB = versionB

versionP :: ContentLine -> Either String Version
versionP = propertyWithNameP "VERSION" $ \ContentLine {..} ->
  Right $ Version {unVersion = contentLineValueRaw contentLineValue}

versionB :: Version -> ContentLine
versionB = mkSimpleContentLine "VERSION" . unVersion

-- [section 3.8.4.7](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.4.7)
newtype UID = UID {unUID :: Text}
  deriving (Show, Eq, Generic)

instance Validity UID

instance IsProperty UID where
  propertyP = uidP
  propertyB = uidB

uidP :: ContentLine -> Either String UID
uidP = propertyWithNameP "UID" $ \ContentLine {..} ->
  Right $ UID {unUID = contentLineValueRaw contentLineValue}

uidB :: UID -> ContentLine
uidB = mkSimpleContentLine "UID" . unUID

-- [section 3.8.7.2](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.7.2)
newtype DateTimeStamp = DateTimeStamp {unDateTimeStamp :: DateTime}
  deriving (Show, Eq, Generic)

instance Validity DateTimeStamp

instance IsProperty DateTimeStamp where
  propertyP = dateTimeStampP
  propertyB = dateTimeStampB

dateTimeStampP :: ContentLine -> Either String DateTimeStamp
dateTimeStampP = propertyWithNameP "DTSTAMP" $ \ContentLine {..} ->
  DateTimeStamp <$> dateTimeP contentLineValue

dateTimeStampB :: DateTimeStamp -> ContentLine
dateTimeStampB = ContentLine "DTSTAMP" . dateTimeB . unDateTimeStamp

-- [section 3.8.3.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.3.1)
newtype TZID = TZID {unTZID :: Text}
  deriving (Show, Eq, Generic)

instance Validity TZID

instance IsProperty TZID where
  propertyP = tzIDP
  propertyB = tzIDB

tzIDP :: ContentLine -> Either String TZID
tzIDP = propertyWithNameP "TZID" $ \ContentLine {..} ->
  Right $ TZID {unTZID = contentLineValueRaw contentLineValue}

tzIDB :: TZID -> ContentLine
tzIDB = mkSimpleContentLine "TZID" . unTZID
