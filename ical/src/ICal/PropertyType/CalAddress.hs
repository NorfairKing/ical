{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.PropertyType.CalAddress
  ( CalAddress (..),
    parseCalAddress,
    renderCalAddress,
  )
where

import Control.DeepSeq
import Data.Proxy
import Data.String
import Data.Text (Text)
import Data.Validity
import GHC.Generics (Generic)
import ICal.ContentLine
import ICal.Parameter.ValueDataType
import ICal.PropertyType.Class
import ICal.PropertyType.URI
import qualified Network.URI as Network

-- | Calendar User Address
--
-- === [section 3.3.3](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.3)
--
-- @
-- Value Name:  CAL-ADDRESS
--
-- Purpose:  This value type is used to identify properties that contain
--    a calendar user address.
--
-- Format Definition:  This value type is defined by the following
--    notation:
--
--     cal-address        = uri
--
-- Description:  The value is a URI as defined by [RFC3986] or any other
--    IANA-registered form for a URI.  When used to address an Internet
--    email transport address for a calendar user, the value MUST be a
--    mailto URI, as defined by [RFC2368].  No additional content value
--    encoding (i.e., BACKSLASH character encoding, see Section 3.3.11)
--    is defined for this value type.
--
-- Example:
--
--  mailto:jane_doe@example.com
-- @
newtype CalAddress = CalAddress {unCalAddress :: Network.URI} -- Consider not making this a newtype.
  deriving (Show, Eq, Ord, Generic)

instance Validity CalAddress

instance NFData CalAddress

instance IsString CalAddress where
  fromString s = case parseCalAddress (fromString s) of
    Nothing -> error "unparseable calendar address"
    Just ca -> ca

instance IsPropertyType CalAddress where
  propertyTypeValueType Proxy = TypeCalendarAddress
  propertyTypeP clv = do
    URI uri <- propertyTypeP clv
    pure $ CalAddress uri
  propertyTypeB = mkSimpleContentLineValue . renderCalAddress

parseCalAddress :: Text -> Maybe CalAddress
parseCalAddress = fmap (CalAddress . unURI) . parseURI

renderCalAddress :: CalAddress -> Text
renderCalAddress = renderURI . URI . unCalAddress
