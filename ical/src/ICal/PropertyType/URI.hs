{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.PropertyType.URI where

import Control.DeepSeq
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import Data.Validity.URI
import GHC.Generics (Generic)
import ICal.ContentLine
import ICal.PropertyType.Class
import qualified Network.URI as Network

-- | URI
--
-- === [section 3.3.13](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.13)
--
-- @
-- Value Name:  URI
--
-- Purpose:  This value type is used to identify values that contain a
--    uniform resource identifier (URI) type of reference to the
--    property value.
--
-- Format Definition:  This value type is defined by the following
--    notation:
--
--     uri = <As defined in Section 3 of [RFC3986]>
--
-- Description:  This value type might be used to reference binary
--    information, for values that are large, or otherwise undesirable
--    to include directly in the iCalendar object.
--
--    Property values with this value type MUST follow the generic URI
--    syntax defined in [RFC3986].
--
--    When a property parameter value is a URI value type, the URI MUST
--    be specified as a quoted-string value.
--
--    No additional content value encoding (i.e., BACKSLASH character
--    encoding, see Section 3.3.11) is defined for this value type.
--
-- Example:  The following is a URI for a network file:
--
--     http://example.com/my-report.txt
-- @
newtype URI = URI {unURI :: Network.URI} -- Consider not making this a newtype.
  deriving (Show, Eq, Ord, Generic)

instance Validity URI

instance NFData URI

instance IsPropertyType URI where
  propertyTypeP = uriP
  propertyTypeB = uriB

uriP :: ContentLineValue -> Either String URI
uriP = parseURI . contentLineValueRaw

uriB :: URI -> ContentLineValue
uriB = mkSimpleContentLineValue . renderURI

parseURI :: Text -> Either String URI
parseURI = fmap URI . maybe (Left "Unparseable URI") Right . Network.parseURIReference . T.unpack

renderURI :: URI -> Text
renderURI = T.pack . dangerousURIToString . unURI
