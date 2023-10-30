{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.PropertyType.URI
  ( URI (..),
    parseURI,
    renderURI,
  )
where

import Conformance
import Control.DeepSeq
import Data.Proxy
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import Data.Validity.URI
import GHC.Generics (Generic)
import ICal.ContentLine
import ICal.Parameter.ValueDataType
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

instance IsString URI where
  fromString s = case parseURI (fromString s) of
    Nothing -> error $ "unparseable URI in literal string: " <> s
    Just ca -> ca

instance IsPropertyType URI where
  propertyTypeValueType Proxy = TypeURI
  propertyTypeP clv = do
    -- RFC5545 specifically says in
    -- [Section 3.3.13](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.13):
    -- @
    -- No additional content value encoding (i.e., BACKSLASH character
    -- encoding, see Section 3.3.11) is defined for this value type.
    -- @
    -- This means that we must parse the URI from 'contentLineValueRaw' and not parse the 'ContentLineValue' as a 'Text' Property Type first instead.
    -- However, some providers (such as Google, on 2023-09-27, for example) incorrectly encode commas in URL properties (or other properties with URI property type).
    --
    -- RFC7986 specifically mentions the problem of invalid URI parsing in
    -- [Section 3](https://www.rfc-editor.org/rfc/rfc7986.html#section-3)
    -- @
    -- For example, if a new property
    -- "FOO" were defined with a default value type of URI and a URI value
    -- with a comma was used, an iCalendar generator not aware of this fact
    -- would likely treat the property value as "TEXT" and apply backslash
    -- escaping to the comma in the value, effectively making it an invalid
    -- URI value.
    -- @
    -- To solve this problem, we try to parse the URI correctly.
    -- If that fails, we try to use a fixable error to try and parse the value
    -- if it does succeed after decoding the content line value as a text
    -- property type value instead.
    let t = contentLineValueRaw clv
    case parseURI t of
      Just u -> pure u
      Nothing -> do
        case parseURI (unEscapeText t) of
          Nothing -> unfixableError $ UnparseableURI t
          Just uri -> do
            emitFixableError $ UrlTextEncoded t
            pure uri
  propertyTypeB = mkSimpleContentLineValue . renderURI

parseURI :: Text -> Maybe URI
parseURI = fmap URI . Network.parseURIReference . T.unpack

renderURI :: URI -> Text
renderURI = T.pack . dangerousURIToString . unURI
