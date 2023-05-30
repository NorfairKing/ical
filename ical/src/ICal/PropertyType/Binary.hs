{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.PropertyType.Binary
  ( Binary (..),
    parseBinary,
    renderBinary,
  )
where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.ByteString.Base64
import Data.Proxy
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time as Time
import Data.Validity
import Data.Validity.ByteString ()
import Data.Void
import GHC.Generics (Generic)
import ICal.Conformance
import ICal.ContentLine
import ICal.Parameter
import ICal.PropertyType.Class

-- | Binary
--
-- === [section 3.3.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.1)
--
-- @
-- Value Name:  BINARY
--
-- Purpose:  This value type is used to identify properties that contain
--    a character encoding of inline binary data.  For example, an
--    inline attachment of a document might be included in an iCalendar
--    object.
--
-- Format Definition:  This value type is defined by the following
--    notation:
--
--     binary     = *(4b-char) [b-end]
--     ; A "BASE64" encoded character string, as defined by [RFC4648].
--
--     b-end      = (2b-char "==") / (3b-char "=")
--
--     b-char = ALPHA / DIGIT / "+" / "/"
--
-- Description:  Property values with this value type MUST also include
--    the inline encoding parameter sequence of ";ENCODING=BASE64".
--    That is, all inline binary data MUST first be character encoded
--    using the "BASE64" encoding method defined in [RFC2045].  No
--    additional content value encoding (i.e., BACKSLASH character
--    encoding, see Section 3.3.11) is defined for this value type.
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
newtype Binary = Binary {unBinary :: ByteString}
  deriving (Eq, Ord, Generic)

instance Show Binary where
  show = show . renderBinary

instance Validity Binary

instance NFData Binary

instance IsPropertyType Binary where
  propertyTypeValueType Proxy = TypeBinary
  propertyTypeP = binaryP
  propertyTypeB = binaryB

binaryP :: ContentLineValue -> Conform PropertyTypeParseError Void Void Binary
binaryP = parseBinary . contentLineValueRaw

binaryB :: Binary -> ContentLineValue
binaryB = mkSimpleContentLineValue . renderBinary

parseBinary :: Text -> Conform PropertyTypeParseError Void Void Binary
parseBinary t = case decodeBase64 (TE.encodeUtf8 t) of
  Left err -> unfixableError $ UnparseableBinary t err
  Right sb -> pure $ Binary sb

renderBinary :: Binary -> Text
renderBinary = encodeBase64 . unBinary
