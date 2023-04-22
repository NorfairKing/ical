{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyType.CalAddressSpec where

import ICal.ContentLine
import ICal.PropertyType.CalAddress
import ICal.PropertyType.Gen
import qualified Network.URI as Network
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "CalAddress" $ do
    genValidSpec @CalAddress
    propertyTypeSpec @CalAddress

    -- From the spec:
    -- https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.3
    -- @
    -- Example:
    --
    --  mailto:jane_doe@example.com
    -- @

    uri <- liftIO $ maybe (expectationFailure "could not parse CalAddress") pure $ Network.parseURI "mailto:jane_doe@example.com"

    propertyTypeExampleSpec
      (mkSimpleContentLineValue "mailto:jane_doe@example.com")
      (CalAddress uri)
