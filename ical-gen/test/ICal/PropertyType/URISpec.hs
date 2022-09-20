{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyType.URISpec where

import ICal.ContentLine
import ICal.PropertyType.Gen
import ICal.PropertyType.URI
import qualified Network.URI as Network
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "URI" $ do
    genValidSpec @URI
    propertyTypeSpec @URI

    uri <- liftIO $ maybe (expectationFailure "could not parse URI") pure $ Network.parseURI "http://example.com/my-report.txt"

    propertyTypeExampleSpec
      (mkSimpleContentLineValue "http://example.com/my-report.txt")
      (URI uri)
