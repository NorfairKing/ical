{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyType.URISpec where

import Conformance
import ICal.ContentLine
import ICal.PropertyType
import ICal.PropertyType.Gen
import qualified Network.URI as Network
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "URI" $ do
    genValidSpec @URI
    propertyTypeSpec @URI

    uri1 <- liftIO $ maybe (expectationFailure "could not parse URI") pure $ Network.parseURI "http://example.com/my-report.txt"

    propertyTypeExampleSpec
      (mkSimpleContentLineValue "http://example.com/my-report.txt")
      (URI uri1)

    uri2 <- liftIO $ maybe (expectationFailure "could not parse URI") pure $ Network.parseURI "http://some.url.with.a.comma/foo,bar"
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "http://some.url.with.a.comma/foo,bar")
      (URI uri2)

    it "can parse (with a fixable error) an incorrectly text-encoded uri" $
      let clv = mkSimpleContentLineValue "http://some.url.with.a.comma/foo\\,bar"
          expected = URI uri2
       in context (show clv) $
            case runConformLenient $ propertyTypeP clv of
              Left err -> expectationFailure $ show err
              Right (actual, notes) -> do
                actual `shouldBe` expected
                notes
                  `shouldBe` Notes
                    { notesFixableErrors = [UrlTextEncoded "http://some.url.with.a.comma/foo\\,bar"],
                      notesWarnings = []
                    }
