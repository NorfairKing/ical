{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyType.URISpec where

import Control.Monad
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
  describe "uriP" $ do
    uri <- liftIO $ maybe (expectationFailure "could not parse URI") pure $ Network.parseURI "http://example.com/my-report.txt"

    let examples :: [(URI, ContentLineValue)]
        examples =
          [ (URI uri, mkSimpleContentLineValue "http://example.com/my-report.txt")
          ]
    describe "examples" $
      forM_ examples $ \(u, text) -> do
        it "renders this example uri correctly" $
          uriB u `shouldBe` text
        it "parses this example text correctly" $
          uriP text `shouldBe` Right u
