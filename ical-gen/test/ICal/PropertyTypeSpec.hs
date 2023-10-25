{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyTypeSpec where

import Data.GenValidity.Text
import Data.Int
import Data.Text (Text)
import ICal.ContentLine
import ICal.PropertyType.Class
import ICal.PropertyType.Gen
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Integer" $ do
    propertyTypeSpec @Int32
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "4")
      (4 :: Int32)
    propertyTypeParseExampleSpec
      (mkSimpleContentLineValue "+5")
      (5 :: Int32)
    propertyTypeRenderExampleSpec
      (mkSimpleContentLineValue "5")
      (5 :: Int32)
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "-6")
      (-6 :: Int32)

  describe "Boolean" $ do
    propertyTypeSpec @Bool
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "TRUE")
      True
    propertyTypeParseExampleSpec
      (mkSimpleContentLineValue "FALSE")
      False

  describe "Text" $ do
    propertyTypeSpec @Text
    propertyTypeExampleSpec
      (mkSimpleContentLineValue "Project XYZ Final Review\\nConference Room - 3B\\nCome Prepared.")
      ("Project XYZ Final Review\nConference Room - 3B\nCome Prepared." :: Text)

  let fancyCharacters = genTextBy $ elements ['\n', 'r', '\t', ' ', ';', ',', '\\']
  describe "escapeText" $ do
    it "escapes this diverse example correctly" $
      escapeText "hello world\n\\,;," `shouldBe` "hello world\\n\\\\\\,\\;\\,"

  describe "unEscapeText" $ do
    it "unEscapes this diverse example correctly" $
      unEscapeText "hello world\\n\\N\\\\\\,\\;\\," `shouldBe` "hello world\n\n\\,;,"

    it "roundtrips with escapeText" $
      forAllValid $ \text ->
        unEscapeText (escapeText text) `shouldBe` text

    it "roundtrips with escapeText for fancy characters" $
      forAll fancyCharacters $ \text ->
        unEscapeText (escapeText text) `shouldBe` text
