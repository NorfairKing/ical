{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyTypeSpec where

import Control.Monad
import Data.Text (Text)
import ICal.ContentLine
import ICal.PropertyType.Class
import ICal.PropertyType.Gen
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Text" $ do
    propertyTypeSpec @Text
    let examples :: [(Text, ContentLineValue)]
        examples =
          [ ( "Project XYZ Final Review\nConference Room - 3B\nCome Prepared.",
              mkSimpleContentLineValue "Project XYZ Final Review\\nConference Room - 3B\\nCome Prepared."
            )
          ]
    describe "examples" $
      forM_ examples $ \(text, clv) -> do
        it "renders this example text correctly" $
          propertyTypeB text `shouldBe` clv
        it "parses this example text correctly" $
          propertyTypeP clv `shouldBe` Right text

  describe "escapeText" $ do
    it "escapes this diverse example correctly" $
      escapeText "hello world\n\\,;," `shouldBe` "hello world\\n\\\\\\,\\;\\,"
    it "roundtrips with unEscapeText" $
      forAllValid $ \text -> escapeText (unEscapeText text) `shouldBe` text
  describe "unEscapeText" $ do
    it "unEscapes this diverse example correctly" $
      unEscapeText "hello world\\n\\N\\\\\\,\\;\\," `shouldBe` "hello world\n\n\\,;,"
    it "roundtrips with escapeText" $
      forAllValid $ \text -> unEscapeText (escapeText text) `shouldBe` text
