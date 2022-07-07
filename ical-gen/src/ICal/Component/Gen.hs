{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Component.Gen where

import qualified Data.DList as DList
import Data.GenValidity
import Data.GenValidity.CaseInsensitive ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import qualified Data.Text as T
import ICal.Component
import ICal.ContentLine
import ICal.ContentLine.Gen ()
import ICal.Property.Gen ()
import ICal.PropertyType.Duration.Gen ()
import ICal.PropertyType.Gen ()
import ICal.PropertyType.RecurrenceRule.Gen ()
import ICal.UnfoldedLine
import Test.Syd
import Test.Syd.Validity
import Text.Megaparsec

instance GenValid Calendar

instance GenValid Event

instance GenValid TimeZone

componentSpec ::
  forall a.
  (Show a, Eq a, GenValid a, IsComponent a) =>
  Spec
componentSpec = do
  it "renders to a valid list of ContentLines" $
    forAllValid $ \component ->
      shouldBeValid $ DList.toList $ componentB (component :: a)

  it "parses only valid things" $
    forAllValid $ \component ->
      case parse componentP "test input" (DList.toList (componentB (component :: a))) of
        Left _ -> pure ()
        Right a -> shouldBeValid (a :: a)

  it "roundtrips through ContentLines" $
    forAllValid $ \a ->
      let rendered = DList.toList $ componentB (a :: a)
          renderedText = renderUnfoldedLines $ map renderContentLineToUnfoldedLine rendered
          ctx =
            unlines
              [ -- "Internal representation:",
                -- ppShow rendered,
                -- "",
                "Textual representation:",
                T.unpack renderedText
              ]
       in context ctx $
            case parse componentP "test input" rendered of
              Left err -> expectationFailure $ errorBundlePretty err
              Right parsed -> parsed `shouldBe` a
