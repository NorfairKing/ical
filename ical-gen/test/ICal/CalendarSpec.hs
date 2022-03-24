{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.CalendarSpec where

import qualified Data.DList as DList
import qualified Data.Text as T
import ICal.Calendar
import ICal.Calendar.Gen ()
import ICal.ContentLine
import ICal.UnfoldedLine
import Test.Syd
import Test.Syd.Validity
import Text.Megaparsec

spec :: Spec
spec = do
  describe "Event" $ do
    genValidSpec @Event
    componentSpec @TimeZone

  describe "TimeZone" $ do
    genValidSpec @TimeZone
    componentSpec @TimeZone

  describe "Calendar" $ do
    genValidSpec @Calendar
    componentSpec @Calendar

componentSpec ::
  forall a.
  (Show a, Eq a, GenValid a, IsComponent a) =>
  Spec
componentSpec = do
  it "renders to a valid list of ContentLines" $
    forAllValid $ \component ->
      shouldBeValid $ DList.toList $ componentB (component :: a)
  it "roundtrips through ContentLines" $
    forAllValid $ \a ->
      let rendered = DList.toList $ componentB (a :: a)
          renderedText = renderUnfoldedLinesText $ map renderContentLine rendered
          ctx =
            unlines
              [ "Internal representation:",
                ppShow rendered,
                "",
                "Textual representation:",
                T.unpack renderedText
              ]
       in context ctx $
            case parse componentP "test input" rendered of
              Left err -> expectationFailure $ errorBundlePretty err
              Right parsed -> parsed `shouldBe` a
