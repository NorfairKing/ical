{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.CalendarSpec where

import Data.DList (DList (..))
import qualified Data.DList as DList
import qualified Data.Text as T
import ICal.Calendar
import ICal.Calendar.Gen ()
import ICal.ContentLine
import ICal.UnfoldedLine
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity
import Text.Megaparsec

spec :: Spec
spec = do
  describe "ProdId" $ do
    genValidSpec @ProdId
    propertySpec @ProdId
  describe "prodIdP" $ do
    it "works for this example" $
      prodIdP "PRODID:Example" `shouldBe` Right (ProdId "Example")

  describe "Version" $ do
    genValidSpec @Version
    propertySpec @Version
  describe "versionP" $ do
    it "works for this example" $
      versionP "VERSION:2.0" `shouldBe` Right (Version "2.0")

  describe "UID" $ do
    genValidSpec @UID
    propertySpec @UID
  describe "uidP" $ do
    it "works for this example" $
      uidP "UID:19960401T080045Z-4000F192713-0052@example.com" `shouldBe` Right (UID "19960401T080045Z-4000F192713-0052@example.com")

  describe "TZID" $ do
    genValidSpec @TZID
    propertySpec @TZID
  describe "tzIDP" $ do
    it "works for these examples" $ do
      tzIDP "TZID:America/New_York" `shouldBe` Right (TZID "America/New_York")
      tzIDP "TZID:America/Los_Angeles" `shouldBe` Right (TZID "America/Los_Angeles")
      tzIDP "TZID:/example.org/America/New_York" `shouldBe` Right (TZID "/example.org/America/New_York")

  describe "DateTimeStamp" $ do
    genValidSpec @DateTimeStamp
    propertySpec @DateTimeStamp
  describe "dateTimeStampP" $ do
    -- TODO example-based test
    pure ()

  describe "Event" $ do
    genValidSpec @Event
    componentSpec @TimeZone

  describe "TimeZone" $ do
    genValidSpec @TimeZone
    componentSpec @TimeZone

  describe "Calendar" $ do
    genValidSpec @Calendar
    componentSpec @Calendar

parseSucceedsSpec ::
  (Show a, Eq a) =>
  CP a ->
  [ContentLine] ->
  a ->
  Spec
parseSucceedsSpec parser contentLines expected =
  it "parses these content lines correctly" $
    case parse parser "test input" contentLines of
      Left err -> expectationFailure $ errorBundlePretty err
      Right actual -> actual `shouldBe` expected

componentSpec ::
  forall a.
  (Show a, Eq a, GenValid a, IsComponent a) =>
  Spec
componentSpec = do
  it "renders to a valid list of ContentLines" $
    forAllValid $ \component ->
      shouldBeValid $ DList.toList $ componentB (component :: a)
  it "roundtrips through ContentLines" $
    parserBuilderRoundtrip (componentP :: CP a) componentB

parserBuilderRoundtrip ::
  (Show a, Eq a, GenValid a) =>
  CP a ->
  (a -> DList ContentLine) ->
  Property
parserBuilderRoundtrip parser builder = forAllValid $ \a ->
  let rendered = DList.toList $ builder a
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
        case parse parser "test input" rendered of
          Left err -> expectationFailure $ errorBundlePretty err
          Right parsed -> parsed `shouldBe` a

propertySpec ::
  forall a.
  (Show a, Eq a, GenValid a, IsProperty a) =>
  Spec
propertySpec = do
  it "always renders to a valid content line" $
    forAllValid $ \a ->
      shouldBeValid $ propertyB (a :: a)
  it "roundtrips through ContentLine" $
    forAllValid $ \a ->
      let rendered = propertyB (a :: a)
       in case propertyP rendered of
            Left err -> expectationFailure err
            Right actual -> actual `shouldBe` a
