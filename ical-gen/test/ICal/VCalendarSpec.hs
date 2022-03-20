{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ICal.VCalendarSpec where

import Data.DList (DList (..))
import qualified Data.DList as DList
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Builder as Text
import ICal.ContentLine
import ICal.VCalendar
import ICal.VCalendar.Gen ()
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity
import Text.Megaparsec
import Text.Show.Pretty (ppShow)

spec :: Spec
spec = do
  describe "ProdId" $ do
    genValidSpec @ProdId
  describe "prodIdP" $ do
    parseSucceedsSpec prodIdP ["PRODID:Example"] (ProdId "Example")
    it "roundtrips with prodIdB" $ parserBuilderRoundtrip prodIdP prodIdB

  describe "Version" $
    genValidSpec @Version
  describe "versionP" $ do
    parseSucceedsSpec versionP ["VERSION:2.0"] (Version "2.0")
    it "roundtrips with versionB" $ parserBuilderRoundtrip versionP versionB

  describe "Event" $
    genValidSpec @Event
  describe "vEventP" $
    it "roundtrips with vEventB" $ parserBuilderRoundtrip vEventP vEventB

  describe "Calendar" $
    genValidSpec @Calendar
  describe "vCalendarP" $
    it "roundtrips with vCalendarB" $ parserBuilderRoundtrip vCalendarP vCalendarB

parseSucceedsSpec ::
  (Show a, Eq a) =>
  CP a ->
  [ContentLine] ->
  a ->
  Spec
parseSucceedsSpec parser contentLines expected =
  it (unwords ["parses", show contentLines, "into", show expected]) $
    case parse parser "test input" contentLines of
      Left err -> expectationFailure $ show err
      Right actual -> actual `shouldBe` expected

parserBuilderRoundtrip ::
  (Show a, Eq a, GenValid a) =>
  CP a ->
  (a -> DList ContentLine) ->
  Property
parserBuilderRoundtrip parser builder = forAllValid $ \a ->
  let rendered = DList.toList $ builder a
   in context (ppShow rendered) $
        case parse parser "test input" rendered of
          Left err -> expectationFailure $ show err
          Right parsed -> parsed `shouldBe` a
