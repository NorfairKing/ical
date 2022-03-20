{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ICal.ContentLineSpec where

import Control.Monad
import qualified Data.ByteString as SB
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Builder as Text
import ICal.ContentLine
import ICal.ContentLine.Gen ()
import ICal.UnfoldedLine
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity
import Text.Megaparsec

spec :: Spec
spec = do
  describe "VendorId" $ do
    genValidSpec @VendorId
    it "roundtrips VendorIds" $ parserBuilderRoundtrip vendorIdP vendorIdB

  describe "ParamValue" $ do
    genValidSpec @ParamValue
    it "roundtrips ParamValues" $ parserBuilderRoundtrip paramValueP paramValueB

  describe "ParamName" $ do
    genValidSpec @ParamName
    it "roundtrips ParamNames" $ parserBuilderRoundtrip paramNameP paramNameB

  describe "ContentLineName" $ do
    genValidSpec @ContentLineName
    it "roundtrips ContentLineNames" $ parserBuilderRoundtrip contentLineNameP contentLineNameB

  describe "ContentLine" $ do
    genValidSpec @ContentLine
    it "roundtrips ContentLines" $ parserBuilderRoundtrip contentLineP contentLineB

  describe "parseContentLine" $
    it "roundtrips with renderContentLine" $
      forAllValid $ \contentLine ->
        case parseContentLine (renderContentLine contentLine) of
          Left err -> expectationFailure err
          Right actual -> actual `shouldBe` contentLine

  describe "examples" $ do
    -- Examples from the spec
    let examples :: [(ContentLine, Text)]
        examples =
          [ -- https://datatracker.ietf.org/doc/html/rfc5545#section-3.1
            ( ContentLine
                { contentLineName = "ATTENDEE",
                  contentLineParams =
                    M.fromList
                      [ ("RSVP", ["TRUE"]),
                        ("ROLE", ["REQ-PARTICIPANT"])
                      ],
                  contentLineValue = "mailto:jsmith@example.com"
                },
              "ATTENDEE;RSVP=TRUE;ROLE=REQ-PARTICIPANT:mailto:jsmith@example.com"
            ),
            ( ContentLine
                { contentLineName = "RDATE",
                  contentLineParams = M.fromList [("VALUE", ["DATE"])],
                  contentLineValue = "19970304,19970504,19970704,19970904"
                },
              "RDATE;VALUE=DATE:19970304,19970504,19970704,19970904"
            ),
            ( ContentLine
                { contentLineName = "ATTACH",
                  contentLineParams = M.empty,
                  contentLineValue = "http://example.com/public/quarterly-report.doc"
                },
              "ATTACH:http://example.com/public/quarterly-report.doc"
            ),
            ( ContentLine
                { contentLineName = "ATTACH",
                  contentLineParams =
                    M.fromList
                      [ ("FMTTYPE", ["text/plain"]),
                        ("ENCODING", ["BASE64"]),
                        ("VALUE", ["BINARY"])
                      ],
                  contentLineValue = "http://example.com/public/quarterly-report.doc"
                },
              "ATTACH;FMTTYPE=text/plain;ENCODING=BASE64;VALUE=BINARY:http://example.com/public/quarterly-report.doc"
            ), -- https://datatracker.ietf.org/doc/html/rfc5545#section-3.1
            ( ContentLine
                { contentLineName = "DESCRIPTION",
                  contentLineParams = M.fromList [("ALTREP", ["cid:part1.0001@example.org"])],
                  contentLineValue = "The Fall'98 Wild Wizards Conference - - Las Vegas\\, NV\\, USA"
                },
              "DESCRIPTION;ALTREP=\"cid:part1.0001@example.org\":The Fall'98 Wild Wizards Conference - - Las Vegas\\, NV\\, USA"
            )
          ]
    forM_ examples $ \(contentLine, rendered) -> do
      it (unwords ["renders", show contentLine, "as", show rendered]) $
        let actualRendered = renderContentLine contentLine
         in case (,) <$> parseContentLine actualRendered <*> parseContentLine (UnfoldedLine rendered) of
              Left err -> expectationFailure err
              Right (actual, expected) -> actual `shouldBe` expected
      it (unwords ["parses", show rendered, "into", show contentLine]) $
        case parseContentLine (UnfoldedLine rendered) of
          Left err -> expectationFailure err
          Right actual -> actual `shouldBe` contentLine

    -- Tests based on example calendars
    scenarioDirRecur "test_resources/calendars" $ \calFile ->
      it "can parse and unfold every line" $ do
        contents <- SB.readFile calFile
        case parseUnfoldedLinesByteString contents >>= mapM parseContentLine of
          Left err -> expectationFailure err
          Right contentLines -> shouldBeValid contentLines

parserBuilderRoundtrip ::
  (Show a, Eq a, GenValid a) =>
  P a ->
  (a -> Text.Builder) ->
  Property
parserBuilderRoundtrip parser builder = forAllValid $ \a ->
  let rendered = LT.toStrict $ LTB.toLazyText $ builder a
   in case parse parser "test input" rendered of
        Left err -> expectationFailure $ errorBundlePretty err
        Right parsed -> parsed `shouldBe` a
