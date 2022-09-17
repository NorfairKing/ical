{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ICal.ContentLineSpec where

import Control.Arrow (left)
import Control.Monad
import qualified Data.ByteString as SB
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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

  describe "ContentLineValue" $ do
    genValidSpec @ContentLineValue
    it "roundtrips ContentLineValues" $ parserBuilderRoundtrip contentLineValueP contentLineValueB

  describe "ContentLine" $ do
    genValidSpec @ContentLine
    it "roundtrips ContentLines" $ parserBuilderRoundtrip contentLineP contentLineB

  describe "parseContentLineFromUnfoldedLine" $
    it "roundtrips with renderContentLineToUnfoldedLine" $
      forAllValid $ \contentLine ->
        case parseContentLineFromUnfoldedLine (renderContentLineToUnfoldedLine contentLine) of
          Left err -> expectationFailure err
          Right actual -> actual `shouldBe` contentLine

  describe "examples" $ do
    -- Examples from the spec
    let examples :: [(ContentLine, Text)]
        examples =
          [ -- https://datatracker.ietf.org/doc/html/rfc5545#section-3.1
            ( ContentLine
                { contentLineName = "ATTENDEE",
                  contentLineValue =
                    ContentLineValue
                      { contentLineValueParams =
                          M.fromList
                            [ ("RSVP", ["TRUE"]),
                              ("ROLE", ["REQ-PARTICIPANT"])
                            ],
                        contentLineValueRaw = "mailto:jsmith@example.com"
                      }
                },
              "ATTENDEE;RSVP=TRUE;ROLE=REQ-PARTICIPANT:mailto:jsmith@example.com"
            ),
            ( ContentLine
                { contentLineName = "RDATE",
                  contentLineValue =
                    ContentLineValue
                      { contentLineValueParams = M.fromList [("VALUE", ["DATE"])],
                        contentLineValueRaw = "19970304,19970504,19970704,19970904"
                      }
                },
              "RDATE;VALUE=DATE:19970304,19970504,19970704,19970904"
            ),
            ( ContentLine
                { contentLineName = "ATTACH",
                  contentLineValue =
                    ContentLineValue
                      { contentLineValueParams = M.empty,
                        contentLineValueRaw = "http://example.com/public/quarterly-report.doc"
                      }
                },
              "ATTACH:http://example.com/public/quarterly-report.doc"
            ),
            ( ContentLine
                { contentLineName = "ATTACH",
                  contentLineValue =
                    ContentLineValue
                      { contentLineValueParams =
                          M.fromList
                            [ ("FMTTYPE", ["text/plain"]),
                              ("ENCODING", ["BASE64"]),
                              ("VALUE", ["BINARY"])
                            ],
                        contentLineValueRaw = "http://example.com/public/quarterly-report.doc"
                      }
                },
              "ATTACH;FMTTYPE=text/plain;ENCODING=BASE64;VALUE=BINARY:http://example.com/public/quarterly-report.doc"
            ), -- https://datatracker.ietf.org/doc/html/rfc5545#section-3.1
            ( ContentLine
                { contentLineName = "DESCRIPTION",
                  contentLineValue =
                    ContentLineValue
                      { contentLineValueParams =
                          M.fromList
                            [ ( "ALTREP",
                                ["cid:part1.0001@example.org"]
                              )
                            ],
                        contentLineValueRaw = "The Fall'98 Wild Wizards Conference - - Las Vegas\\, NV\\, USA"
                      }
                },
              "DESCRIPTION;ALTREP=\"cid:part1.0001@example.org\":The Fall'98 Wild Wizards Conference - - Las Vegas\\, NV\\, USA"
            ), -- https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.10
            ( ContentLine
                { contentLineName = "DTSTART",
                  contentLineValue =
                    ContentLineValue
                      { contentLineValueParams =
                          M.fromList
                            [ ("TZID", [UnquotedParam "America/New_York"])
                            ],
                        contentLineValueRaw = "19970105T083000"
                      }
                },
              "DTSTART;TZID=America/New_York:19970105T083000"
            ),
            ( ContentLine
                { contentLineName = "RRULE",
                  contentLineValue =
                    ContentLineValue
                      { contentLineValueParams = M.empty,
                        contentLineValueRaw = "FREQ=YEARLY;INTERVAL=2;BYMONTH=1;BYDAY=SU;BYHOUR=8,9;BYMINUTE=30"
                      }
                },
              "RRULE:FREQ=YEARLY;INTERVAL=2;BYMONTH=1;BYDAY=SU;BYHOUR=8,9;BYMINUTE=30"
            )
          ]
    forM_ examples $ \(contentLine, rendered) -> do
      it "renders this example correctly" $
        let actualRendered = renderContentLineToUnfoldedLine contentLine
         in case (,)
              <$> parseContentLineFromUnfoldedLine actualRendered
              <*> parseContentLineFromUnfoldedLine (UnfoldedLine rendered) of
              Left err -> expectationFailure err
              Right (actual, expected) -> actual `shouldBe` expected
      it "parses this example correctly" $
        case parseContentLineFromUnfoldedLine (UnfoldedLine rendered) of
          Left err -> expectationFailure err
          Right actual -> actual `shouldBe` contentLine

    -- Tests based on example calendars
    scenarioDirRecur "test_resources/calendars" $ \calFile ->
      it "can parse and unfold every line" $ do
        contents <- SB.readFile calFile
        case mapM parseContentLineFromUnfoldedLine =<< left show . parseUnfoldedLines =<< left show (TE.decodeUtf8' contents) of
          Left err -> expectationFailure err
          Right contentLines -> shouldBeValid contentLines

parseSucceedsSpec :: (Show a, Eq a) => P a -> Text -> a -> Spec
parseSucceedsSpec parser string expected =
  it "parses this value correctly" $
    case parse parser "test input" string of
      Left err -> expectationFailure $ errorBundlePretty err
      Right parsed -> parsed `shouldBe` expected

parserBuilderRoundtrip ::
  (Show a, Eq a, GenValid a) =>
  P a ->
  (a -> Text.Builder) ->
  Property
parserBuilderRoundtrip parser builder = forAllValid $ \a ->
  let rendered = LT.toStrict $ LTB.toLazyText $ builder a
   in context (T.unpack rendered) $
        case parse parser "test input" rendered of
          Left err -> expectationFailure $ errorBundlePretty err
          Right parsed -> parsed `shouldBe` a
