{-# LANGUAGE OverloadedStrings #-}

module ICal.ContentLineSpec where

import Control.Monad
import qualified Data.Map as M
import ICal.ContentLine
import ICal.ContentLine.Gen ()
import ICal.UnfoldedLine
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "parseContentLine" $
    it "roundtrips with renderContentLine" $
      forAllValid $ \contentLine ->
        parseContentLine (renderContentLine contentLine)
          `shouldBe` Right contentLine
  describe "examples" $ do
    -- Examples from the spec
    let examples =
          [ -- https://datatracker.ietf.org/doc/html/rfc5545#section-3.1
            ( ContentLine
                { contentLineName = "ATTENDEE",
                  contentLineParams = M.fromList [("RSVP", "TRUE"), ("ROLE", "REQ-PARTICIPANT")],
                  contentLineValue = "mailto:jsmith@example.com"
                },
              "ATTENDEE;RSVP=TRUE;ROLE=REQ-PARTICIPANT:mailto:jsmith@example.com"
            ),
            ( ContentLine
                { contentLineName = "RDATE",
                  contentLineParams = M.fromList [("VALUE", "DATE")],
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
                  contentLineParams = M.fromList [("FMTTYPE", "text/plain"), ("ENCODING", "BASE64"), ("VALUE", "BINARY")],
                  contentLineValue = "http://example.com/public/quarterly-report.doc"
                },
              "     ATTACH;FMTTYPE=text/plain;ENCODING=BASE64;VALUE=BINARY:VGhlIHF1aWNrIGJyb3duIGZveCBqdW1wcyBvdmVyIHRoZSBsYXp5IGRvZy4"
            ), -- https://datatracker.ietf.org/doc/html/rfc5545#section-3.1
            ( ContentLine
                { contentLineName = "DESCRIPTION",
                  contentLineParams = M.fromList [("ALTREP", "cid:part1.0001@example.org")],
                  contentLineValue = "The Fall'98 Wild Wizards Conference - - Las Vegas, NV, USA"
                },
              "DESCRIPTION;ALTREP=\"cid:part1.0001@example.org\":The Fall'98 Wild Wizards Conference - - Las Vegas\\, NV\\, USA"
            )
          ]
    forM_ examples $ \(contentLine, rendered) -> do
      it (unwords ["renders", show contentLine, "as", show rendered]) $
        renderContentLine contentLine `shouldBe` UnfoldedLine rendered
      it (unwords ["parses", show rendered, "into", show contentLine]) $
        parseContentLine (UnfoldedLine rendered) `shouldBe` Right contentLine