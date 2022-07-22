module VCalSpec (spec) where

import qualified Data.ByteString as SB
import qualified Data.Text as T
import ICal
import ICal.Component.Gen ()
import Path
import Path.IO
import System.Exit
import System.Process
import Test.Syd
import Test.Syd.Path
import Test.Syd.Validity

spec :: Spec
spec =
  modifyMaxSize (* 10) $
    tempDirSpec "ical-integration-test" $
      it "produces calendars that vcal can parse" $ \tdir ->
        forAllValid $ \calendar -> do
          calendarFile <- resolveFile tdir "calendar.ics"
          SB.writeFile (fromAbsFile calendarFile) (renderICalendarByteString [calendar])
          let cp = proc "vcal" ["-raw", fromAbsFile calendarFile]
          (ec, _, _) <- readCreateProcessWithExitCode cp ""
          case ec of
            ExitSuccess -> pure ()
            ExitFailure c ->
              expectationFailure $
                unlines
                  [ unwords
                      [ "vcal exited with code",
                        show c
                      ],
                    "calendar:",
                    ppShow calendar,
                    "rendered:",
                    T.unpack $ renderVCalendar calendar
                  ]