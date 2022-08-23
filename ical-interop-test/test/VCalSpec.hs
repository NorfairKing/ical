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
import Test.Syd.Validity

spec :: Spec
spec =
  modifyMaxSize (* 3) . modifyMaxSuccess (`div` 10) $
    it "produces calendars that vcal can parse" $
      forAllValid $ \calendar ->
        withSystemTempDir "ical-integration-test" $ \tdir -> do
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
