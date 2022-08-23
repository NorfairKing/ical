module PythonInteropSpec (spec) where

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
  modifyMaxSuccess (`div` 10) . xdescribe "does not pass yet." $
    it "produces calendars that the python library can parse" $
      forAllValid $ \calendar ->
        withSystemTempDir "ical-integration-test" $ \tdir -> do
          print calendar
          calendarFile <- resolveFile tdir "calendar.ics"
          SB.writeFile (fromAbsFile calendarFile) (renderICalendarByteString [calendar])
          let cp = proc "python-echo" [fromAbsFile calendarFile]
          (ec, out, err) <- readCreateProcessWithExitCode cp ""
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
                    T.unpack $ renderVCalendar calendar,
                    "out:",
                    out,
                    "err:",
                    err
                  ]
