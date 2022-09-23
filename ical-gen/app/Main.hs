{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Path
import Path.IO
import System.Environment
import System.Exit
import Text.Colour
import Text.Colour.Term

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> die "Usage: ical-spec-coverage <path-to-rfc5545.txt>"
    (specFile : _) -> coverSpecFile specFile

coverSpecFile :: FilePath -> IO ()
coverSpecFile specFile = do
  specLines <- getSpecLines specFile
  commentLines <- getCommentLines
  let report = produceReport specLines commentLines
  let coverage = deriveCoverage report
  putChunksLocale $ concatMap (\l -> l <> ["\n"]) (renderReport report coverage)
  unless (coverageComplete coverage) exitFailure

renderReport :: Report -> Coverage -> [[Chunk]]
renderReport Report {..} coverage =
  concat
    [ map (uncurry renderReportLine) (V.toList reportLines),
      [[""]],
      renderCoverage coverage
    ]

renderReportLine :: Bool -> Text -> [Chunk]
renderReportLine covered line =
  [ if covered then fore green "  COVERED" else fore red "UNCOVERED",
    " ",
    chunk line
  ]

produceReport :: Vector Text -> Set Text -> Report
produceReport specLines commentLines = Report $
  flip V.map specLines $ \specLine ->
    let covered = S.member (T.strip specLine) commentLines
     in (covered, specLine)

newtype Report = Report {reportLines :: Vector (Bool, Text)}
  deriving (Show)

deriveCoverage :: Report -> Coverage
deriveCoverage Report {..} =
  flip V.foldMap reportLines $ \(covered, _) ->
    Coverage {coverageCovered = if covered then 1 else 0, coverageTotal = 1}

data Coverage = Coverage {coverageCovered :: !Word, coverageTotal :: !Word}
  deriving (Show)

instance Semigroup Coverage where
  (<>) (Coverage c1 t1) (Coverage c2 t2) = Coverage (c1 + c2) (t1 + t2)

instance Monoid Coverage where
  mempty = Coverage 0 0
  mappend = (<>)

renderCoverage :: Coverage -> [[Chunk]]
renderCoverage Coverage {..} =
  [ [fore blue "Covered: ", fore green $ chunk $ T.pack $ show coverageCovered],
    [fore blue "Total:   ", fore blue $ chunk $ T.pack $ show coverageTotal]
  ]

coverageComplete :: Coverage -> Bool
coverageComplete Coverage {..} = coverageCovered >= coverageTotal

getCommentLines :: IO (Set Text)
getCommentLines = do
  here <- getCurrentDir
  fs <- snd <$> listDirRecurRel here
  let haskellFiles = filter (not . hidden) $ filter ((== Just ".hs") . fileExtension) fs
  fmap S.unions $
    forM haskellFiles $ \haskellFile -> do
      contents <- T.readFile (fromRelFile haskellFile)
      let ls = T.lines contents
      let commentLines = flip mapMaybe ls $ \l ->
            let commentSign = "-- "
                (_, rest) = T.breakOn commentSign l
             in T.strip <$> T.stripPrefix commentSign rest
      pure $ S.fromList commentLines

hidden :: Path Rel File -> Bool
hidden = goFile
  where
    goFile :: Path Rel File -> Bool
    goFile f = isHiddenIn (parent f) f || goDir (parent f)
    goDir :: Path Rel Dir -> Bool
    goDir f
      | parent f == f = False
      | otherwise = isHiddenIn (parent f) f || goDir (parent f)

isHiddenIn :: Path b Dir -> Path b t -> Bool
isHiddenIn curdir ad =
  case stripProperPrefix curdir ad of
    Nothing -> False
    Just rp -> "." `isPrefixOf` toFilePath rp

getSpecLines :: FilePath -> IO (Vector Text)
getSpecLines specFile = do
  contents <- T.readFile specFile
  let ls = T.lines contents
  let nonemptyLines = filter (not . T.null . T.strip) ls
  let relevantLines = flip filter nonemptyLines $ \t ->
        not ("Desruisseaux                Standards Track" `T.isInfixOf` t)
          && not ("RFC 5545                       iCalendar" `T.isInfixOf` t)
  pure $ V.fromList relevantLines
