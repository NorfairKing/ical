{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- This module uses list as a monad a lot, make sure you understand it before reading this module.
module ICal.Recurrence.RecurrenceRule.Util where

import Control.Monad
import Data.Fixed
import Data.List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time
import Data.Time.Calendar.MonthDay
import ICal.PropertyType.RecurrenceRule
import ICal.Recurrence.RecurrenceRule.WeekDate

takeEvery :: Word -> [a] -> [a]
takeEvery i = go 0
  where
    go _ [] = []
    go 0 (l : ls) = l : go (i - 1) ls
    go j (_ : ls) = go (pred j) ls

byMonthLimitMonth :: Set ByMonth -> Month -> Bool
byMonthLimitMonth = limitBy $ \m1 (ByMonth m2) -> m1 == m2

byMonthLimit :: Set ByMonth -> Day -> Bool
byMonthLimit = limitBy $ \d (ByMonth m) ->
  let (_, month, _) = toGregorian d
   in month == monthToMonthNo m

byMonthDayLimit :: Set ByMonthDay -> Day -> Bool
byMonthDayLimit = limitBy $ \d (ByMonthDay md) ->
  let (positiveMonthDayIndex, negativeMonthDayIndex) = monthIndices d
   in positiveMonthDayIndex == md
        || negativeMonthDayIndex == md

monthIndices :: Day -> (Int, Int) -- (Positive index, Negative index)
monthIndices d =
  let (y, month, day) = toGregorian d
      leap = isLeapYear y
      monthLen = monthLength leap month
      negativeMonthDayIndex = negate $ monthLen - day + 1
   in (day, negativeMonthDayIndex)

byDayLimit :: Set ByDay -> Day -> Bool
byDayLimit = limitBy $ \d bd -> case bd of
  Every dow -> dayOfWeek d == dow
  Specific i dow ->
    dayOfWeek d == dow
      && ( let (pos, neg) = specificWeekDayIndex d
            in i == pos || i == neg
         )

byDayLimitInYear :: Set ByDay -> Day -> Bool
byDayLimitInYear = limitBy $ \d bd -> case bd of
  Every dow -> dayOfWeek d == dow
  Specific i dow ->
    dayOfWeek d == dow
      && ( let (pos, neg) = specificYearWeekDayIndex d
            in i == pos || i == neg
         )

byEveryWeekDayLimit :: Set DayOfWeek -> Day -> Bool
byEveryWeekDayLimit = limitBy $ \d dow -> dow == dayOfWeek d

byYearDayExpand :: Integer -> Set ByYearDay -> Maybe (NonEmpty Word)
byYearDayExpand year s = NE.nonEmpty $
  sort $
    flip mapMaybe (S.toList s) $ \(ByYearDay yd) ->
      let days = daysInYear year
       in case compare yd 0 of
            EQ -> Nothing -- Wouldn't be valid, but that's fine
            GT -> Just $ fromIntegral yd
            LT -> Just $ fromIntegral $ days + yd + 1 -- Must be positive

byMonthDayExpandEveryMonth :: Integer -> Set ByMonthDay -> Maybe (NonEmpty (Month, Word))
byMonthDayExpandEveryMonth year s = NE.nonEmpty $
  sort $
    flip concatMap (S.toList s) $ \(ByMonthDay md) -> do
      month <- [January .. December]
      let days = monthLength (isLeapYear year) (monthToMonthNo month)
      case compare md 0 of
        EQ -> [] -- Wouldn't be valid, but that's fine
        GT -> pure (month, fromIntegral md)
        LT -> pure (month, fromIntegral $ days + md + 1) -- Must be positive

byMonthDayExpandMonth :: Integer -> Month -> Set ByMonthDay -> Maybe (NonEmpty Word)
byMonthDayExpandMonth year month s = NE.nonEmpty $
  sort $
    flip mapMaybe (S.toList s) $ \(ByMonthDay md) ->
      let days = monthLength (isLeapYear year) (monthToMonthNo month)
       in case compare md 0 of
            EQ -> Nothing -- Wouldn't be valid, but that's fine
            GT -> Just $ fromIntegral md
            LT -> Just $ fromIntegral $ days + md + 1 -- Must be positive

byEveryWeekDayWeek :: Set ByDay -> Maybe (NonEmpty DayOfWeek)
byEveryWeekDayWeek =
  NE.nonEmpty
    . mapMaybe
      ( \case
          Every dow -> Just dow
          _ -> Nothing
      )
    . S.toList

byEveryWeekDayExpandYear :: WeekStart -> Integer -> Set ByDay -> Maybe (NonEmpty Day)
byEveryWeekDayExpandYear weekStart year s = NE.nonEmpty $
  sort $
    flip concatMap (S.toList s) $
      \case
        Every dow -> do
          wn <- [1 .. weeksInYear weekStart year]
          maybeToList $ fromWeekDateWithStart weekStart year wn dow
        Specific i dow -> do
          wn <- [1 .. weeksInYear weekStart year]
          d <- maybeToList $ fromWeekDateWithStart weekStart year wn dow
          guard $ dayOfWeek d == dow
          let (pn, nn) = specificYearWeekDayIndex d
          guard $ i == pn || i == nn
          pure d

byWeekNoExpand :: WeekStart -> Integer -> Set ByWeekNo -> Maybe (NonEmpty Word)
byWeekNoExpand weekStart year s =
  NE.nonEmpty $
    sort $
      flip mapMaybe (S.toList s) $ \(ByWeekNo wn) ->
        let weeks = weeksInYear weekStart year
         in case compare wn 0 of
              EQ -> Nothing -- Wouldn't be valid, but that's fine
              GT -> Just $ fromIntegral wn
              LT -> Just $ fromIntegral $ fromIntegral weeks + wn + 1 -- Must be positive

byDayExpand :: Integer -> Int -> Int -> Set ByDay -> [Day]
byDayExpand y m md s =
  concat $
    expand
      ( \bd ->
          let qdrups = daysOfMonth y m
           in case bd of
                Every dow ->
                  mapMaybe
                    ( \(d, _, _, dow') ->
                        if dow == dow' then Just d else Nothing
                    )
                    qdrups
                Specific i dow ->
                  mapMaybe
                    ( \(d, p, n, dow') ->
                        if dow == dow' && (i == p || i == n) then Just d else Nothing
                    )
                    qdrups
      )
      (maybeToList $ fromGregorianValid y m md)
      s

byMonthExpand :: Set ByMonth -> Maybe (NonEmpty Month)
byMonthExpand = NE.nonEmpty . map unByMonth . S.toList

byMonthDayExpand :: Integer -> Month -> Int -> Set ByMonthDay -> [Int]
byMonthDayExpand y m = expandM $ \(ByMonthDay md) ->
  let len = monthLength (isLeapYear y) (monthToMonthNo m)
   in case compare md 0 of
        EQ -> Nothing -- Should not happen
        LT ->
          -- Negative
          Just $ len + md + 1
        GT -> Just md

byEveryWeekDayExpand :: DayOfWeek -> Set DayOfWeek -> [DayOfWeek]
byEveryWeekDayExpand = expand id

timeOfDayExpand :: TimeOfDay -> Set ByHour -> Set ByMinute -> Set BySecond -> [TimeOfDay]
timeOfDayExpand (TimeOfDay h_ m_ s_) byHours byMinutes bySeconds = do
  h <- byHourExpand h_ byHours
  m <- byMinuteExpand m_ byMinutes
  s <- bySecondExpand s_ bySeconds
  let tod = TimeOfDay h m s
  pure tod

byHourExpand :: Int -> Set ByHour -> [Int]
byHourExpand = expand (fromIntegral . unByHour)

byMinuteExpand :: Int -> Set ByMinute -> [Int]
byMinuteExpand = expand (fromIntegral . unByMinute)

bySecondExpand :: Pico -> Set BySecond -> [Pico]
bySecondExpand = expand (fromIntegral . unBySecond)

expand :: (b -> a) -> a -> Set b -> [a]
expand func def bys = if S.null bys then pure def else map func (S.toList bys)

expandM :: (b -> Maybe a) -> a -> Set b -> [a]
expandM func def bys = if S.null bys then pure def else mapMaybe func (S.toList bys)

expandL :: (b -> [a]) -> a -> Set b -> [a]
expandL func def bys = if S.null bys then pure def else concatMap func (S.toList bys)

filterSetPos :: Set BySetPos -> [a] -> [a]
filterSetPos poss values =
  map snd (filter (limitBy func poss) (zip [1 ..] values))
  where
    len = length values
    func (i, _) (BySetPos pos) =
      let toNegative positive = negate $ len - positive + 1
       in i == pos || toNegative i == pos

limitBy :: (b -> a -> Bool) -> Set a -> b -> Bool
limitBy func bys b =
  if S.null bys
    then True
    else any (func b) bys

-- This can probably be sped up a lot using the weekdate module
specificWeekDayIndex :: Day -> (Int, Int) -- (Positive index, Negative index)
specificWeekDayIndex d =
  let (y, month, _) = toGregorian d
      firstDayOfTheMonth = fromGregorian y month 1
      lastDayOfTheMonth = fromGregorian y month 31 -- Will be clipped
      daysOfThisMonth = numberWeekdays [firstDayOfTheMonth .. lastDayOfTheMonth]
      numberOfThisWeekDayInTheMonth = length $ filter ((== dayOfWeek d) . fst . snd) daysOfThisMonth
      (_, positiveSpecificWeekDayIndex) = fromJust (lookup d daysOfThisMonth) -- Must be there
   in (positiveSpecificWeekDayIndex, negate $ numberOfThisWeekDayInTheMonth - positiveSpecificWeekDayIndex + 1)

-- This can probably be sped up a lot using the weekdate module
specificYearWeekDayIndex :: Day -> (Int, Int) -- (Positive index, Negative index)
specificYearWeekDayIndex d =
  let (y, _, _) = toGregorian d
      firstDayOfTheYear = fromGregorian y 01 01
      lastDayOfTheYear = fromGregorian y 12 31
      daysOfThisYear = numberWeekdays [firstDayOfTheYear .. lastDayOfTheYear]
      numberOfThisWeekDayInTheYear = length $ filter ((== dayOfWeek d) . fst . snd) daysOfThisYear
      (_, positiveSpecificWeekDayIndex) = fromJust (lookup d daysOfThisYear) -- Must be there
   in (positiveSpecificWeekDayIndex, negate $ numberOfThisWeekDayInTheYear - positiveSpecificWeekDayIndex + 1)

-- Quadruples: Day, Positive index, negative index, day of week
daysOfMonth :: Integer -> Int -> [(Day, Int, Int, DayOfWeek)]
daysOfMonth year month = map go daysOfThisMonth
  where
    firstDayOfTheMonth = fromGregorian year month 1
    lastDayOfTheMonth = fromGregorian year month 31 -- Will be clipped
    days = [firstDayOfTheMonth .. lastDayOfTheMonth]
    daysOfThisMonth = numberWeekdays days
    numberOfThisWeekDayInTheMonth wd = fromMaybe 1 $ M.lookup wd $ count $ map dayOfWeek days
    go :: (Day, (DayOfWeek, Int)) -> (Day, Int, Int, DayOfWeek)
    go (d, (dow, p)) =
      let n = negate $ fromIntegral (numberOfThisWeekDayInTheMonth dow) - p + 1
       in (d, p, n, dow)

numberWeekdays :: [Day] -> [(Day, (DayOfWeek, Int))]
numberWeekdays = go M.empty
  where
    go _ [] = []
    go m (d_ : ds) =
      let dow = dayOfWeek d_
          (mv, m') =
            M.insertLookupWithKey
              (\_ new old -> new + old) -- If found, just increment
              dow
              1 -- If not found, insert 1
              m
       in (d_, (dow, maybe 1 succ mv)) : go m' ds

count :: forall a. Ord a => [a] -> Map a Word
count = foldl go M.empty
  where
    go :: Map a Word -> a -> Map a Word
    go m a = M.alter go2 a m
    go2 :: Maybe Word -> Maybe Word
    go2 Nothing = Just 1
    go2 (Just i) = Just $ i + 1
