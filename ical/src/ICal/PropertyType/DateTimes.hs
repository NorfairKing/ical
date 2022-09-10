{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.PropertyType.DateTimes where

import Control.DeepSeq
import qualified Data.Map as M
import Data.Set
import qualified Data.Set as S
import qualified Data.Time as Time
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import GHC.Generics (Generic)
import ICal.ContentLine
import ICal.Parameter
import ICal.PropertyType.Class
import ICal.PropertyType.DateTime
import Text.Show

data DateTimes
  = DateTimesEmpty
  | DateTimesFloating !(Set Time.LocalTime)
  | DateTimesUTC !(Set Time.UTCTime)
  | DateTimesZoned !TZIDParam !(Set Time.LocalTime)
  deriving (Eq, Ord, Generic)

instance Validity DateTimes where
  validate dt =
    mconcat
      [ genericValidate dt,
        case dt of
          DateTimesEmpty -> mempty
          DateTimesFloating ls ->
            mconcat
              [ declare "The set is nonempty" $ not $ S.null ls,
                decorateList (S.toList ls) $ \l -> validateImpreciseLocalTime l
              ]
          DateTimesUTC us ->
            mconcat
              [ declare "The set is nonempty" $ not $ S.null us,
                decorateList (S.toList us) $ \u -> validateImpreciseUTCTime u
              ]
          DateTimesZoned _ ls ->
            mconcat
              [ declare "The set is nonempty" $ not $ S.null ls,
                decorateList (S.toList ls) $ \l -> validateImpreciseLocalTime l
              ]
      ]

instance Show DateTimes where
  showsPrec d =
    showParen (d > 10) . \case
      DateTimesEmpty -> showString "DateTimesEmpty"
      DateTimesFloating ls -> showString "DateTimesFloating " . setShowsPrec localTimeShowsPrec 11 ls
      DateTimesUTC us -> showString "DateTimesUTC " . setShowsPrec utcTimeShowsPrec 11 us
      DateTimesZoned tzid ls -> showString "DateTimesZoned " . showsPrec 11 tzid . showString " " . setShowsPrec localTimeShowsPrec 11 ls

setShowsPrec :: (Int -> a -> ShowS) -> Int -> Set a -> ShowS
setShowsPrec go d set =
  showParen (d > 10) $
    showString "S.fromList" . showListWith (go 11) (S.toList set)

instance NFData DateTimes

instance IsPropertyType DateTimes where
  propertyTypeP = dateTimesP
  propertyTypeB = dateTimesB

-- TODO this can probably be much more efficient using a custom parser.
dateTimesP :: ContentLineValue -> Either String DateTimes
dateTimesP clv = do
  set <- propertyTypeSetP clv
  let goOn = fromSet set
  case M.lookup "VALUE" (contentLineValueParams clv) of
    Just t -> if t == ["DATE-TIME"] then goOn else Left "Invalid VALUE"
    _ -> goOn

fromSet :: Set DateTime -> Either String DateTimes
fromSet set = case S.lookupMin set of
  Nothing -> Right DateTimesEmpty
  Just dt -> case dt of
    DateTimeFloating _ -> goFloating set
    DateTimeUTC _ -> goUTC set
    DateTimeZoned tzid _ -> goZoned tzid set
  where
    goFloating :: Set DateTime -> Either String DateTimes
    goFloating =
      fmap (DateTimesFloating . S.fromList)
        . mapM
          ( \case
              DateTimeFloating lt -> Right lt
              _ -> Left "Must be Floating"
          )
        . S.toList
    goUTC :: Set DateTime -> Either String DateTimes
    goUTC =
      fmap (DateTimesUTC . S.fromList)
        . mapM
          ( \case
              DateTimeUTC u -> Right u
              _ -> Left "Must be UTC"
          )
        . S.toList
    goZoned :: TZIDParam -> Set DateTime -> Either String DateTimes
    goZoned tzid =
      fmap (DateTimesZoned tzid . S.fromList)
        . mapM
          ( \case
              DateTimeZoned tzid' lt -> if tzid == tzid' then Right lt else Left "Must be the same tzid"
              _ -> Left "Must be UTC"
          )
        . S.toList

dateTimesB :: DateTimes -> ContentLineValue
dateTimesB = addValue . propertyTypeSetB . toSet
  where
    addValue clv = clv {contentLineValueParams = M.insert "VALUE" ["DATE-TIME"] (contentLineValueParams clv)}

toSet :: DateTimes -> Set DateTime
toSet = \case
  DateTimesEmpty -> S.empty
  DateTimesFloating lts -> S.map DateTimeFloating lts
  DateTimesUTC us -> S.map DateTimeUTC us
  DateTimesZoned tzid lts -> S.map (DateTimeZoned tzid) lts
