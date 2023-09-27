{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.PropertyType.DateTimes
  ( DateTimes (..),
    dateTimesB,
    dateTimesP,
    toSet,
  )
where

import Control.DeepSeq
import Data.Proxy
import Data.Set
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Time as Time
import Data.Validity
import Data.Validity.Text ()
import Data.Validity.Time ()
import Data.Void
import GHC.Generics (Generic)
import ICal.Conformance
import ICal.ContentLine
import ICal.Parameter
import ICal.PropertyType.Class
import ICal.PropertyType.DateTime
import Text.Show

data DateTimes
  = DateTimesEmpty
  | DateTimesFloating !(Set Time.LocalTime)
  | DateTimesUTC !(Set Time.UTCTime)
  | DateTimesZoned !TimeZoneIdentifierParam !(Set Time.LocalTime)
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
  propertyTypeValueType Proxy = TypeDateTime
  propertyTypeP = dateTimesP
  propertyTypeB = dateTimesB

dateTimesP :: ContentLineValue -> Conform PropertyTypeParseError PropertyTypeFixableError Void DateTimes
dateTimesP ContentLineValue {..} =
  if T.null contentLineValueRaw
    then pure DateTimesEmpty
    else case lookupParam contentLineValueParams of
      Nothing ->
        (DateTimesUTC <$> parseTimesSetText dateTimeUTCFormatStr contentLineValueRaw)
          `altConform` (DateTimesFloating <$> parseTimesSetText dateTimeFloatingFormatStr contentLineValueRaw)
      Just conformTzid ->
        DateTimesZoned
          <$> conformMapAll ParameterParseError absurd id conformTzid
          <*> parseTimesSetText dateTimeZonedFormatStr contentLineValueRaw

dateTimesB :: DateTimes -> ContentLineValue
dateTimesB = propertyTypeSetB . toSet

toSet :: DateTimes -> Set DateTime
toSet = \case
  DateTimesEmpty -> S.empty
  DateTimesFloating lts -> S.map DateTimeFloating lts
  DateTimesUTC us -> S.map DateTimeUTC us
  DateTimesZoned tzid lts -> S.map (DateTimeZoned tzid) lts
