{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Calendar.Gen where

import Data.GenValidity
import Data.GenValidity.CaseInsensitive ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Time (LocalTime (..), TimeOfDay (..))
import ICal.Calendar
import ICal.Parameter.Gen ()
import Test.QuickCheck

instance GenValid Calendar

instance GenValid ProdId

instance GenValid Version

instance GenValid UID

instance GenValid DateTimeStamp

instance GenValid Date

instance GenValid TZID

instance GenValid Time where
  genValid = do
    lt <- genImpreciseTimeOfDay
    oneof
      [ pure $ TimeFloating lt,
        pure $ TimeUTC lt,
        TimeZoned <$> genValid <*> pure lt
      ]

instance GenValid DateTime where
  genValid = do
    lt <- genImpreciseLocalTime
    oneof
      [ pure $ DateTimeFloating lt,
        pure $ DateTimeUTC lt,
        DateTimeZoned <$> genValid <*> pure lt
      ]

instance GenValid Event

instance GenValid TimeZone

genImpreciseLocalTime :: Gen LocalTime
genImpreciseLocalTime = LocalTime <$> genValid <*> genImpreciseTimeOfDay

genImpreciseTimeOfDay :: Gen TimeOfDay
genImpreciseTimeOfDay =
  TimeOfDay
    <$> choose (0, 23)
    <*> choose (0, 59)
    <*> (fromIntegral <$> (choose (0, 60) :: Gen Int))
