{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.PropertyType.Gen where

import Data.GenValidity
import Data.GenValidity.CaseInsensitive ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.Time (LocalTime (..), TimeOfDay (..))
import ICal.Parameter.Gen ()
import ICal.PropertyType
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

instance GenValid Date

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

instance GenValid RecurrenceRule

genImpreciseLocalTime :: Gen LocalTime
genImpreciseLocalTime = LocalTime <$> genValid <*> genImpreciseTimeOfDay

genImpreciseTimeOfDay :: Gen TimeOfDay
genImpreciseTimeOfDay =
  TimeOfDay
    <$> choose (0, 23)
    <*> choose (0, 59)
    <*> (fromIntegral <$> (choose (0, 60) :: Gen Int))

propertyTypeSpec ::
  forall a.
  (Show a, Eq a, GenValid a, IsPropertyType a) =>
  Spec
propertyTypeSpec = do
  it "always renders to a valid content line" $
    forAllValid $ \propertyType ->
      shouldBeValid $ propertyTypeB (propertyType :: a)
  it "roundtrips through ContentLine" $
    forAllValid $ \propertyType ->
      let value = propertyTypeB (propertyType :: a)
       in case propertyTypeP value of
            Left err -> expectationFailure err
            Right actual -> actual `shouldBe` propertyType
