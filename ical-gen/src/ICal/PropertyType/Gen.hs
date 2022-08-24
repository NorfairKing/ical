{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.PropertyType.Gen where

import Data.GenValidity
import Data.GenValidity.CaseInsensitive ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.GenValidity.URI ()
import Data.Maybe
import Data.Time (LocalTime (..), TimeOfDay (..), UTCTime, localTimeToUTC, utc, utcToLocalTime)
import GHC.Stack
import ICal.Parameter ()
import ICal.Parameter.Gen ()
import ICal.PropertyType.Class
import ICal.PropertyType.Date
import ICal.PropertyType.DateTime
import ICal.PropertyType.FloatingPoint
import ICal.PropertyType.Time
import ICal.PropertyType.URI
import ICal.PropertyType.UTCOffset
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

instance GenValid FloatingPoint where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Date where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Time where
  genValid = do
    lt <- genImpreciseTimeOfDay
    oneof
      [ pure $ TimeFloating lt,
        pure $ TimeUTC lt,
        TimeZoned <$> genValid <*> pure lt
      ]
  shrinkValid = \case
    TimeFloating tod -> TimeFloating <$> shrinkImpreciseTimeOfDay tod
    TimeUTC tod -> TimeUTC <$> shrinkImpreciseTimeOfDay tod
    TimeZoned tzid tod -> TimeZoned <$> shrinkValid tzid <*> shrinkImpreciseTimeOfDay tod

instance GenValid DateTime where
  genValid =
    oneof
      [ DateTimeFloating <$> genImpreciseLocalTime,
        DateTimeUTC <$> genImpreciseUTCTime,
        DateTimeZoned <$> genValid <*> genImpreciseLocalTime
      ]
  shrinkValid = \case
    DateTimeFloating lt -> DateTimeFloating <$> shrinkImpreciseLocalTime lt
    DateTimeUTC ut -> DateTimeUTC <$> shrinkImpreciseUTCTime ut
    DateTimeZoned tzid lt -> DateTimeZoned <$> shrinkValid tzid <*> shrinkImpreciseLocalTime lt

genImpreciseUTCTime :: Gen UTCTime
genImpreciseUTCTime = localTimeToUTC utc <$> genImpreciseLocalTime

shrinkImpreciseUTCTime :: UTCTime -> [UTCTime]
shrinkImpreciseUTCTime =
  fmap (localTimeToUTC utc)
    . shrinkImpreciseLocalTime
    . utcToLocalTime utc

genImpreciseLocalTime :: Gen LocalTime
genImpreciseLocalTime = LocalTime <$> genValid <*> genImpreciseTimeOfDay

shrinkImpreciseLocalTime :: LocalTime -> [LocalTime]
shrinkImpreciseLocalTime (LocalTime d tod) =
  LocalTime
    <$> shrinkValid d
    <*> shrinkImpreciseTimeOfDay tod

genImpreciseTimeOfDay :: Gen TimeOfDay
genImpreciseTimeOfDay =
  TimeOfDay
    <$> choose (0, 23)
    <*> choose (0, 59)
    <*> (fromIntegral <$> (choose (0, 60) :: Gen Int))

shrinkImpreciseTimeOfDay :: TimeOfDay -> [TimeOfDay]
shrinkImpreciseTimeOfDay (TimeOfDay h m s) =
  TimeOfDay
    <$> shrinkRange (0, 23) h
    <*> shrinkRange (0, 59) m
    <*> (fromIntegral @Int <$> shrinkRange (0, 60) (round s))

shrinkRange :: (Ord a, GenValid a) => (a, a) -> a -> [a]
shrinkRange (lower, upper) =
  mapMaybe (clampMaybe lower upper) . shrinkValid

clampMaybe :: Ord a => a -> a -> a -> Maybe a
clampMaybe lower upper value =
  if lower <= value && value <= upper
    then Just value
    else Nothing

instance GenValid URI where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid UTCOffset where
  genValid =
    let inclusiveBound = utcOffsetAbsBound - 1
     in UTCOffset <$> choose (-inclusiveBound, inclusiveBound)
  shrinkValid =
    let inclusiveBound = utcOffsetAbsBound - 1
     in fmap UTCOffset . shrinkRange (-inclusiveBound, inclusiveBound) . unUTCOffset

propertyTypeSpec ::
  forall a.
  (HasCallStack, Show a, Eq a, GenValid a, IsPropertyType a) =>
  Spec
propertyTypeSpec = withFrozenCallStack $ do
  it "always renders to a valid content line" $
    forAllValid $ \propertyType ->
      shouldBeValid $ propertyTypeB (propertyType :: a)

  it "parses only valid things" $
    forAllValid $ \a ->
      case propertyTypeP (propertyTypeB (a :: a)) of
        Left _ -> pure ()
        Right a' -> shouldBeValid (a' :: a)

  it "roundtrips through ContentLine" $
    forAllValid $ \propertyType ->
      let value = propertyTypeB (propertyType :: a)
       in context (show value) $ case propertyTypeP value of
            Left err -> expectationFailure err
            Right actual -> actual `shouldBe` propertyType
