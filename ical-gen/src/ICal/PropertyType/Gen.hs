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
import Data.Time (LocalTime (..), TimeOfDay (..), UTCTime (..), localTimeToUTC, utc, utcToLocalTime)
import GHC.Stack
import ICal.ContentLine
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
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

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
    TimeUTC tod ->
      TimeFloating tod :
      (TimeUTC <$> shrinkImpreciseTimeOfDay tod)
    TimeZoned tzid tod ->
      TimeFloating tod :
      TimeUTC tod :
      ( do
          (tzid', tod') <- shrinkTuple shrinkValid shrinkImpreciseTimeOfDay (tzid, tod)
          pure (TimeZoned tzid' tod')
      )

instance GenValid DateTime where
  genValid =
    oneof
      [ DateTimeFloating <$> genImpreciseLocalTime,
        DateTimeUTC <$> genImpreciseUTCTime,
        DateTimeZoned <$> genValid <*> genImpreciseLocalTime
      ]
  shrinkValid = \case
    DateTimeFloating lt -> DateTimeFloating <$> shrinkImpreciseLocalTime lt
    DateTimeUTC ut ->
      DateTimeFloating (utcToLocalTime utc ut) :
      (DateTimeUTC <$> shrinkImpreciseUTCTime ut)
    DateTimeZoned tzid lt ->
      DateTimeFloating lt :
      DateTimeUTC (localTimeToUTC utc lt) :
      ( do
          (tzid', lt') <- shrinkTuple shrinkValid shrinkImpreciseLocalTime (tzid, lt)
          pure (DateTimeZoned tzid' lt')
      )

genImpreciseUTCTime :: Gen UTCTime
genImpreciseUTCTime = localTimeToUTC utc <$> genImpreciseLocalTime

shrinkImpreciseUTCTime :: UTCTime -> [UTCTime]
shrinkImpreciseUTCTime (UTCTime d dt) = do
  (d', dt') <-
    shrinkTuple
      shrinkValid
      (fmap (fromIntegral @Int) . shrinkValid . floor)
      (d, dt)
  pure (UTCTime d' dt')

genImpreciseLocalTime :: Gen LocalTime
genImpreciseLocalTime = LocalTime <$> genValid <*> genImpreciseTimeOfDay

shrinkImpreciseLocalTime :: LocalTime -> [LocalTime]
shrinkImpreciseLocalTime (LocalTime d tod) = do
  (d', tod') <- shrinkTuple shrinkValid shrinkImpreciseTimeOfDay (d, tod)
  pure $ LocalTime d' tod'

genImpreciseTimeOfDay :: Gen TimeOfDay
genImpreciseTimeOfDay =
  TimeOfDay
    <$> choose (0, 23)
    <*> choose (0, 59)
    <*> (fromIntegral <$> (choose (0, 60) :: Gen Int))

shrinkImpreciseTimeOfDay :: TimeOfDay -> [TimeOfDay]
shrinkImpreciseTimeOfDay (TimeOfDay h m s) = do
  (h', (m', s')) <-
    shrinkTuple
      (shrinkRangeDown (0, 23))
      ( shrinkTuple
          (shrinkRangeDown (0, 59))
          (fmap (fromIntegral @Int) . shrinkRangeDown (0, 60) . floor)
      )
      (h, (m, s))
  pure $ TimeOfDay h' m' s'

-- | Shrink in two bounds, towards the upper in the first and towards the lower in the second
shrinkRange2 :: (Ord a) => (a, a) -> (a, a) -> a -> [a]
shrinkRange2 t1@(_, upper1) t2@(lower2, _) value
  | value <= upper1 = shrinkRangeUp t1 value
  | value >= lower2 = shrinkRangeDown t2 value
  | otherwise = []

-- Shrink towards the lower bound
shrinkRangeDown :: (Ord a) => (a, a) -> a -> [a]
shrinkRangeDown (lower, upper) value
  | value <= lower = []
  | value > upper = [upper]
  | otherwise = [lower] -- Could maybe have more than this

-- Shrink towards the upper bound
shrinkRangeUp :: (Ord a) => (a, a) -> a -> [a]
shrinkRangeUp (lower, upper) value
  | value >= upper = []
  | value < lower = [lower]
  | otherwise = [upper] -- Could maybe have more than this

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
     in fmap UTCOffset . shrinkRange2 (-inclusiveBound, 0) (0, inclusiveBound) . unUTCOffset

propertyTypeRenderExampleSpec ::
  ( Show propertyType,
    IsPropertyType propertyType,
    HasCallStack
  ) =>
  propertyType ->
  ContentLineValue ->
  Spec
propertyTypeRenderExampleSpec value expected =
  withFrozenCallStack $
    it "renders this example correctly" $
      context (show value) $
        propertyTypeB value `shouldBe` expected

propertyTypeParseExampleSpec ::
  ( Show propertyType,
    Eq propertyType,
    IsPropertyType propertyType,
    HasCallStack
  ) =>
  ContentLineValue ->
  propertyType ->
  Spec
propertyTypeParseExampleSpec clv expected = withFrozenCallStack $
  it "parses this example correctly" $
    context (show clv) $
      case propertyTypeP clv of
        Left err -> expectationFailure err
        Right actual -> actual `shouldBe` expected

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
