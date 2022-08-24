{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Property.Gen where

import Data.GenValidity
import Data.GenValidity.CaseInsensitive ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import GHC.Stack
import ICal.Parameter.Gen ()
import ICal.Property
import ICal.PropertyType.Gen
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

instance GenValid ProdId where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Version where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid CalendarScale where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid UID where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid TZID where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid DateTimeStamp where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid DateTimeStart where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Classification where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Created where
  genValid = Created <$> genImpreciseUTCTime
  shrinkValid = fmap Created . shrinkImpreciseUTCTime . unCreated

instance GenValid Summary where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Description where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid GeographicPosition where
  genValid =
    GeographicPosition
      <$> choose (-90, 90)
      <*> choose (-360, 360) -- Could have any value that's not written in scientific notation. This is probably good enough.
  shrinkValid = error "GeographicPosition"

instance GenValid LastModified where
  genValid = LastModified <$> genImpreciseUTCTime
  shrinkValid = fmap LastModified . shrinkImpreciseUTCTime . unLastModified

instance GenValid Location where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Status where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid URL where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid DateTimeEnd where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Transparency where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid TimeZoneName where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Comment where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid TimeZoneOffsetFrom where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid TimeZoneOffsetTo where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

propertySpec ::
  forall a.
  (HasCallStack, Show a, Eq a, GenValid a, IsProperty a) =>
  Spec
propertySpec = withFrozenCallStack $ do
  it "always renders to a valid content line" $
    forAllValid $ \a ->
      shouldBeValid $ propertyContentLineB (a :: a)

  it "parses only valid things" $
    forAllValid $ \a ->
      case propertyContentLineP (propertyContentLineB (a :: a)) of
        Left _ -> pure ()
        Right a' -> shouldBeValid (a' :: a)

  it "roundtrips through ContentLine" $
    forAllValid $ \a ->
      let rendered = propertyContentLineB (a :: a)
       in context (show rendered) $ case propertyContentLineP rendered of
            Left err -> expectationFailure err
            Right actual -> actual `shouldBe` a
