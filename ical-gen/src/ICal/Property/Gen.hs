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

instance GenValid ProdId

instance GenValid Version

instance GenValid CalendarScale

instance GenValid UID

instance GenValid TZID

instance GenValid DateTimeStamp

instance GenValid DateTimeStart

instance GenValid Classification

instance GenValid Created where
  genValid = Created <$> genImpreciseUTCTime

instance GenValid Summary

instance GenValid Description

instance GenValid GeographicPosition where
  genValid =
    GeographicPosition
      <$> choose (-90, 90)
      <*> choose (-360, 360) -- Could have any value that's not written in scientific notation. This is probably good enough.

instance GenValid LastModified where
  genValid = LastModified <$> genImpreciseUTCTime

instance GenValid Location

instance GenValid Status

instance GenValid URL

instance GenValid DateTimeEnd

instance GenValid Transparency

instance GenValid TimeZoneName

instance GenValid Comment

instance GenValid TimeZoneOffsetFrom

instance GenValid TimeZoneOffsetTo

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
