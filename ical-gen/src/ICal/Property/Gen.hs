{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Property.Gen where

import Data.GenValidity
import Data.GenValidity.CaseInsensitive ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import ICal.Parameter.Gen ()
import ICal.Property
import ICal.PropertyType.Gen ()
import Test.Syd
import Test.Syd.Validity

instance GenValid ProdId

instance GenValid Version

instance GenValid UID

instance GenValid DateTimeStamp

instance GenValid TZID

propertySpec ::
  forall a.
  (Show a, Eq a, GenValid a, IsProperty a) =>
  Spec
propertySpec = do
  it "always renders to a valid content line" $
    forAllValid $ \a ->
      shouldBeValid $ propertyB (a :: a)
  it "roundtrips through ContentLine" $
    forAllValid $ \a ->
      let rendered = propertyB (a :: a)
       in case propertyP rendered of
            Left err -> expectationFailure err
            Right actual -> actual `shouldBe` a
