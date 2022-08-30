{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Parameter.Gen where

import Data.GenValidity
import Data.GenValidity.CaseInsensitive ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import ICal.Parameter
import Test.Syd
import Test.Syd.Validity

instance GenValid TZIDParam where
  genValid = genValidStructurally

  -- Shrink to 'UTC' and to a few given timezones before that.
  shrinkValid = \case
    TZIDParam "UTC" -> []
    TZIDParam "A" -> [TZIDParam "UTC"]
    TZIDParam "B" -> [TZIDParam "UTC", TZIDParam "A"]
    TZIDParam "C" -> [TZIDParam "UTC", TZIDParam "A", TZIDParam "B"]
    TZIDParam _ -> [TZIDParam "UTC", TZIDParam "A", TZIDParam "B", TZIDParam "C"]

parameterSpec ::
  forall a.
  (Show a, Eq a, GenValid a, IsParameter a) =>
  Spec
parameterSpec = do
  it "always renders a valid parameter values" $
    forAllValid $ \parameter ->
      shouldBeValid $ parameterB (parameter :: a)

  it "parses only valid things" $
    forAllValid $ \a ->
      case parameterP (parameterB (a :: a)) of
        Left _ -> pure ()
        Right a' -> shouldBeValid (a' :: a)

  it "roundtrips through parameter values" $
    forAllValid $ \parameter ->
      let values = parameterB (parameter :: a)
       in case parameterP values of
            Left err -> expectationFailure err
            Right actual -> actual `shouldBe` parameter
