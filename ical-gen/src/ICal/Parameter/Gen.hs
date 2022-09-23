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
import Data.List.NonEmpty (NonEmpty)
import GHC.Stack
import ICal.Conformance
import ICal.Conformance.TestUtils
import ICal.ContentLine
import ICal.ContentLine.Gen ()
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

instance GenValid ValueDataType

parameterExampleSpec ::
  (Show parameter, Eq parameter, IsParameter parameter, HasCallStack) =>
  NonEmpty ParamValue ->
  parameter ->
  Spec
parameterExampleSpec params val = withFrozenCallStack $ do
  parameterParseExampleSpec params val
  parameterRenderExampleSpec params val

parameterParseExampleSpec ::
  (Show parameter, Eq parameter, IsParameter parameter, HasCallStack) =>
  NonEmpty ParamValue ->
  parameter ->
  Spec
parameterParseExampleSpec params expected = withFrozenCallStack $ do
  it "parses this example correctly" $
    context (show params) $ do
      actual <- shouldConformStrict $ parameterP params
      actual `shouldBe` expected

parameterRenderExampleSpec ::
  (Show parameter, IsParameter parameter, HasCallStack) =>
  NonEmpty ParamValue ->
  parameter ->
  Spec
parameterRenderExampleSpec expected value = withFrozenCallStack $ do
  it "renders this example correctly" $
    context (show value) $
      let actual = parameterB value
       in actual `shouldBe` expected

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
      case runConformStrict $ parameterP (parameterB (a :: a)) of
        Left _ -> pure ()
        Right a' -> shouldBeValid (a' :: a)

  it "roundtrips through parameter values" $
    forAllValid $ \parameter -> do
      let values = parameterB (parameter :: a)
      actual <- shouldConformStrict $ parameterP values
      actual `shouldBe` parameter
