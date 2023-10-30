{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Parameter.Gen where

import Conformance
import Conformance.TestUtils
import Data.GenValidity
import Data.GenValidity.CaseInsensitive ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import GHC.Stack
import ICal.ContentLine
import ICal.ContentLine.Gen ()
import ICal.Parameter
import ICal.PropertyType.CalAddress.Gen ()
import ICal.PropertyType.URI.Gen ()
import Test.Syd
import Test.Syd.Validity

instance GenValid AlternateTextRepresentation where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid CommonName where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid CalendarUserType where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Delegator where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Delegatee where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid DirectoryEntryReference where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Encoding where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid FormatType where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid FreeBusyTimeType where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Language where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Membership where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ParticipationStatus where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid RecurrenceIdentifierRange where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid AlarmTriggerRelationship where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid RelationshipType where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ParticipationRole where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid RSVPExpectation where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid SentBy where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid TimeZoneIdentifierParam where
  genValid = genValidStructurally

  -- Shrink to 'UTC' and to a few given timezones before that.
  shrinkValid = \case
    TimeZoneIdentifierParam "UTC" -> []
    TimeZoneIdentifierParam "A" -> [TimeZoneIdentifierParam "UTC"]
    TimeZoneIdentifierParam "B" -> [TimeZoneIdentifierParam "UTC", TimeZoneIdentifierParam "A"]
    TimeZoneIdentifierParam "C" -> [TimeZoneIdentifierParam "UTC", TimeZoneIdentifierParam "A", TimeZoneIdentifierParam "B"]
    TimeZoneIdentifierParam _ -> [TimeZoneIdentifierParam "UTC", TimeZoneIdentifierParam "A", TimeZoneIdentifierParam "B", TimeZoneIdentifierParam "C"]

instance GenValid ValueDataType where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Display where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

parameterExampleSpec ::
  (Show parameter, Eq parameter, IsParameter parameter, HasCallStack) =>
  ParamValue ->
  parameter ->
  Spec
parameterExampleSpec params val = withFrozenCallStack $ do
  parameterParseExampleSpec params val
  parameterRenderExampleSpec params val

parameterParseExampleSpec ::
  (Show parameter, Eq parameter, IsParameter parameter, HasCallStack) =>
  ParamValue ->
  parameter ->
  Spec
parameterParseExampleSpec params expected = withFrozenCallStack $ do
  it "parses this example correctly" $
    context (show params) $ do
      actual <- shouldConformStrict $ parameterP params
      actual `shouldBe` expected

parameterRenderExampleSpec ::
  (Show parameter, IsParameter parameter, HasCallStack) =>
  ParamValue ->
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
