{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Property.Gen where

import Data.GenValidity
import Data.GenValidity.CaseInsensitive ()
import Data.GenValidity.Containers ()
import Data.GenValidity.Text ()
import qualified Data.Text as T
import GHC.Stack
import ICal.Conformance
import ICal.ContentLine
import ICal.Parameter.Gen ()
import ICal.Property
import ICal.PropertyType.Gen
import ICal.UnfoldedLine
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

instance GenValid Method where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ProductIdentifier where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Version where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid CalendarScale where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Attachment where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Categories

instance GenValid RecurrenceIdentifier where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid UID where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid TimeZoneIdentifier where
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

instance GenValid Organizer where
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
  shrinkValid (GeographicPosition lat lon) =
    GeographicPosition
      <$> shrinkRange2 (-90, 0) (0, 90) lat
      <*> shrinkRange2 (-360, 0) (0, 360) lon

instance GenValid LastModified where
  genValid = LastModified <$> genImpreciseUTCTime
  shrinkValid = fmap LastModified . shrinkImpreciseUTCTime . unLastModified

instance GenValid Location where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid PercentComplete

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

instance GenValid Attendee where
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

instance GenValid ExceptionDateTimes where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid RecurrenceDateTimes where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Action where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Repeat where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Trigger where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Image where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

propertyRenderExampleSpec ::
  ( Show property,
    IsProperty property,
    HasCallStack
  ) =>
  ContentLine ->
  property ->
  Spec
propertyRenderExampleSpec expected value =
  withFrozenCallStack $
    it "renders this example correctly" $
      context (show value) $
        let cl = propertyContentLineB value
         in context (T.unpack (renderUnfoldedLines [renderContentLineToUnfoldedLine cl])) $
              cl `shouldBe` expected

propertyParseExampleSpec ::
  ( IsProperty property,
    Show property,
    Eq property,
    HasCallStack
  ) =>
  ContentLine ->
  property ->
  Spec
propertyParseExampleSpec cl expected = withFrozenCallStack $
  it "parses this example correctly" $
    context (show cl) $
      case runConformStrict $ propertyContentLineP cl of
        Left err -> expectationFailure $ show err
        Right actual -> actual `shouldBe` expected

propertyExampleSpec ::
  ( IsProperty property,
    Show property,
    Eq property,
    HasCallStack
  ) =>
  ContentLine ->
  property ->
  Spec
propertyExampleSpec cl v = withFrozenCallStack $ do
  propertyParseExampleSpec cl v
  propertyRenderExampleSpec cl v

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
      case runConformStrict $ propertyContentLineP (propertyContentLineB (a :: a)) of
        Left _ -> pure ()
        Right a' -> shouldBeValid (a' :: a)

  it "roundtrips through ContentLine" $
    forAllValid $ \a ->
      let rendered = propertyContentLineB (a :: a)
       in context (show rendered) $
            case runConformStrict $ propertyContentLineP rendered of
              Left err -> expectationFailure $ show err
              Right actual -> actual `shouldBe` a
