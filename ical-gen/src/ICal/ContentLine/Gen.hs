{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.ContentLine.Gen where

import qualified Data.CaseInsensitive as CI
import Data.GenValidity
import Data.GenValidity.CaseInsensitive ()
import Data.GenValidity.Map ()
import Data.GenValidity.Text
import Data.Text (Text)
import qualified Data.Text as T
import ICal.ContentLine
import ICal.UnfoldedLine
import Test.QuickCheck

instance GenValid ContentLine where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ContentLineValue where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ContentLineName where
  genValid = ContentLineName . CI.mk <$> genNonEmptyTextBy genNameChar
  shrinkValid = shrinkValidStructurally

instance GenValid ParamName where
  genValid = ParamName . CI.mk <$> genNonEmptyTextBy genNameChar
  shrinkValid = shrinkValidStructurally

genNonEmptyTextBy :: Gen Char -> Gen Text
genNonEmptyTextBy gen = genTextBy gen `suchThat` (not . T.null)

genNameChar :: Gen Char
genNameChar = genValid `suchThat` (validationIsValid . validateNameChar)

instance GenValid ParamValue where
  genValid =
    oneof
      [ UnquotedParam . CI.mk <$> genTextBy genSafeChar,
        QuotedParam <$> genTextBy genQSafeChar
      ]
  shrinkValid = \case
    UnquotedParam c -> UnquotedParam <$> shrinkValid c
    QuotedParam t -> UnquotedParam (CI.mk t) : (QuotedParam <$> shrinkValid t)

genQSafeChar :: Gen Char
genQSafeChar = genValid `suchThat` (validationIsValid . validateQSafeChar)

genSafeChar :: Gen Char
genSafeChar = genValid `suchThat` (validationIsValid . validateSafeChar)

renderContentLines :: [ContentLine] -> Text
renderContentLines =
  renderUnfoldedLines
    . map renderContentLineToUnfoldedLine
