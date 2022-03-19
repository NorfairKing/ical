{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.ContentLine.Gen where

import Data.CaseInsensitive as CI
import Data.GenValidity
import Data.GenValidity.Map
import Data.GenValidity.Text
import Data.Map as M
import Data.Text
import qualified Data.Text as T
import ICal.ContentLine
import Test.QuickCheck

instance (FoldCase a, GenValid a) => GenValid (CI a) where
  genValid = CI.mk <$> genValid
  shrinkValid = fmap CI.mk . shrinkValid . CI.original

instance GenValid ContentLine

instance GenValid ContentLineName

instance GenValid ParamValue

-- where
--   genValid = do
--     contentLineName <- genKeyText
--     contentLineParams <- genMapOf $ do
--       key <- genKeyText
--       val <- genValText
--       pure (key, val)
--     contentLineValue <- genValText
--     pure ContentLine {..}
--
-- genKeyText :: Gen (CI Text)
-- genKeyText =
--   CI.mk
--     <$> genTextBy
--       (genValid `suchThat` (validationIsValid . validateKeyChar))
--       `suchThat` (not . T.null)
--
-- genValText :: Gen Text
-- genValText = genValid
