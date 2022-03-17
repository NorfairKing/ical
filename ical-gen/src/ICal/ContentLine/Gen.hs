{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.ContentLine.Gen where

import Data.CaseInsensitive as CI
import Data.GenValidity
import Data.GenValidity.Map ()
import Data.GenValidity.Text ()
import ICal.ContentLine

instance (FoldCase a, GenValid a) => GenValid (CI a) where
  genValid = CI.mk <$> genValid
  shrinkValid = fmap CI.mk . shrinkValid . CI.original

instance GenValid ContentLine
