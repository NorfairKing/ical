{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.ParameterSpec where

import ICal.Calendar.Gen ()
import ICal.Parameter
import ICal.Parameter.Gen
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "TZIDParam" $ do
    genValidSpec @TZIDParam
    parameterSpec @TZIDParam
  describe "tzIDP" $ do
    it "works for these examples" $ do
      tzIDParamP ["America/New_York"] `shouldBe` Right (TZIDParam "America/New_York")
      tzIDParamP ["/example.org/America/New_York"] `shouldBe` Right (TZIDParam "/example.org/America/New_York")
