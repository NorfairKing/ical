{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.ParameterSpec where

import ICal.Parameter
import ICal.Parameter.Gen
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "TZIDParam" $ do
    genValidSpec @TZIDParam
    parameterSpec @TZIDParam
    -- From the spec:
    -- @
    --    The following are examples of this property parameter:
    --
    --     DTSTART;TZID=America/New_York:19980119T020000
    --
    --     DTEND;TZID=America/New_York:19980119T030000
    -- @
    parameterExampleSpec ["America/New_York"] (TZIDParam "America/New_York")
    parameterExampleSpec ["/example.org/America/New_York"] (TZIDParam "/example.org/America/New_York")

  describe "ValueDataType" $ do
    genValidSpec @ValueDataType
    parameterSpec @ValueDataType
    parameterExampleSpec ["DATE"] TypeDate
    parameterExampleSpec ["DATE-TIME"] TypeDateTime
