{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.ComponentSpec where

import ICal.Component
import ICal.Component.Gen
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Standard" $ do
    genValidSpec @Standard
    componentSpec @Standard

  describe "Daylight" $ do
    genValidSpec @Daylight
    componentSpec @Daylight

  describe "TimeZone" $ do
    genValidSpec @TimeZone
    componentSpec @TimeZone

  describe "Event" $ do
    genValidSpec @Event
    componentSpec @Event

  describe "Calendar" $ do
    genValidSpec @Calendar
    componentSpec @Calendar
