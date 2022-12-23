{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Recurrence.TimeZone
  ( resolveLocalTime,
    unresolveLocalTime,
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Time as Time
import ICal.Component.TimeZone
import ICal.PropertyType.UTCOffset

resolveLocalTime :: TimeZone -> Time.LocalTime -> Time.UTCTime
resolveLocalTime = undefined

unresolveLocalTime :: TimeZone -> Time.UTCTime -> Time.LocalTime
unresolveLocalTime = undefined

chooseOffset :: TimeZone -> Time.LocalTime -> UTCOffset
chooseOffset = undefined

-- | Compute the closest timezone transition to the given local time in that
-- timezone.
chooseRuleToApply ::
  Time.LocalTime ->
  TimeZone ->
  Maybe (Time.LocalTime, (UTCOffset, UTCOffset))
chooseRuleToApply = undefined

-- | Compute a map of the timezone utc offset transitions.
--
-- It's a map of when the transition happened, to a tupled of the "from" offset
-- and the "to" offset.
timeZoneRuleOccurrences :: Time.LocalTime -> TimeZone -> Map Time.LocalTime (UTCOffset, UTCOffset)
timeZoneRuleOccurrences limit = undefined
