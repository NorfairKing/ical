{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Data.GenValidity.Criterion
import Data.GenValidity.Vector ()
import ICal
import ICal.Component.Gen ()
import ICal.Property.Gen ()
import ICal.PropertyType.Duration
import ICal.PropertyType.FloatingPoint
import ICal.PropertyType.Gen
import ICal.PropertyType.RecurrenceRule
import ICal.PropertyType.URI
import ICal.PropertyType.UTCOffset

main :: IO ()
main = do
  Criterion.defaultMain
    [ bgroup
        "generators"
        [ genValidBench @RecurrenceRule,
          genValidBench @Observance,
          genValidBench @TimeZone,
          genValidBench @Event,
          genValidBench @Calendar,
          genValidBench @ICalendar
        ],
      bgroup
        "shrinkers"
        [ --
          shrinkBench "shrinkImpreciseTimeOfDay" shrinkImpreciseTimeOfDay,
          shrinkBench "shrinkImpreciseLocalTime" shrinkImpreciseLocalTime,
          shrinkBench "shrinkImpreciseUTCTime" shrinkImpreciseUTCTime,
          shrinkValidBench @FloatingPoint,
          shrinkValidBench @Time,
          shrinkValidBench @Date,
          shrinkValidBench @DateTime,
          shrinkValidBench @URI,
          shrinkValidBench @UTCOffset,
          shrinkValidBench @DateTimeStamp,
          shrinkValidBench @UID,
          shrinkValidBench @DateTimeStart,
          shrinkValidBench @Classification,
          shrinkValidBench @Created,
          shrinkValidBench @Description,
          shrinkValidBench @GeographicPosition,
          shrinkValidBench @LastModified,
          shrinkValidBench @Location,
          shrinkValidBench @Status,
          shrinkValidBench @Summary,
          shrinkValidBench @Transparency,
          shrinkValidBench @URL,
          shrinkValidBench @DateTimeEnd,
          shrinkValidBench @Duration,
          shrinkValidBench @RecurrenceRule,
          shrinkValidBench @Event,
          shrinkValidBench @Observance,
          shrinkValidBench @TimeZone,
          shrinkValidBench @Calendar,
          shrinkValidBench @ICalendar
        ]
    ]
