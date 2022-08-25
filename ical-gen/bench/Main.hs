{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.DeepSeq
import Criterion.Main as Criterion
import Data.GenValidity
import Data.GenValidity.Criterion
import Data.GenValidity.Vector ()
import Data.Maybe (listToMaybe)
import Data.Typeable
import Data.Vector (Vector)
import qualified Data.Vector as V
import ICal
import ICal.Component.Gen ()
import ICal.Property.Gen ()
import ICal.PropertyType.Duration
import ICal.PropertyType.FloatingPoint
import ICal.PropertyType.Gen
import ICal.PropertyType.RecurrenceRule
import ICal.PropertyType.URI
import ICal.PropertyType.UTCOffset
import Test.QuickCheck
import Test.QuickCheck.Gen (Gen (..))
import Test.QuickCheck.Random (mkQCGen)

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
          shrinkerBench "shrinkImpreciseTimeOfDay" shrinkImpreciseTimeOfDay,
          shrinkerBench "shrinkImpreciseLocalTime" shrinkImpreciseLocalTime,
          shrinkerBench "shrinkImpreciseUTCTime" shrinkImpreciseUTCTime,
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

shrinkValidBench :: forall a. (Typeable a, NFData a, GenValid a) => Benchmark
shrinkValidBench =
  shrinkerBench
    ("shrinkValid " <> nameOf @a)
    (shrinkValid @a)

shrinkerBench :: forall a. (NFData a, GenValid a) => String -> (a -> [a]) -> Benchmark
shrinkerBench name shrinker =
  withArgs $ \args ->
    bench
      name
      (nf (V.map (listToMaybe . shrinker)) (args :: Vector a))

nameOf ::
  forall a.
  Typeable a =>
  String
nameOf =
  let s = show $ typeRep (Proxy @a)
   in if ' ' `elem` s
        then "(" ++ s ++ ")"
        else s

withArgs :: (NFData arg, GenValid arg) => (Vector arg -> Benchmark) -> Benchmark
withArgs = env (pure (generateDeterministically $ V.replicateM 10 genValid))

generateDeterministically :: Gen a -> a
generateDeterministically (MkGen f) = f seed size
  where
    seed = mkQCGen 42
    size = 30
