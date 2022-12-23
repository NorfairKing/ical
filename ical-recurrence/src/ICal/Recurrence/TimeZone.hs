{-# LANGUAGE AllowAmbiguousTypes #-}
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

import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Time as Time
import Data.Void
import ICal.Component.TimeZone
import ICal.Conformance
import ICal.Property
import ICal.PropertyType.DateTime
import ICal.PropertyType.UTCOffset
import ICal.Recurrence
import ICal.Recurrence.Class

data ResolutionError
  = RecurrenceError !RecurrenceError
  | NoApplicableOffset
  deriving (Show, Eq)

instance Exception ResolutionError where
  displayException = \case
    RecurrenceError re -> displayException re
    NoApplicableOffset -> "No applicable timezone offset."

data ResolutionFixableError = RecurrenceFixableError !RecurrenceFixableError
  deriving (Show, Eq)

instance Exception ResolutionFixableError where
  displayException = \case
    RecurrenceFixableError rfe -> displayException rfe

type Resolv = Conform ResolutionError ResolutionFixableError Void

resolveLocalTime :: TimeZone -> Time.LocalTime -> Resolv (Maybe Time.UTCTime)
resolveLocalTime zone localTime = do
  mOffset <- chooseOffset zone localTime
  pure $ do
    offset <- mOffset
    let tz = offsetTimeZone offset
    pure $ Time.localTimeToUTC tz localTime

unresolveLocalTime :: TimeZone -> Time.UTCTime -> Time.LocalTime
unresolveLocalTime = undefined

offsetTimeZone :: UTCOffset -> Time.TimeZone
offsetTimeZone (UTCOffset w) = Time.minutesToTimeZone $ fromIntegral w

chooseOffset :: TimeZone -> Time.LocalTime -> Resolv (Maybe UTCOffset)
chooseOffset zone localTime = do
  offsetMap <- timeZoneRuleOccurrences (Time.localDay localTime) zone
  let mTransition = M.lookupLE localTime offsetMap <|> M.lookupGE localTime offsetMap
  pure $ do
    (transitionTime, (from, to)) <- mTransition
    pure $
      if localTime < transitionTime
        then from
        else to

-- | Compute a map of the timezone utc offset transitions.
--
-- It's a map of when the transition happened, to a tupled of the "from" offset
-- and the "to" offset.
timeZoneRuleOccurrences :: Time.Day -> TimeZone -> Resolv (Map Time.LocalTime (UTCOffset, UTCOffset))
timeZoneRuleOccurrences limit zone = do
  let observances = NE.toList (timeZoneObservances zone)
  maps <- forM observances $ \tzo -> do
    let o@Observance {..} = case tzo of
          StandardObservance (Standard s) -> s
          DaylightObservance (Daylight d) -> d
    occurrences <-
      conformMapAll RecurrenceError RecurrenceFixableError absurd $
        observanceOccurrences limit o
    pure $
      M.fromSet
        ( const
            ( unTimeZoneOffsetFrom observanceTimeZoneOffsetFrom,
              unTimeZoneOffsetTo observanceTimeZoneOffsetTo
            )
        )
        occurrences
  pure $ M.unions maps

-- | Compute when, until a given limit, the following observance changes the UTC offset
observanceOccurrences :: Time.Day -> Observance -> R (Set Time.LocalTime)
observanceOccurrences limit Observance {..} = do
  let recurringEvent =
        RecurringEvent
          { recurringEventStart = Just (DateTimeStartDateTime (DateTimeFloating observanceDateTimeStart)),
            recurringEventEndOrDuration = Nothing,
            recurringEventRecurrence =
              Recurrence
                { recurrenceExceptionDateTimes = S.empty, -- TODO
                  recurrenceRecurrenceDateTimes = S.empty,
                  recurrenceRecurrenceRules = observanceRecurrenceRules
                }
          }
  events <- recurEvents limit recurringEvent

  -- TODO use errors instead of Nothings?
  let go :: EventOccurrence -> Maybe Time.LocalTime
      go eo = do
        dts <- eventOccurrenceStart eo
        case dts of
          DateTimeStartDate _ -> Nothing
          DateTimeStartDateTime dt -> case dt of
            DateTimeZoned _ _ -> Nothing
            DateTimeUTC _ -> Nothing
            DateTimeFloating lt -> Just lt

  pure $ S.fromList $ mapMaybe go $ S.toList events
