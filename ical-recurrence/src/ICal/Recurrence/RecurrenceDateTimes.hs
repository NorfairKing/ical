{-# LANGUAGE LambdaCase #-}

module ICal.Recurrence.RecurrenceDateTimes
  ( recurRecurrenceDateTimes,
  )
where

import Control.Monad
import Data.Set (Set)
import qualified Data.Set as S
import ICal.Property
import ICal.PropertyType
import qualified ICal.PropertyType.DateTimes as DateTimes
import ICal.Recurrence.Class

-- | Compute the occurrences that the recurrence date times imply
recurRecurrenceDateTimes ::
  DateTimeStart ->
  Maybe (Either DateTimeEnd Duration) ->
  Set RecurrenceDateTimes ->
  R (Set EventOccurrence)
recurRecurrenceDateTimes dateTimeStart endOrDuration recurrenceDateTimess =
  fmap (S.unions . map S.fromList) $
    forM (S.toList recurrenceDateTimess) $
      let withNewStart newStart = do
            resolvedEndOrDuration <- resolveEndOrDurationDate dateTimeStart endOrDuration newStart
            pure $
              EventOccurrence
                { eventOccurrenceStart = Just newStart,
                  eventOccurrenceEndOrDuration = resolvedEndOrDuration
                }
       in \case
            RecurrenceDates dates ->
              mapM
                (withNewStart . DateTimeStartDate)
                (S.toList dates)
            RecurrenceDateTimes dateTimes ->
              mapM
                (withNewStart . DateTimeStartDateTime)
                (S.toList (DateTimes.toSet dateTimes))
            RecurrencePeriods periods ->
              pure $
                map
                  ( \case
                      PeriodStartEnd start end ->
                        EventOccurrence
                          { eventOccurrenceStart = Just $ DateTimeStartDateTime (DateTimeUTC start),
                            eventOccurrenceEndOrDuration = Just (Left (DateTimeEndDateTime (DateTimeUTC end)))
                          }
                      PeriodStartDuration start duration ->
                        EventOccurrence
                          { eventOccurrenceStart = Just $ DateTimeStartDateTime (DateTimeUTC start),
                            eventOccurrenceEndOrDuration = Just (Right duration)
                          }
                  )
                  (S.toList periods)
