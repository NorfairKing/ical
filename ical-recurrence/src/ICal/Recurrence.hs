{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | ICal Recurrence
--
-- This module exists to help you canculate the recurrence set of an event.
--
-- [section 3.8.5](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.5)
--
-- @
-- The following properties specify recurrence information in calendar
-- components.
-- @
--
-- [section 3.8.5.1](https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.5.1)
--
-- @
-- The recurrence set is the complete
-- set of recurrence instances for a calendar component.  The
-- recurrence set is generated by considering the initial "DTSTART"
-- property along with the "RRULE", "RDATE", and "EXDATE" properties
-- contained within the recurring component.
-- [...]
-- The final recurrence set is generated by gathering all of the
-- start DATE-TIME values generated by any of the specified "RRULE"
-- and "RDATE" properties, and then excluding any start DATE-TIME
-- values specified by "EXDATE" properties.
-- @
--
-- However, because recurrence sets can be infinite, we do not reify them
-- entirely and instead provide only functions that produce finite results.
--
--
-- This module is quite big and that's on purpose.
-- The problem is the following cyclical dependencies:
--
-- recurEvents -> resolveEndOrDuration
-- resolveEndOrDuration -> resolveLocalTime
-- resolveLocalTime -> recurEvents
module ICal.Recurrence
  ( HasRecurrence (..),
    recur,
    RecurringEvent (..),
    Recurrence (..),
    EventOccurrence (..),
    R,
    RecurrenceError (..),
    recurEvents,
    recurRecurrenceDateTimes,
    recurRecurrenceRules,
    recurRecurrenceRuleLocalTimes,
    removeExceptionDatetimes,
    resolveLocalTime,
    unresolveLocalTime,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Time as Time
import ICal.Component.Event
import ICal.Component.TimeZone
import ICal.Property
import ICal.PropertyType.Date
import ICal.PropertyType.DateTime
import ICal.PropertyType.DateTimes as DateTimes
import ICal.PropertyType.Duration
import ICal.PropertyType.Period
import ICal.PropertyType.RecurrenceRule
import ICal.PropertyType.UTCOffset
import ICal.Recurrence.Class
import ICal.Recurrence.RecurrenceRule

class HasRecurrence a where
  getRecurringEvent :: a -> RecurringEvent
  makeOccurrence :: a -> EventOccurrence -> a

instance HasRecurrence Event where
  getRecurringEvent Event {..} =
    let recurringEventStart = eventDateTimeStart
        recurringEventEndOrDuration = eventDateTimeEndDuration
        recurrenceExceptionDateTimes = eventExceptionDateTimes
        recurrenceRecurrenceDateTimes = eventRecurrenceDateTimes
        recurrenceRecurrenceRules = eventRecurrenceRules
        recurringEventRecurrence = Recurrence {..}
     in RecurringEvent {..}
  makeOccurrence e EventOccurrence {..} =
    e
      { eventDateTimeStart = eventOccurrenceStart,
        eventDateTimeEndDuration = eventOccurrenceEndOrDuration
      }

recur :: (Ord event, HasRecurrence event) => Time.Day -> event -> R (Set event)
recur limit event = do
  let recurringEvent = getRecurringEvent event
  occurrences <- recurEvents limit recurringEvent
  pure $ S.map (makeOccurrence event) occurrences

-- | Compute the recurrence set, up to a given limit
recurEvents :: Time.Day -> RecurringEvent -> R (Set EventOccurrence)
recurEvents limit RecurringEvent {..} =
  let -- @
      -- The "DTSTART" property for a "VEVENT" specifies the inclusive
      -- start of the event.  For recurring events, it also specifies the
      -- very first instance in the recurrence set.
      -- @
      -- @
      -- The "DTSTART" property
      -- defines the first instance in the recurrence set.
      -- @
      startEvent =
        EventOccurrence
          { eventOccurrenceStart = recurringEventStart,
            eventOccurrenceEndOrDuration = recurringEventEndOrDuration
          }
   in case recurringEventStart of
        Nothing -> pure $ S.singleton startEvent
        Just startDateTime -> do
          let Recurrence {..} = recurringEventRecurrence
          occurrencesFromRecurrenceDateTimes <- recurRecurrenceDateTimes startDateTime recurringEventEndOrDuration recurrenceRecurrenceDateTimes
          occurrencesFromRecurrenceRules <- recurRecurrenceRules limit startDateTime recurringEventEndOrDuration recurrenceRecurrenceRules
          -- @
          -- The final recurrence set is generated by gathering all of the
          -- start DATE-TIME values generated by any of the specified "RRULE"
          -- and "RDATE" properties, and then excluding any start DATE-TIME
          -- values specified by "EXDATE" properties.
          -- @
          -- @
          -- Where duplicate instances are generated by the "RRULE"
          -- and "RDATE" properties, only one recurrence is considered.
          -- Duplicate instances are ignored.
          -- @
          let preliminarySet = S.insert startEvent (S.union occurrencesFromRecurrenceDateTimes occurrencesFromRecurrenceRules)
          pure $ removeExceptionDatetimesSet recurrenceExceptionDateTimes preliminarySet

-- | Compute the occurrences that the recurrence rules imply
--
-- TODO implement this:
-- @
-- The recurrence set generated with multiple "RRULE" properties is
-- undefined.
-- @

-- | Compute the occurrences that the recurrence rules imply
recurRecurrenceRules ::
  -- | Limit
  Time.Day ->
  DateTimeStart ->
  Maybe (Either DateTimeEnd Duration) ->
  Set RecurrenceRule ->
  R (Set EventOccurrence)
recurRecurrenceRules limit start mEndOrDuration recurrenceRules = do
  case S.toList recurrenceRules of
    [] -> pure S.empty
    [recurrenceRule] -> recurRecurrenceRule limit start mEndOrDuration recurrenceRule
    l -> do
      -- The spec says:
      --
      -- @
      -- The recurrence set generated with multiple "RRULE" properties is
      -- undefined.
      -- @
      --
      -- However, we choose to define it as the union of the
      -- reccurence sets defined by the recurrence rules.
      emitFixableErrorR $ RecurrenceMultipleRecurrenceRules recurrenceRules
      S.unions <$> mapM (recurRecurrenceRule limit start mEndOrDuration) l

recurRecurrenceRule ::
  -- | Limit
  Time.Day ->
  DateTimeStart ->
  Maybe (Either DateTimeEnd Duration) ->
  RecurrenceRule ->
  R (Set EventOccurrence)
recurRecurrenceRule limit start mEndOrDuration recurrenceRule = do
  starts <- recurRecurrenceRuleDateTimeStarts limit start recurrenceRule
  fmap S.fromList $
    forM (S.toList starts) $ \newStart -> do
      newMEndOrDuration <- resolveEndOrDurationDate start mEndOrDuration newStart
      pure
        EventOccurrence
          { eventOccurrenceStart = Just newStart,
            eventOccurrenceEndOrDuration = newMEndOrDuration
          }

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

-- TODO about why we need the start date
-- @
-- Information, not contained in the rule, necessary to determine the
-- various recurrence instance start time and dates are derived from
-- the Start Time ("DTSTART") component attribute.  For example,
-- "FREQ=YEARLY;BYMONTH=1" doesn't specify a specific day within the
-- month or a time.  This information would be the same as what is
-- specified for "DTSTART".
-- @

-- TODO about invalid dates:
-- @
-- Recurrence rules may generate recurrence instances with an invalid
-- date (e.g., February 30) or nonexistent local time (e.g., 1:30 AM
-- on a day where the local time is moved forward by an hour at 1:00
-- AM).  Such recurrence instances MUST be ignored and MUST NOT be
-- counted as part of the recurrence set.
-- @

-- TODO About computing end and duration:
-- @
-- If the duration of the recurring component is specified with the
-- "DTEND" or "DUE" property, then the same exact duration will apply
-- to all the members of the generated recurrence set.  Else, if the
-- duration of the recurring component is specified with the
-- "DURATION" property, then the same nominal duration will apply to
-- all the members of the generated recurrence set and the exact
-- duration of each recurrence instance will depend on its specific
-- start time.  For example, recurrence instances of a nominal
-- duration of one day will have an exact duration of more or less
-- than 24 hours on a day where a time zone shift occurs.  The
-- duration of a specific recurrence may be modified in an exception
-- component or simply by using an "RDATE" property of PERIOD value
-- type.
-- @

-- | Remove the occurrences that the exception date times imply should be removed
removeExceptionDatetimesSet ::
  Set ExceptionDateTimes ->
  Set EventOccurrence ->
  Set EventOccurrence
removeExceptionDatetimesSet exceptionSet occurrences =
  S.foldl' (flip removeExceptionDatetimes) occurrences exceptionSet

removeExceptionDatetimes ::
  ExceptionDateTimes ->
  Set EventOccurrence ->
  Set EventOccurrence
removeExceptionDatetimes exceptions = S.filter $ \occurrence ->
  case eventOccurrenceStart occurrence of
    Nothing -> False
    Just start -> case start of
      DateTimeStartDateTime dateTime -> case exceptions of
        ExceptionDateTimes dateTimes -> dateTime `S.member` DateTimes.toSet dateTimes
        _ -> False
      DateTimeStartDate date -> case exceptions of
        ExceptionDates dates -> date `S.member` dates
        _ -> False

resolveEndOrDurationDate ::
  DateTimeStart ->
  Maybe (Either DateTimeEnd Duration) ->
  DateTimeStart ->
  R (Maybe (Either DateTimeEnd Duration))
resolveEndOrDurationDate originalStart mEndOrDuration newStart = case mEndOrDuration of
  Nothing -> pure Nothing
  Just (Right duration) -> pure $ Just (Right duration)
  Just (Left end) -> do
    -- @
    -- If the duration of the recurring component is specified with the
    -- "DTEND" or "DUE" property, then the same exact duration will apply
    -- to all the members of the generated recurrence set.  Else, if the
    -- duration of the recurring component is specified with the
    -- "DURATION" property, then the same nominal duration will apply to
    -- all the members of the generated recurrence set and the exact
    -- duration of each recurrence instance will depend on its specific
    -- start time.  For example, recurrence instances of a nominal
    -- duration of one day will have an exact duration of more or less
    -- than 24 hours on a day where a time zone shift occurs.  The
    -- duration of a specific recurrence may be modified in an exception
    -- component or simply by using an "RDATE" property of PERIOD value
    -- type.
    -- @
    newEnd <- computeNewEnd originalStart end newStart
    pure $ Just $ Left newEnd

computeNewEnd :: DateTimeStart -> DateTimeEnd -> DateTimeStart -> R DateTimeEnd
computeNewEnd originalStart end newStart =
  case (originalStart, end) of
    (DateTimeStartDate startDate, DateTimeEndDate endDate) ->
      let exactDuration = dateExactDuration startDate endDate
       in case newStart of
            DateTimeStartDate newDate -> pure $ DateTimeEndDate (dateAddDays exactDuration newDate)
            _ -> unfixableErrorR $ StartStartMismatch originalStart newStart
    (DateTimeStartDateTime startDateTime, DateTimeEndDateTime endDateTime) ->
      case newStart of
        DateTimeStartDateTime newDateTime -> do
          exactDuration <- dateTimeExactDuration startDateTime endDateTime
          newEndDateTime <- addExactDuration exactDuration newDateTime
          pure $ DateTimeEndDateTime newEndDateTime
        _ -> unfixableErrorR $ StartStartMismatch originalStart newStart
    -- These two cases represent invalid ical:
    -- @
    -- The "VEVENT" is also the calendar component used to specify an
    -- anniversary or daily reminder within a calendar.
    -- These events
    -- have a DATE value type for the "DTSTART" property instead of the
    -- default value type of DATE-TIME.  If such a "VEVENT" has a "DTEND"
    -- property, it MUST be specified as a DATE value also.
    -- @
    -- However, this is a new restriction, see the following, so we do
    -- _something_ in this case, to not have to error.
    --
    -- In the section: "A.1. New Restrictions":
    -- @
    -- The value type of the "DTEND" or "DUE" properties MUST match the
    -- value type of "DTSTART" property.
    -- @
    (DateTimeStartDate _, DateTimeEndDateTime _) ->
      unfixableErrorR $ StartEndMismatch originalStart end
    (DateTimeStartDateTime _, DateTimeEndDate _) ->
      unfixableErrorR $ StartEndMismatch originalStart end

-- TODO it is not clear at all whether this is the intended interpretation.
-- In fact, if these two dates are in a timezone with day light savings time, then the exact duration will not be an integer number of days.
dateExactDuration :: Date -> Date -> Integer
dateExactDuration = diffDates

dateTimeExactDuration :: DateTime -> DateTime -> R Time.NominalDiffTime
dateTimeExactDuration dt1 dt2 = case (dt1, dt2) of
  (DateTimeFloating lt1, DateTimeFloating lt2) ->
    -- Assuming the same timezone
    pure $ Time.diffLocalTime lt1 lt2
  (DateTimeUTC ut1, DateTimeUTC ut2) -> pure $ Time.diffUTCTime ut1 ut2
  (DateTimeZoned tzid1 lt1, DateTimeZoned tzid2 lt2) -> do
    tz1 <- requireTimeZone tzid1
    tz2 <- requireTimeZone tzid2
    u1 <- resolveLocalTimeR tz1 lt1
    u2 <- resolveLocalTimeR tz2 lt2
    dateTimeExactDuration (DateTimeUTC u1) (DateTimeUTC u2)
  _ -> unfixableErrorR $ ExactDurationMismatch dt1 dt2

addExactDuration :: Time.NominalDiffTime -> DateTime -> R DateTime
addExactDuration ndt = \case
  DateTimeFloating lt -> pure $ DateTimeFloating $ Time.addLocalTime ndt lt
  DateTimeUTC ut -> pure $ DateTimeUTC $ Time.addUTCTime ndt ut
  DateTimeZoned tzid lt -> do
    zone <- requireTimeZone tzid
    ut <- resolveLocalTimeR zone lt
    -- TODO if we could unresolve, we could implement this using DateTimeZoned.
    pure $ DateTimeUTC ut

resolveLocalTimeR :: TimeZone -> Time.LocalTime -> R Time.UTCTime
resolveLocalTimeR zone localTime = do
  mUtcTime <- R $ lift $ resolveLocalTime zone localTime
  case mUtcTime of
    Nothing -> unfixableErrorR $ FailedToResolveLocalTime zone localTime
    Just ut -> pure ut

resolveLocalTime :: TimeZone -> Time.LocalTime -> Resolv (Maybe Time.UTCTime)
resolveLocalTime zone localTime = do
  mOffset <- chooseOffset zone localTime
  pure $ do
    offset <- mOffset
    let tz = utcOffsetTimeZone offset
    pure $ Time.localTimeToUTC tz localTime

-- TODO: It's not clear if un-resolution is something that's computable at all
unresolveLocalTime :: TimeZone -> Time.UTCTime -> Time.LocalTime
unresolveLocalTime = undefined

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
    -- We MUST use M.empty here, otherwise resolving timezones might use
    -- timezone resolution recursively
    occurrences <- runR M.empty $ observanceOccurrences limit o
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
                { recurrenceExceptionDateTimes = S.empty,
                  recurrenceRecurrenceDateTimes = observanceRecurrenceDateTimes,
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
