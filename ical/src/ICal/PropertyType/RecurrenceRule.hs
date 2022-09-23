{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.PropertyType.RecurrenceRule where

import Control.DeepSeq
import Control.Monad
import Data.CaseInsensitive (CI (..))
import qualified Data.CaseInsensitive as CI
import Data.Maybe
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time as Time
import Data.Validity
import Data.Validity.Containers ()
import Data.Validity.Time ()
import Data.Void
import GHC.Generics (Generic)
import ICal.Conformance
import ICal.ContentLine
import ICal.PropertyType.Class
import ICal.PropertyType.Date
import ICal.PropertyType.DateTime
import Text.Read

#if !MIN_VERSION_time(1,9,5)
deriving instance Ord DayOfWeek -- Silly that this doesn't exist. We need to be able to put days in a set
#endif

deriving instance Generic DayOfWeek

deriving instance Bounded DayOfWeek -- Silly that this doesn't exist.

instance NFData DayOfWeek where
  rnf !_ = ()

-- | Recurrence Rule
--
-- === [section 3.3.10](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.10)
--
-- @
-- Value Name:  RECUR
--
-- Purpose:  This value type is used to identify properties that contain
--    a recurrence rule specification.
--
-- Format Definition:  This value type is defined by the following
--    notation:
--
--     recur           = recur-rule-part *( ";" recur-rule-part )
--                     ;
--                     ; The rule parts are not ordered in any
--                     ; particular sequence.
--                     ;
--                     ; The FREQ rule part is REQUIRED,
--                     ; but MUST NOT occur more than once.
--                     ;
--                     ; The UNTIL or COUNT rule parts are OPTIONAL,
--                     ; but they MUST NOT occur in the same 'recur'.
--                     ;
--
--                     ; The other rule parts are OPTIONAL,
--                     ; but MUST NOT occur more than once.
--
--     recur-rule-part = ( "FREQ" "=" freq )
--                     / ( "UNTIL" "=" enddate )
--                     / ( "COUNT" "=" 1*DIGIT )
--                     / ( "INTERVAL" "=" 1*DIGIT )
--                     / ( "BYSECOND" "=" byseclist )
--                     / ( "BYMINUTE" "=" byminlist )
--                     / ( "BYHOUR" "=" byhrlist )
--                     / ( "BYDAY" "=" bywdaylist )
--                     / ( "BYMONTHDAY" "=" bymodaylist )
--                     / ( "BYYEARDAY" "=" byyrdaylist )
--                     / ( "BYWEEKNO" "=" bywknolist )
--                     / ( "BYMONTH" "=" bymolist )
--                     / ( "BYSETPOS" "=" bysplist )
--                     / ( "WKST" "=" weekday )
--
--     freq        = "SECONDLY" / "MINUTELY" / "HOURLY" / "DAILY"
--                 / "WEEKLY" / "MONTHLY" / "YEARLY"
--
--     enddate     = date / date-time
--
--     byseclist   = ( seconds *("," seconds) )
--
--     seconds     = 1*2DIGIT       ;0 to 60
--
--     byminlist   = ( minutes *("," minutes) )
--
--     minutes     = 1*2DIGIT       ;0 to 59
--
--     byhrlist    = ( hour *("," hour) )
--
--     hour        = 1*2DIGIT       ;0 to 23
--
--     bywdaylist  = ( weekdaynum *("," weekdaynum) )
--
--     weekdaynum  = [[plus / minus] ordwk] weekday
--
--     plus        = "+"
--
--     minus       = "-"
--
--     ordwk       = 1*2DIGIT       ;1 to 53
--
--     weekday     = "SU" / "MO" / "TU" / "WE" / "TH" / "FR" / "SA"
--     ;Corresponding to SUNDAY, MONDAY, TUESDAY, WEDNESDAY, THURSDAY,
--     ;FRIDAY, and SATURDAY days of the week.
--
--
--     bymodaylist = ( monthdaynum *("," monthdaynum) )
--
--     monthdaynum = [plus / minus] ordmoday
--
--     ordmoday    = 1*2DIGIT       ;1 to 31
--
--     byyrdaylist = ( yeardaynum *("," yeardaynum) )
--
--     yeardaynum  = [plus / minus] ordyrday
--
--     ordyrday    = 1*3DIGIT      ;1 to 366
--
--     bywknolist  = ( weeknum *("," weeknum) )
--
--     weeknum     = [plus / minus] ordwk
--
--     bymolist    = ( monthnum *("," monthnum) )
--
--     monthnum    = 1*2DIGIT       ;1 to 12
--
--     bysplist    = ( setposday *("," setposday) )
--
--     setposday   = yeardaynum
--
-- Description:  This value type is a structured value consisting of a
--    list of one or more recurrence grammar parts.  Each rule part is
--    defined by a NAME=VALUE pair.  The rule parts are separated from
--    each other by the SEMICOLON character.  The rule parts are not
--    ordered in any particular sequence.  Individual rule parts MUST
--    only be specified once.  Compliant applications MUST accept rule
--    parts ordered in any sequence, but to ensure backward
--    compatibility with applications that pre-date this revision of
--    iCalendar the FREQ rule part MUST be the first rule part specified
--    in a RECUR value.
--
--    The FREQ rule part identifies the type of recurrence rule.  This
--    rule part MUST be specified in the recurrence rule.  Valid values
--    include SECONDLY, to specify repeating events based on an interval
--    of a second or more; MINUTELY, to specify repeating events based
--    on an interval of a minute or more; HOURLY, to specify repeating
--    events based on an interval of an hour or more; DAILY, to specify
--    repeating events based on an interval of a day or more; WEEKLY, to
--    specify repeating events based on an interval of a week or more;
--    MONTHLY, to specify repeating events based on an interval of a
--    month or more; and YEARLY, to specify repeating events based on an
--    interval of a year or more.
--
--
--    The INTERVAL rule part contains a positive integer representing at
--    which intervals the recurrence rule repeats.  The default value is
--    "1", meaning every second for a SECONDLY rule, every minute for a
--    MINUTELY rule, every hour for an HOURLY rule, every day for a
--    DAILY rule, every week for a WEEKLY rule, every month for a
--    MONTHLY rule, and every year for a YEARLY rule.  For example,
--    within a DAILY rule, a value of "8" means every eight days.
--
--    The UNTIL rule part defines a DATE or DATE-TIME value that bounds
--    the recurrence rule in an inclusive manner.  If the value
--    specified by UNTIL is synchronized with the specified recurrence,
--    this DATE or DATE-TIME becomes the last instance of the
--    recurrence.  The value of the UNTIL rule part MUST have the same
--    value type as the "DTSTART" property.  Furthermore, if the
--    "DTSTART" property is specified as a date with local time, then
--    the UNTIL rule part MUST also be specified as a date with local
--    time.  If the "DTSTART" property is specified as a date with UTC
--    time or a date with local time and time zone reference, then the
--    UNTIL rule part MUST be specified as a date with UTC time.  In the
--    case of the "STANDARD" and "DAYLIGHT" sub-components the UNTIL
--    rule part MUST always be specified as a date with UTC time.  If
--    specified as a DATE-TIME value, then it MUST be specified in a UTC
--    time format.  If not present, and the COUNT rule part is also not
--    present, the "RRULE" is considered to repeat forever.
--
--    The COUNT rule part defines the number of occurrences at which to
--    range-bound the recurrence.  The "DTSTART" property value always
--    counts as the first occurrence.
--
--    The BYSECOND rule part specifies a COMMA-separated list of seconds
--    within a minute.  Valid values are 0 to 60.  The BYMINUTE rule
--    part specifies a COMMA-separated list of minutes within an hour.
--    Valid values are 0 to 59.  The BYHOUR rule part specifies a COMMA-
--    separated list of hours of the day.  Valid values are 0 to 23.
--    The BYSECOND, BYMINUTE and BYHOUR rule parts MUST NOT be specified
--    when the associated "DTSTART" property has a DATE value type.
--    These rule parts MUST be ignored in RECUR value that violate the
--    above requirement (e.g., generated by applications that pre-date
--    this revision of iCalendar).
--
--    The BYDAY rule part specifies a COMMA-separated list of days of
--    the week; SU indicates Sunday; MO indicates Monday; TU indicates
--    Tuesday; WE indicates Wednesday; TH indicates Thursday; FR
--    indicates Friday; and SA indicates Saturday.
--
--    Each BYDAY value can also be preceded by a positive (+n) or
--    negative (-n) integer.  If present, this indicates the nth
--    occurrence of a specific day within the MONTHLY or YEARLY "RRULE".
--
--
--    For example, within a MONTHLY rule, +1MO (or simply 1MO)
--    represents the first Monday within the month, whereas -1MO
--    represents the last Monday of the month.  The numeric value in a
--    BYDAY rule part with the FREQ rule part set to YEARLY corresponds
--    to an offset within the month when the BYMONTH rule part is
--    present, and corresponds to an offset within the year when the
--    BYWEEKNO or BYMONTH rule parts are present.  If an integer
--    modifier is not present, it means all days of this type within the
--    specified frequency.  For example, within a MONTHLY rule, MO
--    represents all Mondays within the month.  The BYDAY rule part MUST
--    NOT be specified with a numeric value when the FREQ rule part is
--    not set to MONTHLY or YEARLY.  Furthermore, the BYDAY rule part
--    MUST NOT be specified with a numeric value with the FREQ rule part
--    set to YEARLY when the BYWEEKNO rule part is specified.
--
--    The BYMONTHDAY rule part specifies a COMMA-separated list of days
--    of the month.  Valid values are 1 to 31 or -31 to -1.  For
--    example, -10 represents the tenth to the last day of the month.
--    The BYMONTHDAY rule part MUST NOT be specified when the FREQ rule
--    part is set to WEEKLY.
--
--    The BYYEARDAY rule part specifies a COMMA-separated list of days
--    of the year.  Valid values are 1 to 366 or -366 to -1.  For
--    example, -1 represents the last day of the year (December 31st)
--    and -306 represents the 306th to the last day of the year (March
--    1st).  The BYYEARDAY rule part MUST NOT be specified when the FREQ
--    rule part is set to DAILY, WEEKLY, or MONTHLY.
--
--    The BYWEEKNO rule part specifies a COMMA-separated list of
--    ordinals specifying weeks of the year.  Valid values are 1 to 53
--    or -53 to -1.  This corresponds to weeks according to week
--    numbering as defined in [ISO.8601.2004].  A week is defined as a
--    seven day period, starting on the day of the week defined to be
--    the week start (see WKST).  Week number one of the calendar year
--    is the first week that contains at least four (4) days in that
--    calendar year.  This rule part MUST NOT be used when the FREQ rule
--    part is set to anything other than YEARLY.  For example, 3
--    represents the third week of the year.
--
--       Note: Assuming a Monday week start, week 53 can only occur when
--       Thursday is January 1 or if it is a leap year and Wednesday is
--       January 1.
--
--    The BYMONTH rule part specifies a COMMA-separated list of months
--    of the year.  Valid values are 1 to 12.
--
--    The WKST rule part specifies the day on which the workweek starts.
--    Valid values are MO, TU, WE, TH, FR, SA, and SU.  This is
--
--
--
--    significant when a WEEKLY "RRULE" has an interval greater than 1,
--    and a BYDAY rule part is specified.  This is also significant when
--    in a YEARLY "RRULE" when a BYWEEKNO rule part is specified.  The
--    default value is MO.
--
--    The BYSETPOS rule part specifies a COMMA-separated list of values
--    that corresponds to the nth occurrence within the set of
--    recurrence instances specified by the rule.  BYSETPOS operates on
--    a set of recurrence instances in one interval of the recurrence
--    rule.  For example, in a WEEKLY rule, the interval would be one
--    week A set of recurrence instances starts at the beginning of the
--    interval defined by the FREQ rule part.  Valid values are 1 to 366
--    or -366 to -1.  It MUST only be used in conjunction with another
--    BYxxx rule part.  For example "the last work day of the month"
--    could be represented as:
--
--     FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1
--
--    Each BYSETPOS value can include a positive (+n) or negative (-n)
--    integer.  If present, this indicates the nth occurrence of the
--    specific occurrence within the set of occurrences specified by the
--    rule.
--
--    Recurrence rules may generate recurrence instances with an invalid
--    date (e.g., February 30) or nonexistent local time (e.g., 1:30 AM
--    on a day where the local time is moved forward by an hour at 1:00
--    AM).  Such recurrence instances MUST be ignored and MUST NOT be
--    counted as part of the recurrence set.
--
--    Information, not contained in the rule, necessary to determine the
--    various recurrence instance start time and dates are derived from
--    the Start Time ("DTSTART") component attribute.  For example,
--    "FREQ=YEARLY;BYMONTH=1" doesn't specify a specific day within the
--    month or a time.  This information would be the same as what is
--    specified for "DTSTART".
--
--    BYxxx rule parts modify the recurrence in some manner.  BYxxx rule
--    parts for a period of time that is the same or greater than the
--    frequency generally reduce or limit the number of occurrences of
--    the recurrence generated.  For example, "FREQ=DAILY;BYMONTH=1"
--    reduces the number of recurrence instances from all days (if
--    BYMONTH rule part is not present) to all days in January.  BYxxx
--    rule parts for a period of time less than the frequency generally
--    increase or expand the number of occurrences of the recurrence.
--    For example, "FREQ=YEARLY;BYMONTH=1,2" increases the number of
--    days within the yearly recurrence set from 1 (if BYMONTH rule part
--    is not present) to 2.
--
--
--    If multiple BYxxx rule parts are specified, then after evaluating
--    the specified FREQ and INTERVAL rule parts, the BYxxx rule parts
--    are applied to the current set of evaluated occurrences in the
--    following order: BYMONTH, BYWEEKNO, BYYEARDAY, BYMONTHDAY, BYDAY,
--    BYHOUR, BYMINUTE, BYSECOND and BYSETPOS; then COUNT and UNTIL are
--    evaluated.
--
--    The table below summarizes the dependency of BYxxx rule part
--    expand or limit behavior on the FREQ rule part value.
--
--    The term "N/A" means that the corresponding BYxxx rule part MUST
--    NOT be used with the corresponding FREQ value.
--
--    BYDAY has some special behavior depending on the FREQ value and
--    this is described in separate notes below the table.
--
-- +----------+--------+--------+-------+-------+------+-------+------+
-- |          |SECONDLY|MINUTELY|HOURLY |DAILY  |WEEKLY|MONTHLY|YEARLY|
-- +----------+--------+--------+-------+-------+------+-------+------+
-- |BYMONTH   |Limit   |Limit   |Limit  |Limit  |Limit |Limit  |Expand|
-- +----------+--------+--------+-------+-------+------+-------+------+
-- |BYWEEKNO  |N/A     |N/A     |N/A    |N/A    |N/A   |N/A    |Expand|
-- +----------+--------+--------+-------+-------+------+-------+------+
-- |BYYEARDAY |Limit   |Limit   |Limit  |N/A    |N/A   |N/A    |Expand|
-- +----------+--------+--------+-------+-------+------+-------+------+
-- |BYMONTHDAY|Limit   |Limit   |Limit  |Limit  |N/A   |Expand |Expand|
-- +----------+--------+--------+-------+-------+------+-------+------+
-- |BYDAY     |Limit   |Limit   |Limit  |Limit  |Expand|Note 1 |Note 2|
-- +----------+--------+--------+-------+-------+------+-------+------+
-- |BYHOUR    |Limit   |Limit   |Limit  |Expand |Expand|Expand |Expand|
-- +----------+--------+--------+-------+-------+------+-------+------+
-- |BYMINUTE  |Limit   |Limit   |Expand |Expand |Expand|Expand |Expand|
-- +----------+--------+--------+-------+-------+------+-------+------+
-- |BYSECOND  |Limit   |Expand  |Expand |Expand |Expand|Expand |Expand|
-- +----------+--------+--------+-------+-------+------+-------+------+
-- |BYSETPOS  |Limit   |Limit   |Limit  |Limit  |Limit |Limit  |Limit |
-- +----------+--------+--------+-------+-------+------+-------+------+
--
--    Note 1:  Limit if BYMONTHDAY is present; otherwise, special expand
--             for MONTHLY.
--
--    Note 2:  Limit if BYYEARDAY or BYMONTHDAY is present; otherwise,
--             special expand for WEEKLY if BYWEEKNO present; otherwise,
--             special expand for MONTHLY if BYMONTH present; otherwise,
--             special expand for YEARLY.
--
--    Here is an example of evaluating multiple BYxxx rule parts.
--
--     DTSTART;TZID=America/New_York:19970105T083000
--     RRULE:FREQ=YEARLY;INTERVAL=2;BYMONTH=1;BYDAY=SU;BYHOUR=8,9;
--      BYMINUTE=30
--
--    First, the "INTERVAL=2" would be applied to "FREQ=YEARLY" to
--    arrive at "every other year".  Then, "BYMONTH=1" would be applied
--    to arrive at "every January, every other year".  Then, "BYDAY=SU"
--    would be applied to arrive at "every Sunday in January, every
--    other year".  Then, "BYHOUR=8,9" would be applied to arrive at
--    "every Sunday in January at 8 AM and 9 AM, every other year".
--    Then, "BYMINUTE=30" would be applied to arrive at "every Sunday in
--    January at 8:30 AM and 9:30 AM, every other year".  Then, lacking
--    information from "RRULE", the second is derived from "DTSTART", to
--    end up in "every Sunday in January at 8:30:00 AM and 9:30:00 AM,
--    every other year".  Similarly, if the BYMINUTE, BYHOUR, BYDAY,
--    BYMONTHDAY, or BYMONTH rule part were missing, the appropriate
--    minute, hour, day, or month would have been retrieved from the
--    "DTSTART" property.
--
--    If the computed local start time of a recurrence instance does not
--    exist, or occurs more than once, for the specified time zone, the
--    time of the recurrence instance is interpreted in the same manner
--    as an explicit DATE-TIME value describing that date and time, as
--    specified in Section 3.3.5.
--
--    No additional content value encoding (i.e., BACKSLASH character
--    encoding, see Section 3.3.11) is defined for this value type.
--
-- Example:  The following is a rule that specifies 10 occurrences that
--    occur every other day:
--
--     FREQ=DAILY;COUNT=10;INTERVAL=2
--
--    There are other examples specified in Section 3.8.5.3.
-- @
data RecurrenceRule = RecurrenceRule
  { -- | The FREQ rule part identifies the type of recurrence rule.
    --
    -- See 'Frequency'
    recurrenceRuleFrequency :: !Frequency,
    -- | The INTERVAL rule part contains a positive integer representing at which intervals the recurrence rule repeats.
    --
    -- See 'Interval'
    recurrenceRuleInterval :: !Interval,
    -- | This is one haskell-field based on two fields in the spec: UNTIL and COUNT together.
    --
    -- This because the spec says
    --
    -- > "The UNTIL or COUNT rule parts are OPTIONAL, but they MUST NOT occur in the same 'recur'."
    --
    -- See 'UntilCount'
    recurrenceRuleUntilCount :: !(Maybe (Either Until Count)),
    -- | The BYSECOND rule part specifies a COMMA-separated list of seconds within a minute.
    --
    -- See 'BySecond'
    recurrenceRuleBySecond :: !(Set BySecond),
    -- | The BYMINUTE rule part specifies a COMMA-separated list of minutes within an hour.
    --
    -- See 'ByMinute'
    recurrenceRuleByMinute :: !(Set ByMinute),
    -- | The BYHOUR rule part specifies a COMMA-separated list of hours of the day.
    --
    -- The BYSECOND, BYMINUTE and BYHOUR rule parts MUST NOT be specified when the
    -- associated "DTSTART" property has a DATE value type.  These rule parts MUST
    -- be ignored in RECUR value that violate the above requirement (e.g.,
    -- generated by applications that pre-date this revision of iCalendar).
    --
    -- See 'ByHour'
    recurrenceRuleByHour :: !(Set ByHour),
    -- | The BYDAY rule part specifies a COMMA-separated list of days of the week; [...]
    --
    -- The BYDAY rule part MUST NOT be specified with a numeric value when
    -- the FREQ rule part is not set to MONTHLY or YEARLY.  Furthermore,
    -- the BYDAY rule part MUST NOT be specified with a numeric value with
    -- the FREQ rule part set to YEARLY when the BYWEEKNO rule part is
    -- specified.
    --
    -- See 'ByDay'
    recurrenceRuleByDay :: !(Set ByDay),
    -- | The BYMONTHDAY rule part specifies a COMMA-separated list of days of the month.
    --
    -- The BYMONTHDAY rule part
    -- MUST NOT be specified when the FREQ rule part is set to WEEKLY
    --
    -- See 'ByMonthDay'
    recurrenceRuleByMonthDay :: !(Set ByMonthDay),
    -- | The BYYEARDAY rule part specifies a COMMA-separated list of days of the year.
    --
    -- The BYYEARDAY rule
    -- part MUST NOT be specified when the FREQ rule part is set to DAILY,
    -- WEEKLY, or MONTHLY.
    --
    -- See 'ByYearDay'
    recurrenceRuleByYearDay :: !(Set ByYearDay),
    -- | The BYWEEKNO rule part specifies a COMMA-separated list of ordinals specifying weeks of the year.
    --
    -- This rule part MUST NOT be used when
    -- the FREQ rule part is set to anything other than YEARLY.
    --
    -- See 'ByWeekNo'
    recurrenceRuleByWeekNo :: !(Set ByWeekNo),
    -- | The BYMONTH rule part specifies a COMMA-separated list of months of the year.
    --
    -- See 'ByMonth'
    recurrenceRuleByMonth :: !(Set ByMonth),
    -- | The WKST rule part specifies the day on which the workweek starts.
    --
    -- Valid values are MO, TU, WE, TH, FR, SA, and SU.  This is
    -- significant when a WEEKLY "RRULE" has an interval greater than 1,
    -- and a BYDAY rule part is specified.  This is also significant when
    -- in a YEARLY "RRULE" when a BYWEEKNO rule part is specified.  The
    -- default value is MO.
    --
    -- Note: We did not chose 'Maybe DayOfWeek' because that would have two ways to represent the default value.
    recurrenceRuleWeekStart :: !WeekStart,
    -- | The BYSETPOS rule part specifies a COMMA-separated list of values
    -- that corresponds to the nth occurrence within the set of recurrence
    -- instances specified by the rule.
    --
    -- It MUST only be used in conjunction with another BYxxx rule part.
    --
    -- See 'BySetPos'
    recurrenceRuleBySetPos :: !(Set BySetPos)
  }
  deriving stock (Show, Eq, Ord, Generic)

instance Validity RecurrenceRule where
  validate rule@RecurrenceRule {..} =
    mconcat
      [ genericValidate rule,
        decorateList (S.toList recurrenceRuleByDay) $ \bd ->
          declare "The BYDAY rule part MUST NOT be specified with a numeric value when the FREQ rule part is not set to MONTHLY or YEARLY." $
            let care = case bd of
                  Every _ -> False
                  Specific _ _ -> True
             in case recurrenceRuleFrequency of
                  Monthly -> care
                  Yearly -> care
                  _ -> True,
        declare "The BYMONTHDAY rule part MUST NOT be specified when the FREQ rule part is set to WEEKLY" $
          case recurrenceRuleFrequency of
            Weekly -> S.null recurrenceRuleByMonthDay
            _ -> True,
        declare "The BYYEARDAY rule part MUST NOT be specified when the FREQ rule part is set to DAILY, WEEKLY, or MONTHLY." $
          case recurrenceRuleFrequency of
            Daily -> S.null recurrenceRuleByYearDay
            Weekly -> S.null recurrenceRuleByYearDay
            Monthly -> S.null recurrenceRuleByYearDay
            _ -> True,
        declare "The BYWEEKNO rule part MUST NOT be used when the FREQ rule part is set to anything other than YEARLY." $
          case recurrenceRuleFrequency of
            Yearly -> True
            _ -> null recurrenceRuleByWeekNo,
        declare "the BYSETPOST rule part MUST only be used in conjunction with another BYxxx rule part." $
          if S.null recurrenceRuleBySetPos
            then True
            else
              any
                not
                [ S.null recurrenceRuleBySecond,
                  S.null recurrenceRuleByMinute,
                  S.null recurrenceRuleByHour,
                  S.null recurrenceRuleByDay,
                  S.null recurrenceRuleByMonthDay,
                  S.null recurrenceRuleByYearDay,
                  S.null recurrenceRuleByWeekNo,
                  S.null recurrenceRuleByMonth
                ]
      ]

instance NFData RecurrenceRule

instance IsPropertyType RecurrenceRule where
  propertyTypeP = recurrenceRuleP
  propertyTypeB = recurrenceRuleB

recurrenceRuleP ::
  ContentLineValue ->
  Conform PropertyTypeParseError Void Void RecurrenceRule
recurrenceRuleP ContentLineValue {..} = do
  -- contentLineValueParams are ignored
  let parts = T.splitOn ";" contentLineValueRaw
  tups <- forM parts $ \partText -> case T.splitOn "=" partText of
    [] -> unfixableError $ OtherPropertyTypeParseError "Could not parse recurrence rule part."
    (k : vs) -> pure (k, T.intercalate "=" vs)
  let parsePart :: forall part. IsRecurrenceRulePart part => Conform PropertyTypeParseError Void Void part
      parsePart =
        let name = recurrenceRulePartName (Proxy :: Proxy part)
         in case lookup name tups of
              Nothing -> unfixableError $ OtherPropertyTypeParseError $ "Recurrence rule part not found: " <> show name
              Just val -> recurrenceRulePartP val

      parseMPart :: forall part. IsRecurrenceRulePart part => Conform PropertyTypeParseError Void Void (Maybe part)
      parseMPart =
        let name = recurrenceRulePartName (Proxy :: Proxy part)
         in mapM recurrenceRulePartP (lookup name tups)

      parseDPart :: forall part. IsRecurrenceRulePart part => part -> Conform PropertyTypeParseError Void Void part
      parseDPart defaultValue = fromMaybe defaultValue <$> parseMPart

      parseSetPart :: forall part. IsRecurrenceRulePart (Set part) => Conform PropertyTypeParseError Void Void (Set part)
      parseSetPart = parseDPart S.empty

  recurrenceRuleFrequency <- parsePart
  recurrenceRuleInterval <- parseDPart (Interval 1)
  mUntil <- parseMPart
  mCount <- parseMPart
  let recurrenceRuleUntilCount = case (mUntil, mCount) of
        (Nothing, Nothing) -> Nothing
        (Nothing, Just c) -> Just (Left c)
        -- Don't reject invalid ical that defines both, but ignore the count.
        -- TODO emit a fixable warning here.
        (Just u, _) -> Just (Right u)

  recurrenceRuleBySecond <- parseSetPart
  recurrenceRuleByMinute <- parseSetPart
  recurrenceRuleByHour <- parseSetPart
  recurrenceRuleByDay <- parseSetPart
  recurrenceRuleByMonthDay <- parseSetPart
  recurrenceRuleByYearDay <- parseSetPart
  recurrenceRuleByWeekNo <- parseSetPart
  recurrenceRuleByMonth <- parseSetPart
  recurrenceRuleWeekStart <- parseDPart (WeekStart Monday)
  recurrenceRuleBySetPos <- parseSetPart

  pure RecurrenceRule {..}

-- TODO comply with this:
-- @
-- Compliant applications MUST accept rule
-- parts ordered in any sequence, but to ensure backward
-- compatibility with applications that pre-date this revision of
-- iCalendar the FREQ rule part MUST be the first rule part specified
-- in a RECUR value.
-- @
recurrenceRuleB ::
  RecurrenceRule -> ContentLineValue
recurrenceRuleB RecurrenceRule {..} =
  let tup :: forall part. IsRecurrenceRulePart part => part -> (Text, Text)
      tup part = (recurrenceRulePartName (Proxy :: Proxy part), recurrenceRulePartB part)
      dTup :: forall part. (Eq part, IsRecurrenceRulePart part) => part -> part -> [(Text, Text)]
      dTup defaultValue p = if p == defaultValue then [] else [tup p]
      setTup :: forall part. IsRecurrenceRulePart (Set part) => Set part -> [(Text, Text)]
      setTup set = if S.null set then [] else [tup set]
      tups :: [(Text, Text)]
      tups =
        concat
          [ [tup recurrenceRuleFrequency],
            dTup (Interval 1) recurrenceRuleInterval,
            case recurrenceRuleUntilCount of
              Just (Left u) -> [tup u]
              Just (Right c) -> [tup c]
              Nothing -> [],
            setTup recurrenceRuleBySecond,
            setTup recurrenceRuleByMinute,
            setTup recurrenceRuleByHour,
            setTup recurrenceRuleByDay,
            setTup recurrenceRuleByMonthDay,
            setTup recurrenceRuleByYearDay,
            setTup recurrenceRuleByWeekNo,
            setTup recurrenceRuleByMonth,
            dTup (WeekStart Monday) recurrenceRuleWeekStart,
            setTup recurrenceRuleBySetPos
          ]
      parts :: [Text]
      parts = map (\(k, v) -> k <> "=" <> v) tups
   in mkSimpleContentLineValue $ T.intercalate ";" parts

class IsRecurrenceRulePart part where
  recurrenceRulePartName :: Proxy part -> Text
  recurrenceRulePartP :: Text -> Conform PropertyTypeParseError Void Void part
  recurrenceRulePartB :: part -> Text

setP :: Ord part => (Text -> Conform PropertyTypeParseError Void Void part) -> Text -> Conform PropertyTypeParseError Void Void (Set part)
setP parser t =
  if T.null t
    then pure S.empty
    else S.fromList <$> mapM parser (T.splitOn "," t)

setB :: (part -> Text) -> Set part -> Text
setB renderer set =
  if S.null set
    then ""
    else T.intercalate "," (map renderer (S.toList set))

-- | Frequency
--
-- Part of [section 3.3.10](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.10)
--
--
-- @
-- freq        = "SECONDLY" / "MINUTELY" / "HOURLY" / "DAILY"
--             / "WEEKLY" / "MONTHLY" / "YEARLY"
-- @
--
-- @
-- The FREQ rule part identifies the type of recurrence rule.  This
-- rule part MUST be specified in the recurrence rule.  Valid values
-- include SECONDLY, to specify repeating events based on an interval
-- of a second or more; MINUTELY, to specify repeating events based
-- on an interval of a minute or more; HOURLY, to specify repeating
-- events based on an interval of an hour or more; DAILY, to specify
-- repeating events based on an interval of a day or more; WEEKLY, to
-- specify repeating events based on an interval of a week or more;
-- MONTHLY, to specify repeating events based on an interval of a
-- month or more; and YEARLY, to specify repeating events based on an
-- interval of a year or more.
-- @
data Frequency
  = Secondly
  | Minutely
  | Hourly
  | Daily
  | Weekly
  | Monthly
  | Yearly
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)

instance Validity Frequency

instance NFData Frequency

instance IsRecurrenceRulePart Frequency where
  recurrenceRulePartName Proxy = "FREQ"
  recurrenceRulePartP = frequencyP
  recurrenceRulePartB = frequencyB

frequencyP :: Text -> Conform PropertyTypeParseError Void Void Frequency
frequencyP = \case
  "SECONDLY" -> pure Secondly
  "MINUTELY" -> pure Minutely
  "HOURLY" -> pure Hourly
  "DAILY" -> pure Daily
  "WEEKLY" -> pure Weekly
  "MONTHLY" -> pure Monthly
  "YEARLY" -> pure Yearly
  t -> unfixableError $ UnknownFrequency t

frequencyB :: Frequency -> Text
frequencyB = \case
  Secondly -> "SECONDLY"
  Minutely -> "MINUTELY"
  Hourly -> "HOURLY"
  Daily -> "DAILY"
  Weekly -> "WEEKLY"
  Monthly -> "MONTHLY"
  Yearly -> "YEARLY"

-- | Interval
--
-- The default value is "1", meaning every second for a SECONDLY rule,
-- every minute for a MINUTELY rule, every hour for an HOURLY rule,
-- every day for a DAILY rule, every week for a WEEKLY rule, every
-- month for a MONTHLY rule, and every year for a YEARLY rule.  For
-- example, within a DAILY rule, a value of "8" means every eight days.
--
-- Note: We did not chose 'Maybe Word' because that would have two ways to represent the default value.
newtype Interval = Interval {unInterval :: Word}
  deriving stock (Show, Eq, Ord, Generic)

instance Validity Interval where
  validate i@(Interval w) = mconcat [genericValidate i, declare "The interval is not zero" $ w /= 0]

instance NFData Interval

instance IsRecurrenceRulePart Interval where
  recurrenceRulePartName Proxy = "INTERVAL"
  recurrenceRulePartP = intervalP
  recurrenceRulePartB = intervalB

intervalP :: Text -> Conform PropertyTypeParseError Void Void Interval
intervalP t =
  case readMaybe (T.unpack t) of
    Nothing -> unfixableError $ UnReadableInterval t
    Just w -> pure $ Interval w

intervalB :: Interval -> Text
intervalB = T.pack . show . unInterval

-- | The UNTIL rule part defines a DATE or DATE-TIME value that bounds the recurrence rule in an inclusive manner.
--
-- @
-- If the value specified by UNTIL is synchronized with the specified
-- recurrence, this DATE or DATE-TIME becomes the last instance of the
-- recurrence.  The value of the UNTIL rule part MUST have the same value
-- type as the "DTSTART" property.  Furthermore, if the "DTSTART" property
-- is specified as a date with local time, then the UNTIL rule part MUST
-- also be specified as a date with local time.  If the "DTSTART" property
-- is specified as a date with UTC time or a date with local time and time
-- zone reference, then the UNTIL rule part MUST be specified as a date
-- with UTC time.  In the case of the "STANDARD" and "DAYLIGHT"
-- sub-components the UNTIL rule part MUST always be specified as a date
-- with UTC time.  If specified as a DATE-TIME value, then it MUST be
-- specified in a UTC time format.
-- @
--
-- However, we find the following in [Erratum 4414 (verified)]https://www.rfc-editor.org/errata/eid4414):
-- @
-- Section 3.3.10 says:
--       The UNTIL rule part defines a DATE or DATE-TIME value that bounds
--       the recurrence rule in an inclusive manner.  If the value
--       specified by UNTIL is synchronized with the specified recurrence,
--       this DATE or DATE-TIME becomes the last instance of the
--       recurrence.  The value of the UNTIL rule part MUST have the same
--       value type as the "DTSTART" property.  Furthermore, if the
--       "DTSTART" property is specified as a date with local time, then
--       the UNTIL rule part MUST also be specified as a date with local
--       time.  If the "DTSTART" property is specified as a date with UTC
--       time or a date with local time and time zone reference, then the
--       UNTIL rule part MUST be specified as a date with UTC time.  In the
--       case of the "STANDARD" and "DAYLIGHT" sub-components the UNTIL
--       rule part MUST always be specified as a date with UTC time.  If
--       specified as a DATE-TIME value, then it MUST be specified in a UTC
--       time format.  If not present, and the COUNT rule part is also not
--       present, the "RRULE" is considered to repeat forever.
--
-- It should say:
--
--       The UNTIL rule part defines a DATE or DATE-TIME value that bounds
--       the recurrence rule in an inclusive manner.  If the value
--       specified by UNTIL is synchronized with the specified recurrence,
--       this DATE or DATE-TIME becomes the last instance of the
--       recurrence.  The value of the UNTIL rule part MUST have the same
--       value type as the "DTSTART" property.  Furthermore, if the
--       "DTSTART" property is specified as a date with local time, then
--       the UNTIL rule part MUST also be specified as a date with local
--       time.  If the "DTSTART" property is specified as a date with UTC
--       time or a date with local time and time zone reference, then the
--       UNTIL rule part MUST be specified as a date with UTC time.  In the
--       case of the "STANDARD" and "DAYLIGHT" sub-components the UNTIL
--       rule part MUST always be specified as a date with UTC time.
--       If not present, and the COUNT rule part is also not
--       present, the "RRULE" is considered to repeat forever.
--
-- Notes:
--
-- The following sentence from RFC 2445 should have been removed from the text.
--
-- If
-- specified as a DATE-TIME value, then it MUST be specified in a UTC
-- time format.
-- @
data Until
  = UntilDate !Date
  | UntilDateTimeFloating !Time.LocalTime
  | UntilDateTimeUTC !Time.UTCTime
  deriving (Show, Eq, Ord, Generic)

instance Validity Until where
  validate u =
    mconcat
      [ genericValidate u,
        case u of
          UntilDate _ -> valid
          UntilDateTimeFloating lt -> validateImpreciseLocalTime lt
          UntilDateTimeUTC ut -> validateImpreciseUTCTime ut
      ]

instance NFData Until

instance IsRecurrenceRulePart Until where
  recurrenceRulePartName Proxy = "UNTIL"
  recurrenceRulePartP = untilP
  recurrenceRulePartB = untilB

untilP :: Text -> Conform PropertyTypeParseError Void Void Until
untilP t =
  (UntilDate <$> parseDate t)
    `altConform` (UntilDateTimeUTC <$> parseDateTimeUTC t)
    `altConform` (UntilDateTimeFloating <$> parseDateTimeFloating t)

untilB :: Until -> Text
untilB = \case
  UntilDate d -> renderDate d
  UntilDateTimeFloating lt -> renderDateTimeFloating lt
  UntilDateTimeUTC ut -> renderDateTimeUTC ut

newtype Count = Count {unCount :: Word}
  deriving (Show, Eq, Ord, Generic)

instance Validity Count where
  validate s@(Count w) =
    mconcat
      [ genericValidate s,
        declare "Valid values are 0 to 60." $
          w >= 0 && w <= 60
      ]

instance NFData Count

instance IsRecurrenceRulePart Count where
  recurrenceRulePartName Proxy = "COUNT"
  recurrenceRulePartP = countP
  recurrenceRulePartB = countB

countP :: Text -> Conform PropertyTypeParseError Void Void Count
countP t =
  case readMaybe (T.unpack t) of
    Nothing -> unfixableError $ UnReadableCount t
    Just w -> pure $ Count w

countB :: Count -> Text
countB = T.pack . show . unCount

-- | A second within a minute
--
-- Valid values are 0 to 60.
newtype BySecond = BySecond {unBySecond :: Word}
  deriving (Show, Eq, Ord, Generic)

instance Validity BySecond where
  validate s@(BySecond w) =
    mconcat
      [ genericValidate s,
        declare "Valid values are 0 to 60." $
          w >= 0 && w <= 60
      ]

instance NFData BySecond

instance IsRecurrenceRulePart (Set BySecond) where
  recurrenceRulePartName Proxy = "BYSECOND"
  recurrenceRulePartP = setP bySecondP
  recurrenceRulePartB = setB bySecondB

bySecondP :: Text -> Conform PropertyTypeParseError Void Void BySecond
bySecondP t =
  case readMaybe (T.unpack t) of
    Nothing -> unfixableError $ UnReadableBySecond t
    Just w -> pure $ BySecond w

bySecondB :: BySecond -> Text
bySecondB = T.pack . show . unBySecond

-- | A minute within an hour
--
-- Valid values are 0 to 59.
newtype ByMinute = ByMinute {unByMinute :: Word}
  deriving (Show, Eq, Ord, Generic)

instance Validity ByMinute where
  validate s@(ByMinute w) =
    mconcat
      [ genericValidate s,
        declare "Valid values are 0 to 59." $
          w >= 0 && w <= 59
      ]

instance NFData ByMinute

instance IsRecurrenceRulePart (Set ByMinute) where
  recurrenceRulePartName Proxy = "BYMINUTE"
  recurrenceRulePartP = setP byMinuteP
  recurrenceRulePartB = setB byMinuteB

byMinuteP :: Text -> Conform PropertyTypeParseError Void Void ByMinute
byMinuteP t =
  case readMaybe (T.unpack t) of
    Nothing -> unfixableError $ UnReadableByMinute t
    Just w -> pure $ ByMinute w

byMinuteB :: ByMinute -> Text
byMinuteB = T.pack . show . unByMinute

-- | An hour within a day
--
-- Valid values are 0 to 23.
newtype ByHour = ByHour {unByHour :: Word}
  deriving (Show, Eq, Ord, Generic)

instance Validity ByHour where
  validate s@(ByHour w) =
    mconcat
      [ genericValidate s,
        declare "Valid values are 0 to 23." $
          w >= 0 && w <= 23
      ]

instance NFData ByHour

instance IsRecurrenceRulePart (Set ByHour) where
  recurrenceRulePartName Proxy = "BYHOUR"
  recurrenceRulePartP = setP byHourP
  recurrenceRulePartB = setB byHourB

byHourP :: Text -> Conform PropertyTypeParseError Void Void ByHour
byHourP t =
  case readMaybe (T.unpack t) of
    Nothing -> unfixableError $ UnReadableByHour t
    Just w -> pure $ ByHour w

byHourB :: ByHour -> Text
byHourB = T.pack . show . unByHour

-- | The BYDAY rule part specifies a COMMA-separated list of days of the week;
--
-- Each BYDAY value can also be preceded by a positive (+n) or
-- negative (-n) integer.  If present, this indicates the nth
-- occurrence of a specific day within the MONTHLY or YEARLY "RRULE".
--
-- For example, within a MONTHLY rule, +1MO (or simply 1MO)
-- represents the first Monday within the month, whereas -1MO
-- represents the last Monday of the month.  The numeric value in a
-- BYDAY rule part with the FREQ rule part set to YEARLY corresponds
-- to an offset within the month when the BYMONTH rule part is
-- present, and corresponds to an offset within the year when the
-- BYWEEKNO or BYMONTH rule parts are present.  If an integer
-- modifier is not present, it means all days of this type within the
-- specified frequency.  For example, within a MONTHLY rule, MO
-- represents all Mondays within the month.
data ByDay
  = Every DayOfWeek
  | Specific Int DayOfWeek
  deriving stock (Show, Eq, Ord, Generic)

instance Validity ByDay where
  validate bd =
    mconcat
      [ genericValidate bd,
        case bd of
          Every _ -> valid
          Specific i _ ->
            mconcat
              [ declare "The specific weekday number is not zero" $ i /= 0,
                declare "The specific weekday number is between -5 and 5" $ i >= -5 && i <= 5
              ]
      ]

instance NFData ByDay

instance IsRecurrenceRulePart (Set ByDay) where
  recurrenceRulePartName Proxy = "BYDAY"
  recurrenceRulePartP = setP byDayP
  recurrenceRulePartB = setB byDayB

byDayP :: Text -> Conform PropertyTypeParseError Void Void ByDay
byDayP t =
  let ci = CI.mk t
   in everyP ci `altConform` specificP ci

everyP :: CI Text -> Conform PropertyTypeParseError Void Void ByDay
everyP = fmap Every . parseDayOfWeek

specificP :: CI Text -> Conform PropertyTypeParseError Void Void ByDay
specificP ci =
  let t = CI.original ci
   in case T.unpack t of
        '-' : d : rest -> case readMaybe [d] of
          Nothing -> unfixableError $ UnReadableByDay t
          Just i -> do
            dow <- parseDayOfWeek (CI.mk (T.pack rest))
            pure $ Specific (negate i) dow
        d : rest -> case readMaybe [d] of
          Nothing -> undefined
          Just i -> do
            dow <- parseDayOfWeek (CI.mk (T.pack rest))
            pure $ Specific i dow
        _ -> unfixableError $ UnReadableByDay t

parseDayOfWeek :: CI Text -> Conform PropertyTypeParseError Void Void DayOfWeek
parseDayOfWeek = \case
  "MO" -> pure Monday
  "TU" -> pure Tuesday
  "WE" -> pure Wednesday
  "TH" -> pure Thursday
  "FR" -> pure Friday
  "SA" -> pure Saturday
  "SU" -> pure Sunday
  t -> unfixableError $ UnReadableDayOfWeek t

renderDayOfWeek :: DayOfWeek -> CI Text
renderDayOfWeek = \case
  Monday -> "MO"
  Tuesday -> "TU"
  Wednesday -> "WE"
  Thursday -> "TH"
  Friday -> "FR"
  Saturday -> "SA"
  Sunday -> "SU"

byDayB :: ByDay -> Text
byDayB =
  CI.original . \case
    Every dow -> renderDayOfWeek dow
    Specific i dow -> CI.mk (T.pack (show i)) <> renderDayOfWeek dow

-- | A day within a month
--
-- Valid values are 1 to 31 or -31 to -1.  For example, -10 represents the
-- tenth to the last day of the month.
newtype ByMonthDay = ByMonthDay {unByMonthDay :: Int}
  deriving (Show, Eq, Ord, Generic)

instance Validity ByMonthDay where
  validate md@(ByMonthDay i) =
    mconcat
      [ genericValidate md,
        declare "Valid values are 1 to 31 or -31 to -1." $
          i /= 0 && i >= -31 && i <= 31
      ]

instance NFData ByMonthDay

instance IsRecurrenceRulePart (Set ByMonthDay) where
  recurrenceRulePartName Proxy = "BYMONTHDAY"
  recurrenceRulePartP = setP byMonthDayP
  recurrenceRulePartB = setB byMonthDayB

byMonthDayP :: Text -> Conform PropertyTypeParseError Void Void ByMonthDay
byMonthDayP t =
  case readMaybe (T.unpack t) of
    Nothing -> unfixableError $ UnReadableByMonthDay t
    Just w -> pure $ ByMonthDay w

byMonthDayB :: ByMonthDay -> Text
byMonthDayB = T.pack . show . unByMonthDay

-- | A day within a year
--
-- Valid values are 1 to 366 or -366 to -1.  For example, -1 represents the
-- last day of the year (December 31st) and -306 represents the 306th to the
-- last day of the year (March 1st).
newtype ByYearDay = ByYearDay {unByYearDay :: Int}
  deriving (Show, Eq, Ord, Generic)

instance Validity ByYearDay where
  validate md@(ByYearDay i) =
    mconcat
      [ genericValidate md,
        declare "Valid values are 1 to 366 or -366 to -1." $
          i /= 0 && i >= -366 && i <= 366
      ]

instance NFData ByYearDay

instance IsRecurrenceRulePart (Set ByYearDay) where
  recurrenceRulePartName Proxy = "BYYEARDAY"
  recurrenceRulePartP = setP byYearDayP
  recurrenceRulePartB = setB byYearDayB

byYearDayP :: Text -> Conform PropertyTypeParseError Void Void ByYearDay
byYearDayP t =
  case readMaybe (T.unpack t) of
    Nothing -> unfixableError $ UnReadableByYearDay t
    Just w -> pure $ ByYearDay w

byYearDayB :: ByYearDay -> Text
byYearDayB = T.pack . show . unByYearDay

-- | A week within a year
--
-- Valid values are 1 to 53 or -53 to -1.  This corresponds to weeks according
-- to week numbering as defined in
-- [ISO.8601.2004](https://tools.ietf.org/html/rfc5545#ref-ISO.8601.2004).  A
-- week is defined as a seven day period, starting on the day of the week
-- defined to be the week start (see WKST).  Week number one of the calendar
-- year is the first week that contains at least four (4) days in that calendar
-- year.
--
-- For example, 3 represents the third week of the year.
--
-- Note: Assuming a Monday week start, week 53 can only occur when Thursday is
-- January 1 or if it is a leap year and Wednesday is January 1.
newtype ByWeekNo = ByWeekNo {unByWeekNo :: Int}
  deriving (Show, Eq, Ord, Generic)

instance Validity ByWeekNo where
  validate bwn@(ByWeekNo i) =
    mconcat
      [ genericValidate bwn,
        declare "Valid values are 1 to 53 or -53 to -1." $
          i /= 0 && i >= -53 && i <= 53
      ]

instance NFData ByWeekNo

instance IsRecurrenceRulePart (Set ByWeekNo) where
  recurrenceRulePartName Proxy = "BYWEEKNO"
  recurrenceRulePartP = setP byWeekNoP
  recurrenceRulePartB = setB byWeekNoB

byWeekNoP :: Text -> Conform PropertyTypeParseError Void Void ByWeekNo
byWeekNoP t =
  case readMaybe (T.unpack t) of
    Nothing -> unfixableError $ UnReadableByWeekNo t
    Just w -> pure $ ByWeekNo w

byWeekNoB :: ByWeekNo -> Text
byWeekNoB = T.pack . show . unByWeekNo

-- | A month within a year
--
-- Valid values are 1 to 12.
--
-- In Haskell we represent these using a 'Month' value.
newtype ByMonth = ByMonth {unByMonth :: Month}
  deriving (Show, Eq, Ord, Generic)

instance Validity ByMonth

instance NFData ByMonth

instance IsRecurrenceRulePart (Set ByMonth) where
  recurrenceRulePartName Proxy = "BYMONTH"
  recurrenceRulePartP = setP byMonthP
  recurrenceRulePartB = setB byMonthB

byMonthP :: Text -> Conform PropertyTypeParseError Void Void ByMonth
byMonthP t = case readMaybe (T.unpack t) >>= monthNoToMonth of
  Nothing -> unfixableError $ UnReadableByMonth t
  Just w -> pure $ ByMonth w

byMonthB :: ByMonth -> Text
byMonthB = T.pack . show . monthToMonthNo . unByMonth

-- | A position within the recurrence set
--
-- BYSETPOS operates on
-- a set of recurrence instances in one interval of the recurrence
-- rule.  For example, in a WEEKLY rule, the interval would be one
-- week A set of recurrence instances starts at the beginning of the
-- interval defined by the FREQ rule part.  Valid values are 1 to 366
-- or -366 to -1.
--
-- For example "the last work day of the month"
-- could be represented as:
--
--  FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1
--
-- Each BYSETPOS value can include a positive (+n) or negative (-n)
-- integer.  If present, this indicates the nth occurrence of the
-- specific occurrence within the set of occurrences specified by the
-- rule.
newtype BySetPos = BySetPos {unBySetPos :: Int}
  deriving (Show, Eq, Ord, Generic)

instance Validity BySetPos where
  validate sp@(BySetPos w) =
    mconcat
      [ genericValidate sp,
        declare "The set position is not zero" $
          w /= 0
      ]

instance NFData BySetPos

instance IsRecurrenceRulePart (Set BySetPos) where
  recurrenceRulePartName Proxy = "BYSETPOS"
  recurrenceRulePartP = setP bySetPosP
  recurrenceRulePartB = setB bySetPosB

bySetPosP :: Text -> Conform PropertyTypeParseError Void Void BySetPos
bySetPosP t =
  case readMaybe (T.unpack t) of
    Nothing -> unfixableError $ UnReadableBySetPos t
    Just w -> pure $ BySetPos w

bySetPosB :: BySetPos -> Text
bySetPosB = T.pack . show . unBySetPos

-- | Week Start
-- @
-- The WKST rule part specifies the day on which the workweek starts.
-- Valid values are MO, TU, WE, TH, FR, SA, and SU.  This is
-- significant when a WEEKLY "RRULE" has an interval greater than 1,
-- and a BYDAY rule part is specified.  This is also significant when
-- in a YEARLY "RRULE" when a BYWEEKNO rule part is specified.  The
-- default value is MO.
-- @
newtype WeekStart = WeekStart {unWeekStart :: DayOfWeek}
  deriving (Show, Eq, Ord, Generic)

instance Validity WeekStart

instance NFData WeekStart

instance IsRecurrenceRulePart WeekStart where
  recurrenceRulePartName Proxy = "WKST"
  recurrenceRulePartP = weekStartP
  recurrenceRulePartB = weekStartB

weekStartP :: Text -> Conform PropertyTypeParseError Void Void WeekStart
weekStartP = fmap WeekStart <$> parseDayOfWeek . CI.mk

weekStartB :: WeekStart -> Text
weekStartB = CI.original . renderDayOfWeek . unWeekStart

-- A month within a year
--
-- Until 'time' has this too'
data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Validity Month where
  validate = trivialValidation

instance NFData Month

monthToMonthNo :: Month -> Int
monthToMonthNo = \case
  January -> 1
  February -> 2
  March -> 3
  April -> 4
  May -> 5
  June -> 6
  July -> 7
  August -> 8
  September -> 9
  October -> 10
  November -> 11
  December -> 12

monthNoToMonth :: Int -> Maybe Month
monthNoToMonth = \case
  1 -> Just January
  2 -> Just February
  3 -> Just March
  4 -> Just April
  5 -> Just May
  6 -> Just June
  7 -> Just July
  8 -> Just August
  9 -> Just September
  10 -> Just October
  11 -> Just November
  12 -> Just December
  _ -> Nothing

makeRecurrenceRule :: Frequency -> RecurrenceRule
makeRecurrenceRule freq =
  RecurrenceRule
    { recurrenceRuleFrequency = freq,
      recurrenceRuleInterval = Interval 1,
      recurrenceRuleUntilCount = Nothing,
      recurrenceRuleBySecond = S.empty,
      recurrenceRuleByMinute = S.empty,
      recurrenceRuleByHour = S.empty,
      recurrenceRuleByDay = S.empty,
      recurrenceRuleByMonthDay = S.empty,
      recurrenceRuleByYearDay = S.empty,
      recurrenceRuleByWeekNo = S.empty,
      recurrenceRuleByMonth = S.empty,
      recurrenceRuleWeekStart = WeekStart Monday,
      recurrenceRuleBySetPos = S.empty
    }
