module ICal.PropertyType.Duration where

import Data.Time
import ICal.PropertyType.Class

-- | Duration
--
-- === [section 3.3.6](https://datatracker.ietf.org/doc/html/rfc5545#section-3.3.6)
--
-- @
--     Value Name:  DURATION
--
--     Purpose:  This value type is used to identify properties that contain
--        a duration of time.
--
--     Format Definition:  This value type is defined by the following
--        notation:
--
--         dur-value  = (["+"] / "-") "P" (dur-date / dur-time / dur-week)
--
--         dur-date   = dur-day [dur-time]
--         dur-time   = "T" (dur-hour / dur-minute / dur-second)
--         dur-week   = 1*DIGIT "W"
--         dur-hour   = 1*DIGIT "H" [dur-minute]
--         dur-minute = 1*DIGIT "M" [dur-second]
--         dur-second = 1*DIGIT "S"
--         dur-day    = 1*DIGIT "D"
--
--     Description:  If the property permits, multiple "duration" values are
--        specified by a COMMA-separated list of values.  The format is
--        based on the [ISO.8601.2004] complete representation basic format
--        with designators for the duration of time.  The format can
--        represent nominal durations (weeks and days) and accurate
--        durations (hours, minutes, and seconds).  Note that unlike
--        [ISO.8601.2004], this value type doesn't support the "Y" and "M"
--        designators to specify durations in terms of years and months.
--
--        The duration of a week or a day depends on its position in the
--        calendar.  In the case of discontinuities in the time scale, such
--        as the change from standard time to daylight time and back, the
--        computation of the exact duration requires the subtraction or
--        addition of the change of duration of the discontinuity.  Leap
--        seconds MUST NOT be considered when computing an exact duration.
--        When computing an exact duration, the greatest order time
--        components MUST be added first, that is, the number of days MUST
--        be added first, followed by the number of hours, number of
--        minutes, and number of seconds.
--
--        Negative durations are typically used to schedule an alarm to
--        trigger before an associated time (see Section 3.8.6.3).
--
--        No additional content value encoding (i.e., BACKSLASH character
--        encoding, see Section 3.3.11) are defined for this value type.
--
--     Example:  A duration of 15 days, 5 hours, and 20 seconds would be:
--
--         P15DT5H0M20S
--
--        A duration of 7 weeks would be:
--
--         P7W
-- @
newtype Duration = Duration {unDuration :: NominalDiffTime}

instance IsPropertyType Duration
