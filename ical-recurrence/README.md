# ICal Recurrence


If you look at this package's API and are confused about why this is all so
complicated, make sure you read this document (and the spec) first.

In order to turn an ICalendar into a set of events with a local start and end time, the following things need to happen correctly:

1. Parse ICalendar (using the `ical` package or so)
1. Pick out:
   1. Events
   1. Timezones
1. Recur the start times

   Events may (but don't have to ?!) have a start date(time) defined.
   Events may (but don't have to) have either and end date(time) or a duration specified.

   Date(time)'s may be specified in four ways:
   - Date without time
   - Date and time without a timezone (floating)
   - Date and time in UTC
   - Date and time with a timezone identifier

   Events can recur using the following recurrence information
   - Recurrence date(time)s
   - Recurrence Rules
   - Exception date(time)s

   Once the start times of events have been recurred, we also need to resolve the new end or duration of each event.
   This is nontrivial because of the way timezone changes work.
   - If the end date(time) is specified, the exact duration between the end and start time has to be computed.
     This is non-trivial because a fixed duration after a given time may look different in that timezone if the timezone offset changes inbetween the begin and end date(time).
   - If the duration is specified, that duration is applied exactly.
     This has the same issues as above.
1. Resolve the timezone identifier information
   Next up, we need to compute when events occur either locally, or in UTC, so all other timezone information is resolved.
   (We must not do this before recurrence, because recurrence might cross timezone offset change boundaries.)
   In order to resolve timezone information, we must be able to compute when timezone offsets change in the given timezone.
   This requires recurrence (the above step) and caching, because there are a lot of those offset changes over time.
   We also cannot resolve all date(times) to UTC-time because some times are specified in local time only, without any timezone information.
   In that case the timestamps are specified local-to-the-event. (And it's a good thing there are no timezone offset changes over 24h (?).)
1. Resolve the UTC times to local times.
   This requires IO because there is no way to know what the actual current timezone is, from the ICal file.
   Indeed, the OS knows this, but we can only ask for the timezone offset at a current time, not an entire ruleset (?).
   So for each utc time, we need to ask the OS the timezone offset at that time, to resolve it to a local time in our current timezone.
   

