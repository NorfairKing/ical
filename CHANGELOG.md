# Changelog

## ical-recurrence 0.3.0.0

* `RECURRENCE-ID` is handled: a component carrying one replaces the instance it
  names instead of appearing beside it. Google and Outlook write one for every
  "this event only" edit, so such a calendar previously produced that instance
  twice. `RANGE=THISANDFUTURE` reschedules every later instance by the same
  difference and applies the override's duration, leaving alone any instance
  that has an override of its own.

* Recurrence is computed over collections of components, because the
  occurrences of one depend on the others sharing its UID. Start from
  `recurCalendar`, with `runCalendarR` for the time zones. `recur` and
  `recurEvents` keep their names with incompatible types, so old call sites are
  compile errors rather than silent changes of meaning; the single-component
  expansion `recurEvents` used to do is now `expandRecurring`. `HasRecurrence`,
  `getRecurringEvent`, `makeOccurrence`, `RecurringEvent`, `EventOccurrence`,
  `ResolvedEvent` and `resolveEventOccurrence` give way to `Recurring`,
  `Occurrence`, `Resolved` and `resolveOccurrence`, parameterised over the
  component.

* `VTODO` and `VJOURNAL` recur, not only `VEVENT`. Only the end of an instance
  differs (`DTEND`, `DUE`, none), so `RecurrenceEnd` is that shared spelling.

## ical 0.3.0.0

* `COUNT` is no longer capped at 60. Its `Validity` instance carried the
  sentence that belongs to `BYSECOND` ("Valid values are 0 to 60"), three
  declarations further down the same RFC 5545 paragraph, and §3.3.10 bounds
  `COUNT` nowhere. `COUNT=100`, a weekly meeting for two years, was an invalid
  value, and the parser returned one for that ordinary input. `GenValid Count`
  was capped by the same bound, so no test had ever exercised a count above 60.

* `BYSETPOS` is now bounded. §3.3.10 gives "Valid values are 1 to 366 or -366 to
  -1"; the instance declared only that the position was not zero, so
  `BYSETPOS=400` passed as valid.

* A `BYDAY` ordinal may have two digits. `weekdaynum` is
  `[[plus / minus] ordwk] weekday` with `ordwk = 1*2DIGIT ;1 to 53`, but the
  parser read exactly one digit, so `BYDAY=10MO` read the `1` and then failed to
  make a day of week out of `"0MO"`. Every ordinal from 10 to 53 was
  unparseable, in both signs, as was a leading `+`. These are not exotic: for a
  `YEARLY` rule without `BYMONTH` the ordinal is an offset within the year, so
  `FREQ=YEARLY;BYDAY=20MO` means the twentieth Monday of the year.

* A `BYDAY` ordinal is bounded at ±53 rather than ±5. The tighter bound suits a
  `MONTHLY` rule, where a weekday recurs at most five times, but the type does
  not know the frequency and a restriction that depends on it belongs in
  `Validity RecurrenceRule`. Between this and the parser above, `BYDAY=6MO`
  through `9MO` parsed into values that failed their own `Validity`.

* A recurrence rule that specifies both `UNTIL` and `COUNT` is reported.
  §3.3.10 says they "MUST NOT occur in the same 'recur'", but such a rule
  parsed silently, so a conforming run could not tell the input was not
  conforming. It is now a fixable error: strict parsing refuses the rule and
  lenient parsing keeps the `COUNT`. Which of the two survives is arbitrary
  because the spec does not say; it was already the `COUNT` and still is.

* An out-of-range rule part value is refused instead of parsed. `INTERVAL=0`,
  `BYSECOND=61`, `BYMINUTE=60`, `BYHOUR=24`, `BYMONTHDAY=32`, `BYYEARDAY=367`,
  `BYWEEKNO=54`, `BYSETPOS=367`, `BYDAY=54MO` and the zero and negative
  counterparts all produced a `RecurrenceRule` that failed its own `Validity`,
  which breaks the invariant the rest of the library relies on. The range is not
  restated in the parser: each type already declares it in its `Validity`
  instance, and the parser asks that, so the two cannot drift apart. Refused
  rather than repaired, because dropping the value would empty the rule part,
  and for a part that limits rather than expands an empty part means no
  restriction at all — a forbidden value would widen the recurrence set.

* Note for anyone matching on them exhaustively:
  `PropertyTypeParseError` gains `RecurrenceRulePartOutOfRange` and
  `PropertyTypeFixableError` gains `RecurrenceRuleHasBothUntilAndCount`.

## ical-recurrence 0.2.0.0

* `FREQ=DAILY;BYDAY=2MO` matches Mondays rather than every day. A numeric
  `BYDAY` is forbidden at a frequency other than `MONTHLY` or `YEARLY` (RFC 5545
  §3.3.10), and the whole rule part used to be dropped, which emptied the day
  filter. An empty day filter is no restriction, so forbidden input widened the
  recurrence set instead of narrowing it. The number is now discarded and the
  week day kept, which is what the unnumbered form already means and what
  `byEveryWeekDayWeek` already did. `RecurrenceByDayNumeric` is still emitted,
  so only a lenient run gets the repair.

* An instance holding a leap second is ignored whatever `DTSTART`'s value type
  is. `BYSECOND=60` is legal and names a leap second, and the expansion put it
  into a wall-clock time unchanged. `localTimeExists` then judged only a zoned
  `DTSTART`, because the resolve-and-unresolve round trip it uses needs a time
  zone, so the same rule produced three instances from a floating `DTSTART` and
  two from a zoned one — one of those on a different day. Whether a leap second
  survives that round trip depends on the time of day and the offset, so the
  recurrence set came to depend on facts about neither the rule nor the
  calendar. The consequence is that `BYSECOND=60` now generates no instance at
  all, which is the honest outcome: real leap seconds are announced for
  particular dates and nothing here knows which.

## ical 0.2.0.0

* Parse and retain the non-standard `X-WR-TIMEZONE` calendar property (as
  `calendarTimeZoneIdentifier`). It is emitted by most real-world calendar
  producers (Google Calendar, Apple, ...) to declare the calendar's default
  display time zone, and was previously dropped. Note: this adds a field to the
  `Calendar` record.

## ical-recurrence 0.1.0.0

* Add `unresolveTimestampR`, to render a resolved `Timestamp` as a wall-clock
  time in a given display timezone, DST-correctly (via the `VTIMEZONE` rules).
  Displaying a UTC or zoned event time requires converting the resolved instant
  back into a display timezone; doing that by hand (keeping the UTC wall-clock)
  is wrong by the zone's offset.

## ical 0.1.0.0

* Normalize LF-only line endings to CRLF as a fixable error.
  Many iCal producers serve files with LF line endings instead of CRLF
  as required by RFC 5545. In lenient mode, these are now accepted.
