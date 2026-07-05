# Changelog

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
