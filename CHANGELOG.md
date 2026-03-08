# Changelog

## 0.1.0.0

* Normalize LF-only line endings to CRLF as a fixable error.
  Many iCal producers serve files with LF line endings instead of CRLF
  as required by RFC 5545. In lenient mode, these are now accepted.
