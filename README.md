# ICal

An implementation of:

* [RCF5545: Internet Calendaring and Scheduling Core Object Specification (iCalendar)](https://tools.ietf.org/html/rfc5545)
* [RFC6868: Parameter Value Encoding in iCalendar and vCard](https://datatracker.ietf.org/doc/html/rfc6868)
* [RFC7986: New Properties for iCalendar](https://datatracker.ietf.org/doc/html/rfc7986#section-5.10).

## Status

This library now covers all of `RFC 5545` and some of `RFC 7986`.

It's already being used in production at https://smos.online and https://social-dance.today.

## Goals

* Strict RFC Compliance
* Being able to deal with some of other implementors' failures to comply with the spec

## Implementation checklist

### RFC 5545

- [x] 1.  Introduction
- [x] 2.  Basic Grammar and Conventions
  - [x] 2.1.  Formatting Conventions
  - [x] 2.2.  Related Memos
- [x] 3.  iCalendar Object Specification
  - [x] 3.1.  Content Lines
    - [x] 3.1.1.  List and Field Separators
    - [x] 3.1.2.  Multiple Values
    - [x] 3.1.3.  Binary Content
    - [x] 3.1.4.  Character Set
  - [x] 3.2.  Property Parameters
    - [x] 3.2.1.  Alternate Text Representation
    - [x] 3.2.2.  Common Name
    - [x] 3.2.3.  Calendar User Type
    - [x] 3.2.4.  Delegators
    - [x] 3.2.5.  Delegatees
    - [x] 3.2.6.  Directory Entry Reference
    - [x] 3.2.7.  Inline Encoding
    - [x] 3.2.8.  Format Type
    - [x] 3.2.9.  Free/Busy Time Type
    - [x] 3.2.10. Language
    - [x] 3.2.11. Group or List Membership
    - [x] 3.2.12. Participation Status
    - [x] 3.2.13. Recurrence Identifier Range
    - [x] 3.2.14. Alarm Trigger Relationship
    - [x] 3.2.15. Relationship Type
    - [x] 3.2.16. Participation Role
    - [x] 3.2.17. RSVP Expectation
    - [x] 3.2.18. Sent By
    - [x] 3.2.19. Time Zone Identifier
    - [x] 3.2.20. Value Data Types
  - [x] 3.3.  Property Value Data Types
    - [x] 3.3.1.  Binary
    - [x] 3.3.2.  Boolean
    - [x] 3.3.3.  Calendar User Address
    - [x] 3.3.4.  Date
    - [x] 3.3.5.  Date-Time
    - [x] 3.3.6.  Duration
    - [x] 3.3.7.  Float
    - [x] 3.3.8.  Integer
    - [x] 3.3.9.  Period of Time
    - [x] 3.3.10. Recurrence Rule
    - [x] 3.3.11. Text
    - [x] 3.3.12. Time
    - [x] 3.3.13. URI
    - [x] 3.3.14. UTC Offset
  - [x] 3.4.  iCalendar Object
  - [x] 3.5.  Property
  - [x] 3.6.  Calendar Components
    - [x] 3.6.1.  Event Component
    - [x] 3.6.2.  To-Do Component
    - [x] 3.6.3.  Journal Component
    - [x] 3.6.4.  Free/Busy Component
    - [x] 3.6.5.  Time Zone Component
    - [x] 3.6.6.  Alarm Component
  - [x] 3.7.  Calendar Properties
    - [x] 3.7.1.  Calendar Scale
    - [x] 3.7.2.  Method
    - [x] 3.7.3.  Product Identifier
    - [x] 3.7.4.  Version
  - [x] 3.8.  Component Properties
    - [x] 3.8.1.  Descriptive Component Properties
      - [x] 3.8.1.1.  Attachment
      - [x] 3.8.1.2.  Categories
      - [x] 3.8.1.3.  Classification
      - [x] 3.8.1.4.  Comment
      - [x] 3.8.1.5.  Description
      - [x] 3.8.1.6.  Geographic Position
      - [x] 3.8.1.7.  Location
      - [x] 3.8.1.8.  Percent Complete
      - [x] 3.8.1.9.  Priority
      - [x] 3.8.1.10. Resources
      - [x] 3.8.1.11. Status
      - [x] 3.8.1.12. Summary
    - [x] 3.8.2.  Date and Time Component Properties
      - [x] 3.8.2.1.  Date-Time Completed
      - [x] 3.8.2.2.  Date-Time End
      - [x] 3.8.2.3.  Date-Time Due
      - [x] 3.8.2.4.  Date-Time Start
      - [x] 3.8.2.5.  Duration
      - [x] 3.8.2.6.  Free/Busy Time
      - [x] 3.8.2.7.  Time Transparency
    - [x] 3.8.3.  Time Zone Component Properties
      - [x] 3.8.3.1.  Time Zone Identifier
      - [x] 3.8.3.2.  Time Zone Name
      - [x] 3.8.3.3.  Time Zone Offset From
      - [x] 3.8.3.4.  Time Zone Offset To
      - [x] 3.8.3.5.  Time Zone URL
    - [x] 3.8.4.  Relationship Component Properties
      - [x] 3.8.4.1.  Attendee
      - [x] 3.8.4.2.  Contact
      - [x] 3.8.4.3.  Organizer
      - [x] 3.8.4.4.  Recurrence ID
      - [x] 3.8.4.5.  Related To
      - [x] 3.8.4.6.  Uniform Resource Locator
      - [x] 3.8.4.7.  Unique Identifier
    - [x] 3.8.5.  Recurrence Component Properties
      - [x] 3.8.5.1.  Exception Date-Times
      - [x] 3.8.5.2.  Recurrence Date-Times
      - [x] 3.8.5.3.  Recurrence Rule
    - [x] 3.8.6.  Alarm Component Properties
      - [x] 3.8.6.1.  Action
      - [x] 3.8.6.2.  Repeat Count
      - [x] 3.8.6.3.  Trigger
    - [x] 3.8.7.  Change Management Component Properties
      - [x] 3.8.7.1.  Date-Time Created
      - [x] 3.8.7.2.  Date-Time Stamp
      - [x] 3.8.7.3.  Last Modified
      - [x] 3.8.7.4.  Sequence Number
    - [x] 3.8.8.  Miscellaneous Component Properties
      - [x] 3.8.8.1.  IANA Properties
      - [x] 3.8.8.2.  Non-Standard Properties
      - [x] 3.8.8.3.  Request Status
- [x] 4.  iCalendar Object Examples
- [x] 5.  Recommended Practices
- [x] 6.  Internationalization Considerations
- [x] 7.  Security Considerations
- [x] 8.  IANA Considerations
  - [x] 8.1.  iCalendar Media Type Registration
  - [x] 8.2.  New iCalendar Elements Registration
    - [x] 8.2.1.  iCalendar Elements Registration Procedure
    - [x] 8.2.2.  Registration Template for Components
    - [x] 8.2.3.  Registration Template for Properties
    - [x] 8.2.4.  Registration Template for Parameters
    - [x] 8.2.5.  Registration Template for Value Data Types
    - [x] 8.2.6.  Registration Template for Values
  - [x] 8.3.  Initial iCalendar Elements Registries
    - [x] 8.3.1.  Components Registry
    - [x] 8.3.2.  Properties Registry
    - [x] 8.3.3.  Parameters Registry
    - [x] 8.3.4.  Value Data Types Registry
    - [x] 8.3.5.  Calendar User Types Registry
    - [x] 8.3.6.  Free/Busy Time Types Registry
    - [x] 8.3.7.  Participation Statuses Registry
    - [x] 8.3.8.  Relationship Types Registry
    - [x] 8.3.9.  Participation Roles Registry
    - [x] 8.3.10. Actions Registry
    - [x] 8.3.11. Classifications Registry
    - [x] 8.3.12. Methods Registry
- [x] 9.  Acknowledgments
- [x] 10. References
  - [x] 10.1. Normative References
  - [x] 10.2. Informative References
- [x] Appendix A.  Differences from RFC 2445
  - [x] A.1.  New Restrictions
  - [x] A.2.  Restrictions Removed
  - [x] A.3.  Deprecated Features

#### [Errata](https://www.rfc-editor.org/errata/rfc5545)


- [x] [1911](https://www.rfc-editor.org/errata/eid1911)
- [x] [1916](https://www.rfc-editor.org/errata/eid1916)
- [x] [4271](https://www.rfc-editor.org/errata/eid4271)
- [x] [2677](https://www.rfc-editor.org/errata/eid2677)
- [x] [3740](https://www.rfc-editor.org/errata/eid3740)
- [x] [3779](https://www.rfc-editor.org/errata/eid3779)
- [x] [3883](https://www.rfc-editor.org/errata/eid3883)
- [x] [4149](https://www.rfc-editor.org/errata/eid4149)
- [x] [2497](https://www.rfc-editor.org/errata/eid2497)
- [x] [2038](https://www.rfc-editor.org/errata/eid2038)
- [x] [2516](https://www.rfc-editor.org/errata/eid2516)
- [x] [2527](https://www.rfc-editor.org/errata/eid2527)
- [x] [3747](https://www.rfc-editor.org/errata/eid3747)
- [x] [4414](https://www.rfc-editor.org/errata/eid4414)

- [x] [5794](https://www.rfc-editor.org/errata/eid5794)
- [x] [5214](https://www.rfc-editor.org/errata/eid5214)
- [x] [5215](https://www.rfc-editor.org/errata/eid5215)
- [x] [5505](https://www.rfc-editor.org/errata/eid5505)
- [ ] [5602](https://www.rfc-editor.org/errata/eid5602)
- [ ] [5920](https://www.rfc-editor.org/errata/eid5920)
- [ ] [6212](https://www.rfc-editor.org/errata/eid6212)
- [ ] [6109](https://www.rfc-editor.org/errata/eid6109)
- [ ] [6316](https://www.rfc-editor.org/errata/eid6316)
- [ ] [7332](https://www.rfc-editor.org/errata/eid7332)
- [ ] [2495](https://www.rfc-editor.org/errata/eid2495)
- [ ] [4626](https://www.rfc-editor.org/errata/eid4626)

### RFC 6868

- [x] 1. Introduction
- [x] 2. Conventions Used in This Document
- [ ] 3. Parameter Value Encoding Scheme
    - [x] 3.1. iCalendar Example
    - [ ] 3.2. vCard Example
- [x] 4. Security Considerations
- [x] 5. Acknowledgments
- [x] 6. Normative References
- [x] Appendix A. Choice of Quoting Mechanism

#### [Errata](https://www.rfc-editor.org/errata/rfc6868)

- [ ] [4383](https://www.rfc-editor.org/errata/eid4383)

### RFC 7986

- [ ] 1. Introduction
- [ ] 2. Conventions Used in This Document
- [ ] 3. Backwards-Compatible Extension Properties
- [ ] 4. Modifications to Calendar Components
- [ ] 5. Properties
   - [ ] 5.1. NAME Property
   - [x] 5.2. DESCRIPTION Property
   - [x] 5.3. UID Property
   - [x] 5.4. LAST-MODIFIED Property
   - [x] 5.5. URL Property
   - [ ] 5.6. CATEGORIES Property
   - [ ] 5.7. REFRESH-INTERVAL Property
   - [ ] 5.8. SOURCE Property
   - [ ] 5.9. COLOR Property
   - [x] 5.10. IMAGE Property
   - [ ] 5.11. CONFERENCE Property
- [ ] 6. Property Parameters
   - [x] 6.1. DISPLAY Property Parameter
   - [ ] 6.2. EMAIL Property Parameter
   - [ ] 6.3. FEATURE Property Parameter
   - [ ] 6.4. LABEL Property Parameter
- [ ] 7. Security Considerations
- [ ] 8. Privacy Considerations
- [x] 9. IANA Considerations
   - [x] 9.1. Property Registrations
   - [x] 9.2. Parameter Registrations
   - [x] 9.3. Property Parameter Value Registries
- [x] 10. References
   - [x] 10.1. Normative References
   - [x] 10.2. Informative References

#### Errata

TODO
