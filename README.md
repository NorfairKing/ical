# ICal

An implementation of [RCF5545](https://tools.ietf.org/html/rfc5545) and [RFC7986](https://datatracker.ietf.org/doc/html/rfc7986#section-5.10).

## Status

This library does not cover even close to all parts of the spec yet, but it is good at the parts that it does cover already.
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
       - [X] 3.1.1.  List and Field Separators
       - [X] 3.1.2.  Multiple Values
       - [X] 3.1.3.  Binary Content
       - [X] 3.1.4.  Character Set
     - [ ] 3.2.  Property Parameters
       - [X] 3.2.1.  Alternate Text Representation
       - [X] 3.2.2.  Common Name
       - [X] 3.2.3.  Calendar User Type
       - [X] 3.2.4.  Delegators
       - [X] 3.2.5.  Delegatees
       - [X] 3.2.6.  Directory Entry Reference
       - [ ] 3.2.7.  Inline Encoding
       - [ ] 3.2.8.  Format Type
       - [ ] 3.2.9.  Free/Busy Time Type
       - [X] 3.2.10. Language
       - [ ] 3.2.11. Group or List Membership
       - [X] 3.2.12. Participation Status
       - [X] 3.2.13. Recurrence Identifier Range
       - [X] 3.2.14. Alarm Trigger Relationship
       - [ ] 3.2.15. Relationship Type
       - [X] 3.2.16. Participation Role
       - [X] 3.2.17. RSVP Expectation
       - [ ] 3.2.18. Sent By
       - [X] 3.2.19. Time Zone Identifier
       - [X] 3.2.20. Value Data Types
     - [X] 3.3.  Property Value Data Types
       - [X] 3.3.1.  Binary
       - [X] 3.3.2.  Boolean
       - [X] 3.3.3.  Calendar User Address
       - [X] 3.3.4.  Date
       - [X] 3.3.5.  Date-Time
       - [X] 3.3.6.  Duration
       - [X] 3.3.7.  Float
       - [X] 3.3.8.  Integer
       - [X] 3.3.9.  Period of Time
       - [X] 3.3.10. Recurrence Rule
       - [X] 3.3.11. Text
       - [X] 3.3.12. Time
       - [X] 3.3.13. URI
       - [X] 3.3.14. UTC Offset
     - [X] 3.4.  iCalendar Object
     - [X] 3.5.  Property
     - [X] 3.6.  Calendar Components
       - [X] 3.6.1.  Event Component
       - [ ] 3.6.2.  To-Do Component
       - [ ] 3.6.3.  Journal Component
       - [ ] 3.6.4.  Free/Busy Component
       - [ ] 3.6.5.  Time Zone Component
       - [X] 3.6.6.  Alarm Component
     - [X] 3.7.  Calendar Properties
       - [X] 3.7.1.  Calendar Scale
       - [X] 3.7.2.  Method
       - [X] 3.7.3.  Product Identifier
       - [X] 3.7.4.  Version
     - [ ] 3.8.  Component Properties
       - [ ] 3.8.1.  Descriptive Component Properties
         - [X] 3.8.1.1.  Attachment
         - [ ] 3.8.1.2.  Categories
         - [X] 3.8.1.3.  Classification
         - [X] 3.8.1.4.  Comment
         - [X] 3.8.1.5.  Description
         - [X] 3.8.1.6.  Geographic Position
         - [X] 3.8.1.7.  Location
         - [ ] 3.8.1.8.  Percent Complete
         - [ ] 3.8.1.9.  Priority
         - [ ] 3.8.1.10. Resources
         - [X] 3.8.1.11. Status
         - [X] 3.8.1.12. Summary
       - [ ] 3.8.2.  Date and Time Component Properties
         - [ ] 3.8.2.1.  Date-Time Completed
         - [X] 3.8.2.2.  Date-Time End
         - [ ] 3.8.2.3.  Date-Time Due
         - [X] 3.8.2.4.  Date-Time Start
         - [X] 3.8.2.5.  Duration
         - [ ] 3.8.2.6.  Free/Busy Time
         - [X] 3.8.2.7.  Time Transparency
       - [X] 3.8.3.  Time Zone Component Properties
         - [X] 3.8.3.1.  Time Zone Identifier
         - [X] 3.8.3.2.  Time Zone Name
         - [X] 3.8.3.3.  Time Zone Offset From
         - [X] 3.8.3.4.  Time Zone Offset To
         - [X] 3.8.3.5.  Time Zone URL
       - [ ] 3.8.4.  Relationship Component Properties
         - [X] 3.8.4.1.  Attendee
         - [ ] 3.8.4.2.  Contact
         - [X] 3.8.4.3.  Organizer
         - [X] 3.8.4.4.  Recurrence ID
         - [ ] 3.8.4.5.  Related To
         - [X] 3.8.4.6.  Uniform Resource Locator
         - [X] 3.8.4.7.  Unique Identifier
       - [X] 3.8.5.  Recurrence Component Properties
         - [X] 3.8.5.1.  Exception Date-Times
         - [X] 3.8.5.2.  Recurrence Date-Times
         - [X] 3.8.5.3.  Recurrence Rule
       - [X] 3.8.6.  Alarm Component Properties
         - [X] 3.8.6.1.  Action
         - [X] 3.8.6.2.  Repeat Count
         - [X] 3.8.6.3.  Trigger
       - [ ] 3.8.7.  Change Management Component Properties
         - [ ] 3.8.7.1.  Date-Time Created
         - [ ] 3.8.7.2.  Date-Time Stamp
         - [ ] 3.8.7.3.  Last Modified
         - [ ] 3.8.7.4.  Sequence Number
       - [ ] 3.8.8.  Miscellaneous Component Properties
         - [ ] 3.8.8.1.  IANA Properties
         - [ ] 3.8.8.2.  Non-Standard Properties
         - [ ] 3.8.8.3.  Request Status
   - [X] 4.  iCalendar Object Examples
   - [X] 5.  Recommended Practices
   - [X] 6.  Internationalization Considerations
   - [X] 7.  Security Considerations
   - [X] 8.  IANA Considerations
     - [X] 8.1.  iCalendar Media Type Registration
     - [X] 8.2.  New iCalendar Elements Registration
       - [X] 8.2.1.  iCalendar Elements Registration Procedure
       - [X] 8.2.2.  Registration Template for Components
       - [X] 8.2.3.  Registration Template for Properties
       - [X] 8.2.4.  Registration Template for Parameters
       - [X] 8.2.5.  Registration Template for Value Data Types
       - [X] 8.2.6.  Registration Template for Values
     - [X] 8.3.  Initial iCalendar Elements Registries
       - [X] 8.3.1.  Components Registry
       - [X] 8.3.2.  Properties Registry
       - [X] 8.3.3.  Parameters Registry
       - [X] 8.3.4.  Value Data Types Registry
       - [X] 8.3.5.  Calendar User Types Registry
       - [X] 8.3.6.  Free/Busy Time Types Registry
       - [X] 8.3.7.  Participation Statuses Registry
       - [X] 8.3.8.  Relationship Types Registry
       - [X] 8.3.9.  Participation Roles Registry
       - [X] 8.3.10. Actions Registry
       - [X] 8.3.11. Classifications Registry
       - [X] 8.3.12. Methods Registry
   - [X] 9.  Acknowledgments
   - [X] 10. References
     - [X] 10.1. Normative References
     - [X] 10.2. Informative References
   - [X] Appendix A.  Differences from RFC 2445
     - [X] A.1.  New Restrictions
     - [X] A.2.  Restrictions Removed
     - [X] A.3.  Deprecated Features

### RFC 7986

   - [ ] 1. Introduction
   - [ ] 2. Conventions Used in This Document
   - [ ] 3. Backwards-Compatible Extension Properties
   - [ ] 4. Modifications to Calendar Components
   - [ ] 5. Properties
      - [ ] 5.1. NAME Property
      - [X] 5.2. DESCRIPTION Property
      - [X] 5.3. UID Property
      - [X] 5.4. LAST-MODIFIED Property
      - [X] 5.5. URL Property
      - [ ] 5.6. CATEGORIES Property
      - [ ] 5.7. REFRESH-INTERVAL Property
      - [ ] 5.8. SOURCE Property
      - [ ] 5.9. COLOR Property
      - [X] 5.10. IMAGE Property
      - [ ] 5.11. CONFERENCE Property
   - [ ] 6. Property Parameters
      - [X] 6.1. DISPLAY Property Parameter
      - [ ] 6.2. EMAIL Property Parameter
      - [ ] 6.3. FEATURE Property Parameter
      - [ ] 6.4. LABEL Property Parameter
   - [ ] 7. Security Considerations
   - [ ] 8. Privacy Considerations
   - [X] 9. IANA Considerations
      - [X] 9.1. Property Registrations
      - [X] 9.2. Parameter Registrations
      - [X] 9.3. Property Parameter Value Registries
   - [X] 10. References
      - [X] 10.1. Normative References
      - [X] 10.2. Informative References
