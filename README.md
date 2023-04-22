# ICal

An implementation of [RCF5545](https://tools.ietf.org/html/rfc5545).

## Status

This library does not cover even close to all parts of the spec yet, but it is good at the parts that it does cover already.
It's already being used in production at https://smos.online and https://social-dance.today.

## Goals

* Strict RFC Compliance
* Being able to deal with some of other implementors' failures to comply with the spec

## Implementation checklist

   - [x] 1.  Introduction
   - [x] 2.  Basic Grammar and Conventions
     - [x] 2.1.  Formatting Conventions
     - [x] 2.2.  Related Memos
   - [x] 3.  iCalendar Object Specification
     - [x] 3.1.  Content Lines
       - [ ] 3.1.1.  List and Field Separators
       - [ ] 3.1.2.  Multiple Values
       - [ ] 3.1.3.  Binary Content
       - [ ] 3.1.4.  Character Set
     - [ ] 3.2.  Property Parameters
       - [ ] 3.2.1.  Alternate Text Representation
       - [X] 3.2.2.  Common Name
       - [ ] 3.2.3.  Calendar User Type
       - [ ] 3.2.4.  Delegators
       - [ ] 3.2.5.  Delegatees
       - [ ] 3.2.6.  Directory Entry Reference
       - [ ] 3.2.7.  Inline Encoding
       - [ ] 3.2.8.  Format Type
       - [ ] 3.2.9.  Free/Busy Time Type
       - [ ] 3.2.10. Language
       - [ ] 3.2.11. Group or List Membership
       - [X] 3.2.12. Participation Status
       - [ ] 3.2.13. Recurrence Identifier Range
       - [ ] 3.2.14. Alarm Trigger Relationship
       - [ ] 3.2.15. Relationship Type
       - [X] 3.2.16. Participation Role
       - [X] 3.2.17. RSVP Expectation
       - [ ] 3.2.18. Sent By
       - [ ] 3.2.19. Time Zone Identifier
       - [X] 3.2.20. Value Data Types
     - [ ] 3.3.  Property Value Data Types
       - [ ] 3.3.1.  Binary
       - [ ] 3.3.2.  Boolean
       - [X] 3.3.3.  Calendar User Address
       - [X] 3.3.4.  Date
       - [X] 3.3.5.  Date-Time
       - [X] 3.3.6.  Duration
       - [X] 3.3.7.  Float
       - [ ] 3.3.8.  Integer
       - [X] 3.3.9.  Period of Time
       - [X] 3.3.10. Recurrence Rule
       - [X] 3.3.11. Text
       - [X] 3.3.12. Time
       - [X] 3.3.13. URI
       - [X] 3.3.14. UTC Offset
     - [ ] 3.4.  iCalendar Object
     - [ ] 3.5.  Property
     - [ ] 3.6.  Calendar Components
       - [ ] 3.6.1.  Event Component
       - [ ] 3.6.2.  To-Do Component
       - [ ] 3.6.3.  Journal Component
       - [ ] 3.6.4.  Free/Busy Component
       - [ ] 3.6.5.  Time Zone Component
       - [ ] 3.6.6.  Alarm Component
     - [ ] 3.7.  Calendar Properties
       - [ ] 3.7.1.  Calendar Scale
       - [ ] 3.7.2.  Method
       - [ ] 3.7.3.  Product Identifier
       - [ ] 3.7.4.  Version
     - [ ] 3.8.  Component Properties
       - [ ] 3.8.1.  Descriptive Component Properties
         - [ ] 3.8.1.1.  Attachment
         - [ ] 3.8.1.2.  Categories
         - [X] 3.8.1.3.  Classification
         - [X] 3.8.1.4.  Comment
         - [X] 3.8.1.5.  Description
         - [X] 3.8.1.6.  Geographic Position
         - [ ] 3.8.1.7.  Location
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
         - [ ] 3.8.4.1.  Attendee
         - [ ] 3.8.4.2.  Contact
         - [ ] 3.8.4.3.  Organizer
         - [ ] 3.8.4.4.  Recurrence ID
         - [ ] 3.8.4.5.  Related To
         - [ ] 3.8.4.6.  Uniform Resource Locator
         - [ ] 3.8.4.7.  Unique Identifier
       - [X] 3.8.5.  Recurrence Component Properties
         - [X] 3.8.5.1.  Exception Date-Times
         - [X] 3.8.5.2.  Recurrence Date-Times
         - [X] 3.8.5.3.  Recurrence Rule
       - [ ] 3.8.6.  Alarm Component Properties
         - [ ] 3.8.6.1.  Action
         - [ ] 3.8.6.2.  Repeat Count
         - [ ] 3.8.6.3.  Trigger
       - [ ] 3.8.7.  Change Management Component Properties
         - [ ] 3.8.7.1.  Date-Time Created
         - [ ] 3.8.7.2.  Date-Time Stamp
         - [ ] 3.8.7.3.  Last Modified
         - [ ] 3.8.7.4.  Sequence Number
       - [ ] 3.8.8.  Miscellaneous Component Properties
         - [ ] 3.8.8.1.  IANA Properties
         - [ ] 3.8.8.2.  Non-Standard Properties
         - [ ] 3.8.8.3.  Request Status
   - [ ] 4.  iCalendar Object Examples
   - [ ] 5.  Recommended Practices
   - [ ] 6.  Internationalization Considerations
   - [ ] 7.  Security Considerations
   - [ ] 8.  IANA Considerations
     - [ ] 8.1.  iCalendar Media Type Registration
     - [ ] 8.2.  New iCalendar Elements Registration
       - [ ] 8.2.1.  iCalendar Elements Registration Procedure
       - [ ] 8.2.2.  Registration Template for Components
       - [ ] 8.2.3.  Registration Template for Properties
       - [ ] 8.2.4.  Registration Template for Parameters
       - [ ] 8.2.5.  Registration Template for Value Data Types
       - [ ] 8.2.6.  Registration Template for Values
     - [ ] 8.3.  Initial iCalendar Elements Registries
       - [ ] 8.3.1.  Components Registry
       - [ ] 8.3.2.  Properties Registry
       - [ ] 8.3.3.  Parameters Registry
       - [ ] 8.3.4.  Value Data Types Registry
       - [ ] 8.3.5.  Calendar User Types Registry
       - [ ] 8.3.6.  Free/Busy Time Types Registry
       - [ ] 8.3.7.  Participation Statuses Registry
       - [ ] 8.3.8.  Relationship Types Registry
       - [ ] 8.3.9.  Participation Roles Registry
       - [ ] 8.3.10. Actions Registry
       - [ ] 8.3.11. Classifications Registry
       - [ ] 8.3.12. Methods Registry
   - [ ] 9.  Acknowledgments
   - [ ] 10. References
     - [ ] 10.1. Normative References
     - [ ] 10.2. Informative References
   - [ ] Appendix A.  Differences from RFC 2445
     - [ ] A.1.  New Restrictions
     - [ ] A.2.  Restrictions Removed
     - [ ] A.3.  Deprecated Features
