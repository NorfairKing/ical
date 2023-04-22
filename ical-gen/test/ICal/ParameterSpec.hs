{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.ParameterSpec where

import ICal.Parameter
import ICal.Parameter.Gen
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "ParticipationStatus" $ do
    genValidSpec @ParticipationStatus
    parameterSpec @ParticipationStatus
    -- From the spec:
    -- @
    -- Example:
    --
    --     ATTENDEE;PARTSTAT=DECLINED:mailto:jsmith@example.com
    -- @
    parameterExampleSpec ["ACCEPTED"] ParticipationStatusAccepted
    parameterExampleSpec ["DECLINED"] ParticipationStatusDeclined

  describe "RSVPExpectation" $ do
    genValidSpec @RSVPExpectation
    parameterSpec @RSVPExpectation
    -- From the spec:
    -- @
    -- Example:
    --
    --     ATTENDEE;RSVP=TRUE:mailto:jsmith@example.com
    -- @
    parameterExampleSpec ["TRUE"] RSVPExpectationTrue
    parameterExampleSpec ["FALSE"] RSVPExpectationFalse

  describe "ParticipationRole" $ do
    genValidSpec @ParticipationRole
    parameterSpec @ParticipationRole
    -- From the spec:
    -- @
    -- Example:
    --
    --     ATTENDEE;ROLE=CHAIR:mailto:mrbig@example.com
    -- @
    parameterExampleSpec ["CHAIR"] ParticipationRoleChair
    parameterExampleSpec ["REQ-PARTICIPANT"] ParticipationRoleRequiredParticipant
    parameterExampleSpec ["OPT-PARTICIPANT"] ParticipationRoleOptionalParticipant

  describe "TZIDParam" $ do
    genValidSpec @TZIDParam
    parameterSpec @TZIDParam
    -- From the spec:
    -- @
    --    The following are examples of this property parameter:
    --
    --     DTSTART;TZID=America/New_York:19980119T020000
    --
    --     DTEND;TZID=America/New_York:19980119T030000
    -- @
    parameterExampleSpec ["America/New_York"] (TZIDParam "America/New_York")
    parameterExampleSpec ["/example.org/America/New_York"] (TZIDParam "/example.org/America/New_York")

  describe "ValueDataType" $ do
    genValidSpec @ValueDataType
    parameterSpec @ValueDataType
    parameterExampleSpec ["DATE"] TypeDate
    parameterExampleSpec ["DATE-TIME"] TypeDateTime
