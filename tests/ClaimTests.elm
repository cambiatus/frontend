module ClaimTests exposing (all)

import Action
import Claim
import Expect
import Test exposing (..)
import TestUtils


all : Test
all =
    describe "Claim" [ isValidated, isVotable ]



-- IS VALIDATED


isValidated : Test
isValidated =
    fuzz2 TestUtils.claimFuzzer TestUtils.nameFuzzer "isValidated" <|
        \fuzzClaim fuzzName ->
            fuzzClaim.status
                /= Claim.Pending
                || List.any (\c -> c.validator.account == fuzzName) fuzzClaim.checks
                |> Expect.equal (Claim.isValidated fuzzClaim fuzzName)



-- IS VOTABLE


isVotable : Test
isVotable =
    fuzz3 TestUtils.claimFuzzer TestUtils.nameFuzzer TestUtils.timeFuzzer "isVotable" <|
        \fuzzClaim fuzzName fuzzTime ->
            List.any (\v -> v.account == fuzzName) fuzzClaim.action.validators
                && not (Claim.isValidated fuzzClaim fuzzName)
                && not (Action.isClosed fuzzClaim.action fuzzTime)
                && not fuzzClaim.action.isCompleted
                |> Expect.equal (Claim.isVotable fuzzClaim fuzzName fuzzTime)
