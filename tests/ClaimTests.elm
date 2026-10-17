module ClaimTests exposing (all)

import Action
import Cambiatus.Enum.Permission as Permission
import Claim
import Expect
import Test exposing (..)
import TestHelpers.Fuzz as Fuzz


all : Test
all =
    describe "Claim" [ isValidated, isVotable, isNotVotableByClaimer ]



-- IS VALIDATED


isValidated : Test
isValidated =
    fuzz2 Fuzz.claim Fuzz.name "isValidated" <|
        \fuzzClaim fuzzName ->
            fuzzClaim.status
                /= Claim.Pending
                || List.any (\c -> c.validator.account == fuzzName) fuzzClaim.checks
                |> Expect.equal (Claim.isValidated fuzzClaim fuzzName)



-- IS VOTABLE


isVotable : Test
isVotable =
    fuzz3 Fuzz.claim Fuzz.name (Fuzz.pair Fuzz.permissions Fuzz.time) "isVotable" <|
        \fuzzClaim fuzzName ( fuzzPermissions, fuzzTime ) ->
            (List.any (\v -> v.account == fuzzName) fuzzClaim.action.validators
                || (List.isEmpty fuzzClaim.action.validators
                        && List.member Permission.Verify fuzzPermissions
                   )
            )
                && (fuzzClaim.claimer.account /= fuzzName)
                && not (Claim.isValidated fuzzClaim fuzzName)
                && not (Action.isClosed fuzzClaim.action fuzzTime)
                && not fuzzClaim.action.isCompleted
                |> Expect.equal (Claim.isVotable fuzzClaim fuzzName fuzzPermissions fuzzTime)


isNotVotableByClaimer : Test
isNotVotableByClaimer =
    fuzz3 Fuzz.claim Fuzz.permissions Fuzz.time "the claimer cannot vote on their own claim" <|
        \fuzzClaim fuzzPermissions fuzzTime ->
            let
                claimer =
                    fuzzClaim.claimer
            in
            Claim.isVotable fuzzClaim claimer.account fuzzPermissions fuzzTime
                |> Expect.equal False
