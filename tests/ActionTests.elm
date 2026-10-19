module ActionTests exposing (all)

import Action
import Cambiatus.Enum.ClaimEligibilityStatus as ClaimEligibilityStatus
import Cambiatus.Scalar
import Expect
import Iso8601
import Test exposing (..)
import TestHelpers.Fuzz as Fuzz
import Time
import Translation
import Utils


all : Test
all =
    describe "Action"
        [ isPastDeadline
        , isClosed
        , daysLeft
        , limitNotice
        ]



-- IS PAST DEADLINE


isPastDeadline : Test
isPastDeadline =
    fuzz2 Fuzz.action Fuzz.time "isPastDeadline" <|
        \fuzzAction fuzzTime ->
            case fuzzAction.deadline of
                Just _ ->
                    Action.isPastDeadline fuzzAction fuzzTime
                        |> Expect.equal (Time.posixToMillis fuzzTime > Time.posixToMillis (Utils.fromMaybeDateTime fuzzAction.deadline))

                Nothing ->
                    Action.isPastDeadline fuzzAction fuzzTime
                        |> Expect.false "Expected action to not be considered past deadline"



-- IS CLOSED


isClosed : Test
isClosed =
    fuzz2 Fuzz.action Fuzz.time "isClosed" <|
        \fuzzAction fuzzTime ->
            Action.isPastDeadline fuzzAction fuzzTime
                || (fuzzAction.usages > 0 && fuzzAction.usagesLeft == 0)
                |> Expect.equal (Action.isClosed fuzzAction fuzzTime)



-- DAYS LEFT


daysLeft : Test
daysLeft =
    describe "daysLeft"
        [ test "rounds a partial day up" <|
            \_ ->
                Action.daysLeft now (plusMs 1000)
                    |> Expect.equal 1
        , test "a full day is one day" <|
            \_ ->
                Action.daysLeft now (plusMs 86400000)
                    |> Expect.equal 1
        , test "a day and a millisecond is two days" <|
            \_ ->
                Action.daysLeft now (plusMs 86400001)
                    |> Expect.equal 2
        , test "a time in the past is one day" <|
            \_ ->
                Action.daysLeft now (plusMs -5000)
                    |> Expect.equal 1
        ]



-- LIMIT NOTICE


limitNotice : Test
limitNotice =
    describe "limitNotice"
        [ fuzz Fuzz.action "is Nothing without a claim limit" <|
            \fuzzAction ->
                Action.limitNotice translators now { fuzzAction | myClaimEligibility = Nothing }
                    |> Expect.equal Nothing
        , fuzz Fuzz.action "is completed when the action is done" <|
            \fuzzAction ->
                withEligibility ClaimEligibilityStatus.Completed Nothing fuzzAction
                    |> Action.limitNotice translators now
                    |> Expect.equal (Just "community.objectives.claim_limit.completed")
        , fuzz Fuzz.action "is pending when the claim is in review" <|
            \fuzzAction ->
                withEligibility ClaimEligibilityStatus.Pending Nothing fuzzAction
                    |> Action.limitNotice translators now
                    |> Expect.equal (Just "community.objectives.claim_limit.pending")
        , fuzz Fuzz.action "shows the cooldown in days when more than a day is left" <|
            \fuzzAction ->
                withEligibility ClaimEligibilityStatus.Cooldown (Just (plusMs threeDays)) fuzzAction
                    |> Action.limitNotice translators now
                    |> Expect.equal (Just "community.objectives.claim_limit.cooldown")
        , fuzz Fuzz.action "shows the cooldown in one day when less than a day is left" <|
            \fuzzAction ->
                withEligibility ClaimEligibilityStatus.Cooldown (Just (plusMs twoHours)) fuzzAction
                    |> Action.limitNotice translators now
                    |> Expect.equal (Just "community.objectives.claim_limit.cooldown_one")
        , fuzz Fuzz.action "is Nothing when the action is available" <|
            \fuzzAction ->
                withEligibility ClaimEligibilityStatus.Available Nothing fuzzAction
                    |> Action.limitNotice translators now
                    |> Expect.equal Nothing
        ]


translators : Translation.Translators
translators =
    { t = identity
    , tr = \key _ -> key
    }


withEligibility :
    ClaimEligibilityStatus.ClaimEligibilityStatus
    -> Maybe Time.Posix
    -> Action.Action
    -> Action.Action
withEligibility status availableAt action =
    { action
        | myClaimEligibility =
            Just
                { status = status
                , availableAt = Maybe.map (Iso8601.fromTime >> Cambiatus.Scalar.DateTime) availableAt
                , approvedCount = 1
                }
    }


now : Time.Posix
now =
    Time.millisToPosix 1000000000000


plusMs : Int -> Time.Posix
plusMs ms =
    Time.millisToPosix (Time.posixToMillis now + ms)


threeDays : Int
threeDays =
    3 * 86400000


twoHours : Int
twoHours =
    2 * 3600000
