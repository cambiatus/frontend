module ActionTests exposing (all)

import Action
import Expect
import Test exposing (..)
import TestUtils
import Time
import Utils


all : Test
all =
    describe "Action"
        [ isPastDeadline
        , isClosed
        ]



-- IS PAST DEADLINE


isPastDeadline : Test
isPastDeadline =
    fuzz2 TestUtils.actionFuzzer TestUtils.timeFuzzer "isPastDeadline" <|
        \fuzzAction fuzzTime ->
            case fuzzAction.deadline of
                Just _ ->
                    Action.isPastDeadline fuzzAction fuzzTime
                        |> Expect.equal (Time.posixToMillis fuzzTime > Time.posixToMillis (Utils.posixDateTime fuzzAction.deadline))

                Nothing ->
                    Action.isPastDeadline fuzzAction fuzzTime
                        |> Expect.false "Expected action to not be considered past deadline"



-- IS CLOSED


isClosed : Test
isClosed =
    fuzz2 TestUtils.actionFuzzer TestUtils.timeFuzzer "isClosed" <|
        \fuzzAction fuzzTime ->
            Action.isPastDeadline fuzzAction fuzzTime
                || (fuzzAction.usages > 0 && fuzzAction.usagesLeft == 0)
                |> Expect.equal (Action.isClosed fuzzAction fuzzTime)
