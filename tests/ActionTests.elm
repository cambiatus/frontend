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
        [ isPastDeadline ]



-- IS PAST DEADLINE


isPastDeadline : Test
isPastDeadline =
    describe "isPastDeadline"
        [ fuzz2 TestUtils.actionFuzzer TestUtils.timeFuzzer "test" <|
            \fuzzAction fuzzTime ->
                case fuzzAction.deadline of
                    Just _ ->
                        Action.isPastDeadline fuzzAction fuzzTime
                            |> Expect.equal (Time.posixToMillis fuzzTime > Time.posixToMillis (Utils.posixDateTime fuzzAction.deadline))

                    Nothing ->
                        Action.isPastDeadline fuzzAction fuzzTime
                            |> Expect.false "Expected action to not be considered past deadline"
        ]
