module UtilsTests exposing (all)

import Cambiatus.Scalar exposing (DateTime(..))
import Expect
import Fuzz
import Iso8601
import Test exposing (..)
import TestHelpers.Fuzz as Fuzz
import Time
import Utils


all : Test
all =
    describe "Utils"
        [ posixDateTime
        , formatFloat
        ]



-- POSIX DATE TIME


posixDateTime : Test
posixDateTime =
    fuzz Fuzz.maybeDateTime "posixDateTime" <|
        \fuzzMaybeDateTime ->
            case fuzzMaybeDateTime of
                Nothing ->
                    Time.millisToPosix 0
                        |> Expect.equal (Utils.posixDateTime fuzzMaybeDateTime)

                Just (DateTime fuzzDateTime) ->
                    case Iso8601.toTime fuzzDateTime of
                        Ok fuzzPosix ->
                            fuzzPosix
                                |> Expect.equal (Utils.posixDateTime fuzzMaybeDateTime)

                        Err _ ->
                            Time.millisToPosix 0
                                |> Expect.equal (Utils.posixDateTime fuzzMaybeDateTime)



-- FORMAT FLOAT


formatFloat : Test
formatFloat =
    let
        fuzzer =
            fuzz3 Fuzz.float (Fuzz.intRange -10 1000) Fuzz.bool

        stringToFloat : Bool -> String -> Maybe Float
        stringToFloat useSeparator floatString =
            if useSeparator then
                floatString
                    |> String.replace "." ""
                    |> String.replace "," "."
                    |> String.toFloat

            else
                floatString
                    |> String.toFloat

        separator : Bool -> String
        separator useSeparator =
            if useSeparator then
                ","

            else
                "."
    in
    describe "formatFloat"
        [ fuzzer "should have the right amount of decimalCases" <|
            \fuzzNumber fuzzDecimalCases fuzzUseSeparator ->
                case
                    Utils.formatFloat fuzzNumber fuzzDecimalCases fuzzUseSeparator
                        |> String.split (separator fuzzUseSeparator)
                of
                    [] ->
                        Expect.fail "didn't expect empty list"

                    [ _ ] ->
                        Expect.equal (max 0 fuzzDecimalCases) 0

                    [ _, afterSeparator ] ->
                        Expect.equal fuzzDecimalCases (String.length afterSeparator)

                    _ ->
                        Expect.fail "didn't expect list with more than 2 elements"
        , fuzzer "should be able to convert back to float" <|
            \fuzzNumber fuzzDecimalCases fuzzUseSeparator ->
                Utils.formatFloat fuzzNumber fuzzDecimalCases fuzzUseSeparator
                    |> stringToFloat fuzzUseSeparator
                    |> Expect.notEqual Nothing
        , fuzzer "should be the same when truncated" <|
            \fuzzNumber fuzzDecimalCases fuzzUseSeparator ->
                Utils.formatFloat fuzzNumber fuzzDecimalCases fuzzUseSeparator
                    |> stringToFloat fuzzUseSeparator
                    |> Maybe.map truncate
                    |> Expect.equal (Just (truncate fuzzNumber))
        , fuzzer "value of decimal cases should remain the same, but truncated" <|
            \fuzzNumber fuzzDecimalCases fuzzUseSeparator ->
                let
                    decimalCasesValue =
                        case fuzzNumber |> String.fromFloat |> String.split "." of
                            [] ->
                                ""

                            [ _ ] ->
                                String.repeat fuzzDecimalCases "0"
                                    |> String.left (max 0 fuzzDecimalCases)

                            [ _, value ] ->
                                value
                                    ++ String.repeat
                                        (fuzzDecimalCases - String.length value)
                                        "0"
                                    |> String.left (max 0 fuzzDecimalCases)

                            _ ->
                                ""
                in
                case
                    Utils.formatFloat fuzzNumber fuzzDecimalCases fuzzUseSeparator
                        |> String.split (separator fuzzUseSeparator)
                of
                    [] ->
                        Expect.fail "didn't expect empty list"

                    [ _ ] ->
                        Expect.equal "" decimalCasesValue

                    [ _, afterSeparator ] ->
                        afterSeparator
                            |> Expect.equal decimalCasesValue

                    _ ->
                        Expect.fail "didn't expect list with more than 2 elements"
        ]
