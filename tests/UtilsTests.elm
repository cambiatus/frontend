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
        [ fromMaybeDateTime
        , areSameDay
        , formatFloat
        ]



-- FROM MAYBE DATE TIME


fromMaybeDateTime : Test
fromMaybeDateTime =
    fuzz Fuzz.maybeDateTime "posixDateTime" <|
        \fuzzMaybeDateTime ->
            case fuzzMaybeDateTime of
                Nothing ->
                    Time.millisToPosix 0
                        |> Expect.equal (Utils.fromMaybeDateTime fuzzMaybeDateTime)

                Just (DateTime fuzzDateTime) ->
                    case Iso8601.toTime fuzzDateTime of
                        Ok fuzzPosix ->
                            fuzzPosix
                                |> Expect.equal (Utils.fromMaybeDateTime fuzzMaybeDateTime)

                        Err _ ->
                            Time.millisToPosix 0
                                |> Expect.equal (Utils.fromMaybeDateTime fuzzMaybeDateTime)



-- ARE SAME DAY


areSameDay : Test
areSameDay =
    describe "areSameDay"
        [ test "same instant" <|
            \() ->
                Utils.areSameDay Time.utc (Time.millisToPosix 1000) (Time.millisToPosix 1000)
                    |> Expect.true "Expected to be the same day"
        , test "one hour difference on the same day" <|
            \() ->
                Utils.areSameDay Time.utc (Time.millisToPosix 1625683508000) (Time.millisToPosix 1625676308000)
                    |> Expect.true "Expected to be the same day"
        , test "start and end of day" <|
            \() ->
                Utils.areSameDay Time.utc (Time.millisToPosix 1625616010000) (Time.millisToPosix 1625702399000)
                    |> Expect.true "Expected to be the same day"
        , test "23:59h and 00:01h" <|
            \() ->
                Utils.areSameDay Time.utc (Time.millisToPosix 1625615999000) (Time.millisToPosix 1625616060000)
                    |> Expect.false "Expected to not be the same day"
        , test "one day difference" <|
            \() ->
                Utils.areSameDay Time.utc (Time.millisToPosix 1625683508000) (Time.millisToPosix 1625597108000)
                    |> Expect.false "Expected to not be the same day"
        ]



-- FORMAT FLOAT


formatFloat : Test
formatFloat =
    let
        fuzzer =
            fuzz2 Fuzz.float (Fuzz.intRange -10 1000)

        stringToFloat : String -> Maybe Float
        stringToFloat floatString =
            floatString
                |> String.toFloat

        separator : String
        separator =
            "."
    in
    describe "formatFloat"
        [ fuzzer "should have the right amount of decimalCases" <|
            \fuzzNumber fuzzDecimalCases ->
                case
                    Utils.formatFloat Nothing fuzzDecimalCases fuzzNumber
                        |> String.split separator
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
            \fuzzNumber fuzzDecimalCases ->
                Utils.formatFloat Nothing fuzzDecimalCases fuzzNumber
                    |> stringToFloat
                    |> Expect.notEqual Nothing
        , fuzzer "should be the same when truncated" <|
            \fuzzNumber fuzzDecimalCases ->
                Utils.formatFloat Nothing fuzzDecimalCases fuzzNumber
                    |> stringToFloat
                    |> Maybe.map truncate
                    |> Expect.equal (Just (truncate fuzzNumber))
        , fuzzer "value of decimal cases should remain the same, but truncated" <|
            \fuzzNumber fuzzDecimalCases ->
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
                    Utils.formatFloat Nothing fuzzDecimalCases fuzzNumber
                        |> String.split separator
                of
                    [] ->
                        Expect.fail "didn't expect empty list"

                    [ _ ] ->
                        let
                            numbersBeforeSeparator =
                                String.fromFloat fuzzNumber
                                    |> String.split "."
                                    |> List.head
                                    |> Maybe.withDefault "0"
                        in
                        Expect.equal
                            (if fuzzDecimalCases > 0 then
                                numbersBeforeSeparator ++ "." ++ String.repeat fuzzDecimalCases "0"

                             else
                                numbersBeforeSeparator
                            )
                            (Utils.formatFloat Nothing fuzzDecimalCases fuzzNumber)

                    [ _, afterSeparator ] ->
                        afterSeparator
                            |> Expect.equal decimalCasesValue

                    _ ->
                        Expect.fail "didn't expect list with more than 2 elements"
        ]
