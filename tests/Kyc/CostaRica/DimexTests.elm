module Kyc.CostaRica.DimexTests exposing (all)

import Expect
import Fuzz
import Kyc.CostaRica.Dimex
import Random
import Shrink
import Test exposing (..)
import TestUtils


all : Test
all =
    describe "Kyc.CostaRica.Dimex"
        [ isValid ]


isValid : Test
isValid =
    describe "isValid"
        [ fuzz dimexFuzzer "Accepts valid `String`s" <|
            \dimexFuzz ->
                Kyc.CostaRica.Dimex.isValid dimexFuzz
                    |> Expect.true "Expected dimex to be valid"
        , describe "Rejects invalid `String`s"
            [ test "Starting with `0`" <|
                \() ->
                    "01234567890"
                        |> Kyc.CostaRica.Dimex.isValid
                        |> Expect.false "Expected to not start with 0"
            , test "Without enough digits" <|
                \() ->
                    "1234567890"
                        |> Kyc.CostaRica.Dimex.isValid
                        |> Expect.false "Expected to have more digits"
            , test "With more digits than allowed" <|
                \() ->
                    "1234567890123"
                        |> Kyc.CostaRica.Dimex.isValid
                        |> Expect.false "Expected to have fewer digits"
            , test "With invalid digits" <|
                \() ->
                    "abcde12345678901abcde"
                        |> Kyc.CostaRica.Dimex.isValid
                        |> Expect.false "Expected to only contain valid characters"
            ]
        ]


dimexFuzzer : Fuzz.Fuzzer String
dimexFuzzer =
    Fuzz.custom dimexGenerator Shrink.string


dimexGenerator : Random.Generator String
dimexGenerator =
    let
        tailDigits =
            TestUtils.digitGenerator
                |> TestUtils.randomListWithRandomLength 10 11
                |> Random.map String.concat
    in
    TestUtils.nonZeroDigitGenerator
        |> TestUtils.appendGenerators tailDigits
