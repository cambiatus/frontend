module Kyc.CostaRica.NiteTests exposing (all)

import Expect
import Fuzz
import Kyc.CostaRica.Nite
import Random
import Shrink
import Test exposing (..)
import TestUtils


all : Test
all =
    describe "Kyc.CostaRica.Nite" [ isValid ]


isValid : Test
isValid =
    describe "isValid"
        [ fuzz niteFuzzer "Accepts valid `String`s" <|
            \niteFuzz ->
                Kyc.CostaRica.Nite.isValid niteFuzz
                    |> Expect.true "Expected nite to be valid"
        , describe "Rejects invalid `String`s"
            [ test "Starting with `0`" <|
                \() ->
                    "0123456789"
                        |> Kyc.CostaRica.Nite.isValid
                        |> Expect.false "Expected to not start with `0`"
            , test "Without enough digits" <|
                \() ->
                    "123456789"
                        |> Kyc.CostaRica.Nite.isValid
                        |> Expect.false "Expected to have more digits"
            , test "With more digits than allowed" <|
                \() ->
                    "12345678901"
                        |> Kyc.CostaRica.Nite.isValid
                        |> Expect.false "Expected to have fewer digits"
            , test "With invalid digits" <|
                \() ->
                    "abcde123456789abcde"
                        |> Kyc.CostaRica.Nite.isValid
                        |> Expect.false "Expected to only contain valid characters"
            ]
        ]


niteFuzzer : Fuzz.Fuzzer String
niteFuzzer =
    Fuzz.custom niteGenerator Shrink.string


niteGenerator : Random.Generator String
niteGenerator =
    let
        tailDigits =
            Random.list 9 TestUtils.digitGenerator
                |> Random.map String.concat
    in
    TestUtils.nonZeroDigitGenerator
        |> TestUtils.appendGenerators tailDigits
