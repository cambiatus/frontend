module Kyc.CostaRica.CedulaDeIdentidadTests exposing (all)

import Expect
import Fuzz
import Kyc.CostaRica.CedulaDeIdentidad
import Random
import Random.Extra
import Shrink
import Test exposing (..)
import TestUtils


all : Test
all =
    describe "Kyc.CostaRica.CedulaDeIdentidad"
        [ isValid ]


isValid : Test
isValid =
    describe "isValid"
        [ fuzz cedulaDeIdentidadFuzzer "Accepts valid `String`s" <|
            \cedulaDeIdentidadFuzz ->
                Kyc.CostaRica.CedulaDeIdentidad.isValid cedulaDeIdentidadFuzz
                    |> Expect.true "Expected the cedulaDeIdentidad to be valid"
        , describe "Rejects invalid `String`s"
            [ test "Starting with `0`" <|
                \() ->
                    "0-1234-5678"
                        |> Kyc.CostaRica.CedulaDeIdentidad.isValid
                        |> Expect.false "Expected to not start with 0"
            , test "Without enough digits" <|
                \() ->
                    "1234-5678"
                        |> Kyc.CostaRica.CedulaDeIdentidad.isValid
                        |> Expect.false "Expected to have more digits"
            , test "With more digits than allowed" <|
                \() ->
                    "1234-56789"
                        |> Kyc.CostaRica.CedulaDeIdentidad.isValid
                        |> Expect.false "Expected to have fewer digits"
            , test "With invalid digits" <|
                \() ->
                    "abcde1-1234-5678abcde"
                        |> Kyc.CostaRica.CedulaDeIdentidad.isValid
                        |> Expect.false "Expected to only contain valid characters"
            ]
        ]


cedulaDeIdentidadFuzzer : Fuzz.Fuzzer String
cedulaDeIdentidadFuzzer =
    Fuzz.custom cedulaDeIdentidadGenerator Shrink.string


cedulaDeIdentidadGenerator : Random.Generator String
cedulaDeIdentidadGenerator =
    let
        maybeDashGenerator =
            Random.Extra.choice "" "-"
    in
    TestUtils.nonZeroDigitGenerator
        |> TestUtils.appendGenerators maybeDashGenerator
        |> TestUtils.appendGenerators
            (Random.list 4 TestUtils.digitGenerator
                |> Random.map String.concat
            )
        |> TestUtils.appendGenerators maybeDashGenerator
        |> TestUtils.appendGenerators
            (Random.list 4 TestUtils.digitGenerator
                |> Random.map String.concat
            )
