module Kyc.CostaRica.CedulaDeIdentidadTests exposing (all)

import Expect
import Fuzz
import Kyc.CostaRica.CedulaDeIdentidad
import Random
import Shrink
import Test exposing (..)


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
            ]
        ]


cedulaDeIdentidadFuzzer : Fuzz.Fuzzer String
cedulaDeIdentidadFuzzer =
    Fuzz.custom cedulaDeIdentidadGenerator Shrink.string


cedulaDeIdentidadGenerator : Random.Generator String
cedulaDeIdentidadGenerator =
    let
        nonZeroDigitGenerator =
            Random.int 1 9
                |> Random.map String.fromInt

        digitGenerator =
            Random.int 0 9
                |> Random.map String.fromInt

        maybeDashGenerator =
            Random.uniform "" [ "-" ]

        appendGenerators : Random.Generator String -> Random.Generator String -> Random.Generator String
        appendGenerators secondGenerator firstGenerator =
            firstGenerator
                |> Random.andThen (\first -> Random.map (\second -> first ++ second) secondGenerator)
    in
    nonZeroDigitGenerator
        |> appendGenerators maybeDashGenerator
        |> appendGenerators (Random.list 4 digitGenerator |> Random.map String.concat)
        |> appendGenerators maybeDashGenerator
        |> appendGenerators (Random.list 4 digitGenerator |> Random.map String.concat)
