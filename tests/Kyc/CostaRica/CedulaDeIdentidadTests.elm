module Kyc.CostaRica.CedulaDeIdentidadTests exposing (all)

import Expect
import Kyc.CostaRica.CedulaDeIdentidad
import Test exposing (..)
import TestHelpers.Fuzz as Fuzz


all : Test
all =
    describe "Kyc.CostaRica.CedulaDeIdentidad"
        [ isValid ]


isValid : Test
isValid =
    describe "isValid"
        [ fuzz Fuzz.cedulaDeIdentidad "Accepts valid `String`s" <|
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
