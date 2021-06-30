module Kyc.CostaRica.DimexTests exposing (all)

import Expect
import Kyc.CostaRica.Dimex
import Test exposing (..)
import TestHelpers.Fuzz as Fuzz


all : Test
all =
    describe "Kyc.CostaRica.Dimex"
        [ isValid ]


isValid : Test
isValid =
    describe "isValid"
        [ fuzz Fuzz.dimex "Accepts valid `String`s" <|
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
