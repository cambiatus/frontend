module Kyc.CostaRica.PhoneTests exposing (all)

import Expect
import Kyc.CostaRica.Phone
import Test exposing (..)
import TestHelpers.Fuzz as Fuzz


all : Test
all =
    describe "Kyc.CostaRica.Phone"
        [ isValid ]


isValid : Test
isValid =
    describe "isValid"
        [ fuzz Fuzz.phone "Accepts valid `String`s" <|
            \phoneFuzz ->
                Kyc.CostaRica.Phone.isValid phoneFuzz
                    |> Expect.true "Expected dimex to be valid"
        , describe "Rejects invalid `String`s"
            [ test "Starting with `0`" <|
                \() ->
                    "0123-1234"
                        |> Kyc.CostaRica.Phone.isValid
                        |> Expect.false "Expected to not start with 0"
            , test "Without enough digits" <|
                \() ->
                    "1234-123"
                        |> Kyc.CostaRica.Phone.isValid
                        |> Expect.false "Expected to have more digits"
            , test "With more digits than allowed" <|
                \() ->
                    "1234-12345"
                        |> Kyc.CostaRica.Phone.isValid
                        |> Expect.false "Expected to have fewer digits"
            , test "With invalid digits" <|
                \() ->
                    "abcde1234-1234abcde"
                        |> Kyc.CostaRica.Phone.isValid
                        |> Expect.false "Expected to only contain valid characters"
            ]
        ]
