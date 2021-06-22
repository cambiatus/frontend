module Kyc.CostaRica.PhoneTests exposing (all)

import Expect
import Fuzz
import Kyc.CostaRica.Phone
import Random
import Shrink
import Test exposing (..)
import TestUtils


all : Test
all =
    describe "Kyc.CostaRica.Phone"
        [ isValid ]


isValid : Test
isValid =
    describe "isValid"
        [ fuzz phoneFuzzer "Accepts valid `String`s" <|
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


phoneFuzzer : Fuzz.Fuzzer String
phoneFuzzer =
    Fuzz.custom phoneGenerator Shrink.string


phoneGenerator : Random.Generator String
phoneGenerator =
    TestUtils.nonZeroDigitGenerator
        |> TestUtils.appendGenerators (Random.list 3 TestUtils.digitGenerator |> Random.map String.concat)
        |> TestUtils.appendGenerators (TestUtils.generateEither "" "-")
        |> TestUtils.appendGenerators (Random.list 4 TestUtils.digitGenerator |> Random.map String.concat)
