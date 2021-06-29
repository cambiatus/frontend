module TokenTests exposing (all)

import Expect
import Json.Decode as Decode
import Test exposing (..)
import TestUtils
import Token


all : Test
all =
    describe "Token"
        [ updateTokenData
        , createTokenData
        , expiryOptsData
        ]



-- UPDATE TOKEN DATA


updateTokenData : Test
updateTokenData =
    describe "UpdateTokenData"
        [ fuzz TestUtils.updateTokenDataFuzzer "encoding and decoding is a no-op" <|
            \fuzzUpdateTokenData ->
                fuzzUpdateTokenData
                    |> Token.encodeUpdateTokenData
                    |> Decode.decodeValue Token.updateTokenDataDecoder
                    |> Expect.equal (Ok fuzzUpdateTokenData)
        ]



-- TOKEN TYPE


createTokenData : Test
createTokenData =
    describe "CreateTokenData"
        [ fuzz TestUtils.createTokenDataFuzzer "encoding and decoding is a no-op" <|
            \fuzzCreateTokenData ->
                fuzzCreateTokenData
                    |> Token.encodeCreateTokenData
                    |> Decode.decodeValue Token.createTokenDataDecoder
                    |> Expect.equal (Ok fuzzCreateTokenData)
        ]



-- EXPIRY OPTS


expiryOptsData : Test
expiryOptsData =
    describe "ExpiryOptsData"
        [ fuzz TestUtils.expiryOptsDataFuzzer "encoding and decoding is a no-op" <|
            \fuzzExpiryOptsData ->
                fuzzExpiryOptsData
                    |> Token.encodeExpiryOpts
                    |> Decode.decodeValue Token.expiryOptsDataDecoder
                    |> Expect.equal (Ok fuzzExpiryOptsData)
        ]
