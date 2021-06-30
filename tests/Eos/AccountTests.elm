module Eos.AccountTests exposing (all)

import Eos.Account as Eos
import Expect
import Fuzz
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


all : Test
all =
    describe "Eos.Account"
        [ name
        , privateKey
        ]



-- NAME


name : Test
name =
    describe "Name"
        [ nameWithString
        , nameWithJson
        , nameWithHtml
        , nameWithUrl
        ]


nameWithString : Test
nameWithString =
    describe "`String` interaction"
        [ fuzz Fuzz.string "Converting from `String` and going back returns the same `String`" <|
            \fuzzName ->
                Eos.stringToName fuzzName
                    |> Eos.nameToString
                    |> Expect.equal fuzzName
        ]


nameWithJson : Test
nameWithJson =
    describe "JSON interaction"
        [ fuzz Fuzz.string "Encoding a `Name` encodes it as a `String`" <|
            \fuzzName ->
                Eos.stringToName fuzzName
                    |> Eos.encodeName
                    |> Expect.equal (Encode.string fuzzName)
        , fuzz Fuzz.string "Decoding a `String` returns a `Name`" <|
            \fuzzName ->
                fuzzName
                    |> Encode.string
                    |> Decode.decodeValue Eos.nameDecoder
                    |> Expect.equal (Ok (Eos.stringToName fuzzName))
        , fuzz Fuzz.string "Encoding and decoding a name returns the `Name`" <|
            \fuzzName ->
                Eos.stringToName fuzzName
                    |> Eos.encodeName
                    |> Decode.decodeValue Eos.nameDecoder
                    |> Expect.equal (Ok (Eos.stringToName fuzzName))
        ]


nameWithHtml : Test
nameWithHtml =
    describe "HTML interaction"
        [ fuzz Fuzz.string "Viewing a `Name` returns an `Html.text` of that `Name`" <|
            \fuzzName ->
                Eos.stringToName fuzzName
                    |> Eos.viewName
                    |> Query.fromHtml
                    |> Query.has [ Selector.text fuzzName ]
        ]


nameWithUrl : Test
nameWithUrl =
    describe "URL query interaction"
        [ fuzz Fuzz.string "Parsing a `String` returns a `Name` that can be converted back to that `String`" <|
            \fuzzName ->
                Eos.nameQueryUrlParser fuzzName
                    |> Eos.nameToString
                    |> Expect.equal fuzzName
        ]



-- PRIVATE KEY


privateKey : Test
privateKey =
    describe "Private Key"
        [ privateKeyWithJson ]


privateKeyWithJson : Test
privateKeyWithJson =
    describe "JSON interaction"
        [ fuzz Fuzz.string "Decoding a `PrivateKey` returns the `PrivateKey`" <|
            \fuzzKey ->
                Encode.string fuzzKey
                    |> Decode.decodeValue Eos.privateKeyDecoder
                    |> Result.map Eos.privateKeyToString
                    |> Expect.equal (Ok fuzzKey)
        ]
