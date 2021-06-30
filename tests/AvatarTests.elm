module AvatarTests exposing (all)

import Avatar
import Expect
import Fuzz
import Test exposing (..)
import TestHelpers.Fuzz as Fuzz


all : Test
all =
    describe "Avatar"
        [ convertingToAndFromString ]



-- CONVERTING FROM AND TO STRING


convertingToAndFromString : Test
convertingToAndFromString =
    describe "converting to and from string"
        [ fuzz Fuzz.avatar "converting to maybeString and comparing fromString should be the same" <|
            \fuzzAvatar ->
                case Avatar.toMaybeString fuzzAvatar of
                    Nothing ->
                        Expect.equal fuzzAvatar Avatar.empty

                    Just avatar ->
                        Avatar.fromString avatar
                            |> Expect.equal fuzzAvatar
        , fuzz Fuzz.string "creating with a string and analyzing toMaybeString should be the same" <|
            \fuzzString ->
                Avatar.fromString fuzzString
                    |> Avatar.toMaybeString
                    |> Expect.equal (Just fuzzString)
        ]
