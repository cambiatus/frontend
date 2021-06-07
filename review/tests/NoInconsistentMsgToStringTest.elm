module NoInconsistentMsgToStringTest exposing (all)

import NoInconsistentMsgToString exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoInconsistentMsgToString"
        [ test "should report an error when `msgToString` returns a single-element list that differs from the `Msg`s name" <|
            \() ->
                """module A exposing (..)
type Msg
    = Apples
    | Bananas
msgToString : Msg -> List String
msgToString msg =
    case msg of
        Apples ->
            [ "Oranges" ]
        Bananas ->
            [ "Bananas" ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The result should start with `Apples`"
                            , details = [ "Make sure the string representation of the `Msg` is equal to that `Msg`s name" ]
                            , under = "\"Oranges\""
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
type Msg
    = Apples
    | Bananas
msgToString : Msg -> List String
msgToString msg =
    case msg of
        Apples ->
            [ "Apples" ]
        Bananas ->
            [ "Bananas" ]
"""
                        ]
        , test "should be successful when `msgToString` returns a single-element list that is equal to the `Msg`s name" <|
            \() ->
                """module A exposing (..)
type Msg
    = Apples
    | Bananas

msgToString : Msg -> List String
msgToString msg =
    case msg of
        Apples ->
            [ "Apples" ]

        Bananas ->
            [ "Bananas" ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when `msgToString` returns a multiple-element list in which the first element differs from the `Msg`s name " <|
            \() ->
                """module A exposing (..)
type Msg
    = Apples
    | Bananas
msgToString : Msg -> List String
msgToString msg =
    case msg of
        Apples ->
            "Oranges" :: [ "Strawberries", "Watermelons" ]
        Bananas ->
            [ "Bananas" ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The result should start with `Apples`"
                            , details = [ "Make sure the string representation of the `Msg` is equal to that `Msg`s name" ]
                            , under = "\"Oranges\""
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
type Msg
    = Apples
    | Bananas
msgToString : Msg -> List String
msgToString msg =
    case msg of
        Apples ->
            "Apples" :: [ "Strawberries", "Watermelons" ]
        Bananas ->
            [ "Bananas" ]
"""
                        ]
        , test "should be successful when `msgToString` returns a multiple-element list in which the first element is equal to the `Msg`s name " <|
            \() ->
                """module A exposing (..)
type Msg
    = Apples
    | Bananas
msgToString : Msg -> List String
msgToString msg =
    case msg of
        Apples ->
            "Apples" :: [ "Strawberries", "Watermelons" ]
        Bananas ->
            [ "Bananas" ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]
