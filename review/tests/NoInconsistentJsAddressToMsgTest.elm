module NoInconsistentJsAddressToMsgTest exposing (all)

import NoInconsistentJsAddressToMsg exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoInconsistentJsAddressToMsg"
        [ test "should report an error when a jsAddressToMsg case comparison starts with an invalid `Msg` in an expression with `::`" <|
            \() ->
                """module A exposing (..)

type Msg
    = Apples
    | Bananas

jsAddressToMsg : List String -> Encode.Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "Oranges" :: _ ->
            Nothing

        "Bananas" :: _ ->
            Nothing
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "There is no `Msg` called `Oranges`"
                            , details =
                                [ "The first element should be a valid `Msg` name"
                                , "Make sure this name represents a valid `Msg` constructor from this module"
                                , "Maybe you meant one of these:\n\n\tApples\n\tBananas\n"
                                ]
                            , under = "\"Oranges\""
                            }
                        ]
        , test "should be successful when every address is a valid `Msg` name in an expression with `::`" <|
            \() ->
                """module A exposing (..)

type Msg
    = Apples
    | Bananas

jsAddressToMsg : List String -> Encode.Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "Apples" :: _ ->
            Nothing

        "Bananas" :: _ ->
            Nothing
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when a jsAddressToMsg case comparison starts with an invalid `Msg` in a list expression" <|
            \() ->
                """module A exposing (..)

type Msg
    = Apples
    | Bananas

jsAddressToMsg : List String -> Encode.Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        [ "Oranges", "Strawberries" ] ->
            Nothing

        "Bananas" :: _ ->
            Nothing
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "There is no `Msg` called `Oranges`"
                            , details =
                                [ "The first element should be a valid `Msg` name"
                                , "Make sure this name represents a valid `Msg` constructor from this module"
                                , "Maybe you meant one of these:\n\n\tApples\n\tBananas\n"
                                ]
                            , under = "\"Oranges\""
                            }
                        ]
        , test "should be successful when every address is a valid `Msg` name in a list expression" <|
            \() ->
                """module A exposing (..)

type Msg
    = Apples
    | Bananas

jsAddressToMsg : List String -> Encode.Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        [ "Apples", "Strawberries" ] ->
            Nothing

        "Bananas" :: _ ->
            Nothing
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]
