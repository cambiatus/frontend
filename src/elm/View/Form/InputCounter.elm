module View.Form.InputCounter exposing (CounterType(..), viewWithAttrs)

{-| Creates a Cambiatus-style input counter.
-}

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import I18Next


viewWithAttrs : (String -> I18Next.Replacements -> String) -> Int -> String -> List (Html.Attribute msg) -> CounterType -> Html msg
viewWithAttrs tr max str attrs counterType =
    let
        currentLength =
            case counterType of
                CountLetters ->
                    String.length str

                CountWords ->
                    String.words str
                        |> List.filter (not << String.isEmpty)
                        |> List.length
    in
    div (class "input-counter" :: attrs)
        [ text <|
            tr "edit.input_counter"
                [ ( "current", String.fromInt currentLength )
                , ( "max", String.fromInt max )
                ]
        ]


type CounterType
    = CountLetters
    | CountWords
