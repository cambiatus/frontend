module View.Form.InputCounter exposing (view, viewWithAttrs)

{-| Creates a Cambiatus-style input counter.
-}

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import I18Next


view : (String -> I18Next.Replacements -> String) -> Int -> String -> Html msg
view tr max str =
    viewWithAttrs tr max str []


viewWithAttrs : (String -> I18Next.Replacements -> String) -> Int -> String -> List (Html.Attribute msg) -> Html msg
viewWithAttrs tr max str attrs =
    div (class "input-counter" :: attrs)
        [ text <|
            tr "edit.input_counter"
                [ ( "current", String.fromInt <| String.length str )
                , ( "max", String.fromInt max )
                ]
        ]
