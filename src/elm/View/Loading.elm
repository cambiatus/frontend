module View.Loading exposing (view)

import Html exposing (Html, div)
import Html.Attributes exposing (class)


view : String -> Html msg
view classes =
    div
        [ class ("spinner spinner--delay " ++ classes) ]
        []
