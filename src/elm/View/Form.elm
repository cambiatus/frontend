module View.Form exposing (label)

import Html exposing (Html, span, text)
import Html.Attributes exposing (class, for)


label : String -> String -> Html a
label id_ labelText =
    Html.label
        [ class "block"
        , for id_
        ]
        [ span [ class "text-green tracking-wide uppercase text-caption block mb-1" ]
            [ text labelText ]
        ]
