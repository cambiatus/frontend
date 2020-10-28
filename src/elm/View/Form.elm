module View.Form exposing (label, noGrammarly, primaryLabel)

import Html exposing (Html, span, text)
import Html.Attributes exposing (class, for)


type alias LabelId =
    String


primaryLabel : LabelId -> String -> Html a
primaryLabel id_ labelText =
    Html.label
        [ class "block"
        , for id_
        ]
        [ span [ class "text-green tracking-wide text-sm block mb-1" ]
            [ text labelText ]
        ]


label : LabelId -> String -> Html a
label id_ labelText =
    Html.label
        [ class "block"
        , for id_
        ]
        [ span [ class "text-green tracking-wide uppercase text-caption block mb-1" ]
            [ text labelText ]
        ]


noGrammarly : Html.Attribute msg
noGrammarly =
    Html.Attributes.attribute "data-gramm_editor" "false"
