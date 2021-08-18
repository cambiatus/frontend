module View.Form exposing (label, noGrammarly)

import Html exposing (Html, text)
import Html.Attributes exposing (class, for)


type alias LabelId =
    String


label : LabelId -> String -> Html a
label id_ labelText =
    Html.label
        [ class "label"
        , for id_
        ]
        [ text labelText ]


noGrammarly : Html.Attribute msg
noGrammarly =
    Html.Attributes.attribute "data-gramm_editor" "false"
