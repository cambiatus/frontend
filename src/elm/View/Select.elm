module View.Select exposing (init, toHtml, withOption)

import Html exposing (Html, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onInput)
import View.Form


type alias Select a =
    { options : List (Html a)
    , onInput : String -> a
    , label : String
    , id : String
    }


type alias Option =
    { value : String
    , label : String
    }


init : String -> String -> (String -> a) -> Select a
init id label onInput =
    { options = [], onInput = onInput, id = id, label = label }


withOption : Option -> Select a -> Select a
withOption option select =
    let
        html =
            Html.option [ value option.value ] [ text option.label ]
    in
    { select | options = html :: select.options }


toHtml : Select a -> Html a
toHtml select =
    Html.div []
        [ View.Form.label select.id select.label
        , Html.select [ class "form-select select w-full", onInput select.onInput ] select.options
        ]
