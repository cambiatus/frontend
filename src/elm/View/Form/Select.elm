module View.Form.Select exposing (init, toHtml, withOption)

{-| Creates a Cambiatus-style dropdown

    View.Form.Select.init "country_select" "Country" EnteredCountry
        |> View.Form.Select.withOption { value = "brasil", label = "Brasil" }
        |> View.Form.Select.withOption { value = "costa_rica", label = "Costa Rica" }
        |> View.Form.Select.withOption { value = "argentina", label = "Argentina" }
        |> View.Form.Select.toHtml

-}

import Html exposing (Html, text)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (onInput)
import View.Form


{-| Initializes a Cambiatus-style dropdown
-}
init : String -> String -> (String -> a) -> String -> Select a
init id label onInput value =
    { options = [], onInput = onInput, id = id, label = label, value = value }


{-| Adds a new option field to a dropdown

    View.Form.Select.withOption { value = "brasil", label = "Brasil" } mySelect

-}
withOption : Option -> Select a -> Select a
withOption option select =
    let
        html =
            Html.option
                [ value option.value
                , selected
                    (if select.value == option.value then
                        True

                     else
                        False
                    )
                ]
                [ text option.label ]
    in
    { select | options = html :: select.options }


{-| Converts a Cambiatus-style dropdown into Html to be used in view code
-}
toHtml : Select a -> Html a
toHtml select =
    Html.div [ class "mb-10" ]
        [ View.Form.label select.id select.label
        , Html.select [ class "form-select select w-full", onInput select.onInput ] select.options
        ]



--- INTERNAL


type alias Select a =
    { options : List (Html a)
    , onInput : String -> a
    , label : String
    , id : String
    , value : String
    }


type alias Option =
    { value : String
    , label : String
    }
