module View.Form.Select exposing (init, toHtml, withOption)

{- | Creates a Cambiatus-style dropdown

   View.Form.Select.init "country_select" "Country" EnteredCountry
       |> View.Form.Select.withOption { value = "brasil", label = "Brasil" }
       |> View.Form.Select.withOption { value = "costa_rica", label = "Costa Rica" }
       |> View.Form.Select.withOption { value = "argentina", label = "Argentina" }
       |> View.Form.Select.toHtml

-}

import Html exposing (Html, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onInput)
import View.Form


{-| Initializes a Cambiatus-style dropdown
-}
init : String -> String -> (String -> a) -> Select a
init id label onInput =
    { options = [], onInput = onInput, id = id, label = label }


{-| Adds a new option field to a dropdown

    View.Form.Select.withOption { value = "brasil", label = "Brasil" } mySelect

-}
withOption : Option -> Select a -> Select a
withOption option select =
    let
        html =
            Html.option [ value option.value ] [ text option.label ]
    in
    { select | options = html :: select.options }


{-| Converts a Cambiatus-style dropdown into Html to be used in view code
-}
toHtml : Select a -> Html a
toHtml select =
    Html.div []
        [ View.Form.label select.id select.label
        , Html.select [ class "form-select select w-full", onInput select.onInput ] select.options
        ]



--- INTERNAL


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
