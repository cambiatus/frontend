module View.Form.Select exposing
    ( Option
    , init
    , toHtml
    , withContainerAttrs
    , withOptions
    )

{- | Creates a Cambiatus-style dropdown

   View.Form.Select.init "country_select" "Country" EnteredCountry
       |> View.Form.Select.withOption { value = "brasil", label = "Brasil" }
       |> View.Form.Select.withOption { value = "costa_rica", label = "Costa Rica" }
       |> View.Form.Select.withOption { value = "argentina", label = "Argentina" }
       |> View.Form.Select.toHtml

-}

import Html exposing (Html, li, text, ul)
import Html.Attributes exposing (class, classList, disabled, selected, value)
import Html.Events exposing (onInput)
import View.Form


type alias RequiredOptions a msg =
    { id : String
    , label : String
    , onInput : a -> msg
    , firstOption : Option a
    , value : a
    , valueToString : a -> String
    , disabled : Bool
    , problems : Maybe (List String)
    }


{-| Initializes a Cambiatus-style dropdown
-}
init : RequiredOptions a msg -> Select a msg
init requiredOptions =
    { options = []
    , onInput = requiredOptions.onInput
    , id = requiredOptions.id
    , label = requiredOptions.label
    , value = requiredOptions.value
    , firstOption = requiredOptions.firstOption
    , valueToString = requiredOptions.valueToString
    , disabled = requiredOptions.disabled
    , containerAttrs = []
    , problems = requiredOptions.problems
    }


withContainerAttrs : List (Html.Attribute msg) -> Select a msg -> Select a msg
withContainerAttrs attrs select =
    { select | containerAttrs = select.containerAttrs ++ attrs }


{-| Adds a new option field to a dropdown

    View.Form.Select.withOption { value = "brasil", label = "Brasil" } mySelect

-}
withOption : Option a -> Select a msg -> Select a msg
withOption option select =
    { select | options = option :: select.options }


{-| Adds multiple options
-}
withOptions : List (Option a) -> Select a msg -> Select a msg
withOptions options select =
    List.foldr withOption select options


{-| Converts a Cambiatus-style dropdown into Html to be used in view code
-}
toHtml : Select a msg -> Html msg
toHtml select =
    let
        optionToHtml option =
            Html.option
                [ value (select.valueToString option.value)
                , selected (select.value == option.value)
                ]
                [ text option.label ]

        onInput_ inputString =
            case
                List.filter (\option -> select.valueToString option.value == inputString) select.options
                    |> List.head
            of
                Nothing ->
                    select.onInput select.firstOption.value

                Just selectedOption ->
                    select.onInput selectedOption.value
    in
    Html.div (class "mb-10" :: select.containerAttrs)
        [ if String.isEmpty select.label then
            text ""

          else
            View.Form.label [] select.id select.label
        , Html.select
            [ class "form-select w-full"
            , classList
                [ ( "bg-gray-500", select.disabled )
                , ( "with-error"
                  , select.problems
                        |> Maybe.map List.length
                        |> Maybe.withDefault 0
                        |> (\length -> length > 0)
                  )
                ]
            , onInput onInput_
            , disabled select.disabled
            ]
            (List.map optionToHtml (select.firstOption :: select.options))
        , ul []
            (select.problems
                |> Maybe.withDefault []
                |> List.map viewFieldProblem
            )
        ]



--- INTERNAL


type alias Select a msg =
    { options : List (Option a)
    , onInput : a -> msg
    , label : String
    , id : String
    , value : a
    , firstOption : Option a
    , valueToString : a -> String
    , disabled : Bool
    , containerAttrs : List (Html.Attribute msg)
    , problems : Maybe (List String)
    }


type alias Option a =
    { value : a
    , label : String
    }


viewFieldProblem : String -> Html msg
viewFieldProblem problem =
    li [ class "form-error absolute mr-8" ] [ text problem ]
