module View.Form.Input exposing (init, input, toHtml, withCounter)

import Html exposing (Html, div, input, li, text, ul)
import Html.Attributes exposing (class, disabled, id, placeholder, value)
import Html.Events exposing (onInput)
import Session.Shared exposing (Translators)
import View.Form


{-| Required options for an input
-}
type alias InputOptions a =
    { label : String
    , id : String
    , onInput : String -> a
    , disabled : Bool
    , value : String
    , placeholder : Maybe String
    , problems : Maybe (List String)
    , translators : Translators
    , maximumCounterValue : Maybe Int
    }


type alias RequiredInputOptions a =
    { label : String
    , id : String
    , onInput : String -> a
    , disabled : Bool
    , value : String
    , placeholder : Maybe String
    , problems : Maybe (List String)
    , translators : Translators
    }


init : RequiredInputOptions a -> InputOptions a
init options =
    { label = options.label
    , id = options.id
    , onInput = options.onInput
    , disabled = options.disabled
    , value = options.value
    , placeholder = options.placeholder
    , problems = options.problems
    , maximumCounterValue = Nothing
    , translators = options.translators
    }


toHtml : InputOptions a -> Html a
toHtml options =
    div [ class "mb-10 relative" ]
        [ View.Form.label options.id options.label
        , input options
        , case options.maximumCounterValue of
            Just number ->
                div [ class "input-label pr-1 text-right text-purple-100 font-bold mt-1 absolute right-0" ]
                    [ text <|
                        options.translators.tr
                            "edit.input_counter"
                            [ ( "current", String.fromInt <| String.length options.value )
                            , ( "max", String.fromInt number )
                            ]
                    ]

            Nothing ->
                text ""
        , ul []
            (options.problems
                |> Maybe.withDefault []
                |> List.map viewFieldProblem
            )
        ]


input : InputOptions a -> Html a
input options =
    Html.input
        [ id options.id
        , onInput options.onInput
        , class "input min-w-full"
        , disabled options.disabled
        , value options.value
        , placeholder (Maybe.withDefault "" options.placeholder)
        ]
        []


withCounter : Int -> InputOptions a -> InputOptions a
withCounter maximum options =
    { options | maximumCounterValue = Just maximum }


viewFieldProblem : String -> Html a
viewFieldProblem problem =
    li [ class "form-error absolute mr-8" ] [ text problem ]
