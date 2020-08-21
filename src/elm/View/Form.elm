module View.Form exposing (input, label)

import Html exposing (Html, div, input, li, span, text, ul)
import Html.Attributes exposing (class, disabled, for, id, placeholder, value)
import Html.Events exposing (onInput)
import Session.Shared exposing (Translators)


type alias InputOptions a =
    { label : String
    , id : String
    , onInput : String -> a
    , disabled : Bool
    , value : String
    , placeholder : Maybe String
    , problems : Maybe (List String)
    , maximumCounterValue : Maybe Int
    }


label : String -> String -> Html a
label id_ labelText =
    Html.label
        [ class "block"
        , for id_
        ]
        [ span [ class "text-green tracking-wide uppercase text-caption block mb-1" ]
            [ text labelText ]
        ]


input : Translators -> InputOptions a -> Html a
input { tr } options =
    div [ class "mb-10 relative" ]
        [ label options.id options.label
        , Html.input
            [ id options.id
            , onInput options.onInput
            , class "input min-w-full"
            , disabled options.disabled
            , value options.value
            , placeholder (Maybe.withDefault "" options.placeholder)
            ]
            []
        , case options.maximumCounterValue of
            Just number ->
                div [ class "input-label pr-1 text-right text-purple-100 font-bold mt-1 absolute right-0" ]
                    [ text <|
                        tr
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


viewFieldProblem : String -> Html a
viewFieldProblem problem =
    li [ class "form-error absolute mr-8" ] [ text problem ]
