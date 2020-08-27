module Page.Register.Common exposing (Errors(..), fieldProblems, viewSelectField, viewTitleForStep)

import Html exposing (Html, p, strong, text)
import Html.Attributes exposing (class)
import Session.Shared exposing (Translators)
import View.Form.Input
import View.Form.Select


viewSelectField : String -> String -> (String -> msg) -> List { value : String, label : String } -> Translators -> Html msg
viewSelectField label initialValue onInput options translators =
    let
        form =
            View.Form.Select.init "document_select" label onInput initialValue
    in
    List.foldl View.Form.Select.withOption form options
        |> View.Form.Select.toHtml


viewTitleForStep : Translators -> Int -> Html msg
viewTitleForStep translators s =
    let
        { t, tr } =
            translators

        step =
            String.fromInt s
    in
    p
        [ class "py-4 mb-4 text-body border-b border-dotted text-grey border-grey-500" ]
        [ text (tr "register.form.step" [ ( "stepNum", step ) ])
        , text " / "
        , strong
            [ class <|
                if s == 1 then
                    "text-black"

                else
                    "text-white"
            ]
            [ text <| t ("register.form.step" ++ step ++ "_title") ]
        ]


fieldProblems : a -> List ( a, String ) -> Maybe (List String)
fieldProblems field problems =
    let
        list =
            problems
                |> List.filter (\x -> Tuple.first x == field)
                |> List.map (\x -> Tuple.second x)
    in
    if List.length list > 0 then
        Just list

    else
        Nothing


type Errors
    = InvalidField
