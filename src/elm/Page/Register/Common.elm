module Page.Register.Common exposing (documentInput, viewSelectField, viewTitleForStep)

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


documentInput : Translators -> (String -> msg) -> String -> String -> String -> Html msg
documentInput translators onInput value documentType formTranslationString =
    let
        selectedDocumentTranslationString =
            formTranslationString ++ documentType
    in
    View.Form.Input.init
        { id = "document"
        , label = translators.t (selectedDocumentTranslationString ++ ".label")
        , onInput = onInput
        , disabled = False
        , value = value
        , placeholder = Just (translators.t (selectedDocumentTranslationString ++ ".placeholder"))
        , problems = Nothing
        , translators = translators
        }
        |> View.Form.Input.withCounter
            (selectedDocumentTranslationString
                ++ ".maximum"
                |> translators.t
                |> String.toInt
                |> Maybe.withDefault 10
            )
        |> View.Form.Input.toHtml
