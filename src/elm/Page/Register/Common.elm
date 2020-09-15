module Page.Register.Common exposing (Errors(..), fieldProblems, findId, getCities, getDistricts, ifEmptyTuple, viewSelectField, viewTitleForStep)

import Address
import Cambiatus.Scalar exposing (Id(..))
import Cambiatus.ScalarCodecs
import Html exposing (Html, p, strong, text)
import Html.Attributes exposing (class)
import Session.Shared exposing (Translators)
import Validate exposing (Validator)
import View.Form.Select


viewSelectField : String -> String -> Bool -> (String -> msg) -> List { value : String, label : String } -> Translators -> Html msg
viewSelectField label initialValue enabled onInput options _ =
    let
        form =
            View.Form.Select.init "document_select" label onInput initialValue
    in
    List.foldl View.Form.Select.withOption form options
        |> (if enabled then
                View.Form.Select.enable

            else
                View.Form.Select.disable
           )
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


findId : String -> List { a | id : Id, name : String } -> ( String, String )
findId str list =
    let
        getId (Id id) =
            id
    in
    ( list
        |> List.filter (\x -> x.name == str)
        |> List.map (\x -> getId x.id)
        |> List.head
        |> Maybe.withDefault ""
    , str
    )


getCities : List Address.State -> String -> List Address.City
getCities states selectedState =
    let
        foundState =
            List.head (List.filter (\state -> state.name == selectedState) states)
    in
    foundState
        |> Maybe.map (\state -> state.cities)
        |> Maybe.withDefault []


getDistricts : List Address.City -> String -> List Address.Neighborhood
getDistricts cities selectedCity =
    let
        foundState =
            List.head (List.filter (\city -> city.name == selectedCity) cities)
    in
    foundState
        |> Maybe.map (\city -> city.neighborhoods)
        |> Maybe.withDefault []


ifEmptyTuple data error =
    Validate.ifFalse
        (\subject ->
            if Tuple.first (data subject) == "" then
                False

            else
                True
        )
        error


type Errors
    = InvalidField
