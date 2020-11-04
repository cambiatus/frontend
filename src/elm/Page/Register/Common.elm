module Page.Register.Common exposing
    ( Errors(..)
    , ProblemEvent(..)
    , containsLetters
    , containsNumberGreaterThan
    , fieldProblems
    , findId
    , getCities
    , getDistricts
    , ifEmptyTuple
    , validateAccountName
    , viewSelectField
    )

import Address
import Cambiatus.Scalar exposing (Id(..))
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Maybe.Extra as MaybeExtra
import Regex
import Session.Shared exposing (Translators)
import Validate
import View.Form.Select


viewSelectField : String -> String -> Bool -> (String -> msg) -> List { value : String, label : String } -> Maybe (List String) -> Html msg
viewSelectField label initialValue enabled onInput options problems =
    let
        form =
            View.Form.Select.init "document_select" label onInput initialValue problems
    in
    div
        (if enabled then
            []

         else
            [ class "hidden" ]
        )
        [ List.foldl View.Form.Select.withOption form options
            |> (if enabled then
                    View.Form.Select.enable

                else
                    View.Form.Select.disable
               )
            |> View.Form.Select.toHtml
        ]


type ProblemEvent
    = OnInput
    | OnSubmit


fieldProblems : a -> List ( a, String, ProblemEvent ) -> Maybe (List String)
fieldProblems field problems =
    let
        list =
            problems
                |> List.filter (\( f, _, _ ) -> f == field)
                |> List.map (\( _, msg, _ ) -> msg)
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


getCities : List Address.State -> String -> Translators -> List Address.City
getCities states selectedState translators =
    let
        foundState =
            List.head (List.filter (\state -> state.name == selectedState) states)
    in
    foundState
        |> Maybe.map (\state -> state.cities)
        |> Maybe.withDefault []
        |> (::) (Address.City (Id "") (translators.t "register.form.select.city") [])
        |> List.sortWith byId


getDistricts : List Address.City -> String -> Translators -> List Address.Neighborhood
getDistricts cities selectedCity translators =
    let
        foundState =
            List.head (List.filter (\city -> city.name == selectedCity) cities)
    in
    foundState
        |> Maybe.map (\city -> city.neighborhoods)
        |> Maybe.withDefault []
        |> (::) (Address.Neighborhood (Id "") (translators.t "register.form.select.district"))
        |> List.sortWith byId


byId : { a | id : Id } -> { b | id : Id } -> Order
byId a b =
    let
        getId (Id id) =
            id
    in
    case compare (getId a.id) (getId b.id) of
        LT ->
            GT

        GT ->
            LT

        EQ ->
            EQ


containsNumberGreaterThan : Int -> String -> Bool
containsNumberGreaterThan number str =
    str
        |> String.split ""
        |> List.filterMap String.toInt
        |> List.filter (\x -> x < 1 || x > number)
        |> List.length
        |> (/=) 0


containsLetters : String -> Bool
containsLetters str =
    if String.length str == 0 then
        False

    else
        str
            |> String.toInt
            |> MaybeExtra.isNothing


ifEmptyTuple : (a -> ( String, b )) -> c -> Validate.Validator c a
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


validateAccountName : Translators -> String -> String -> ( String, Maybe String )
validateAccountName { tr } enteredAccountName currentAccountName =
    let
        preparedAccountName =
            enteredAccountName
                |> String.trim
                |> String.toLower
                |> String.left 12

        validAccountName : Regex.Regex
        validAccountName =
            Maybe.withDefault Regex.never <|
                Regex.fromString "^[a-z1-5]{0,12}$"

        isAccountNameValid : Bool
        isAccountNameValid =
            preparedAccountName
                |> Regex.contains validAccountName
    in
    if isAccountNameValid then
        ( preparedAccountName
        , Nothing
        )

    else
        let
            invalidSymbol =
                String.right 1 preparedAccountName
        in
        -- Leave account name unchanged if there's an error:
        ( currentAccountName
        , Just <| tr "error.notAllowedChar" [ ( "char", invalidSymbol ) ]
        )
