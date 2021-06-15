module Page.Register.Common exposing
    ( ProblemEvent(..)
    , containsLetters
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
import Html.Attributes exposing (classList)
import Maybe.Extra as MaybeExtra
import Regex
import Session.Shared exposing (Translators)
import Validate
import View.Form.Select


viewSelectField :
    { id : String
    , label : String
    , onInput : a -> msg
    , options : List (View.Form.Select.Option a)
    , value : a
    , valueToString : a -> String
    , enabled : Bool
    , problems : Maybe (List String)
    }
    -> Html msg
viewSelectField options =
    div [ classList [ ( "hidden", not options.enabled ) ] ]
        (case List.reverse options.options of
            [] ->
                []

            first :: rest ->
                [ View.Form.Select.init
                    { id = options.id
                    , label = options.label
                    , onInput = options.onInput
                    , firstOption = first
                    , value = options.value
                    , valueToString = options.valueToString
                    , disabled = not options.enabled
                    , problems = options.problems
                    }
                    |> View.Form.Select.withOptions rest
                    |> View.Form.Select.toHtml
                ]
        )


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
        (data
            >> Tuple.first
            >> String.isEmpty
            >> not
        )
        error


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
