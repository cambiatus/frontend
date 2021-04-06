module View.Form.Input exposing (FieldType(..), init, input, toHtml, withAttrs, withCounter, withCounterAttrs, withCounterType, withElement, withErrorAttrs, withType)

{-| Creates a Cambiatus-style text input that supports error reporting, placeholders, localization
and character counters.

       View.Form.Input.init
           { label = "Username"
           , id = "username_input"
           , onInput = EnteredUsername
           , disabled = False
           , value = model.username
           , placeholder = "Enter your username"
           , problems = model.username\_problems
           , translators = shared.translators
           }
           |> View.Form.Input.withCounter 12
           |> View.Form.Input.toHtml

-}

import Html exposing (Html, div, input, li, text, ul)
import Html.Attributes exposing (class, classList, disabled, id, placeholder, value)
import Html.Events exposing (onInput)
import Session.Shared exposing (Translators)
import View.Form
import View.Form.InputCounter


{-| Required options for an input
-}
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


{-| Initializes an input
-}
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
    , extraAttrs = []
    , counterAttrs = []
    , extraElement = Nothing
    , errorAttrs = []
    , fieldType = Text
    , counterType = View.Form.InputCounter.CountLetters
    }


{-| Converts a Cambiatus input into Html to be used in view code
-}
toHtml : InputOptions a -> Html a
toHtml options =
    div [ class "mb-10 relative" ]
        [ View.Form.label options.id options.label
        , input options
        , case options.maximumCounterValue of
            Just number ->
                View.Form.InputCounter.viewWithAttrs options.translators.tr
                    number
                    options.value
                    options.counterAttrs
                    options.counterType

            Nothing ->
                text ""
        , ul []
            (options.problems
                |> Maybe.withDefault []
                |> List.map (viewFieldProblem options.errorAttrs)
            )
        ]


{-| Basic Cambiatus-style input
-}
input : InputOptions a -> Html a
input options =
    let
        inputElement =
            case options.fieldType of
                Text ->
                    Html.input

                TextArea ->
                    Html.textarea

        isInput =
            case options.fieldType of
                Text ->
                    True

                TextArea ->
                    False
    in
    Html.div [ class "relative" ]
        [ inputElement
            ([ id options.id
             , onInput options.onInput
             , class "min-w-full"
             , classList [ ( "input", isInput ) ]
             , disabled options.disabled
             , value options.value
             , placeholder (Maybe.withDefault "" options.placeholder)
             ]
                ++ options.extraAttrs
            )
            []
        , case options.extraElement of
            Nothing ->
                text ""

            Just element ->
                element
        ]


{-| Adds a character counter to your input. This does not limit the amount of characters automatically.
Validation must be done in your onInput handler.

For more information, see the InputCounter module

-}
withCounter : Int -> InputOptions a -> InputOptions a
withCounter maximum options =
    { options | maximumCounterValue = Just maximum }


withCounterAttrs : List (Html.Attribute a) -> InputOptions a -> InputOptions a
withCounterAttrs attrs options =
    { options | counterAttrs = attrs }


withErrorAttrs : List (Html.Attribute a) -> InputOptions a -> InputOptions a
withErrorAttrs attrs options =
    { options | errorAttrs = attrs }


withAttrs : List (Html.Attribute a) -> InputOptions a -> InputOptions a
withAttrs attrs options =
    { options | extraAttrs = attrs }


withElement : Html a -> InputOptions a -> InputOptions a
withElement element options =
    { options | extraElement = Just element }


withType : FieldType -> InputOptions a -> InputOptions a
withType fieldType options =
    { options | fieldType = fieldType }


withCounterType : View.Form.InputCounter.CounterType -> InputOptions a -> InputOptions a
withCounterType counterType options =
    { options | counterType = counterType }



--- INTERNAL


viewFieldProblem : List (Html.Attribute a) -> String -> Html a
viewFieldProblem attrs problem =
    li (class "form-error absolute mr-10" :: attrs) [ text problem ]


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
    , extraAttrs : List (Html.Attribute a)
    , counterAttrs : List (Html.Attribute a)
    , extraElement : Maybe (Html a)
    , errorAttrs : List (Html.Attribute a)
    , fieldType : FieldType
    , counterType : View.Form.InputCounter.CounterType
    }


type FieldType
    = Text
    | TextArea
