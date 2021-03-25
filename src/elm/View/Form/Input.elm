module View.Form.Input exposing (init, input, toHtml, toHtmlTextArea, withAttrs, withCounter, withElement)

{- | Creates a Cambiatus-style text input that supports error reporting, placeholders, localization
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
import Html.Attributes exposing (class, disabled, id, placeholder, value)
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


{-| Lists out the possible input types
-}
type InputType
    = Input
    | TextArea


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
    , extraElement = Nothing
    }


{-| Converts a Cambiatus input into Html to be used in view code
-}
toHtmlWith : InputType -> InputOptions a -> Html a
toHtmlWith inputType options =
    div [ class "mb-10 relative" ]
        [ View.Form.label options.id options.label
        , input inputType options
        , case options.maximumCounterValue of
            Just number ->
                View.Form.InputCounter.view options.translators.tr number options.value

            Nothing ->
                text ""
        , ul []
            (options.problems
                |> Maybe.withDefault []
                |> List.map viewFieldProblem
            )
        ]


{-| Converts a Cambiatus input into Html to be used in view code, using Html.input
-}
toHtml : InputOptions a -> Html a
toHtml =
    toHtmlWith Input


{-| Converts a Cambiatus input into Html to be used in view code, using Html.textarea
-}
toHtmlTextArea : InputOptions a -> Html a
toHtmlTextArea =
    toHtmlWith TextArea


{-| Basic Cambiatus-style input
-}
input : InputType -> InputOptions a -> Html a
input inputType options =
    let
        inputElement =
            case inputType of
                Input ->
                    Html.input

                TextArea ->
                    Html.textarea
    in
    div [ class "relative" ]
        [ inputElement
            ([ id options.id
             , onInput options.onInput
             , class "input min-w-full relative"
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

            Just extraElement ->
                div [ class "absolute inset-y-0 right-0 flex items-center pr-3" ] [ extraElement ]
        ]


{-| Adds a character counter to your input. This does not limit the amount of characters automatically.
Validation must be done in your onInput handler.

For more information, see the InputCounter module

-}
withCounter : Int -> InputOptions a -> InputOptions a
withCounter maximum options =
    { options | maximumCounterValue = Just maximum }


withAttrs : List (Html.Attribute a) -> InputOptions a -> InputOptions a
withAttrs attrs options =
    { options | extraAttrs = attrs }


{-| Adds an HTML element to be displayed on the right side of the input
-}
withElement : Html a -> InputOptions a -> InputOptions a
withElement element options =
    { options | extraElement = Just element }



--- INTERNAL


viewFieldProblem : String -> Html a
viewFieldProblem problem =
    li [ class "form-error absolute mr-10" ] [ text problem ]


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
    , extraElement : Maybe (Html a)
    }
