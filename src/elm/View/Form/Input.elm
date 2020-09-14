module View.Form.Input exposing (init, input, toHtml, withCounter)

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
                View.Form.InputCounter.view options.translators.tr number options.value

            Nothing ->
                text ""
        , ul []
            (options.problems
                |> Maybe.withDefault []
                |> List.map viewFieldProblem
            )
        ]


{-| Basic Cambiatus-style input
-}
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


{-| Adds a character counter to your input. This does not limit the amount of characters automatically.
Validation must be done in your onInput handler.

For more information, see the InputCounter module

-}
withCounter : Int -> InputOptions a -> InputOptions a
withCounter maximum options =
    { options | maximumCounterValue = Just maximum }



--- INTERNAL


viewFieldProblem : String -> Html a
viewFieldProblem problem =
    li [ class "form-error absolute mr-8" ] [ text problem ]


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
