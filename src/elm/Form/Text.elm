module Form.Text exposing
    ( init, Options
    , withPlaceholder, withElements, withCurrency, withCounter, Counter(..)
    , withCounterAttrs, withErrorAttrs, withExtraAttrs, withContainerAttrs, withInputContainerAttrs, withLabelAttrs
    , withType, asNumeric, withInputElement, InputType(..), InputElement(..)
    , getId, getErrorAttrs
    , view
    )

{-| Creates a Cambiatus-style text input that supports error reporting,
placeholders, localization and character counters. Use it within a `Form.Form`:

    Form.Text.init
        { label = "Username"
        , id = "username-input"
        , disabled = False
        }
        |> Form.Text.withCounter (Form.Text.CountLetters 12)
        |> Form.textField


# Initializing

@docs init, Options


# Helpers


## Adding elements

@docs withPlaceholder, withElements, withCurrency, withCounter, Counter


## Adding attributes

@docs withCounterAttrs, withErrorAttrs, withExtraAttrs, withContainerAttrs, withInputContainerAttrs, withLabelAttrs


## Changing types

@docs withType, asNumeric, withInputElement, InputType, InputElement


## Masks

-- TODO - Add Masks


# Getters

@docs getId, getErrorAttrs


# View

@docs view

-}

import Eos
import Html exposing (Html, div)
import Html.Attributes exposing (class, classList, disabled, id, placeholder, type_)
import Html.Events as Events exposing (onInput)
import Session.Shared as Shared
import View.Form



-- OPTIONS


type Options msg
    = Options
        { label : String
        , id : String
        , disabled : Bool
        , placeholder : Maybe String
        , labelAttrs : List (Html.Attribute msg)
        , extraAttrs : List (Html.Attribute msg)
        , containerAttrs : List (Html.Attribute msg)
        , inputContainerAttrs : List (Html.Attribute msg)
        , extraElements : List (Html msg)
        , errorAttrs : List (Html.Attribute msg)
        , counterAttrs : List (Html.Attribute msg)
        , counter : Maybe Counter
        , type_ : InputType
        , inputElement : InputElement
        , beforeChangeEvent : String -> String
        }


{-| Initializes an input
-}
init : { label : String, id : String, disabled : Bool } -> Options msg
init { label, id, disabled } =
    Options
        { label = label
        , id = id
        , disabled = disabled
        , placeholder = Nothing
        , labelAttrs = []
        , extraAttrs = []
        , containerAttrs = []
        , inputContainerAttrs = []
        , errorAttrs = []
        , counterAttrs = []
        , extraElements = []
        , counter = Nothing
        , type_ = Text
        , inputElement = TextInput
        , beforeChangeEvent = identity
        }


{-| Determines the counting strategy and the amount it should count up to
-}
type Counter
    = CountLetters Int
    | CountWords Int


{-| Determines the `type_` attribute of the input
-}
type InputType
    = Text
    | Telephone
    | Number


{-| Determines which element to render the input as
-}
type InputElement
    = TextInput
    | TextareaInput



-- ADDING ELEMENTS


{-| Adds a placeholder to the input
-}
withPlaceholder : String -> Options msg -> Options msg
withPlaceholder placeholder (Options options) =
    Options { options | placeholder = Just placeholder }


{-| Adds a character counter to your input. Limits the amount of characters or
words automatically
-}
withCounter : Counter -> Options msg -> Options msg
withCounter counter (Options options) =
    let
        ( addMaxlength, beforeChangeEvent ) =
            case counter of
                CountLetters maxLetters ->
                    ( withExtraAttrs [ Html.Attributes.maxlength maxLetters ]
                    , String.left maxLetters
                    )

                CountWords maxWords ->
                    ( identity
                    , \value ->
                        if List.length (String.words value) >= maxWords then
                            value
                                |> String.words
                                |> List.take maxWords
                                |> String.join " "

                        else
                            value
                    )
    in
    Options
        { options
            | counter = Just counter
            , beforeChangeEvent = beforeChangeEvent
        }
        |> addMaxlength


{-| Adds an element to the input, so we can have elements inside the input

**Note**: the element isn't inside the input by default. You should use the
`absolute` class, along with other classes you may need to position the element

-}
withElements : List (Html msg) -> Options msg -> Options msg
withElements elements (Options options) =
    Options { options | extraElements = options.extraElements ++ elements }


{-| Displays the currency symbol in the input field

**Note**: this is purely visual, you should still validate to check if the number
is valid and has the symbol's precision

-}
withCurrency : Eos.Symbol -> Options msg -> Options msg
withCurrency symbol options =
    options
        |> withElements [ viewCurrencyElement symbol ]
        |> withExtraAttrs [ class "pr-20" ]
        -- TODO - Add number mask
        -- |> withNumberMask (Mask.Precisely (Eos.getSymbolPrecision symbol))
        |> asNumeric



-- ADDING ATTRIBUTES


{-| Adds attributes to the label
-}
withLabelAttrs : List (Html.Attribute msg) -> Options msg -> Options msg
withLabelAttrs attrs (Options options) =
    Options { options | labelAttrs = options.labelAttrs ++ attrs }


{-| Adds attributes to the input field
-}
withExtraAttrs : List (Html.Attribute msg) -> Options msg -> Options msg
withExtraAttrs attrs (Options options) =
    Options { options | extraAttrs = options.extraAttrs ++ attrs }


{-| Adds attributes to the element that contains everything else
-}
withContainerAttrs : List (Html.Attribute msg) -> Options msg -> Options msg
withContainerAttrs attrs (Options options) =
    Options { options | containerAttrs = options.containerAttrs ++ attrs }


{-| Adds attributes to the element that holds the input
-}
withInputContainerAttrs : List (Html.Attribute msg) -> Options msg -> Options msg
withInputContainerAttrs attrs (Options options) =
    Options { options | inputContainerAttrs = options.inputContainerAttrs ++ attrs }


{-| Adds attributes to the field error
-}
withErrorAttrs : List (Html.Attribute msg) -> Options msg -> Options msg
withErrorAttrs attrs (Options options) =
    Options { options | errorAttrs = options.errorAttrs ++ attrs }


{-| Adds attributes to the counter
-}
withCounterAttrs : List (Html.Attribute msg) -> Options msg -> Options msg
withCounterAttrs attrs (Options options) =
    Options { options | counterAttrs = options.counterAttrs ++ attrs }



-- CHANGING TYPES


{-| Determines the `type_` of the input
-}
withType : InputType -> Options msg -> Options msg
withType type_ (Options options) =
    Options { options | type_ = type_ }


{-| Determines what element to render as the input
-}
withInputElement : InputElement -> Options msg -> Options msg
withInputElement inputElement (Options options) =
    Options { options | inputElement = inputElement }


{-| Defines the input as a numeric input
-}
asNumeric : Options msg -> Options msg
asNumeric =
    withExtraAttrs [ Html.Attributes.attribute "inputmode" "numeric" ]



-- VIEW


view :
    Options msg
    ->
        { onChange : String -> msg
        , onBlur : String -> msg
        , value : String
        , error : Html msg
        , hasError : Bool
        , translators : Shared.Translators
        }
    -> Html msg
view (Options options) state =
    div (class "relative mb-10" :: options.containerAttrs)
        [ if String.isEmpty options.label then
            Html.text ""

          else
            View.Form.label options.labelAttrs options.id options.label
        , viewInput (Options options) state
        , div [ class "flex w-full px-1" ]
            [ state.error
            , case options.counter of
                Nothing ->
                    Html.text ""

                Just counter ->
                    let
                        ( currentLength, max ) =
                            case counter of
                                CountLetters maxLetters ->
                                    ( String.length state.value, maxLetters )

                                CountWords maxWords ->
                                    ( String.words state.value
                                        |> List.filter (not << String.isEmpty)
                                        |> List.length
                                    , maxWords
                                    )
                    in
                    Html.p
                        (class "text-purple-100 mt-2 ml-auto uppercase font-bold text-sm flex-shrink-0"
                            :: options.counterAttrs
                        )
                        [ Html.text <|
                            state.translators.tr "edit.input_counter"
                                [ ( "current", String.fromInt currentLength )
                                , ( "max", String.fromInt max )
                                ]
                        ]
            ]
        ]


viewInput :
    Options msg
    ->
        { state
            | onChange : String -> msg
            , onBlur : String -> msg
            , value : String
            , hasError : Bool
        }
    -> Html msg
viewInput (Options options) { onChange, value, hasError, onBlur } =
    let
        ( inputElement, inputClass, typeAttr ) =
            case options.inputElement of
                TextInput ->
                    ( Html.input, "input", type_ (typeToString options.type_) )

                TextareaInput ->
                    ( Html.textarea, "form-input", class "" )
    in
    div (class "relative" :: options.inputContainerAttrs)
        (inputElement
            (id options.id
                :: onInput (options.beforeChangeEvent >> onChange)
                :: Events.onBlur (onBlur options.id)
                :: class ("w-full " ++ inputClass)
                :: classList [ ( "with-error", hasError ) ]
                :: disabled options.disabled
                :: Html.Attributes.value value
                :: placeholder (Maybe.withDefault "" options.placeholder)
                :: typeAttr
                :: options.extraAttrs
            )
            []
            :: options.extraElements
        )



-- GETTERS


getId : Options msg -> String
getId (Options options) =
    options.id


getErrorAttrs : Options msg -> List (Html.Attribute msg)
getErrorAttrs (Options options) =
    options.errorAttrs



-- INTERNAL


typeToString : InputType -> String
typeToString type_ =
    case type_ of
        Text ->
            "text"

        Telephone ->
            "tel"

        Number ->
            "number"


viewCurrencyElement : Eos.Symbol -> Html a
viewCurrencyElement symbol =
    Html.span [ class "absolute right-0 rounded-r-sm border-transparent border bg-purple-500 uppercase inset-y-0 px-4 text-white flex items-center" ]
        [ Html.text <| Eos.symbolToSymbolCodeString symbol ]
