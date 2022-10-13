module Form.Text exposing
    ( init, Options
    , withPlaceholder, withElements, withCurrency, withCounter, Counter(..)
    , withDisabled, withCounterAttrs, withErrorAttrs, withExtraAttrs, withContainerAttrs, withInputContainerAttrs, withLabelAttrs
    , withType, asNumeric, withInputElement, InputType(..), InputElement(..)
    , withMask, withAllowedChars
    , getId, getErrorAttrs, getSubmitOnEnter
    , view
    )

{-| Creates a Cambiatus-style text input that supports error reporting,
placeholders, localization and character counters. Use it within a `Form.Form`:

    Form.Text.init
        { label = "Username"
        , id = "username-input"
        }
        |> Form.Text.withCounter (Form.Text.CountLetters 12)
        |> Form.textField


# Initializing

@docs init, Options


# Helpers


## Adding elements

@docs withPlaceholder, withElements, withCurrency, withCounter, Counter


## Adding attributes

@docs withDisabled, withCounterAttrs, withErrorAttrs, withExtraAttrs, withContainerAttrs, withInputContainerAttrs, withLabelAttrs


## Changing types

@docs withType, asNumeric, withInputElement, InputType, InputElement


## Masks

@docs withMask, withAllowedChars


# Getters

@docs getId, getErrorAttrs, getSubmitOnEnter


# View

@docs view

-}

import Eos
import Html exposing (Html, div)
import Html.Attributes exposing (class, classList, disabled, id, placeholder, required, type_)
import Html.Attributes.Aria exposing (ariaHidden)
import Html.Events as Events exposing (onInput)
import Mask
import Maybe.Extra
import Translation
import View.Components



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
        , extraElements : List (ViewConfig msg -> Html msg)
        , errorAttrs : List (Html.Attribute msg)
        , counterAttrs : List (Html.Attribute msg)
        , counter : Maybe Counter
        , type_ : InputType
        , inputElement : InputElement
        , beforeRenderingValue : String -> String
        , beforeChangeEvent : String -> String
        , mask : Maybe Mask
        }


{-| Initializes an input
-}
init : { label : String, id : String } -> Options msg
init { label, id } =
    Options
        { label = label
        , id = id
        , disabled = False
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
        , beforeRenderingValue = identity
        , beforeChangeEvent = identity
        , mask = Nothing
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
    | Url
    | Time
    | Password


{-| Determines which element to render the input as
-}
type InputElement
    = TextInput
    | TextareaInput { submitOnEnter : Bool }


{-| A mask formats the input as the user writes in it. A common place to find
them is on phone inputs. We can also use them to limit inputs that are meant to
only receive numbers. In that case, we can limit the number of decimal digits. A
NumberMask is already applied on symbol/currency inputs, based on that symbol's
precision.
-}
type Mask
    = NumberMask Mask.DecimalDigits
    | StringMask



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
            , beforeChangeEvent = beforeChangeEvent >> options.beforeChangeEvent
        }
        |> addMaxlength


{-| Adds a regular string mask to the input
-}
withMask : { mask : String, replace : Char } -> Options msg -> Options msg
withMask mask (Options options) =
    Options
        { options
            | beforeChangeEvent = Mask.string mask >> options.beforeChangeEvent
            , beforeRenderingValue = Mask.string mask
            , mask = Just StringMask
        }
        |> withElements
            [ Html.node "masked-input-helper"
                [ Html.Attributes.attribute "target-id" options.id
                , Html.Attributes.attribute "mask-type" "string"
                ]
                []
            ]


{-| Restrict the input to only allow certain characters. Useful for e.g. phone
fields, where the user is only allowed to enter numbers.

Always think if this is going to provide good UX: users can be confused if what
they type doesn't show up on the screen - they might think their keyboard is
broken!

-}
withAllowedChars : (Char -> Bool) -> Options msg -> Options msg
withAllowedChars isAllowed (Options options) =
    Options { options | beforeChangeEvent = String.filter isAllowed >> options.beforeChangeEvent }


{-| Adds a number mask to the input
-}
withNumberMask : Mask.DecimalDigits -> Options msg -> Options msg
withNumberMask decimalDigits (Options options) =
    -- We can't wire up `beforeChangeEvent` and `beforeRenderingValue` here
    -- because we need translators (to determine the digits separators) for that
    Options { options | mask = Just (NumberMask decimalDigits) }


{-| Adds an element to the input, so we can have elements inside the input

**Note**: the element isn't inside the input by default. You should use the
`absolute` class, along with other classes you may need to position the element

-}
withElements : List (Html msg) -> Options msg -> Options msg
withElements elements (Options options) =
    Options
        { options
            | extraElements =
                options.extraElements
                    ++ List.map always elements
        }


{-| Same as `withElements`, but peek into a `ViewConfig` before rendering the
element
-}
withElementsWithConfig : List (ViewConfig msg -> Html msg) -> Options msg -> Options msg
withElementsWithConfig elements (Options options) =
    Options { options | extraElements = options.extraElements ++ elements }


{-| Displays the currency symbol in the input field

**Note**: this is purely visual, you should still validate to check if the number
is valid and has the symbol's precision

-}
withCurrency : Eos.Symbol -> Options msg -> Options msg
withCurrency symbol options =
    options
        |> withElementsWithConfig [ viewCurrencyElement symbol ]
        |> withExtraAttrs [ class "pr-20" ]
        |> withNumberMask (Mask.Precisely (Eos.getSymbolPrecision symbol))
        |> asNumeric



-- ADDING ATTRIBUTES


{-| Determines if the input should be disabled
-}
withDisabled : Bool -> Options msg -> Options msg
withDisabled disabled (Options options) =
    Options { options | disabled = disabled }


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


{-| Adds attributes to the element that contains everything
-}
withContainerAttrs : List (Html.Attribute msg) -> Options msg -> Options msg
withContainerAttrs attrs (Options options) =
    Options { options | containerAttrs = options.containerAttrs ++ attrs }


{-| Adds attributes to the element that contains the input
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


type alias ViewConfig msg =
    { onChange : String -> msg
    , onBlur : msg
    , value : String
    , error : Html msg
    , hasError : Bool
    , translators : Translation.Translators
    , isRequired : Bool
    }


view : Options msg -> ViewConfig msg -> Html msg
view (Options options) viewConfig =
    div (class "relative mb-10" :: options.containerAttrs)
        [ if String.isEmpty options.label then
            Html.text ""

          else
            View.Components.label options.labelAttrs
                { targetId = options.id, labelText = options.label }
        , viewInput (Options options) viewConfig
        , if not viewConfig.hasError && Maybe.Extra.isNothing options.counter then
            Html.text ""

          else
            div [ class "flex w-full px-1" ]
                [ viewConfig.error
                , case options.counter of
                    Nothing ->
                        Html.text ""

                    Just counter ->
                        let
                            ( currentLength, max ) =
                                case counter of
                                    CountLetters maxLetters ->
                                        ( String.length viewConfig.value, maxLetters )

                                    CountWords maxWords ->
                                        ( String.words viewConfig.value
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
                                viewConfig.translators.tr "edit.input_counter"
                                    [ ( "current", String.fromInt currentLength )
                                    , ( "max", String.fromInt max )
                                    ]
                            ]
                ]
        ]


viewInput : Options msg -> ViewConfig msg -> Html msg
viewInput (Options options) ({ onChange, value, hasError, onBlur, translators, isRequired } as viewConfig) =
    let
        ( inputElement, typeAttr ) =
            case options.inputElement of
                TextInput ->
                    ( Html.input, type_ (typeToString options.type_) )

                TextareaInput _ ->
                    ( Html.textarea, class "" )

        paddedTimeValue =
            case options.type_ of
                Time ->
                    case String.split ":" value of
                        [ hour, minute ] ->
                            -- We need to pad values with 0. From MDN:
                            -- The value of the time input is always in 24-hour
                            -- format that includes leading zeros: hh:mm
                            [ String.repeat (2 - String.length hour) "0" ++ hour
                            , String.repeat (2 - String.length minute) "0" ++ minute
                            ]
                                |> String.join ":"

                        _ ->
                            value

                _ ->
                    value

        ( beforeRenderingValue, beforeChangeEvent, maskHelper ) =
            case options.mask of
                Just (NumberMask decimalDigits) ->
                    let
                        decimalDigitsAmount =
                            case decimalDigits of
                                Mask.Precisely x ->
                                    x

                                Mask.AtMost x ->
                                    x

                        separators =
                            Translation.decimalSeparators translators

                        previousDecimalSeparator =
                            if decimalDigitsAmount == 0 then
                                separators.decimalSeparator

                            else
                                value
                                    |> String.dropRight decimalDigitsAmount
                                    |> String.right 1

                        valueWithoutSeparator =
                            String.filter (\char -> Char.isDigit char || String.fromChar char == previousDecimalSeparator || char == '-')
                                >> String.replace previousDecimalSeparator "."
                    in
                    ( \v ->
                        valueWithoutSeparator v
                            |> Mask.floatString decimalDigits separators
                            |> Maybe.withDefault (valueWithoutSeparator v)
                    , \v ->
                        Mask.updateFloatString decimalDigits
                            separators
                            { previousValue = value, newValue = v }
                    , Html.node "masked-input-helper"
                        [ Html.Attributes.attribute "target-id" options.id
                        , Html.Attributes.attribute "mask-type" "number"
                        , Html.Attributes.attribute "decimal-separator" separators.decimalSeparator
                        ]
                        []
                    )

                _ ->
                    ( options.beforeRenderingValue
                    , options.beforeChangeEvent
                    , Html.text ""
                    )
    in
    div (class "relative" :: options.inputContainerAttrs)
        (inputElement
            (id options.id
                :: onInput (beforeChangeEvent >> onChange)
                :: Events.onBlur onBlur
                :: class "w-full input peer"
                :: classList [ ( "with-error", hasError ) ]
                :: disabled options.disabled
                :: Html.Attributes.value (beforeRenderingValue paddedTimeValue)
                :: placeholder (Maybe.withDefault "" options.placeholder)
                :: typeAttr
                :: required isRequired
                :: options.extraAttrs
            )
            []
            :: maskHelper
            :: List.map (\element -> element viewConfig) options.extraElements
        )



-- GETTERS


getId : Options msg -> String
getId (Options options) =
    options.id


getErrorAttrs : Options msg -> List (Html.Attribute msg)
getErrorAttrs (Options options) =
    options.errorAttrs


getSubmitOnEnter : Options msg -> Bool
getSubmitOnEnter (Options options) =
    case options.inputElement of
        TextareaInput { submitOnEnter } ->
            submitOnEnter

        _ ->
            False



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

        Url ->
            "url"

        Time ->
            "time"

        Password ->
            "password"


viewCurrencyElement : Eos.Symbol -> ViewConfig msg -> Html msg
viewCurrencyElement symbol { hasError } =
    Html.span
        [ class
            """
            absolute right-0 inset-y-0 ml-px
            bg-purple-500 text-white uppercase
            px-4 flex items-center rounded-r-md
            border-r border-t border-b
            """
        , classList
            [ ( "border-red", hasError )
            , ( "border-gray-500 peer-focus:border-green", not hasError )
            ]
        , ariaHidden True
        ]
        [ Html.text <| Eos.symbolToSymbolCodeString symbol
        ]
