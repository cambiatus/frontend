module View.Form.Input exposing
    ( init
    , withCounter, withElements, withCurrency
    , withCounterAttrs, withErrorAttrs, withAttrs, withContainerAttrs, withInputContainerAttrs, withLabelAttrs
    , withInputType, withType, withCounterType, asNumeric
    , withMask
    , toHtml
    , CounterType(..), FieldType(..), InputType(..)
    )

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


# Initializing

@docs init


# Helpers


## Adding elements

@docs withCounter, withElements, withCurrency


## Adding attributes

@docs withCounterAttrs, withErrorAttrs, withAttrs, withContainerAttrs, withInputContainerAttrs, withLabelAttrs


## Changing types

@docs withInputType, withType, withCounterType, asNumeric


## Masks

@docs withMask


# Converting to HTML

@docs toHtml

-}

import Eos
import Html exposing (Html, div, li, span, text, ul)
import Html.Attributes exposing (attribute, class, classList, disabled, id, placeholder, type_, value)
import Html.Events exposing (onInput)
import I18Next
import Mask
import Session.Shared as Shared exposing (Translators)
import View.Form


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
    , labelAttrs = []
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
    , containerAttrs = []
    , inputContainerAttrs = []
    , extraElements = []
    , errorAttrs = []
    , inputType = Input
    , fieldType = Text
    , counterType = CountLetters
    , mask = Nothing
    }


{-| Converts a Cambiatus input into Html to be used in view code
-}
toHtml : InputOptions a -> Html a
toHtml options =
    div
        (class "relative mb-10"
            :: options.containerAttrs
        )
        [ if String.isEmpty options.label then
            text ""

          else
            View.Form.label options.labelAttrs options.id options.label
        , input options
        , div [ class "flex w-full px-1" ]
            [ ul [ class "inline-block mr-auto" ]
                (options.problems
                    |> Maybe.withDefault []
                    |> List.map (viewFieldProblem options.errorAttrs)
                )
            , case options.maximumCounterValue of
                Just number ->
                    inputCounterviewWithAttrs options.translators.tr
                        number
                        options.value
                        options.counterAttrs
                        options.counterType

                Nothing ->
                    text ""
            ]
        ]


{-| Basic Cambiatus-style input
-}
input : InputOptions a -> Html a
input options =
    let
        ( inputElement, inputClass, typeAttr ) =
            case options.inputType of
                Input ->
                    ( Html.input, "input", type_ (fieldTypeToString options.fieldType) )

                TextArea ->
                    ( Html.textarea, "form-input", class "" )

        beforeInputFunction =
            case options.mask of
                Nothing ->
                    identity

                Just (StringMask mask) ->
                    Mask.string mask

                Just (NumberMask decimalDigits) ->
                    \v ->
                        Mask.updateFloatString decimalDigits
                            (Shared.decimalSeparators options.translators)
                            { previousValue = options.value, newValue = v }
    in
    div (class "relative" :: options.inputContainerAttrs)
        (inputElement
            (id options.id
                :: onInput (beforeInputFunction >> options.onInput)
                :: class ("w-full " ++ inputClass)
                :: classList [ ( "with-error", hasErrors options ) ]
                :: disabled options.disabled
                :: value options.value
                :: placeholder (Maybe.withDefault "" options.placeholder)
                :: typeAttr
                :: options.extraAttrs
            )
            []
            :: options.extraElements
        )


{-| Adds a character counter to your input. This does not limit the amount of characters automatically.
Validation must be done in your onInput handler.

For more information, see the InputCounter module

-}
withCounter : Int -> InputOptions a -> InputOptions a
withCounter maximum options =
    { options | maximumCounterValue = Just maximum }


{-| Adds attributes to the label
-}
withLabelAttrs : List (Html.Attribute a) -> InputOptions a -> InputOptions a
withLabelAttrs attrs options =
    { options | labelAttrs = options.labelAttrs ++ attrs }


{-| Adds attributes to the counter
-}
withCounterAttrs : List (Html.Attribute a) -> InputOptions a -> InputOptions a
withCounterAttrs attrs options =
    { options | counterAttrs = options.counterAttrs ++ attrs }


{-| Adds attributes to the field error
-}
withErrorAttrs : List (Html.Attribute a) -> InputOptions a -> InputOptions a
withErrorAttrs attrs options =
    { options | errorAttrs = options.errorAttrs ++ attrs }


{-| Adds attributes to the input field
-}
withAttrs : List (Html.Attribute a) -> InputOptions a -> InputOptions a
withAttrs attrs options =
    { options | extraAttrs = options.extraAttrs ++ attrs }


{-| Adds attributes to the element that contains everything else
-}
withContainerAttrs : List (Html.Attribute a) -> InputOptions a -> InputOptions a
withContainerAttrs attrs options =
    { options | containerAttrs = options.containerAttrs ++ attrs }


{-| Adds attributes to the element that holds the input
-}
withInputContainerAttrs : List (Html.Attribute a) -> InputOptions a -> InputOptions a
withInputContainerAttrs attrs options =
    { options | inputContainerAttrs = options.inputContainerAttrs ++ attrs }


{-| Adds an element to the input, so we can have elements inside the input

**Note**: the element isn't inside the input by default. You should use the
`absolute` class, along with other classes you may need to position the element

-}
withElements : List (Html a) -> InputOptions a -> InputOptions a
withElements elements options =
    { options | extraElements = elements ++ options.extraElements }


{-| Displays the currency symbol in the input field

**Note**: this is purely visual, you should still validate to check if the number
is valid and has the symbol's precision

-}
withCurrency : Eos.Symbol -> InputOptions a -> InputOptions a
withCurrency symbol options =
    options
        |> withElements (viewCurrencyElement symbol :: options.extraElements)
        |> withAttrs [ class "pr-20" ]
        |> asNumeric
        |> withNumberMask (Mask.Precisely (Eos.getSymbolPrecision symbol))


{-| Determines the type of the input
-}
withInputType : InputType -> InputOptions a -> InputOptions a
withInputType inputType options =
    { options | inputType = inputType }


{-| Determines the `type_` of the input
-}
withType : FieldType -> InputOptions a -> InputOptions a
withType fieldType options =
    { options | fieldType = fieldType }


{-| Determines the counting strategy for the input
-}
withCounterType : CounterType -> InputOptions a -> InputOptions a
withCounterType counterType options =
    { options | counterType = counterType }


{-| Adds a regular string mask to the input
-}
withMask : { mask : String, replace : Char } -> InputOptions a -> InputOptions a
withMask mask options =
    { options
        | mask = Just (StringMask mask)
        , value = Mask.string mask options.value
    }
        |> withElements
            (Html.node "masked-input-helper"
                [ attribute "target-id" options.id
                , attribute "mask-type" "string"
                ]
                []
                :: options.extraElements
            )


{-| Adds a number mask to the input
-}
withNumberMask : Mask.DecimalDigits -> InputOptions a -> InputOptions a
withNumberMask mask options =
    let
        decimalDigitsAmount =
            case mask of
                Mask.Precisely x ->
                    x

                Mask.AtMost x ->
                    x

        separators =
            Shared.decimalSeparators options.translators

        previousDecimalSeparator =
            if decimalDigitsAmount == 0 then
                separators.decimalSeparator

            else
                options.value
                    |> String.dropRight decimalDigitsAmount
                    |> String.right 1

        valueWithoutSeparator =
            options.value
                |> String.filter (\char -> Char.isDigit char || String.fromList [ char ] == previousDecimalSeparator)
                |> String.replace previousDecimalSeparator "."
    in
    { options
        | mask = Just (NumberMask mask)
        , value =
            valueWithoutSeparator
                |> Mask.floatString mask separators
                |> Maybe.withDefault valueWithoutSeparator
    }
        |> withElements
            (if decimalDigitsAmount == 0 then
                options.extraElements

             else
                Html.node
                    "masked-input-helper"
                    [ attribute "target-id" options.id
                    , attribute "mask-type" "number"
                    , attribute "decimal-separator" separators.decimalSeparator
                    ]
                    []
                    :: options.extraElements
            )


{-| Defines the input as a numeric input
-}
asNumeric : InputOptions a -> InputOptions a
asNumeric options =
    options
        |> withAttrs [ attribute "inputmode" "numeric" ]


{-| Creates a Cambiatus-style input counter.
-}
inputCounterviewWithAttrs : (String -> I18Next.Replacements -> String) -> Int -> String -> List (Html.Attribute msg) -> CounterType -> Html msg
inputCounterviewWithAttrs tr max str attrs counterType =
    let
        currentLength =
            case counterType of
                CountLetters ->
                    String.length str

                CountWords ->
                    String.words str
                        |> List.filter (not << String.isEmpty)
                        |> List.length
    in
    div (class "text-purple-100 mt-2 ml-2 uppercase font-bold text-sm flex-shrink-0" :: attrs)
        [ text <|
            tr "edit.input_counter"
                [ ( "current", String.fromInt currentLength )
                , ( "max", String.fromInt max )
                ]
        ]


type CounterType
    = CountLetters
    | CountWords


{-| A mask formats the input as the user writes in it. A common place to find
them is on phone inputs. Here is an example for a BR phone number:

    StringMask { mask = "(##) ####-####", replace = '#' }

We can also use them to limit inputs that are meant to only receive numbers. In
that case, we can limit the number of decimal digits. A NumberMask is already
applied on symbol/currency inputs, based on that symbol's precision.

-}
type Mask
    = NumberMask Mask.DecimalDigits
    | StringMask { mask : String, replace : Char }



--- INTERNAL


hasErrors : InputOptions a -> Bool
hasErrors options =
    options.problems
        |> Maybe.withDefault []
        |> List.length
        |> (\length -> length > 0)


viewFieldProblem : List (Html.Attribute a) -> String -> Html a
viewFieldProblem attrs problem =
    li (class "form-error" :: attrs) [ text problem ]


viewCurrencyElement : Eos.Symbol -> Html a
viewCurrencyElement symbol =
    span [ class "absolute right-0 rounded-r-sm border-transparent border bg-purple-500 uppercase inset-y-0 px-4 text-white flex items-center" ]
        [ text <| Eos.symbolToSymbolCodeString symbol ]


fieldTypeToString : FieldType -> String
fieldTypeToString fieldType =
    case fieldType of
        Text ->
            "text"

        Telephone ->
            "tel"

        Number ->
            "number"

        Time ->
            "time"


type alias InputOptions a =
    { label : String
    , labelAttrs : List (Html.Attribute a)
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
    , containerAttrs : List (Html.Attribute a)
    , inputContainerAttrs : List (Html.Attribute a)
    , extraElements : List (Html a)
    , errorAttrs : List (Html.Attribute a)
    , inputType : InputType
    , fieldType : FieldType
    , counterType : CounterType
    , mask : Maybe Mask
    }


{-| All possible input types
-}
type InputType
    = Input
    | TextArea


{-| Different possible type\_s
-}
type FieldType
    = Text
    | Telephone
    | Number
    | Time
