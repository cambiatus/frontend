module View.Form.Radio exposing
    ( init
    , withOption, withOptions, withAttrs, withRowAttrs, withVertical, withDisabled, withVariant
    , toHtml
    , Variant(..)
    )

{-| Creates a Cambiatus-style radio button group

    type VerificationType
        = Manual
        | Automatic

    View.Form.Radio.init
        { label = "Verification Type"
        , name = "verification_radio"
        , optionToString = optionToString
        , activeOption = model.verificationType
        , onSelect = SelectedVerificationType
        , areOptionsEqual = (==)
        }
        |> View.Form.Radio.withOption Manual "Manual"
        |> View.Form.Radio.withOption Automatic "Automatic"
        |> View.Form.Radio.toHtml


# Initializing

@docs InitialOptions, init


# Helpers

@docs withOption, withOptions, withAttrs, withRowAttrs, withVertical, withDisabled, withVariant


# Converting to HTML

@docs toHtml

-}

import Html exposing (Html, div, input, label, span, text)
import Html.Attributes exposing (checked, class, classList, disabled, name, type_, value)
import Html.Events exposing (onClick)
import Session.Shared exposing (Translators)



-- MODEL


type alias Options option msg =
    { label : String
    , name : String
    , optionToString : option -> String
    , activeOption : option
    , onSelect : option -> msg
    , areOptionsEqual : option -> option -> Bool
    , options : List ( option, Bool -> Html msg )
    , extraAttrs : List (Html.Attribute msg)
    , rowAttrs : List (Html.Attribute msg)
    , isVertical : Bool
    , isDisabled : Bool
    , variant : Variant
    }



-- INITIALIZING


{-| Defines the required options to initialize the radio group
-}
type alias InitialOptions option msg =
    { label : String
    , name : String
    , optionToString : option -> String
    , activeOption : option
    , onSelect : option -> msg
    , areOptionsEqual : option -> option -> Bool
    }


{-| Initializes the radio group
-}
init : InitialOptions option msg -> Options option msg
init initialOptions =
    { label = initialOptions.label
    , name = initialOptions.name
    , optionToString = initialOptions.optionToString
    , activeOption = initialOptions.activeOption
    , onSelect = initialOptions.onSelect
    , areOptionsEqual = initialOptions.areOptionsEqual
    , options = []
    , extraAttrs = []
    , rowAttrs = []
    , isVertical = False
    , isDisabled = False
    , variant = Default
    }



-- HELPERS


{-| Adds an option to the radio group
-}
withOption : option -> (Bool -> Html msg) -> Options option msg -> Options option msg
withOption option viewOption options =
    { options | options = options.options ++ [ ( option, viewOption ) ] }


{-| Adds multiple options to the radio group
-}
withOptions : List ( option, Bool -> Html msg ) -> Options option msg -> Options option msg
withOptions newOptions options =
    { options | options = options.options ++ newOptions }


{-| Adds attributes to the div that holds the label and the options
-}
withAttrs : List (Html.Attribute msg) -> Options option msg -> Options option msg
withAttrs attrs options =
    { options | extraAttrs = options.extraAttrs ++ attrs }


{-| Adds attribute to the div that holds the options
-}
withRowAttrs : List (Html.Attribute msg) -> Options option msg -> Options option msg
withRowAttrs attrs options =
    { options | rowAttrs = options.rowAttrs ++ attrs }


{-| Defines if the group should be displayed in a row or in a column
-}
withVertical : Bool -> Options option msg -> Options option msg
withVertical isVertical options =
    { options | isVertical = isVertical }


{-| Controls the `disabled` property
-}
withDisabled : Bool -> Options option msg -> Options option msg
withDisabled isDisabled options =
    { options | isDisabled = isDisabled }


{-| Defines the variant to be shown
-}
withVariant : Variant -> Options option msg -> Options option msg
withVariant variant options =
    { options | variant = variant }



-- TO HTML


{-| Transform `Radio.Options` into`Html`

**NOTE**: By default, it turns vertical below the `xs-max` breakpoint

-}
toHtml : Translators -> Options option msg -> Html msg
toHtml { t } options =
    let
        viewOption ( option, view ) =
            case options.variant of
                Default ->
                    viewOptionAsDefault options ( option, view )

                Simplified ->
                    viewOptionAsCircled options ( option, view )
    in
    div options.extraAttrs
        [ span [ class "input-label" ] [ text (t options.label) ]
        , div
            (class "flex mt-6"
                :: classList
                    [ ( "flex-col", options.isVertical )
                    , ( "xs-max:flex-col flex-row xs:flex-wrap", not options.isVertical )
                    ]
                :: options.rowAttrs
            )
            (List.map viewOption options.options)
        ]


isActive : Options option msg -> option -> Bool
isActive options option =
    options.areOptionsEqual options.activeOption option


viewOptionAsDefault : Options option msg -> ( option, Bool -> Html msg ) -> Html msg
viewOptionAsDefault options ( option, view ) =
    label
        [ class "flex items-center"
        , classList
            [ ( "mb-6", options.isVertical )
            , ( "xs-max:mb-6 mr-29", not options.isVertical )
            ]
        ]
        [ input
            [ type_ "radio"
            , class "form-radio h-5 w-5 text-green"
            , name options.name
            , value (options.optionToString option)
            , checked (isActive options option)
            , onClick (options.onSelect option)
            , disabled options.isDisabled
            ]
            []
        , span
            [ class "flex ml-2 text-body"
            , classList
                [ ( "text-green", isActive options option && not options.isDisabled )
                , ( "text-gray-600", options.isDisabled )
                ]
            ]
            [ view (isActive options option) ]
        ]


viewOptionAsCircled : Options option msg -> ( option, Bool -> Html msg ) -> Html msg
viewOptionAsCircled options ( option, view ) =
    label []
        [ view (isActive options option)
        , input
            [ type_ "radio"
            , class "hidden absolute"
            , name options.name
            , value (options.optionToString option)
            , checked (isActive options option)
            , onClick (options.onSelect option)
            , disabled options.isDisabled
            ]
            []
        ]


type Variant
    = Default
    | Simplified
