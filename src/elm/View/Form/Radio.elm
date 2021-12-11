module View.Form.Radio exposing
    ( init
    , withOption, withVertical, withDisabled
    , toHtml
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

@docs withOption, withVertical, withDisabled


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
    , isVertical : Bool
    , isDisabled : Bool
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
    , isVertical = False
    , isDisabled = False
    }



-- HELPERS


{-| Adds an option to the radio group
-}
withOption : option -> (Bool -> Html msg) -> Options option msg -> Options option msg
withOption option viewOption_ options =
    { options | options = options.options ++ [ ( option, viewOption_ ) ] }


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



-- TO HTML


{-| Transform `Radio.Options` into`Html`

**NOTE**: By default, it turns vertical below the `xs-max` breakpoint

-}
toHtml : Translators -> Options option msg -> Html msg
toHtml { t } options =
    div []
        [ span [ class "label" ] [ text (t options.label) ]
        , div
            [ class "flex mt-6"
            , classList
                [ ( "flex-col", options.isVertical )
                , ( "xs-max:flex-col flex-row xs:flex-wrap", not options.isVertical )
                ]
            ]
            (List.map (viewOption options) options.options)
        ]


isActive : Options option msg -> option -> Bool
isActive options option =
    options.areOptionsEqual options.activeOption option


viewOption : Options option msg -> ( option, Bool -> Html msg ) -> Html msg
viewOption options ( option, view ) =
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
            , if isActive options option then
                class ""

              else
                onClick (options.onSelect option)
            , disabled options.isDisabled
            ]
            []
        , span
            [ class "flex ml-2"
            , classList
                [ ( "text-green", isActive options option && not options.isDisabled )
                , ( "text-gray-900", options.isDisabled )
                ]
            ]
            [ view (isActive options option) ]
        ]
