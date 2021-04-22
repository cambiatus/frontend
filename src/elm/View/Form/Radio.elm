module View.Form.Radio exposing
    ( init
    , withOption, withAttrs, withOptionAttrs, withVertical
    , toHtml
    , withDisabled
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

@docs withOption, withAttrs, withOptionAttrs, withVertical


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
    , options : List ( option, Html msg )
    , extraAttrs : List (Html.Attribute msg)
    , optionAttrs : List (Html.Attribute msg)
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
    , extraAttrs = []
    , optionAttrs = []
    , isVertical = False
    , isDisabled = False
    }



-- HELPERS


{-| Adds an option to the radio group
-}
withOption : option -> Html msg -> Options option msg -> Options option msg
withOption option viewOption options =
    { options | options = options.options ++ [ ( option, viewOption ) ] }


{-| Adds attributes to the div that holds the label and the options
-}
withAttrs : List (Html.Attribute msg) -> Options option msg -> Options option msg
withAttrs attrs options =
    { options | extraAttrs = options.extraAttrs ++ attrs }


{-| Adds attributes to the option labels
-}
withOptionAttrs : List (Html.Attribute msg) -> Options option msg -> Options option msg
withOptionAttrs attrs options =
    { options | optionAttrs = options.optionAttrs ++ attrs }


{-| Defines if the group should be displayed in a row or in a column
-}
withVertical : Bool -> Options option msg -> Options option msg
withVertical isVertical options =
    { options | isVertical = isVertical }


withDisabled : Bool -> Options option msg -> Options option msg
withDisabled isDisabled options =
    { options | isDisabled = isDisabled }



-- TO HTML


{-| Transform `Radio.Options` into`Html`
-}
toHtml : Translators -> Options option msg -> Html msg
toHtml { t } options =
    let
        isActive =
            options.areOptionsEqual options.activeOption

        viewOption ( option, view ) =
            label
                ([ class "flex items-center"
                 , classList [ ( "mb-6", options.isVertical ) ]
                 ]
                    ++ options.optionAttrs
                )
                [ input
                    [ type_ "radio"
                    , class "form-radio h-5 w-5 text-green"
                    , name options.name
                    , value (options.optionToString option)
                    , checked (isActive option)
                    , onClick (options.onSelect option)
                    , disabled options.isDisabled
                    ]
                    []
                , span
                    [ class "flex ml-2 text-body"
                    , classList [ ( "text-green", isActive option ) ]
                    ]
                    [ view ]
                ]
    in
    div options.extraAttrs
        [ span [ class "input-label" ] [ text (t options.label) ]
        , div
            [ class "flex mt-6"
            , classList
                [ ( "flex-col", options.isVertical )
                , ( "space-x-29", not options.isVertical )
                ]
            ]
            (List.map viewOption options.options)
        ]
