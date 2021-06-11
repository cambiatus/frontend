module View.Form.Toggle exposing
    ( init
    , withAttrs, withTooltip, withVariant
    , toHtml
    , Variant(..)
    )

{-| Creates a Cambiatus-style toggle input

    View.Toggle.init
        { label = "Feature"
        , id = "feature_toggle"
        , onToggle = ToggledFeature
        , disabled = False
        , value = model.hasFeature
        }
        |> View.Toggle.withTooltip "feature.tooltip"
        |> View.Toggle.toHtml translators


# Initializing

@docs init


# Helpers

@docs withAttrs, withTooltip, withVariant


# Converting to HTML

@docs toHtml

-}

import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (checked, class, disabled, for, id, name, type_)
import Html.Events exposing (onCheck)
import Session.Shared exposing (Translators)
import View.Components



-- TYPES


type alias RequiredOptions msg =
    { label : Html msg
    , id : String
    , onToggle : Bool -> msg
    , disabled : Bool
    , value : Bool
    }


type alias Options msg =
    { label : Html msg
    , id : String
    , onToggle : Bool -> msg
    , disabled : Bool
    , value : Bool
    , variant : Variant
    , tooltip : Maybe String
    , extraAttrs : List (Html.Attribute msg)
    }


type Variant
    = Simple
    | Big


{-| Initialize a Toggle with some required options
-}
init : RequiredOptions msg -> Options msg
init requiredOptions =
    { label = requiredOptions.label
    , id = requiredOptions.id
    , onToggle = requiredOptions.onToggle
    , disabled = requiredOptions.disabled
    , value = requiredOptions.value
    , variant = Big
    , tooltip = Nothing
    , extraAttrs = []
    }



-- HELPERS


{-| Adds a tooltip the user can see when hovering over an icon
-}
withTooltip : String -> Options msg -> Options msg
withTooltip tooltip options =
    { options | tooltip = Just tooltip }


{-| Adds a list of attributes to the toggle
-}
withAttrs : List (Html.Attribute a) -> Options a -> Options a
withAttrs attrs options =
    { options | extraAttrs = options.extraAttrs ++ attrs }


{-| Selects the variant to be displayed
-}
withVariant : Variant -> Options a -> Options a
withVariant variant options =
    { options | variant = variant }



-- TO HTML


{-| Transform `Toggle.Options` into `Html`.

**Note**: All text is translated, so just store the translation key in your model,
not the translated text

-}
toHtml : Translators -> Options msg -> Html msg
toHtml translators options =
    case options.variant of
        Big ->
            viewBig translators options

        Simple ->
            viewSimple translators options


viewSimple : Translators -> Options msg -> Html msg
viewSimple translators options =
    label
        (class "flex w-full items-center text-sm"
            :: for options.id
            :: options.extraAttrs
        )
        [ div [ class "form-switch" ]
            [ input
                [ type_ "checkbox"
                , id options.id
                , name options.id
                , class "form-switch-checkbox"
                , checked options.value
                , onCheck options.onToggle
                , disabled options.disabled
                ]
                []
            , label [ class "form-switch-label", for options.id ] []
            ]
        , div [ class "flex items-center" ]
            [ options.label
            , viewTooltip translators options
            ]
        ]


viewBig : Translators -> Options msg -> Html msg
viewBig ({ t } as translators) options =
    let
        text_ =
            t >> text

        statusColor =
            if options.value && not options.disabled then
                "text-purple-500"

            else
                "text-grey"
    in
    label
        (class "flex w-full justify-between items-center text-sm"
            :: for options.id
            :: options.extraAttrs
        )
        [ div [ class "flex items-center" ]
            [ options.label
            , viewTooltip translators options
            ]
        , div [ class ("flex items-center font-medium lowercase ml-2 " ++ statusColor) ]
            [ label [ for options.id ] [ text_ (statusText options) ]
            , div [ class "form-switch ml-7" ]
                [ input
                    [ type_ "checkbox"
                    , id options.id
                    , name options.id
                    , class "form-switch-checkbox"
                    , checked options.value
                    , onCheck options.onToggle
                    , disabled options.disabled
                    ]
                    []
                , label [ class "form-switch-label", for options.id ] []
                ]
            ]
        ]



-- INTERNAL


viewTooltip : Translators -> Options msg -> Html msg
viewTooltip translators options =
    case options.tooltip of
        Nothing ->
            text ""

        Just tooltip ->
            View.Components.tooltip translators tooltip


statusText : Options msg -> String
statusText options =
    if options.value then
        "settings.features.enabled"

    else
        "settings.features.disabled"
