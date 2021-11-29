module Form.Toggle exposing
    ( init, Options, map
    , withTooltip
    , withDisabled
    , getId
    , view
    )

{-| Creates a Cambiatus-style Toggle. Use it within a `Form.Form`:

    Form.Toggle.init
        { label = text "Feature"
        , id = "feature-toggle"
        }


# Initializing

@docs init, Options, map


# Helpers


## Adding elements

@docs withTooltip


## Adding attributes

@docs withDisabled


# Getters

@docs getId


# View

@docs view

-}

import Html exposing (Html, div, input, label, p, span)
import Html.Attributes exposing (checked, class, classList, disabled, for, id, required, type_)
import Html.Events exposing (onBlur, onCheck)
import Session.Shared as Shared
import View.Components



-- OPTIONS


type Options msg
    = Options
        { label : Html msg
        , id : String
        , disabled : Bool
        , tooltip : Maybe { message : String, iconClass : String }
        }


{-| Initializes a Toggle
-}
init : { label : Html msg, id : String } -> Options msg
init { label, id } =
    Options
        { label = label
        , id = id
        , disabled = False
        , tooltip = Nothing
        }


{-| Change the kind of `msg` on an Options record
-}
map : (msg -> mappedMsg) -> Options msg -> Options mappedMsg
map fn (Options options) =
    Options
        { label = Html.map fn options.label
        , id = options.id
        , disabled = options.disabled
        , tooltip = options.tooltip
        }



-- ADDING ELEMENTS


{-| Adds a tooltip the user can see when hovering over an icon. Useful when we
need to give more information to the user
-}
withTooltip : { message : String, iconClass : String } -> Options msg -> Options msg
withTooltip tooltip (Options options) =
    Options { options | tooltip = Just tooltip }



-- ADDING ATTRIBUTES


{-| Determines if the Toggle should be disabled
-}
withDisabled : Bool -> Options msg -> Options msg
withDisabled disabled (Options options) =
    Options { options | disabled = disabled }



-- VIEW


type alias ViewConfig msg =
    { onToggle : Bool -> msg
    , onBlur : String -> msg
    , value : Bool
    , error : Html msg
    , hasError : Bool
    , isRequired : Bool
    , translators : Shared.Translators
    }


view : Options msg -> ViewConfig msg -> Html msg
view (Options options) viewConfig =
    div []
        [ div
            [ class "flex" ]
            [ span [ class "flex items-center" ]
                [ label
                    [ for options.id
                    , class "cursor-pointer"
                    ]
                    [ options.label ]
                , case options.tooltip of
                    Nothing ->
                        Html.text ""

                    Just tooltip ->
                        View.Components.tooltip tooltip
                ]
            , label
                [ class "flex cursor-pointer ml-auto"
                , for options.id
                ]
                [ p
                    [ class "font-semibold lowercase"
                    , classList
                        [ ( "text-indigo-500", viewConfig.value && not options.disabled && not viewConfig.hasError )
                        , ( "text-gray-700", (not viewConfig.value || options.disabled) && not viewConfig.hasError )
                        , ( "text-red", viewConfig.hasError )
                        ]
                    ]
                    [ Html.text <| statusText viewConfig ]
                , span
                    [ class "select-none ml-7"
                    ]
                    [ input
                        [ type_ "checkbox"
                        , id options.id
                        , checked viewConfig.value
                        , onCheck viewConfig.onToggle
                        , onBlur (viewConfig.onBlur options.id)
                        , disabled options.disabled
                        , required viewConfig.isRequired
                        , class "sr-only"
                        ]
                        []
                    , span
                        [ class "relative block border-2 border-gray-100 rounded-full h-6 w-12 transition-colors ease-in duration-200 input form-switch-label2"
                        , classList
                            [ ( "bg-indigo-500 checked", viewConfig.value )
                            , ( "bg-white", not viewConfig.value )
                            , ( "border-red with-error", viewConfig.hasError )
                            ]
                        ]
                        []
                    ]
                ]
            ]
        , viewConfig.error
        ]


statusText : ViewConfig msg -> String
statusText viewConfig =
    if viewConfig.value then
        viewConfig.translators.t "settings.features.enabled"

    else
        viewConfig.translators.t "settings.features.disabled"



-- GETTERS


getId : Options msg -> String
getId (Options options) =
    options.id
