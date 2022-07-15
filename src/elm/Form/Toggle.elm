module Form.Toggle exposing
    ( init, Options
    , withTooltip, withTopLabel, withStatusText
    , StatusText(..)
    , withDisabled, withToggleSide, withContainerAttrs, withToggleContainerAttrs
    , Side(..)
    , getId
    , view
    )

{-| Creates a Cambiatus-style Toggle. Use it within a `Form.Form`:

    Form.Toggle.init
        { label = text "Feature"
        , id = "feature-toggle"
        }


# Initializing

@docs init, Options


# Helpers


## Adding elements

@docs withTooltip, withTopLabel, withStatusText

@docs StatusText


## Adding attributes

@docs withDisabled, withToggleSide, withContainerAttrs, withToggleContainerAttrs

@docs Side


# Getters

@docs getId


# View

@docs view

-}

import Html exposing (Html, div, input, p, span)
import Html.Attributes exposing (checked, class, classList, disabled, for, id, required, type_)
import Html.Events exposing (onBlur, onCheck)
import Maybe.Extra
import Session.Shared as Shared
import View.Components



-- OPTIONS


type Options msg
    = Options
        { label : Html msg
        , id : String
        , disabled : Bool
        , tooltip : Maybe { message : String, iconClass : String, containerClass : String }
        , side : Side
        , statusText : StatusText
        , topLabel : Maybe String
        , containerAttrs : List (Html.Attribute msg)
        , toggleContainerAttrs : List (Html.Attribute msg)
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
        , side = Right { invert = False }
        , statusText = EnabledDisabled
        , topLabel = Nothing
        , containerAttrs = []
        , toggleContainerAttrs = []
        }


type Side
    = Left
    | Right { invert : Bool }


type StatusText
    = EnabledDisabled
    | YesNo



-- ADDING ELEMENTS


{-| Adds a tooltip the user can see when hovering over an icon. Useful when we
need to give more information to the user
-}
withTooltip : { message : String, iconClass : String, containerClass : String } -> Options msg -> Options msg
withTooltip tooltip (Options options) =
    Options { options | tooltip = Just tooltip }


{-| Change the side that the toggle itself is displayed relative to the label
-}
withToggleSide : Side -> Options msg -> Options msg
withToggleSide side (Options options) =
    Options { options | side = side }


{-| Show a label on top of the toggle, like other form elements such as text input
-}
withTopLabel : String -> Options msg -> Options msg
withTopLabel topLabel (Options options) =
    Options { options | topLabel = Just topLabel }


{-| Selects the kind of status text to be displayed next to the toggle
-}
withStatusText : StatusText -> Options a -> Options a
withStatusText statusText_ (Options options) =
    Options { options | statusText = statusText_ }



-- ADDING ATTRIBUTES


{-| Determines if the Toggle should be disabled
-}
withDisabled : Bool -> Options msg -> Options msg
withDisabled disabled (Options options) =
    Options { options | disabled = disabled }


{-| Add attributes to the container element
-}
withContainerAttrs : List (Html.Attribute msg) -> Options msg -> Options msg
withContainerAttrs attrs (Options options) =
    Options { options | containerAttrs = options.containerAttrs ++ attrs }


{-| Add attributes to the container that contains the toggle and the status text
-}
withToggleContainerAttrs : List (Html.Attribute msg) -> Options msg -> Options msg
withToggleContainerAttrs attrs (Options options) =
    Options { options | toggleContainerAttrs = options.toggleContainerAttrs ++ attrs }



-- VIEW


type alias ViewConfig msg =
    { onToggle : Bool -> msg
    , onBlur : msg
    , value : Bool
    , error : Html msg
    , hasError : Bool
    , isRequired : Bool
    , translators : Shared.Translators
    }


viewLabel : Options msg -> Html msg
viewLabel (Options options) =
    span [ class "flex items-center" ]
        [ Html.label
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


viewStatusText : Options msg -> ViewConfig msg -> Html msg
viewStatusText (Options options) viewConfig =
    p
        [ class "font-semibold lowercase text-sm my-auto"
        , classList
            [ ( "text-indigo-500", viewConfig.value && not options.disabled && not viewConfig.hasError )
            , ( "text-gray-600", (not viewConfig.value || options.disabled) && not viewConfig.hasError )
            , ( "text-red", viewConfig.hasError )
            ]
        ]
        [ Html.text <| statusText (Options options) viewConfig ]


viewToggle : Options msg -> ViewConfig msg -> Html msg
viewToggle (Options options) viewConfig =
    span
        [ class "select-none"
        ]
        [ input
            [ type_ "checkbox"
            , id options.id
            , checked viewConfig.value
            , onCheck viewConfig.onToggle
            , onBlur viewConfig.onBlur
            , disabled options.disabled
            , required viewConfig.isRequired
            , class "sr-only"
            ]
            []
        , span
            [ class "relative block border-2 border-gray-100 rounded-full h-6 w-12 transition-colors ease-in duration-200 input form-switch-label2"
            , class "before:block before:bg-gray-900 before:w-4 before:h-4 before:rounded-full before:absolute before:inset-y-0.5 before:mx-0.5 before:transition-all before:duration-200 before:ease-in before:right-6"
            , classList
                [ ( "bg-indigo-500 before:bg-white before:right-0", viewConfig.value )
                , ( "bg-white", not viewConfig.value )
                , ( "border-red with-error", viewConfig.hasError )
                ]
            ]
            []
        ]


view : Options msg -> ViewConfig msg -> Html msg
view (Options options) viewConfig =
    div options.containerAttrs
        [ case options.topLabel of
            Nothing ->
                Html.text ""

            Just topLabel ->
                View.Components.label [ class "mb-6" ]
                    { targetId = options.id, labelText = topLabel }
        , case options.side of
            Left ->
                div
                    (class "flex space-x-2 text-sm"
                        :: (if Maybe.Extra.isNothing options.topLabel then
                                options.containerAttrs

                            else
                                []
                           )
                    )
                    [ viewToggle (Options options) viewConfig
                    , viewLabel (Options options)
                    ]

            Right { invert } ->
                div
                    (class "flex"
                        :: (if Maybe.Extra.isNothing options.topLabel then
                                options.containerAttrs

                            else
                                []
                           )
                    )
                    [ viewLabel (Options options)
                    , Html.label
                        (class "flex cursor-pointer ml-auto space-x-7 pl-4"
                            :: for options.id
                            :: options.toggleContainerAttrs
                        )
                        (if invert then
                            [ viewToggle (Options options) viewConfig
                            , viewStatusText (Options options) viewConfig
                            ]

                         else
                            [ viewStatusText (Options options) viewConfig
                            , viewToggle (Options options) viewConfig
                            ]
                        )
                    ]
        , viewConfig.error
        ]


statusText : Options msg -> ViewConfig msg -> String
statusText (Options options) viewConfig =
    case options.statusText of
        EnabledDisabled ->
            if viewConfig.value then
                viewConfig.translators.t "settings.features.enabled"

            else
                viewConfig.translators.t "settings.features.disabled"

        YesNo ->
            if viewConfig.value then
                viewConfig.translators.t "community.actions.form.yes"

            else
                viewConfig.translators.t "community.actions.form.no"



-- GETTERS


getId : Options msg -> String
getId (Options options) =
    options.id
