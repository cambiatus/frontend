module Form.Radio exposing
    ( init, Options
    , withOption
    , withDisabled, withContainerAttrs, withLabelAttrs, withHiddenRadioButton
    , Direction(..), withDirection
    , getId, getOptionToString
    , view
    , map
    )

{-| Creates a Cambiatus-style radio button group. Use it within a `Form.Form`:

    Form.Radio.init
        { label = "Verification Type"
        , id = "verification-type-radio"
        }
        |> Form.Radio.withOption ...
        |> Form.Radio.withOption ...
        |> Form.radio


# Initializing

@docs init, Options


# Helpers


## Adding options

@docs withOption


## Adding attributes

@docs withDisabled, withContainerAttrs, withLabelAttrs, withHiddenRadioButton


## Controlling direction

@docs Direction, withDirection


# Getters

@docs getId, getOptionToString


# View

@docs view


# Helpers

@docs map

-}

import Html exposing (Html, div, fieldset, input)
import Html.Attributes exposing (checked, class, classList, disabled, name, type_)
import Html.Events exposing (onBlur, onClick)



-- OPTIONS


type Options option msg
    = Options
        { label : String
        , id : String
        , optionToString : option -> String
        , options : List { option : option, label : Html msg }
        , disabled : Bool
        , containerAttrs : List (Html.Attribute msg)
        , labelAttrs : List (Html.Attribute msg)
        , direction : Direction
        , hideRadioButton : Bool
        }


{-| Initializes a radio group
-}
init : { label : String, id : String, optionToString : option -> String } -> Options option msg
init { label, id, optionToString } =
    Options
        { label = label
        , id = id
        , optionToString = optionToString
        , options = []
        , disabled = False
        , containerAttrs = []
        , labelAttrs = []
        , direction = Horizontal
        , hideRadioButton = False
        }


map :
    (option -> mappedOption)
    -> (mappedOption -> option)
    -> Options option msg
    -> Options mappedOption msg
map fn reverseFn (Options options) =
    Options
        { label = options.label
        , id = options.id
        , optionToString = reverseFn >> options.optionToString
        , options =
            List.map (\{ option, label } -> { option = fn option, label = label })
                options.options
        , disabled = options.disabled
        , containerAttrs = options.containerAttrs
        , labelAttrs = options.labelAttrs
        , direction = options.direction
        , hideRadioButton = options.hideRadioButton
        }



-- ADDING OPTIONS


{-| Add a single option to the radio group. Give it a value and a way to view it
-}
withOption : option -> Html msg -> Options option msg -> Options option msg
withOption optionValue optionLabel (Options options) =
    Options
        { options
            | options =
                { option = optionValue
                , label = optionLabel
                }
                    :: options.options
        }



-- ADDING ATTRIBUTES


{-| Determines if the radio group should be disabled
-}
withDisabled : Bool -> Options option msg -> Options option msg
withDisabled disabled (Options options) =
    Options { options | disabled = disabled }


{-| Adds attribute to the element that contains the radio group itself, the label
and the error message
-}
withContainerAttrs : List (Html.Attribute msg) -> Options option msg -> Options option msg
withContainerAttrs attrs (Options options) =
    Options { options | containerAttrs = options.containerAttrs ++ attrs }


{-| Adds attributes to the label/legend element
-}
withLabelAttrs : List (Html.Attribute msg) -> Options option msg -> Options option msg
withLabelAttrs attrs (Options options) =
    Options { options | labelAttrs = options.labelAttrs ++ attrs }


{-| Determine if we should hide the radio button itself and only show the label
-}
withHiddenRadioButton : Bool -> Options option msg -> Options option msg
withHiddenRadioButton hideRadioButton (Options options) =
    Options { options | hideRadioButton = hideRadioButton }



-- CONTROLLING DIRECTION


{-| Controls if we should display the radio buttons horizontally aligned or
vertically stacked
-}
type Direction
    = Horizontal
    | Vertical


{-| Set a `Direction` to display the group in
-}
withDirection : Direction -> Options option msg -> Options option msg
withDirection direction (Options options) =
    Options { options | direction = direction }



-- GETTERS


getId : Options option msg -> String
getId (Options options) =
    options.id


getOptionToString : Options option msg -> (option -> String)
getOptionToString (Options options) =
    options.optionToString



-- VIEW


type alias ViewConfig option msg =
    { onSelect : option -> msg
    , onBlur : msg
    , value : option
    , error : Html msg
    , hasError : Bool
    }


view : Options option msg -> ViewConfig option msg -> Html msg
view (Options options) viewConfig =
    let
        viewOption : { option : option, label : Html msg } -> Html msg
        viewOption { option, label } =
            let
                isSelected =
                    option == viewConfig.value
            in
            Html.label
                [ class "flex items-center transition-colors duration-100"
                , classList
                    [ ( "text-green", isSelected && not options.disabled )
                    , ( "text-red", isSelected && viewConfig.hasError )
                    , ( "text-gray-900", options.disabled )
                    ]
                ]
                [ input
                    [ type_ "radio"
                    , name options.id
                    , Html.Attributes.value (options.optionToString option)
                    , checked isSelected
                    , onClick (viewConfig.onSelect option)
                    , onBlur viewConfig.onBlur
                    , class "form-radio mr-2 shadow-form-control"
                    , classList
                        [ ( "with-error", isSelected && viewConfig.hasError )
                        , ( "hover:border-green", not viewConfig.hasError && not options.disabled )
                        , ( "hover:border-red", viewConfig.hasError && not options.disabled )
                        , ( "sr-only", options.hideRadioButton )
                        ]
                    ]
                    []
                , label
                ]
    in
    fieldset
        (disabled options.disabled
            :: options.containerAttrs
        )
        [ Html.legend (class "label" :: options.labelAttrs) [ Html.text options.label ]
        , div
            [ class "flex flex-wrap"
            , classList
                [ ( "flex-col gap-4", options.direction == Vertical )
                , ( "gap-4", options.direction == Horizontal && options.hideRadioButton )
                , ( "gap-x-8 gap-y-4", options.direction == Horizontal && not options.hideRadioButton )
                ]
            ]
            (options.options
                |> List.reverse
                |> List.map viewOption
            )
        , viewConfig.error
        ]
