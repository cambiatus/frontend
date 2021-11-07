module Form.Radio exposing
    ( init, Options
    , withOption, withOptions
    , withDisabled, withContainerAttrs, withGroupAttrs
    , Direction(..), withDirection
    , getId
    , view
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

@docs withOption, withOptions


## Adding attributes

@docs withDisabled, withContainerAttrs, withGroupAttrs


## Controlling direction

@docs Direction, withDirection


# Getters

@docs getId


# View

@docs view

-}

import Html exposing (Html, div, fieldset, input, label)
import Html.Attributes exposing (checked, class, classList, disabled, name, type_, value)
import Html.Events exposing (onBlur, onClick)



-- OPTIONS


type Options option msg
    = Options
        { label : String
        , id : String
        , options : List ( option, Html msg )
        , disabled : Bool
        , containerAttrs : List (Html.Attribute msg)
        , groupAttrs : List (Html.Attribute msg)
        , direction : Direction
        }


{-| Initializes a radio group
-}
init : { label : String, id : String } -> Options options msg
init { label, id } =
    Options
        { label = label
        , id = id
        , options = []
        , disabled = False
        , containerAttrs = []
        , groupAttrs = []
        , direction = Horizontal
        }



-- ADDING OPTIONS


{-| Add a single option to the radio group. Give it a value and a way to view it
-}
withOption : option -> Html msg -> Options option msg -> Options option msg
withOption optionValue optionLabel (Options options) =
    Options { options | options = ( optionValue, optionLabel ) :: options.options }


{-| Similar to `withOption`, but you can throw in an entire list at a time
-}
withOptions : List ( option, Html msg ) -> Options option msg -> Options option msg
withOptions optionList (Options options) =
    -- We reverse the list on view in order to display things on the correct
    -- order (because on `withOption` we insert options in the beginning of the
    -- list, for some performance), so we need to insert it reversed here
    Options { options | options = List.reverse optionList ++ options.options }



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


{-| Adds attribute to the element that contains the radio group itself
-}
withGroupAttrs : List (Html.Attribute msg) -> Options option msg -> Options option msg
withGroupAttrs attrs (Options options) =
    Options { options | groupAttrs = options.groupAttrs ++ attrs }



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



-- VIEW


type alias ViewConfig option msg =
    { onSelect : option -> msg
    , onBlur : String -> msg
    , value : option
    , error : Html msg
    , hasError : Bool
    }


view : Options String msg -> ViewConfig String msg -> Html msg
view (Options options) viewConfig =
    let
        viewOption : ( String, Html msg ) -> Html msg
        viewOption ( option, label ) =
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
                    , value option
                    , checked isSelected
                    , onClick (viewConfig.onSelect option)
                    , onBlur (viewConfig.onBlur options.id)
                    , class "form-radio mr-2 shadow-form-control"
                    , classList [ ( "with-error", isSelected && viewConfig.hasError ) ]
                    ]
                    []
                , label
                ]
    in
    fieldset
        (disabled options.disabled
            :: options.containerAttrs
        )
        [ Html.legend [ class "label" ] [ Html.text options.label ]
        , div
            [ class "flex gap-4"
            , classList [ ( "flex-col", options.direction == Vertical ) ]
            ]
            (List.map viewOption options.options)
        , viewConfig.error
        ]
