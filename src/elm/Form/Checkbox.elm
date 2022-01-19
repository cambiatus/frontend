module Form.Checkbox exposing
    ( init, Options
    , withDisabled, withContainerAttrs
    , getId
    , view
    )

{-| Creates a Cambiatus-style checkbox. Use it within a `Form.Form`:

    Form.Checkbox.init
        { label = text "I have saved my 12 words"
        , id = "words-checkbox"
        }
        |> Form.Checkbox.withContainerAttrs [ ... ]
        |> Form.checkbox


# Initializing

@docs init, Options


# Helpers


## Adding attributes

@docs withDisabled, withContainerAttrs


# Getters

@docs getId


# View

@docs view

-}

import Html exposing (Html, div, input)
import Html.Attributes exposing (autocomplete, checked, class, classList, disabled, for, id, required, type_)
import Html.Events exposing (onBlur, onCheck)



-- OPTIONS


type Options msg
    = Options
        { label : Html msg
        , id : String
        , disabled : Bool
        , containerAttrs : List (Html.Attribute msg)
        }


{-| Initializes a checkbox
-}
init : { label : Html msg, id : String } -> Options msg
init { label, id } =
    Options
        { label = label
        , id = id
        , disabled = False
        , containerAttrs = []
        }



-- ADDING ATTRIBUTES


{-| Determines if the checkbox should be disabled
-}
withDisabled : Bool -> Options msg -> Options msg
withDisabled disabled (Options options) =
    Options { options | disabled = disabled }


{-| Adds attribute to the element that contains the checkbox itself, the label
and the error message
-}
withContainerAttrs : List (Html.Attribute msg) -> Options msg -> Options msg
withContainerAttrs attrs (Options options) =
    Options { options | containerAttrs = options.containerAttrs ++ attrs }



-- VIEW


type alias ViewConfig msg =
    { onCheck : Bool -> msg
    , onBlur : msg
    , value : Bool
    , error : Html msg
    , hasError : Bool
    , isRequired : Bool
    }


view : Options msg -> ViewConfig msg -> Html msg
view (Options options) viewConfig =
    div options.containerAttrs
        [ input
            [ type_ "checkbox"
            , class "form-checkbox mr-2"
            , classList [ ( "with-error", viewConfig.hasError ) ]
            , checked viewConfig.value
            , onCheck viewConfig.onCheck
            , id options.id
            , disabled options.disabled
            , onBlur viewConfig.onBlur
            , required viewConfig.isRequired

            -- Firefox saves the value on page refresh
            -- Setting autocomplete to False prevents that
            , autocomplete False
            ]
            []
        , Html.label
            [ for options.id
            , classList
                [ ( "text-green", viewConfig.value )
                , ( "text-gray-900", options.disabled )
                , ( "with-error", viewConfig.hasError )
                ]
            ]
            [ options.label
            ]
        , viewConfig.error
        ]



-- GETTERS


getId : Options msg -> String
getId (Options options) =
    options.id
