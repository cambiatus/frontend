module Form.Select exposing
    ( init, Options
    , withOption
    , withDisabled, withContainerAttrs, withAttrs
    , getId, getOptionToString
    , view
    , map
    )

{-| Creates a Cambiatus-style Select. Use it within a `Form.Form`:

    Form.Select.init
        { label = "Country"
        , id = "country-select"
        }


# Initializing

@docs init, Options


# Helpers


## Adding options

@docs withOption, withOptions


## Adding attributes

@docs withDisabled, withContainerAttrs, withAttrs


# Getters

@docs getId, getOptionToString


# View

@docs view


# Helpers

@docs map

-}

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList, disabled, required, selected, value)
import Html.Events exposing (onBlur)
import Json.Decode
import List.Extra
import View.Form



-- OPTIONS


type Options option msg
    = Options
        { label : String
        , id : String
        , disabled : Bool
        , optionToString : option -> String
        , options : List { option : option, label : String }
        , containerAttrs : List (Html.Attribute msg)
        , extraAttrs : List (Html.Attribute msg)
        }


{-| Initializes a Select
-}
init : { label : String, id : String, optionToString : option -> String } -> Options option msg
init { label, id, optionToString } =
    Options
        { label = label
        , id = id
        , disabled = False
        , optionToString = optionToString
        , options = []
        , containerAttrs = []
        , extraAttrs = []
        }


map : (option -> mappedOption) -> (mappedOption -> option) -> Options option msg -> Options mappedOption msg
map fn reverseFn (Options options) =
    Options
        { label = options.label
        , id = options.id
        , disabled = options.disabled
        , optionToString = reverseFn >> options.optionToString
        , options =
            List.map (\{ option, label } -> { option = fn option, label = label })
                options.options
        , containerAttrs = options.containerAttrs
        , extraAttrs = options.extraAttrs
        }



-- ADDING OPTIONS


{-| Add a single option to the select element. Give it a value and a label
-}
withOption : option -> String -> Options option msg -> Options option msg
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


{-| Determines if the Select should be disabled
-}
withDisabled : Bool -> Options option msg -> Options option msg
withDisabled disabled (Options options) =
    Options { options | disabled = disabled }


withContainerAttrs : List (Html.Attribute msg) -> Options option msg -> Options option msg
withContainerAttrs attrs (Options options) =
    Options { options | containerAttrs = options.containerAttrs ++ attrs }


withAttrs : List (Html.Attribute msg) -> Options option msg -> Options option msg
withAttrs attrs (Options options) =
    Options { options | extraAttrs = options.extraAttrs ++ attrs }



-- VIEW


type alias ViewConfig option msg =
    { onSelect : option -> msg
    , onBlur : String -> msg
    , value : option
    , error : Html msg
    , hasError : Bool
    , isRequired : Bool
    }


view : Options option msg -> ViewConfig option msg -> Html msg
view (Options options) viewConfig =
    let
        viewOption : { option : option, label : String } -> Html msg
        viewOption { option, label } =
            Html.option
                [ value (options.optionToString option)
                , selected (viewConfig.value == option)
                ]
                [ text label ]

        optionFromValue : String -> Maybe option
        optionFromValue newValue =
            List.Extra.findMap
                (\{ option } ->
                    if options.optionToString option == newValue then
                        Just option

                    else
                        Nothing
                )
                options.options
    in
    div options.containerAttrs
        [ View.Form.label [] options.id options.label
        , Html.select
            (class "input pr-10 form-select-icon w-full"
                :: classList [ ( "with-error", viewConfig.hasError ) ]
                :: required viewConfig.isRequired
                :: disabled options.disabled
                :: onBlur (viewConfig.onBlur options.id)
                -- We don't use `Html.Events.onInput` because `optionFromValue`
                -- might return `Nothing`. In that case, the decoder just fails and
                -- doesn't emit a msg
                :: customOnInput optionFromValue viewConfig.onSelect
                :: options.extraAttrs
            )
            (options.options
                |> List.reverse
                |> List.map viewOption
            )
        , viewConfig.error
        ]


customOnInput : (String -> Maybe option) -> (option -> msg) -> Html.Attribute msg
customOnInput optionFromString optionToMsg =
    Html.Events.stopPropagationOn "input"
        (Html.Events.targetValue
            |> Json.Decode.andThen
                (\targetValue ->
                    case optionFromString targetValue of
                        Nothing ->
                            Json.Decode.fail "The selected value is not a valid option"

                        Just optionValue ->
                            Json.Decode.succeed (optionToMsg optionValue)
                )
            |> Json.Decode.map (\optionMsg -> ( optionMsg, True ))
        )



-- GETTERS


getId : Options option msg -> String
getId (Options options) =
    options.id


getOptionToString : Options option msg -> (option -> String)
getOptionToString (Options options) =
    options.optionToString
