module Form.Select exposing
    ( init, Options
    , withDisabled
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


## Adding attributes

@docs withDisabled


# Getters

@docs getId, getOptionToString


# View

@docs view


# Helpers

@docs map

-}

import Html exposing (Html, div, input, label)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onBlur)



-- OPTIONS


type Options option msg
    = Options
        { label : String
        , id : String
        , disabled : Bool
        , optionToString : option -> String
        , options : List { option : option, label : String }
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
        }



-- ADDING ATTRIBUTES


{-| Determines if the Select should be disabled
-}
withDisabled : Bool -> Options option msg -> Options option msg
withDisabled disabled (Options options) =
    Options { options | disabled = disabled }



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
    div []
        []



-- GETTERS


getId : Options option msg -> String
getId (Options options) =
    options.id


getOptionToString : Options option msg -> (option -> String)
getOptionToString (Options options) =
    options.optionToString
