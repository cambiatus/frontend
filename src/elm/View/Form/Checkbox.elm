module View.Form.Checkbox exposing
    ( init
    , withContainerAttrs
    , toHtml
    )

{-| Creates a Cambiatus-style checkbox

    Checkbox.init
        { description = t "register.account_created.i_saved_words"
        , id = "agreed_save_passphrase"
        , value = model.hasAgreedToSavePassphrase
        , disabled = False
        , onCheck = AgreedToSave12Words
        }
        |> Checkbox.toHtml


# Initializing

@docs InitialOptions, init


# Helpers

@docs withContainerAttrs


# Converting to HTML

@docs toHtml

-}

import Html exposing (Html, input, label)
import Html.Attributes exposing (checked, class, classList, disabled, type_)
import Html.Events exposing (onCheck)



-- MODEL


type alias Options msg =
    { description : Html msg
    , id : String
    , value : Bool
    , disabled : Bool
    , onCheck : Bool -> msg
    , containerAttrs : List (Html.Attribute msg)
    }



-- INITIALIZING


{-| Defines the required options to initialize the checkbox
-}
type alias InitialOptions msg =
    { description : Html msg
    , id : String
    , value : Bool
    , disabled : Bool
    , onCheck : Bool -> msg
    }


{-| Initializes the checkbox
-}
init : InitialOptions msg -> Options msg
init initialOptions =
    { description = initialOptions.description
    , id = initialOptions.id
    , value = initialOptions.value
    , disabled = initialOptions.disabled
    , onCheck = initialOptions.onCheck
    , containerAttrs = []
    }



-- HELPERS


{-| Adds attributes to the div that holds the checkbox and the description
-}
withContainerAttrs : List (Html.Attribute msg) -> Options msg -> Options msg
withContainerAttrs attrs options =
    { options | containerAttrs = options.containerAttrs ++ attrs }



-- TO HTML


{-| Transform `Checkbox.Options` into`Html`
-}
toHtml : Options msg -> Html msg
toHtml options =
    label
        (class "block"
            :: classList
                [ ( "text-green", options.value )
                , ( "text-gray-900", options.disabled )
                ]
            :: options.containerAttrs
        )
        [ input
            [ type_ "checkbox"
            , class "form-checkbox mr-2"
            , checked options.value
            , onCheck options.onCheck
            , disabled options.disabled
            ]
            []
        , options.description
        ]
