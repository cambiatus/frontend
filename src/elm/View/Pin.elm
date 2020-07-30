module View.Pin exposing (Pin, isValid, view)

import Html exposing (Html, button, div, input, label, li, text, ul)
import Html.Attributes exposing (attribute, autocomplete, class, for, id, maxlength, placeholder, required, type_, value)
import Html.Events exposing (onClick, onInput)
import I18Next
import Session.Shared exposing (Shared)


type alias Config msg =
    { labelText : String
    , inputId : String
    , inputValue : String
    , onInputMsg : String -> msg
    , onToggleMsg : msg
    , isVisible : Bool
    , errors : List String
    }


type alias Pin =
    String


isValid : Pin -> Bool
isValid pin =
    let
        hasCorrectLength p =
            String.length p == 6

        hasOnlyDigits =
            String.all Char.isDigit
    in
    hasCorrectLength pin && hasOnlyDigits pin


view : Shared -> Config msg -> Html msg
view shared { labelText, inputId, inputValue, onInputMsg, onToggleMsg, isVisible, errors } =
    let
        t =
            I18Next.t shared.translations

        tr : String -> I18Next.Replacements -> String
        tr =
            I18Next.tr shared.translations I18Next.Curly

        inputType =
            if isVisible then
                "text"

            else
                "password"

        errorClass =
            if List.length errors > 0 then
                "field-with-error"

            else
                ""
    in
    div [ class "relative mb-10" ]
        [ label
            [ class "input-label"
            , for inputId
            ]
            [ text labelText
            ]
        , input
            [ class "form-input min-w-full tracking-widest"
            , class errorClass
            , type_ inputType
            , id inputId
            , placeholder "******"
            , maxlength 6
            , value inputValue
            , onInput onInputMsg
            , required True
            , autocomplete False
            , attribute "inputmode" "numeric"
            ]
            []
        , div [ class "input-label pr-1 text-right text-white font-bold mt-1 absolute right-0" ]
            [ text <|
                tr
                    "edit.input_counter"
                    [ ( "current", String.fromInt <| String.length inputValue )
                    , ( "max", "6" )
                    ]
            ]
        , toggleViewPin isVisible (t "auth.pin.toggle.show") (t "auth.pin.toggle.hide") onToggleMsg
        , ul [ class "form-error-on-dark-bg absolute" ]
            (List.map (\error -> li [] [ text (t error) ]) errors)
        ]


toggleViewPin : Bool -> String -> String -> msg -> Html msg
toggleViewPin isVisible showLabel hideLabel msg =
    button
        [ class "absolute mt-3 uppercase text-xs right-0 mr-3 text-orange-300"
        , onClick msg
        , type_ "button"
        , attribute "tabindex" "-1"
        ]
        [ if isVisible then
            text hideLabel

          else
            text showLabel
        ]
