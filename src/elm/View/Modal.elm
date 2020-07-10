module View.Modal exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, stopPropagationOn)
import Icons
import Json.Decode as Decode


type alias Config msg =
    { content : Html msg
    , closeMsg : msg
    , ignoreMsg : msg
    }


type Visibility
    = Hidden
    | Shown


hidden : Visibility
hidden =
    Hidden


shown : Visibility
shown =
    Shown


view : Visibility -> Config msg -> Html msg
view status cfg =
    case status of
        Shown ->
            div
                [ class "modal container fade-in"
                , stopPropagationOn "click" (Decode.succeed ( cfg.ignoreMsg, True ))
                ]
                [ div
                    [ class "modal-bg"
                    , onClick cfg.closeMsg
                    ]
                    []
                , div
                    [ class "modal-content overflow-auto" ]
                    [ button
                        [ class "absolute top-0 right-0 mx-4 my-4"
                        , onClick cfg.closeMsg
                        ]
                        [ Icons.close "text-gray-400 fill-current"
                        ]
                    , div [ class "display flex flex-col justify-around h-full" ]
                        [ cfg.content ]
                    ]
                ]

        Hidden ->
            text ""
