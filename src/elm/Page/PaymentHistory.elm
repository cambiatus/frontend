module Page.PaymentHistory exposing (view)

import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class)
import I18Next exposing (t)
import Page


view : Page.Session -> Html msg
view session =
    let
        shared =
            Page.toShared session

        text_ =
            t shared.translations
    in
    div [ class "" ]
        [ div [ class "bg-black h-48" ]
            [ h1 [ class "text-white text-center font-bold text-3xl" ] [ text "Pura Vida Cafe" ]
            ]
        ]
