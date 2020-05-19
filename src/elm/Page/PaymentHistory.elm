module Page.PaymentHistory exposing (view)

import Html exposing (Html, button, div, h1, h2, img, p, text, ul)
import Html.Attributes exposing (class, src, style)
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
    div [ class "bg-white" ]
        [ viewSplash
        , h2 [ class "text-center text-black text-2xl" ] [ text "Payment History" ]
        , viewAccountAutocomplete
        , viewPeriodSelector
        , viewPayersList
        , viewPagination
        ]


viewSplash =
    div
        [ class "bg-black h-48 mb-6"
        , style "background" "url(/images/bg_cafe.png)"
        ]
        [ h1 [ class "text-white text-center font-bold text-3xl" ] [ text "Pura Vida Cafe" ]
        ]


viewAccountAutocomplete =
    div [] []


viewPeriodSelector =
    div [] []


viewPayment payment =
    div
        [ class "bg-gray-100 text-center p-4 my-6 rounded-lg"
        ]
        [ div [ class "rounded-full m-auto overflow-hidden border-white border-2 bg-grey w-14 h-14" ]
            [ img
                [ class "max-w-full max-h-full"
                , src payment.userpic
                ]
                []
            ]
        , p [ class "text-black" ]
            [ text payment.username ]
        , p [ class "uppercase text-gray-900 text-xs my-1" ]
            [ text payment.paymentDate ]
        , p [ class "text-green text-2xl my-2" ]
            [ text payment.paymentAmount ]
        , p [ class "tracking-widest" ]
            [ text payment.emojiHash ]
        ]


viewPayersList =
    ul [ class "mx-4 max-w-md md:m-auto" ]
        (List.map viewPayment
            (List.repeat
                10
                { userpic = "/images/woman.png"
                , username = "vasya222"
                , paymentDate = "today, 14:03"
                , paymentAmount = "1234 COS"
                , emojiHash = "\u{1F916}  🇨🇷  💜  😙  😎  💻  😇  🎃"
                }
            )
        )


viewPagination =
    div [ class "pb-8" ]
        [ button [ class "button m-auto button-primary" ] [ text "Show More" ] ]
