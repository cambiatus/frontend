module Page.PaymentHistory exposing (view)

import Html exposing (Html, button, div, h1, h2, img, input, label, p, span, text, ul)
import Html.Attributes exposing (class, for, placeholder, src, style)
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
        , div [ class "mx-4 max-w-md md:m-auto" ]
            [ h2 [ class "text-center text-black text-2xl" ] [ text "Payment History" ]
            , viewUserAutocomplete
            , viewPeriodSelector
            , viewPayersList
            , viewPagination
            ]
        ]


viewSplash =
    div
        [ class "bg-black bg-cover h-56 mb-6 flex justify-center items-center"
        , style "background-image" "url(/images/bg_cafe.png)"
        ]
        [ h1 [ class "text-white text-center text-5xl mx-3" ] [ text "Pura Vida Cafe" ]
        ]


viewUserAutocomplete =
    div [ class "my-4" ]
        [ label
            [ class "block" ]
            [ span [ class "text-green tracking-wide uppercase text-caption block mb-1" ]
                [ text "User" ]
            ]
        , input
            [ class "input min-w-full"
            , placeholder "Type username here"
            ]
            []
        ]


viewPeriodSelector =
    div [ class "my-4" ]
        [ label
            [ class "block" ]
            [ span [ class "text-green tracking-wide uppercase text-caption block mb-1" ]
                [ text "Period" ]
            ]
        , input
            [ class "input min-w-full"
            , placeholder "27 Oct 2020 (today)"
            ]
            []
        ]


viewPayment payment =
    div
        [ class "bg-gray-100 text-center py-6 my-6 rounded-lg"
        ]
        [ div [ class "rounded-full m-auto overflow-hidden border-white border-2 bg-grey w-14 h-14" ]
            [ img
                [ class "max-w-full max-h-full"
                , src payment.userpic
                ]
                []
            ]
        , p [ class "text-black mt-2" ]
            [ text payment.username ]
        , p [ class "uppercase text-gray-900 text-xs my-1" ]
            [ text payment.paymentDate ]
        , p [ class "text-green text-4xl my-3" ]
            [ text payment.paymentAmount ]
        , p [ class "tracking-widest text-2xl" ]
            [ text payment.emojiHash ]
        ]


viewPayersList =
    ul [ class "" ]
        (List.map viewPayment
            (List.repeat
                10
                { userpic = "/images/woman.png"
                , username = "vasya222"
                , paymentDate = "today, 14:03"
                , paymentAmount = "1234 COS"
                , emojiHash = "\u{1F916}🇨🇷💜😙😎💻😇🎃"
                }
            )
        )


viewPagination =
    div [ class "pb-8" ]
        [ button [ class "button m-auto button-primary w-full sm:w-40" ] [ text "Show More" ] ]
