module View.Components exposing (loadingLogoAnimated, loadingLogoAnimatedFluid, tooltip)

import Html exposing (Html, div, img, p, span, text)
import Html.Attributes exposing (class, src)
import Icons
import Session.Shared exposing (Translators)


loadingLogoAnimated : Translators -> Html msg
loadingLogoAnimated { t } =
    div [ class "w-full text-center" ]
        [ img [ class "h-16 mx-auto mt-8", src "/images/loading.gif" ] []
        , p [ class "font-bold text-2xl" ] [ text <| t "loading.title" ]
        , p [ class "text-sm" ] [ text <| t "loading.subtitle" ]
        ]


loadingLogoAnimatedFluid : Html msg
loadingLogoAnimatedFluid =
    div [ class "w-full text-center h-full py-2" ]
        [ img [ class "mx-auto h-full", src "/images/loading.gif" ] [] ]


tooltip : Translators -> String -> Html msg
tooltip { t } tooltipMessage =
    span [ class "icon-tooltip ml-1" ]
        [ Icons.question "inline-block"
        , div [ class "icon-tooltip-content" ]
            [ text (t tooltipMessage) ]
        ]
