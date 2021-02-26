module View.Components exposing (loadingLogoAnimated, loadingLogoAnimatedFluid)

import Html exposing (div, img, p, text)
import Html.Attributes exposing (class, src)
import Session.Shared exposing (Translators)


loadingLogoAnimated : Translators -> Html.Html msg
loadingLogoAnimated { t } =
    div [ class "w-full text-center" ]
        [ img [ class "h-16 mx-auto mt-8", src "/images/loading.gif" ] []
        , p [ class "font-bold text-2xl" ] [ text <| t "loading.title" ]
        , p [ class "text-sm" ] [ text <| t "loading.subtitle" ]
        ]


loadingLogoAnimatedFluid : Html.Html msg
loadingLogoAnimatedFluid =
    div [ class "w-full text-center h-full py-2" ]
        [ img [ class "mx-auto h-full", src "/images/loading.gif" ] [] ]
