module View.Components exposing
    ( loadingLogoAnimated, loadingLogoAnimatedFluid
    , dialogBubble
    , tooltip
    )

{-| This module exports some simple components that don't need to manage any
state or configuration, such as loading indicators and containers


# Loading

@docs loadingLogoAnimated, loadingLogoAnimatedFluid


# Containers

@docs dialogBubble


# Elements

@docs tooltip

-}

import Html exposing (Html, div, img, p, span, text)
import Html.Attributes exposing (class, src)
import Icons
import Session.Shared exposing (Translators)



-- LOADING


loadingLogoAnimated : Translators -> String -> Html msg
loadingLogoAnimated { t } class_ =
    div [ class ("w-full text-center " ++ class_) ]
        [ img [ class "h-16 mx-auto mt-8", src "/images/loading.svg" ] []
        , p [ class "font-bold text-2xl" ] [ text <| t "loading.title" ]
        , p [ class "text-sm" ] [ text <| t "loading.subtitle" ]
        ]


{-| A fluid-size loading indicator, fills the space as much as possible
-}
loadingLogoAnimatedFluid : Html msg
loadingLogoAnimatedFluid =
    div [ class "w-full text-center h-full py-2" ]
        [ img [ class "mx-auto h-full", src "/images/loading.svg" ] [] ]



-- CONTAINERS


dialogBubble : List (Html.Attribute msg) -> List (Html msg) -> Html msg
dialogBubble attrs elements =
    div (class "p-6 bg-white flex rounded shadow-2xl" :: attrs)
        (div [ class "absolute transform left-1/2 -bottom-1 -translate-x-1/2 -z-10" ]
            [ div [ class "w-8 h-8 bg-white transform -rotate-45 rounded-sm" ] []
            ]
            :: elements
        )



-- ELEMENTS


tooltip : Translators -> String -> Html msg
tooltip { t } tooltipMessage =
    span [ class "icon-tooltip ml-1" ]
        [ Icons.question "inline-block"
        , div [ class "icon-tooltip-content" ]
            [ text (t tooltipMessage) ]
        ]
