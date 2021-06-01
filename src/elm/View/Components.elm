module View.Components exposing
    ( loadingLogoAnimated, loadingLogoAnimatedFluid
    , dialogBubble
    , tooltip, pdfViewer
    , bgNoScroll
    )

{-| This module exports some simple components that don't need to manage any
state or configuration, such as loading indicators and containers


# Loading

@docs loadingLogoAnimated, loadingLogoAnimatedFluid


# Containers

@docs dialogBubble


## Helper types

@docs Orientation


# Elements

@docs tooltip, pdfViewer


# Helpers

@docs bgNoScroll

-}

import Html exposing (Html, div, img, node, p, span, text)
import Html.Attributes exposing (attribute, class, src)
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


dialogBubble : { class_ : String, minWidth : Int } -> List (Html msg) -> Html msg
dialogBubble { class_, minWidth } elements =
    node "dialog-bubble"
        [ attribute "elm-min-width" (String.fromInt minWidth)
        , attribute "elm-class" class_
        ]
        elements



-- ELEMENTS


tooltip : Translators -> String -> Html msg
tooltip { t } tooltipMessage =
    span [ class "icon-tooltip ml-1" ]
        [ Icons.question "inline-block"
        , div [ class "icon-tooltip-content" ]
            [ text (t tooltipMessage) ]
        ]


{-| Display a PDF coming from a url. If the PDF cannot be read, display an `img`
with `url` as `src`
-}
pdfViewer : List (Html.Attribute msg) -> { url : String, childClass : String } -> Html msg
pdfViewer attrs { url, childClass } =
    node "pdf-viewer"
        ([ attribute "elm-url" url
         , attribute "elm-child-class" childClass
         , class "flex items-center justify-center"
         ]
            ++ attrs
        )
        []



-- HELPERS


{-| A node that prevents the body from scrolling
-}
bgNoScroll : List (Html.Attribute msg) -> Html msg
bgNoScroll attrs =
    node "bg-no-scroll" attrs []
