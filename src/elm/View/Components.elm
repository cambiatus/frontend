module View.Components exposing
    ( loadingLogoAnimated, loadingLogoAnimatedFluid
    , dialogBubble
    , tooltip, pdfViewer
    , bgNoScroll, PreventScroll(..)
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

@docs bgNoScroll, PreventScroll

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
with `url` as `src`. This element automatically shows a loading animation while
it fetches the pdf. If you pass in `Translators`, there will also be a text
under the loading animation
-}
pdfViewer : List (Html.Attribute msg) -> { url : String, childClass : String, maybeTranslators : Maybe Translators } -> Html msg
pdfViewer attrs { url, childClass, maybeTranslators } =
    let
        loadingAttributes =
            case maybeTranslators of
                Nothing ->
                    []

                Just { t } ->
                    [ attribute "elm-loading-title" (t "loading.title")
                    , attribute "elm-loading-subtitle" (t "loading.subtitle")
                    ]
    in
    node "pdf-viewer"
        (attribute "elm-url" url
            :: attribute "elm-child-class" childClass
            :: class "flex flex-col items-center justify-center"
            :: loadingAttributes
            ++ attrs
        )
        []



-- HELPERS


{-| A node that prevents the body from scrolling
-}
bgNoScroll : List (Html.Attribute msg) -> PreventScroll -> Html msg
bgNoScroll attrs preventScroll =
    let
        preventScrollClass =
            case preventScroll of
                PreventScrollOnMobile ->
                    "overflow-hidden md:overflow-auto"

                PreventScrollAlways ->
                    "overflow-hidden"
    in
    node "bg-no-scroll"
        (attribute "elm-prevent-scroll-class" preventScrollClass
            :: attrs
        )
        []


type PreventScroll
    = PreventScrollOnMobile
    | PreventScrollAlways
