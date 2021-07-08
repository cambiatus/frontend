module View.Components exposing
    ( loadingLogoAnimated, loadingLogoAnimatedFluid
    , dialogBubble
    , tooltip, pdfViewer
    , bgNoScroll, PreventScroll(..)
    , dateViewer
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
import Session.Shared exposing (Shared, Translators)
import Time
import Translation
import Utils



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


dialogBubble :
    { class_ : String
    , relativeSelector : Maybe String
    , scrollSelector : Maybe String
    }
    -> List (Html msg)
    -> Html msg
dialogBubble { class_, relativeSelector, scrollSelector } elements =
    node "dialog-bubble"
        [ attribute "elm-class" class_
        , optionalAttr "elm-relative-selector" relativeSelector
        , optionalAttr "elm-scroll-selector" scrollSelector
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


type alias DateTranslations =
    { today : String
    , yesterday : String
    , other : String
    }


{-| A helper to display dates. Supports providing your own translated strings
for when the date is today or yesterday

    dateViewer []
        (\translations ->
            { translations
                | today = t "claim.claimed_today"
                , yesterday = t "claim.claimed_yesterday"
                , other = "claim.claimed_on"
            }
        )
        shared
        claim.claimDate

The `other` key on the translations record needs a `{{date}}` somewhere in the
string so we can replace it on JS

-}
dateViewer :
    List (Html.Attribute msg)
    -> (DateTranslations -> DateTranslations)
    -> Shared
    -> Time.Posix
    -> Html msg
dateViewer attrs fillInTranslations shared time =
    let
        yesterday =
            Utils.previousDay shared.now

        translations =
            fillInTranslations
                { today = shared.translators.t "dates.today"
                , yesterday = shared.translators.t "dates.yesterday"
                , other = "{{date}}"
                }
    in
    if Utils.areSameDay shared.timezone shared.now time then
        span attrs [ text translations.today ]

    else if Utils.areSameDay shared.timezone shared.now yesterday then
        span attrs [ text translations.yesterday ]

    else
        dateFormatter attrs
            { language = shared.language
            , date = time
            , translationString = translations.other
            }



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



-- INTERNALS


optionalAttr : String -> Maybe String -> Html.Attribute msg
optionalAttr attr maybeAttr =
    case maybeAttr of
        Nothing ->
            class ""

        Just attrValue ->
            attribute attr attrValue


dateFormatter :
    List (Html.Attribute msg)
    -> { language : Translation.Language, date : Time.Posix, translationString : String }
    -> Html msg
dateFormatter attrs { language, date, translationString } =
    node "date-formatter"
        (attribute "elm-locale" (Translation.languageToLocale language)
            :: attribute "elm-date" (date |> Time.posixToMillis |> String.fromInt)
            :: attribute "elm-translation" translationString
            :: attrs
        )
        []
