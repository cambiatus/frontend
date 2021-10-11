module View.Components exposing
    ( loadingLogoAnimated, loadingLogoAnimatedFluid, loadingLogoWithCustomText
    , dialogBubble
    , tooltip, pdfViewer, dateViewer, infiniteList, ElementToTrack(..)
    , bgNoScroll, PreventScroll(..), keyListener, Key(..), focusTrap
    )

{-| This module exports some simple components that don't need to manage any
state or configuration, such as loading indicators and containers


# Loading

@docs loadingLogoAnimated, loadingLogoAnimatedFluid, loadingLogoWithCustomText


# Containers

@docs dialogBubble


## Helper types

@docs Orientation


# Elements

@docs tooltip, pdfViewer, dateViewer, infiniteList, ElementToTrack


# Helpers

@docs bgNoScroll, PreventScroll, keyListener, Key, focusTrap

-}

import Html exposing (Html, div, img, node, p, span, text)
import Html.Attributes exposing (attribute, class, src)
import Html.Events exposing (on)
import Icons
import Json.Decode
import Session.Shared exposing (Shared, Translators)
import Time
import Translation
import Utils



-- LOADING


loadingLogoAnimated : Translators -> String -> Html msg
loadingLogoAnimated translators class_ =
    loadingLogoWithCustomText translators "loading.subtitle" class_


loadingLogoWithCustomText : Translators -> String -> String -> Html msg
loadingLogoWithCustomText { t } customTextKey class_ =
    div [ class ("w-full text-center " ++ class_) ]
        [ img [ class "h-16 mx-auto mt-8", src "/images/loading.svg" ] []
        , p [ class "font-bold text-xl" ] [ text <| t "loading.title" ]
        , p [ class "text-sm" ] [ text <| t customTextKey ]
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

        translationString =
            if Utils.areSameDay shared.timezone shared.now time then
                translations.today

            else if Utils.areSameDay shared.timezone shared.now yesterday then
                translations.yesterday

            else
                translations.other
    in
    if String.contains "{{date}}" translationString then
        dateFormatter attrs
            { language = shared.language
            , date = time
            , translationString = translationString
            }

    else if Utils.areSameDay shared.timezone shared.now time then
        span attrs [ text translations.today ]

    else if Utils.areSameDay shared.timezone shared.now yesterday then
        span attrs [ text translations.yesterday ]

    else
        text ""


type ElementToTrack
    = TrackSelf
    | TrackWindow
    | TrackSelector String


{-| An infinite list component. It automatically requests more items as the user
scrolls down, based on a `distanceToRequest`, which is the distance to the
bottom of the container. If you don't want to request more items (i.e. when
there are no more items), just pass `requestedItems = Nothing`.
-}
infiniteList :
    { onRequestedItems : Maybe msg
    , distanceToRequest : Int
    , elementToTrack : ElementToTrack
    }
    -> List (Html.Attribute msg)
    -> List (Html msg)
    -> Html msg
infiniteList options attrs children =
    let
        requestedItemsListener =
            case options.onRequestedItems of
                Nothing ->
                    class ""

                Just onRequestedItems ->
                    on "requested-items" (Json.Decode.succeed onRequestedItems)

        elementToTrackToString elementToTrack =
            case elementToTrack of
                TrackWindow ->
                    "track-window"

                TrackSelf ->
                    "track-self"

                TrackSelector selector ->
                    selector
    in
    node "infinite-list"
        (requestedItemsListener
            :: class "overflow-y-auto inline-block"
            :: attribute "elm-distance-to-request" (String.fromInt options.distanceToRequest)
            :: attribute "elm-element-to-track" (elementToTrackToString options.elementToTrack)
            :: attrs
        )
        children



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


type Key
    = Escape


{-| A node that attaches an event listener on the document to listen for keys.
Useful when we want to listen for keypresses, but don't want to use
subscriptions (because it would add a lot of complexity). Can be useful in
"stateless" components, such as modals.
-}
keyListener :
    { onKeyDown : { acceptedKeys : List Key, toMsg : Key -> msg, stopPropagation : Bool } }
    -> Html msg
keyListener { onKeyDown } =
    let
        keyFromString : String -> Maybe Key
        keyFromString rawKey =
            case rawKey of
                "Esc" ->
                    Just Escape

                "Escape" ->
                    Just Escape

                _ ->
                    Nothing

        keyDecoder : List Key -> (Key -> msg) -> Json.Decode.Decoder msg
        keyDecoder acceptedKeys toMsg =
            Json.Decode.at [ "detail", "key" ] Json.Decode.string
                |> Json.Decode.andThen
                    (\rawKey ->
                        case keyFromString rawKey of
                            Just key ->
                                if List.member key acceptedKeys then
                                    Json.Decode.succeed (toMsg key)

                                else
                                    Json.Decode.fail "This key is not being listened to"

                            Nothing ->
                                Json.Decode.fail "The given key is not registered as a Key that the keyListener can listen to"
                    )
    in
    node "key-listener"
        [ on "listener-keydown" (keyDecoder onKeyDown.acceptedKeys onKeyDown.toMsg)
        , attribute "keydown-stop-propagation" (boolToString onKeyDown.stopPropagation)
        ]
        []


focusTrap : { firstFocusContainer : Maybe String } -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
focusTrap { firstFocusContainer } attrs children =
    node "focus-trap"
        (optionalAttr "first-focus-container" firstFocusContainer :: attrs)
        children



-- INTERNALS


boolToString : Bool -> String
boolToString b =
    if b then
        "true"

    else
        "false"


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
