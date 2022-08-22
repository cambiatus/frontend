module View.Components exposing
    ( loadingLogoAnimated, loadingLogoAnimatedFluid, loadingLogoWithCustomText, loadingLogoWithNoText
    , dialogBubble, masonryLayout, keyedMasonryLayout, Breakpoint(..)
    , tooltip, pdfViewer, PdfViewerFileType(..), dateViewer, infiniteList, ElementToTrack(..), label, disablableLink
    , bgNoScroll, PreventScroll(..), keyListener, Key(..), focusTrap, intersectionObserver, pointerListener
    )

{-| This module exports some simple components that don't need to manage any
state or configuration, such as loading indicators and containers


# Loading

@docs loadingLogoAnimated, loadingLogoAnimatedFluid, loadingLogoWithCustomText, loadingLogoWithNoText


# Containers

@docs dialogBubble, masonryLayout, keyedMasonryLayout, Breakpoint


## Helper types

@docs Orientation


# Elements

@docs tooltip, pdfViewer, PdfViewerFileType, dateViewer, infiniteList, ElementToTrack, label, disablableLink


# Helpers

@docs bgNoScroll, PreventScroll, keyListener, Key, focusTrap, intersectionObserver, pointerListener

-}

import Html exposing (Html, div, img, node, p, span, text)
import Html.Attributes exposing (attribute, class, for, src)
import Html.Events exposing (on)
import Html.Keyed
import Icons
import Json.Decode
import Time
import Translation exposing (Translators)
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


loadingLogoWithNoText : String -> Html msg
loadingLogoWithNoText class_ =
    img [ class class_, src "/images/loading.svg" ] []



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


type Breakpoint
    = Sm
    | Lg
    | Xl


{-| Create a masonry layout, similar to Pinterest. This uses CSS Grid + some JS
magic. If you're changing `gap-y` or `auto-rows`, test it very well, since the
accuracy of this component depends on those properties. If you want vertical
gutters, give each child element a `mb-*` class and this element a negative bottom margin.

If the elements are buggy, try to include `self-start` in them.

You must provide at least one `Breakpoint` to specify screen sizes this should
work as a masonry layout.

-}
masonryLayout :
    List Breakpoint
    -> { transitionWithParent : Bool }
    -> List (Html.Attribute msg)
    -> List (Html msg)
    -> Html msg
masonryLayout breakpoints transitionWithParent attrs children =
    node "masonry-layout"
        (masonryLayoutAttributes breakpoints transitionWithParent attrs)
        children


{-| Create a masonry layout, similar to Pinterest. This uses CSS Grid + some JS
magic. If you're changing `gap-y` or `auto-rows`, test it very well, since the
accuracy of this component depends on those properties. If you want vertical
gutters, give each child element a `mb-*` class and this element a negative bottom margin.

If the elements are buggy, try to include `self-start` in them.

You must provide at least one `Breakpoint` to specify screen sizes this should
work as a masonry layout.

-}
keyedMasonryLayout :
    List Breakpoint
    -> { transitionWithParent : Bool }
    -> List (Html.Attribute msg)
    -> List ( String, Html msg )
    -> Html msg
keyedMasonryLayout breakpoints transitionWithParent attrs children =
    Html.Keyed.node "masonry-layout"
        (masonryLayoutAttributes breakpoints transitionWithParent attrs)
        children


masonryLayoutAttributes :
    List Breakpoint
    -> { transitionWithParent : Bool }
    -> List (Html.Attribute msg)
    -> List (Html.Attribute msg)
masonryLayoutAttributes breakpoints { transitionWithParent } attrs =
    let
        classesForBreakpoint breakpoint =
            case breakpoint of
                Sm ->
                    "sm:gap-y-0 sm:grid sm:auto-rows-[1px]"

                Lg ->
                    -- Tailwind might purge if we do something with List.map instead of explicitly writing these
                    "lg:gap-y-0 lg:grid lg:auto-rows-[1px]"

                Xl ->
                    "xl:gap-y-0 xl:grid xl:auto-rows-[1px]"
    in
    (List.map classesForBreakpoint breakpoints
        |> String.join " "
        |> class
    )
        :: attribute "elm-transition-with-parent" (boolToString transitionWithParent)
        :: attrs



-- ELEMENTS


tooltip : { message : String, iconClass : String, containerClass : String } -> Html msg
tooltip { message, iconClass, containerClass } =
    span [ class ("icon-tooltip ml-1 z-10 " ++ containerClass) ]
        [ Icons.question ("inline-block " ++ iconClass)
        , p [ class "icon-tooltip-content" ]
            [ text message ]
        ]


type PdfViewerFileType
    = Pdf
    | Image


{-| Display a PDF coming from a url. If the PDF cannot be read, display an `img`
with `url` as `src`. This element automatically shows a loading animation while
it fetches the pdf. If you pass in `Translators`, there will also be a text
under the loading animation
-}
pdfViewer :
    List (Html.Attribute msg)
    ->
        { url : String
        , childClass : String
        , maybeTranslators : Maybe Translators
        , onFileTypeDiscovered : Maybe (PdfViewerFileType -> msg)
        }
    -> Html msg
pdfViewer attrs { url, childClass, maybeTranslators, onFileTypeDiscovered } =
    let
        loadingAttributes =
            case maybeTranslators of
                Nothing ->
                    []

                Just { t } ->
                    [ attribute "elm-loading-title" (t "loading.title")
                    , attribute "elm-loading-subtitle" (t "loading.subtitle")
                    ]

        fileTypeDiscoveredListener =
            case onFileTypeDiscovered of
                Nothing ->
                    class ""

                Just eventListener ->
                    on "file-type-discovered"
                        (Json.Decode.string
                            |> Json.Decode.field "detail"
                            |> Json.Decode.andThen
                                (\stringFileType ->
                                    case stringFileType of
                                        "image" ->
                                            Json.Decode.succeed (eventListener Image)

                                        "pdf" ->
                                            Json.Decode.succeed (eventListener Pdf)

                                        _ ->
                                            Json.Decode.fail ("I was expecting either `image` or `pdf`, but got " ++ stringFileType ++ " instead")
                                )
                        )
    in
    node "pdf-viewer"
        (attribute "elm-url" url
            :: attribute "elm-child-class" childClass
            :: class "flex flex-col items-center justify-center"
            :: fileTypeDiscoveredListener
            :: loadingAttributes
            ++ attrs
        )
        []


type alias DateTranslations =
    { today : Maybe String
    , yesterday : Maybe String
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
    ->
        { shared
            | now : Time.Posix
            , timezone : Time.Zone
            , translators : Translators
            , language : Translation.Language
        }
    -> Time.Posix
    -> Html msg
dateViewer attrs fillInTranslations shared time =
    let
        yesterday =
            Utils.previousDay shared.now

        translations =
            fillInTranslations
                { today = Just (shared.translators.t "dates.today")
                , yesterday = Just (shared.translators.t "dates.yesterday")
                , other = "{{date}}"
                }

        translationString =
            if Utils.areSameDay shared.timezone shared.now time then
                translations.today
                    |> Maybe.withDefault translations.other

            else if Utils.areSameDay shared.timezone shared.now yesterday then
                translations.yesterday
                    |> Maybe.withDefault translations.other

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
        span attrs [ text (Maybe.withDefault translations.other translations.today) ]

    else if Utils.areSameDay shared.timezone shared.now yesterday then
        span attrs [ text (Maybe.withDefault translations.other translations.yesterday) ]

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


{-| A label element that enforces the label has an id to point to
-}
label : List (Html.Attribute msg) -> { targetId : String, labelText : String } -> Html msg
label attrs { targetId, labelText } =
    Html.label (class "label" :: for targetId :: attrs)
        [ text labelText
        ]


{-| An element that acts as a link when it's not disabled, or as regular text when it is disabled.
No styling is done, so you need to do it yourself wherever you're using this component, usually with `classList`:

    disablableLink { isDisabled = isDisabled }
        [ Route.href Route.Transfer
        , class "button button-primary"
        , classList [ ( "button-disabled", isDisabled ) ]
        ]
        [ text "Transfer to a friend " ]

-}
disablableLink : { isDisabled : Bool } -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
disablableLink { isDisabled } =
    if isDisabled then
        span

    else
        Html.a



-- HELPERS


{-| A node that prevents the body from scrolling
-}
bgNoScroll : List (Html.Attribute msg) -> PreventScroll -> Html msg
bgNoScroll attrs preventScroll =
    let
        preventScrollClass =
            case preventScroll of
                PreventScrollOnMobile ->
                    "overflow-hidden fixed w-full h-full md:overflow-auto md:static md:w-auto md:h-auto"

                PreventScrollAlways ->
                    "overflow-hidden fixed w-full h-full md:static md:w-auto md:h-auto"
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
    | Enter
    | Space
    | ArrowUp
    | ArrowDown
    | ArrowLeft
    | ArrowRight


{-| A node that attaches an event listener on the document to listen for keys.
Useful when we want to listen for keypresses, but don't want to use
subscriptions (because it would add a lot of complexity). Can be useful in
"stateless" components, such as modals.
-}
keyListener :
    { acceptedKeys : List Key
    , toMsg : Key -> msg
    , stopPropagation : Bool
    , preventDefault : Bool
    }
    -> Html msg
keyListener { acceptedKeys, toMsg, stopPropagation, preventDefault } =
    let
        keyFromString : String -> Maybe Key
        keyFromString rawKey =
            case String.toLower rawKey of
                "esc" ->
                    Just Escape

                "escape" ->
                    Just Escape

                " " ->
                    Just Space

                "enter" ->
                    Just Enter

                "arrowup" ->
                    Just ArrowUp

                "arrowdown" ->
                    Just ArrowDown

                "arrowleft" ->
                    Just ArrowLeft

                "arrowright" ->
                    Just ArrowRight

                _ ->
                    Nothing

        keyToString : Key -> List String
        keyToString key =
            case key of
                Escape ->
                    [ "esc", "escape" ]

                Space ->
                    [ " " ]

                Enter ->
                    [ "enter" ]

                ArrowUp ->
                    [ "arrowup" ]

                ArrowDown ->
                    [ "arrowdown" ]

                ArrowLeft ->
                    [ "arrowleft" ]

                ArrowRight ->
                    [ "arrowright" ]

        keyDecoder : List Key -> (Key -> msg) -> Json.Decode.Decoder msg
        keyDecoder acceptedKeys_ toMsg_ =
            Json.Decode.at [ "detail", "key" ] Json.Decode.string
                |> Json.Decode.andThen
                    (\rawKey ->
                        case keyFromString rawKey of
                            Just key ->
                                if List.member key acceptedKeys_ then
                                    Json.Decode.succeed (toMsg_ key)

                                else
                                    Json.Decode.fail "This key is not being listened to"

                            Nothing ->
                                Json.Decode.fail "The given key is not registered as a Key that the keyListener can listen to"
                    )
    in
    node "key-listener"
        [ on "listener-keydown" (keyDecoder acceptedKeys toMsg)
        , attribute "keydown-stop-propagation" (boolToString stopPropagation)
        , attribute "keydown-prevent-default" (boolToString preventDefault)
        , attribute "accepted-keys"
            (acceptedKeys
                |> List.concatMap keyToString
                |> String.join ","
            )
        ]
        []


focusTrap : { initialFocusId : Maybe String } -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
focusTrap { initialFocusId } attrs children =
    node "focus-trap"
        (optionalAttr "initial-focus-id" initialFocusId :: attrs)
        children


{-| A wrapper around the intersection observer API. Note that targetSelector is
a `String` that works with the `querySelector` API, so if you want to get an element
by id you need to use `#` as a prefix.
-}
intersectionObserver :
    { targetSelectors : List String
    , threshold : Float
    , breakpointToExclude : Maybe Breakpoint
    , onStartedIntersecting : Maybe (String -> msg)
    , onStoppedIntersecting : Maybe (String -> msg)
    }
    -> Html msg
intersectionObserver options =
    let
        optionalEvent eventName maybeToMsg =
            case maybeToMsg of
                Nothing ->
                    class ""

                Just toMsg ->
                    on eventName (decodeTargetId toMsg)

        decodeTargetId toMsg =
            Json.Decode.at [ "detail", "targetId" ] Json.Decode.string
                |> Json.Decode.map toMsg
    in
    node "intersection-observer"
        [ attribute "elm-target" (String.join " " options.targetSelectors)
        , attribute "elm-threshold" (String.fromFloat options.threshold)
        , attribute "elm-max-width"
            (case options.breakpointToExclude of
                Nothing ->
                    "none"

                Just breakpointToExclude ->
                    String.fromInt <| breakpointToPixels breakpointToExclude
            )
        , optionalEvent "started-intersecting" options.onStartedIntersecting
        , optionalEvent "stopped-intersecting" options.onStoppedIntersecting
        ]
        []


{-| A component that attaches events related to pointers to the document and the
element and passes it into Elm.

  - `document.onDragOver`: This event is fired when something is being dragged and
    the pointer moves.
  - `element.onDragEnd`: This event is fired when the user releases the pointer
    after a drag operation.
  - `element.onDragStart`: This event is fired whenever the user starts dragging
    an element.

Make sure the element has `draggble = true`.

-}
pointerListener :
    { document :
        { onDragOver :
            Maybe
                ({ x : Float
                 , y : Float
                 , previousX : Float
                 , previousY : Float
                 }
                 -> msg
                )
        }
    , element :
        { onDragStart :
            Maybe
                { dragImage : Maybe String
                , listener : msg
                }
        , onDragEnd : Maybe msg
        }
    }
    -> List (Html.Attribute msg)
    -> List (Html msg)
    -> Html msg
pointerListener { document, element } attrs children =
    node "pointer-listener"
        (optionalListenerWithArgs "document-dragover"
            document.onDragOver
            (Json.Decode.field "detail"
                (Json.Decode.map4
                    (\x y previousX previousY ->
                        { x = x
                        , y = y
                        , previousX = previousX
                        , previousY = previousY
                        }
                    )
                    (Json.Decode.field "clientX" Json.Decode.float)
                    (Json.Decode.field "clientY" Json.Decode.float)
                    (Json.Decode.field "previousX" Json.Decode.float)
                    (Json.Decode.field "previousY" Json.Decode.float)
                )
            )
            :: optionalListener "element-dragend" element.onDragEnd
            :: optionalListener "element-dragstart" (Maybe.map .listener element.onDragStart)
            :: optionalAttr "element-pointerdown-drag-image" (Maybe.andThen .dragImage element.onDragStart)
            :: attrs
        )
        children



-- INTERNALS


{-| Convert a breakpoint to it's minimum width value in pixels. Should be in sync
with our tailwind config
-}
breakpointToPixels : Breakpoint -> Int
breakpointToPixels breakpoint =
    case breakpoint of
        Sm ->
            640

        Lg ->
            1024

        Xl ->
            1280


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


optionalListener : String -> Maybe msg -> Html.Attribute msg
optionalListener eventName maybeMsg =
    case maybeMsg of
        Nothing ->
            class ""

        Just msg ->
            on eventName (Json.Decode.succeed msg)


optionalListenerWithArgs : String -> Maybe (a -> msg) -> Json.Decode.Decoder a -> Html.Attribute msg
optionalListenerWithArgs eventName maybeMsg decoder =
    case maybeMsg of
        Nothing ->
            class ""

        Just msg ->
            on eventName (Json.Decode.map msg decoder)


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
