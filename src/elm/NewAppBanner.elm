module NewAppBanner exposing (cutover, dismissedFromFlags, handoffUrl, isVisible, view)

{-| Band at the top of every page (members and guests) announcing that the Elm app is
retired on 8 December 2026, with a link that opens the current screen in the new app.

The link goes through the backend's `/app/from-elm?path=` handoff, which maps any Elm
address to its LiveView screen, so the mapping is not repeated here.

Once closed it stays closed: the browser remembers it (`newAppBannerDismissed` flag and
the `storeNewAppBannerDismissed` port). From the cutover on the server redirects every Elm
address, so the band hides itself then.

-}

import Html exposing (Html, a, button, div, p, section, span, text)
import Html.Attributes exposing (attribute, class, href, id, target, type_)
import Html.Events exposing (onClick)
import Icons
import Json.Decode as Decode exposing (Value)
import ReviewMoved
import Session.Shared as Shared
import Time
import Translation
import Url exposing (Url)


{-| 2026-12-08T03:00:00Z, midnight in Brasília.
-}
cutover : Time.Posix
cutover =
    Time.millisToPosix 1796698800000


{-| Also hidden until the first translations arrive, so it never shows raw keys.
-}
isVisible : { dismissed : Bool, now : Time.Posix, translationsStatus : Shared.TranslationStatus } -> Bool
isVisible { dismissed, now, translationsStatus } =
    let
        translationsLoaded =
            case translationsStatus of
                Shared.LoadingTranslation ->
                    False

                Shared.LoadingTranslationFailed _ ->
                    False

                _ ->
                    True
    in
    not dismissed
        && translationsLoaded
        && (Time.posixToMillis now < Time.posixToMillis cutover)


{-| Whether the browser remembers the band as closed. Missing means it was never closed.
-}
dismissedFromFlags : Value -> Bool
dismissedFromFlags flags =
    Decode.decodeValue (Decode.field "newAppBannerDismissed" Decode.bool) flags
        |> Result.withDefault False


{-| Handoff address for the current Elm screen: its path and query (no host or fragment),
URI-encoded into `/app/from-elm?path=`.
-}
handoffUrl : String -> Url -> String
handoffUrl apiUrl url =
    let
        pathAndQuery =
            case url.query of
                Just query ->
                    url.path ++ "?" ++ query

                Nothing ->
                    url.path
    in
    ReviewMoved.appUrl apiUrl ("/app/from-elm?path=" ++ Url.percentEncode pathAndQuery)


view : Translation.Translators -> { href : String, onDismiss : msg } -> Html msg
view { t } config =
    section
        [ id "new-app-banner"
        , class "bg-[#2E4A2C] text-white"
        , attribute "aria-label" (t "new_app_banner.label")
        ]
        [ div [ class "container mx-auto flex items-center gap-2 py-2 pl-4 pr-2" ]
            [ p [ class "flex-1 min-w-0" ]
                [ text (t "new_app_banner.notice")
                , text " "

                -- target "_self" is required: without it Elm routes this same-host
                -- URL inside the SPA (Not Found), see `Page.AdminMoved`.
                , a
                    [ id "new-app-banner-go"
                    , href config.href
                    , target "_self"
                    , class "font-bold underline whitespace-nowrap focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-[#FFE400]"
                    ]
                    [ text (t "new_app_banner.action")
                    , span [ attribute "aria-hidden" "true" ] [ text " →" ]
                    ]
                ]
            , button
                [ id "new-app-banner-dismiss"
                , type_ "button"
                , class "flex-none w-10 h-10 flex items-center justify-center rounded focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-[#FFE400]"
                , attribute "aria-label" (t "new_app_banner.dismiss")
                , onClick config.onDismiss
                ]
                [ Icons.close "w-4 h-4 fill-current" ]
            ]
        ]
