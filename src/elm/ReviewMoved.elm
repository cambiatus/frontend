module ReviewMoved exposing (appUrl, cardCountKey, reviewUrl, viewClaimCta, viewMovedContent)

{-| Claim validation (voting) moved to the LiveView app at /app/review. Everything that
used to let a validator vote in the Elm SPA now links there instead.

Links to the Phoenix server must carry `target "_self"`: without it Elm routes the same-host
URL inside the SPA (Not Found), see `Page.AdminMoved`.

-}

import Html exposing (Html, a, div, h1, p, text)
import Html.Attributes exposing (class, href, id, target)
import Route
import Translation


{-| Absolute URL of a path in the LiveView app: the API base without a trailing slash, then the path.
-}
appUrl : String -> String -> String
appUrl apiUrl path =
    let
        base =
            if String.endsWith "/" apiUrl then
                String.dropRight 1 apiUrl

            else
                apiUrl
    in
    base ++ path


{-| Absolute URL of the review page, optionally focused on one claim. Built on the same API
base as `Page.AdminMoved.loginUrl`.
-}
reviewUrl : String -> Maybe Int -> String
reviewUrl apiUrl maybeClaimId =
    case maybeClaimId of
        Just claimId ->
            appUrl apiUrl ("/app/review?claim=" ++ String.fromInt claimId)

        Nothing ->
            appUrl apiUrl "/app/review"


{-| Translation key of the dashboard card count: singular for one claim, plural otherwise.
-}
cardCountKey : Int -> String
cardCountKey amount =
    if amount == 1 then
        "review_moved.card_count_one"

    else
        "review_moved.card_count"


{-| Short line and button that replace the approve/reject buttons for a claim.
-}
viewClaimCta : Translation.Translators -> String -> Int -> List (Html.Attribute msg) -> Html msg
viewClaimCta { t } apiUrl claimId attributes =
    div (class "flex flex-col items-center text-center" :: attributes)
        [ p [ class "text-gray-333 mb-2" ] [ text (t "review_moved.body") ]
        , a
            [ id ("vote-in-new-app-" ++ String.fromInt claimId)
            , href (reviewUrl apiUrl (Just claimId))
            , target "_self"
            , class "button button-primary w-full"
            ]
            [ text (t "review_moved.go") ]
        ]


{-| Body of the `/dashboard/analysis` page, modelled on `Page.AdminMoved`.
-}
viewMovedContent : Translation.Translators -> String -> Html msg
viewMovedContent { t } apiUrl =
    div [ class "container mx-auto px-4 py-16 md:py-24 flex flex-col items-center text-center" ]
        [ h1 [ class "text-2xl leading-tight font-bold text-black" ] [ text (t "review_moved.title") ]
        , p [ class "mt-4 max-w-md text-gray-333" ] [ text (t "review_moved.body") ]
        , a
            [ id "review-moved-go"
            , href (reviewUrl apiUrl Nothing)
            , target "_self"
            , class "button button-primary w-full md:w-auto mt-8"
            ]
            [ text (t "review_moved.go") ]
        , a [ Route.href Route.Dashboard, class "button button-secondary w-full md:w-auto mt-4" ]
            [ text (t "admin_moved.back") ]
        ]
