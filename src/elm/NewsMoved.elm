module NewsMoved exposing (newsUrl)

{-| Reading community news moved to the LiveView app at /app/community/news. The Elm SPA no
longer shows news: `Route.News` redirects there, and every news link points there.

Like `ReviewMoved`, links to the app must carry `target "_self"` so Elm does not route them
inside the SPA.

-}

import ReviewMoved


{-| Absolute URL of the app's news page, optionally focused on one story.
-}
newsUrl : String -> Maybe Int -> String
newsUrl apiUrl maybeNewsId =
    case maybeNewsId of
        Just newsId ->
            ReviewMoved.appUrl apiUrl ("/app/community/news/" ++ String.fromInt newsId)

        Nothing ->
            ReviewMoved.appUrl apiUrl "/app/community/news"
