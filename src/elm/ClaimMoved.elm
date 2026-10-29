module ClaimMoved exposing (claimUrl, viewClaimLink)

{-| Claiming an action moved to the LiveView app at /app/community/objectives. The action card
in the Elm SPA links there, focused on the action, instead of opening the claim modal.

Like `ReviewMoved`, the link carries `target "_self"` so Elm does not route it inside the SPA.

-}

import Html exposing (Html, a, text)
import Html.Attributes exposing (class, href, id, target)
import ReviewMoved
import Translation


{-| Absolute URL of the app's objectives catalog, focused on one action.
-}
claimUrl : String -> Int -> String
claimUrl apiUrl actionId =
    ReviewMoved.appUrl apiUrl ("/app/community/objectives?action=" ++ String.fromInt actionId)


{-| Link that replaces the "claim" button of an action card.
-}
viewClaimLink : Translation.Translators -> String -> Int -> Html msg
viewClaimLink { t } apiUrl actionId =
    a
        [ id ("claim-in-new-app-" ++ String.fromInt actionId)
        , href (claimUrl apiUrl actionId)
        , target "_self"
        , class "button button-primary w-full sm:col-span-1"
        ]
        [ text (t "claim_moved.go") ]
