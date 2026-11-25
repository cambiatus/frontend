module ShopMoved exposing (Role(..), buyUrl, createUrl, manageUrl, role, viewBuyLink, viewCreateMoved, viewOfferAction, viewSellerCard)

{-| Offers that the server marks `buyInApp` are bought and managed (lotes, orders, deliveries)
in the LiveView app. Elm has no rule of its own about them: it only follows `Product.buyInApp`
and links to the matching /app page through its login, which returns the member to `next`.

New offers are created only in the new app (it asks for the type and whether to publish), so
every "create offer" entry point links there and the editor's create route shows
`viewCreateMoved`. The server refuses a create from Elm.

Like `ReviewMoved`, links to the app carry `target "_self"` so Elm does not route them inside
the SPA.

-}

import Html exposing (Html, a, div, h1, h2, p, text)
import Html.Attributes exposing (class, href, id, target)
import ReviewMoved
import Shop
import Translation
import Url


{-| Absolute URL of the app's login, which sends the member on to `path` once signed in.
-}
loginThenUrl : String -> String -> String
loginThenUrl apiUrl path =
    ReviewMoved.appUrl apiUrl ("/app/login?next=" ++ Url.percentEncode path)


{-| Where an offer is bought in the new app.
-}
buyUrl : String -> Shop.Id -> String
buyUrl apiUrl offerId =
    loginThenUrl apiUrl ("/app/shop/" ++ Shop.idToString offerId ++ "/buy")


{-| Where a new offer is created in the new app.
-}
createUrl : String -> String
createUrl apiUrl =
    loginThenUrl apiUrl "/app/my-shop/offers/new"


{-| Body of the editor's create route (`/shop/new/...`), modelled on `ReviewMoved.viewMovedContent`.
-}
viewCreateMoved : Translation.Translators -> String -> Html msg
viewCreateMoved { t } apiUrl =
    div [ class "container mx-auto px-4 py-16 md:py-24 flex flex-col items-center text-center" ]
        [ h1 [ class "text-2xl leading-tight font-bold text-black" ] [ text (t "shop_moved.create_title") ]
        , p [ class "mt-4 max-w-md text-gray-333" ] [ text (t "shop_moved.create_body") ]
        , a
            [ id "create-in-new-app"
            , href (createUrl apiUrl)
            , target "_self"
            , class "button button-primary w-full md:w-auto mt-8"
            ]
            [ text (t "shop_moved.create_go") ]
        ]


{-| Where the seller manages an offer (lotes, orders, deliveries) in the new app.
-}
manageUrl : String -> Shop.Id -> String
manageUrl apiUrl offerId =
    loginThenUrl apiUrl ("/app/my-shop/offers/" ++ Shop.idToString offerId)


{-| Link that replaces every way of buying an offer that is bought in the new app.
-}
viewBuyLink : Translation.Translators -> String -> Shop.Id -> Html msg
viewBuyLink { t } apiUrl offerId =
    a
        [ id ("buy-in-new-app-" ++ Shop.idToString offerId)
        , href (buyUrl apiUrl offerId)
        , target "_self"
        , class "button button-primary w-full"
        ]
        [ text (t "shop_moved.buy_go") ]


{-| What a member is to an offer. Decided by role first: the owner of a `buyInApp` offer manages
it in the new app, anyone else buys it there, and every other offer is untouched.
-}
type Role
    = Owner
    | Buyer
    | Unchanged


role : { buyInApp : Bool, isOwner : Bool } -> Role
role { buyInApp, isOwner } =
    if not buyInApp then
        Unchanged

    else if isOwner then
        Owner

    else
        Buyer


{-| What the offer page shows where the buy form / edit button lives. `ownView` is exactly what
Elm showed before: untouched for `Unchanged`, below the seller card for `Owner`, and replaced by
the buy link for `Buyer`.
-}
viewOfferAction :
    Translation.Translators
    -> String
    -> { product | id : Shop.Id, buyInApp : Bool }
    -> { isOwner : Bool }
    -> Html msg
    -> Html msg
viewOfferAction translators apiUrl product { isOwner } ownView =
    case role { buyInApp = product.buyInApp, isOwner = isOwner } of
        Unchanged ->
            ownView

        Buyer ->
            viewBuyLink translators apiUrl product.id

        Owner ->
            div [] [ viewSellerCard translators apiUrl product.id, ownView ]


{-| Card telling the creator of a `buyInApp` offer that lotes and deliveries live in the new app.
-}
viewSellerCard : Translation.Translators -> String -> Shop.Id -> Html msg
viewSellerCard { t } apiUrl offerId =
    div
        [ id ("seller-card-new-app-" ++ Shop.idToString offerId)
        , class "w-full mb-4 p-4 rounded bg-white border border-gray-500 flex flex-col"
        ]
        [ h2 [ class "font-bold text-black" ] [ text (t "shop_moved.manage_title") ]
        , p [ class "mt-2 mb-4 text-gray-333" ] [ text (t "shop_moved.manage_body") ]
        , a
            [ id ("manage-in-new-app-" ++ Shop.idToString offerId)
            , href (manageUrl apiUrl offerId)
            , target "_self"
            , class "button button-primary w-full"
            ]
            [ text (t "shop_moved.manage_go") ]
        ]
