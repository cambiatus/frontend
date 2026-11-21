module ShopMoved exposing (buyUrl, manageUrl, showsSellerCard, viewBuyLink, viewBuyOr, viewSellerCard)

{-| Offers that the server marks `buyInApp` are bought and managed (lotes, orders, deliveries)
in the LiveView app. Elm has no rule of its own about them: it only follows `Product.buyInApp`
and links to the matching /app page through its login, which returns the member to `next`.

Like `ReviewMoved`, links to the app carry `target "_self"` so Elm does not route them inside
the SPA.

-}

import Eos.Account
import Html exposing (Html, a, div, h2, p, text)
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


{-| The link to the new app for an offer that is `buyInApp`, otherwise `ownView` untouched.
Every place that would let a member buy an offer goes through this.
-}
viewBuyOr : Translation.Translators -> String -> { product | id : Shop.Id, buyInApp : Bool } -> Html msg -> Html msg
viewBuyOr translators apiUrl product ownView =
    if product.buyInApp then
        viewBuyLink translators apiUrl product.id

    else
        ownView


{-| Only the creator of a `buyInApp` offer is told that lotes and deliveries live in the new app.
-}
showsSellerCard : Eos.Account.Name -> { product | creatorId : Eos.Account.Name, buyInApp : Bool } -> Bool
showsSellerCard viewer product =
    product.buyInApp && product.creatorId == viewer


{-| Card shown to the creator of an offer that is managed in the new app. Empty for anyone else
and for offers that are not `buyInApp`.
-}
viewSellerCard :
    Translation.Translators
    -> String
    -> Eos.Account.Name
    -> { product | id : Shop.Id, creatorId : Eos.Account.Name, buyInApp : Bool }
    -> Html msg
viewSellerCard { t } apiUrl viewer product =
    if showsSellerCard viewer product then
        div
            [ id ("seller-card-new-app-" ++ Shop.idToString product.id)
            , class "w-full mb-4 p-4 rounded bg-white border border-gray-500 flex flex-col"
            ]
            [ h2 [ class "font-bold text-black" ] [ text (t "shop_moved.manage_title") ]
            , p [ class "mt-2 mb-4 text-gray-333" ] [ text (t "shop_moved.manage_body") ]
            , a
                [ id ("manage-in-new-app-" ++ Shop.idToString product.id)
                , href (manageUrl apiUrl product.id)
                , target "_self"
                , class "button button-primary w-full"
                ]
                [ text (t "shop_moved.manage_go") ]
            ]

    else
        text ""
