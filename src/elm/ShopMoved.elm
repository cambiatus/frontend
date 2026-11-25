module ShopMoved exposing (Role(..), buyUrl, createUrl, manageUrl, role, viewCreateMoved, viewOfferAction, viewSellerCard)

{-| Offers that the server marks `buyInApp` are bought and managed (lotes, orders, deliveries)
in the LiveView app. Elm has no rule of its own about them: it only follows `Product.buyInApp`
and links to the matching /app page through its login, which returns the member to `next`.

New offers are created only in the new app (it asks for the type and whether to publish), so
every "create offer" entry point links there and the editor's create route shows
`viewCreateMoved`. The server refuses a create from Elm.

Like `ReviewMoved`, links to the app carry `target "_self"` so Elm does not route them inside
the SPA.

-}

import Eos
import Html exposing (Html, a, div, h1, h2, p, section, span, text)
import Html.Attributes exposing (class, href, id, target)
import NewAppCallout
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
the price and the invitation to buy in the new app for `Buyer`.
-}
viewOfferAction :
    Translation.Translators
    -> String
    -> { product | id : Shop.Id, buyInApp : Bool, price : Float, symbol : Eos.Symbol }
    -> { isOwner : Bool, balance : Maybe Eos.Asset }
    -> Html msg
    -> Html msg
viewOfferAction translators apiUrl product { isOwner, balance } ownView =
    case role { buyInApp = product.buyInApp, isOwner = isOwner } of
        Unchanged ->
            ownView

        Buyer ->
            div [ class "flex flex-col gap-4" ]
                [ viewPrice translators product balance
                , viewBuyInvite translators apiUrl product.id
                ]

        Owner ->
            div [] [ viewSellerCard translators apiUrl product.id, ownView ]


{-| The price block of Elm's buy form, without the form: the offer reads as it always did.
-}
viewPrice :
    Translation.Translators
    -> { product | id : Shop.Id, price : Float, symbol : Eos.Symbol }
    -> Maybe Eos.Asset
    -> Html msg
viewPrice ({ tr } as translators) product balance =
    div
        [ id ("buy-price-" ++ Shop.idToString product.id)
        , class "bg-green pt-4 rounded-sm flex flex-col text-white font-bold text-center"
        ]
        [ span [ class "text-xl px-4" ] [ text (String.fromFloat product.price) ]
        , span [ class "px-4 mb-4" ]
            [ text (tr "shop.transfer.price" [ ( "symbol", Eos.symbolToSymbolCodeString product.symbol ) ]) ]
        , case balance of
            Nothing ->
                text ""

            Just asset ->
                span [ class "text-sm py-3 px-4 bg-black bg-opacity-20 rounded-b-sm uppercase" ]
                    [ text (tr "shop.transfer.balance" [ ( "asset", Eos.assetToString translators asset ) ]) ]
        ]


{-| Invitation to buy in the new app, in the dashboard banner's look (`NewAppCallout`).
-}
viewBuyInvite : Translation.Translators -> String -> Shop.Id -> Html msg
viewBuyInvite { t } apiUrl offerId =
    section
        [ id ("buy-invite-" ++ Shop.idToString offerId)
        , class "relative overflow-hidden rounded-large p-5 text-white bg-gradient-to-br from-[#2E4A2C] to-[#3F6B3A]"
        ]
        [ NewAppCallout.viewSpark "right-6 top-4 text-[#FFE400]"
        , NewAppCallout.viewSpark "right-16 top-14 text-[#5AD1C9]"
        , div [ class "flex items-center gap-3 mb-3" ]
            [ NewAppCallout.viewDoggo "w-12"
            , NewAppCallout.viewEyebrow (t "shop_moved.buy_eyebrow")
            ]
        , h2 [ class "text-lg font-bold leading-snug" ] [ text (t "shop_moved.buy_title") ]
        , p [ class "mt-2 text-[#D7E6D2]" ] [ text (t "shop_moved.buy_body") ]
        , p [ class "mt-2 text-sm text-[#D7E6D2]" ]
            (NewAppCallout.viewEmphasised (t "shop_moved.buy_account"))
        , a
            [ id ("buy-in-new-app-" ++ Shop.idToString offerId)
            , href (buyUrl apiUrl offerId)
            , target "_self"
            , class ("mt-5 " ++ NewAppCallout.buttonClass)
            ]
            [ text (t "shop_moved.buy_go")
            , NewAppCallout.viewArrowIcon
            ]
        ]


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
