module ShopMovedTests exposing (all)

import Eos
import Expect
import Html exposing (div, text)
import Html.Attributes
import Shop
import ShopMoved
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Url
import Url.Parser


translators : { t : String -> String, tr : String -> List ( String, String ) -> String }
translators =
    { t = identity, tr = \key _ -> key }


withOfferId : (Shop.Id -> Expect.Expectation) -> Expect.Expectation
withOfferId expectation =
    Url.fromString "https://cambiatus.test/7"
        |> Maybe.andThen (Url.Parser.parse Shop.idUrlParser)
        |> Maybe.map expectation
        |> Maybe.withDefault (Expect.fail "could not build a Shop.Id")


product : Shop.Id -> Bool -> { id : Shop.Id, buyInApp : Bool, price : Float, symbol : Eos.Symbol }
product offerId buyInApp =
    { id = offerId, buyInApp = buyInApp, price = 30, symbol = muda }


muda : Eos.Symbol
muda =
    Eos.symbolFromString "0,MUDA" |> Maybe.withDefault Eos.cambiatusSymbol


buyer : { isOwner : Bool, balance : Maybe Eos.Asset }
buyer =
    { isOwner = False, balance = Nothing }


owner : { isOwner : Bool, balance : Maybe Eos.Asset }
owner =
    { isOwner = True, balance = Nothing }


all : Test
all =
    describe "ShopMoved"
        [ test "buyUrl goes through the login and returns to the buy page" <|
            \_ ->
                withOfferId <|
                    \offerId ->
                        ShopMoved.buyUrl "https://muda.cambiatus.io/" offerId
                            |> Expect.equal "https://muda.cambiatus.io/app/login?next=%2Fapp%2Fshop%2F7%2Fbuy"
        , test "manageUrl goes through the login and returns to the seller's offer" <|
            \_ ->
                withOfferId <|
                    \offerId ->
                        ShopMoved.manageUrl "http://localhost:4000" offerId
                            |> Expect.equal "http://localhost:4000/app/login?next=%2Fapp%2Fmy-shop%2Foffers%2F7"
        , test "createUrl goes through the login and returns to the new offer form" <|
            \_ ->
                ShopMoved.createUrl "https://muda.cambiatus.io/"
                    |> Expect.equal "https://muda.cambiatus.io/app/login?next=%2Fapp%2Fmy-shop%2Foffers%2Fnew"
        , describe "viewCreateMoved"
            [ test "replaces the offer editor with a same-tab link to the new app" <|
                \_ ->
                    ShopMoved.viewCreateMoved translators "https://x"
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has [ Selector.text "shop_moved.create_title" ]
                            , Query.find [ Selector.id "create-in-new-app" ]
                                >> Query.has
                                    [ Selector.tag "a"
                                    , Selector.attribute (Html.Attributes.href "https://x/app/login?next=%2Fapp%2Fmy-shop%2Foffers%2Fnew")
                                    , Selector.attribute (Html.Attributes.target "_self")
                                    , Selector.text "shop_moved.create_go"
                                    ]
                            ]
            ]
        , describe "role"
            [ test "owner of a buyInApp offer" <|
                \_ -> ShopMoved.role { buyInApp = True, isOwner = True } |> Expect.equal ShopMoved.Owner
            , test "non-owner of a buyInApp offer" <|
                \_ -> ShopMoved.role { buyInApp = True, isOwner = False } |> Expect.equal ShopMoved.Buyer
            , test "owner of an offer that stays in Elm" <|
                \_ -> ShopMoved.role { buyInApp = False, isOwner = True } |> Expect.equal ShopMoved.Unchanged
            , test "non-owner of an offer that stays in Elm" <|
                \_ -> ShopMoved.role { buyInApp = False, isOwner = False } |> Expect.equal ShopMoved.Unchanged
            ]
        , describe "viewOfferAction (the Viewer branch)"
            [ test "a buyer of a buyInApp offer gets the link instead of the form" <|
                \_ ->
                    withOfferId <|
                        \offerId ->
                            ShopMoved.viewOfferAction translators "https://x" (product offerId True) buyer ownView
                                |> Query.fromHtml
                                |> Expect.all
                                    [ Query.has [ Selector.id "buy-in-new-app-7" ]
                                    , Query.hasNot [ Selector.text "elm edit button" ]
                                    , Query.hasNot [ Selector.id "seller-card-new-app-7" ]
                                    ]
            , test "a buyer sees the offer's price and a friendly invite, not a bare button" <|
                \_ ->
                    withOfferId <|
                        \offerId ->
                            ShopMoved.viewOfferAction translators "https://x" (product offerId True) buyer ownView
                                |> Query.fromHtml
                                |> Expect.all
                                    [ Query.find [ Selector.id "buy-price-7" ] >> Query.has [ Selector.text "30" ]
                                    , Query.find [ Selector.id "buy-invite-7" ]
                                        >> Query.has
                                            [ Selector.text "shop_moved.buy_eyebrow"
                                            , Selector.text "shop_moved.buy_title"
                                            , Selector.text "shop_moved.buy_body"
                                            , Selector.text "shop_moved.buy_account"
                                            ]
                                    , Query.find [ Selector.id "buy-in-new-app-7" ]
                                        >> Query.has
                                            [ Selector.tag "a"
                                            , Selector.text "shop_moved.buy_go"
                                            , Selector.attribute (Html.Attributes.href "https://x/app/login?next=%2Fapp%2Fshop%2F7%2Fbuy")
                                            , Selector.attribute (Html.Attributes.target "_self")
                                            ]
                                    ]
            , test "the buyer's balance shows under the price when known" <|
                \_ ->
                    withOfferId <|
                        \offerId ->
                            ShopMoved.viewOfferAction translators
                                "https://x"
                                (product offerId True)
                                { buyer | balance = Just { amount = 120, symbol = muda } }
                                ownView
                                |> Query.fromHtml
                                |> Query.find [ Selector.id "buy-price-7" ]
                                |> Query.has [ Selector.text "shop.transfer.balance" ]
            , test "the owner of a buyInApp offer gets the card, then the old view, and no buy link" <|
                \_ ->
                    withOfferId <|
                        \offerId ->
                            ShopMoved.viewOfferAction translators "https://x" (product offerId True) owner ownView
                                |> Query.fromHtml
                                |> Expect.all
                                    [ Query.has [ Selector.id "seller-card-new-app-7", Selector.text "elm edit button" ]
                                    , Query.hasNot [ Selector.id "buy-in-new-app-7" ]
                                    , Query.find [ Selector.id "manage-in-new-app-7" ]
                                        >> Query.has
                                            [ Selector.attribute (Html.Attributes.href "https://x/app/login?next=%2Fapp%2Fmy-shop%2Foffers%2F7")
                                            , Selector.attribute (Html.Attributes.target "_self")
                                            , Selector.text "shop_moved.manage_go"
                                            ]
                                    ]
            , test "an offer that stays in Elm renders exactly the old view, for owner and buyer" <|
                \_ ->
                    withOfferId <|
                        \offerId ->
                            Expect.all
                                [ \_ -> ShopMoved.viewOfferAction translators "https://x" (product offerId False) owner ownView |> Expect.equal ownView
                                , \_ -> ShopMoved.viewOfferAction translators "https://x" (product offerId False) buyer ownView |> Expect.equal ownView
                                ]
                                ()
            ]
        ]


ownView : Html.Html msg
ownView =
    div [] [ text "elm edit button" ]
