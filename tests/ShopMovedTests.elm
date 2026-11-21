module ShopMovedTests exposing (all)

import Eos.Account
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


seller : Eos.Account.Name
seller =
    Eos.Account.stringToName "seller"


buyer : Eos.Account.Name
buyer =
    Eos.Account.stringToName "buyer"


product : Shop.Id -> Bool -> { id : Shop.Id, creatorId : Eos.Account.Name, buyInApp : Bool }
product offerId buyInApp =
    { id = offerId, creatorId = seller, buyInApp = buyInApp }


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
        , describe "viewBuyLink"
            [ test "is a same-tab link named by its text" <|
                \_ ->
                    withOfferId <|
                        \offerId ->
                            ShopMoved.viewBuyLink translators "https://x" offerId
                                |> Query.fromHtml
                                |> Expect.all
                                    [ Query.has
                                        [ Selector.tag "a"
                                        , Selector.id "buy-in-new-app-7"
                                        , Selector.attribute (Html.Attributes.href "https://x/app/login?next=%2Fapp%2Fshop%2F7%2Fbuy")
                                        , Selector.attribute (Html.Attributes.target "_self")
                                        , Selector.text "shop_moved.buy_go"
                                        ]
                                    ]
            ]
        , describe "viewBuyOr (the offer page and its checkout)"
            [ test "buyInApp offers show the link instead of the form" <|
                \_ ->
                    withOfferId <|
                        \offerId ->
                            ShopMoved.viewBuyOr translators "https://x" (product offerId True) (div [] [ text "elm checkout" ])
                                |> Query.fromHtml
                                |> Expect.all
                                    [ Query.has [ Selector.id "buy-in-new-app-7" ]
                                    , Query.hasNot [ Selector.text "elm checkout" ]
                                    ]
            , test "other offers keep the form untouched" <|
                \_ ->
                    withOfferId <|
                        \offerId ->
                            ShopMoved.viewBuyOr translators "https://x" (product offerId False) (div [] [ text "elm checkout" ])
                                |> Query.fromHtml
                                |> Expect.all
                                    [ Query.has [ Selector.text "elm checkout" ]
                                    , Query.hasNot [ Selector.id "buy-in-new-app-7" ]
                                    ]
            ]
        , describe "viewSellerCard"
            [ test "the creator of a buyInApp offer sees the card and the link" <|
                \_ ->
                    withOfferId <|
                        \offerId ->
                            ShopMoved.viewSellerCard translators "https://x" seller (product offerId True)
                                |> Query.fromHtml
                                |> Expect.all
                                    [ Query.has
                                        [ Selector.text "shop_moved.manage_title"
                                        , Selector.text "shop_moved.manage_body"
                                        ]
                                    , Query.find [ Selector.id "manage-in-new-app-7" ]
                                        >> Query.has
                                            [ Selector.tag "a"
                                            , Selector.attribute (Html.Attributes.href "https://x/app/login?next=%2Fapp%2Fmy-shop%2Foffers%2F7")
                                            , Selector.attribute (Html.Attributes.target "_self")
                                            , Selector.text "shop_moved.manage_go"
                                            ]
                                    ]
            , test "someone else sees nothing" <|
                \_ ->
                    withOfferId <|
                        \offerId ->
                            div [] [ ShopMoved.viewSellerCard translators "https://x" buyer (product offerId True) ]
                                |> Query.fromHtml
                                |> Query.hasNot [ Selector.tag "a" ]
            , test "the creator of a regular offer sees nothing" <|
                \_ ->
                    withOfferId <|
                        \offerId ->
                            div [] [ ShopMoved.viewSellerCard translators "https://x" seller (product offerId False) ]
                                |> Query.fromHtml
                                |> Query.hasNot [ Selector.tag "a" ]
            ]
        ]
