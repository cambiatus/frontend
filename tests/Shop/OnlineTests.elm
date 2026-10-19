module Shop.OnlineTests exposing (all)

import Cambiatus.Enum.OfferType as OfferType
import Expect
import Graphql.Document
import Markdown
import Shop
import Test exposing (Test, describe, test)
import Url
import Url.Parser


all : Test
all =
    describe "Shop online flag"
        [ test "creating an offer sends online: true" <|
            \_ ->
                createMutation True
                    |> Expect.all
                        [ String.contains "online: true" >> Expect.equal True
                        , String.contains "id:" >> Expect.equal False
                        ]
        , test "creating an offer sends online: false when the switch is off" <|
            \_ ->
                createMutation False
                    |> String.contains "online: false"
                    |> Expect.equal True
        , test "updating an offer sends online and the id" <|
            \_ ->
                withOfferId <|
                    \offerId ->
                        Shop.updateProduct
                            { id = offerId
                            , title = "Bolo"
                            , description = Markdown.empty
                            , images = []
                            , categories = []
                            , price = 10
                            , stockTracking = Shop.NoTracking
                            , online = True
                            , offerType = OfferType.Simple
                            }
                            Shop.idSelectionSet
                            |> Graphql.Document.serializeMutation
                            |> Expect.all
                                [ String.contains "online: true" >> Expect.equal True
                                , String.contains "id: 7" >> Expect.equal True
                                ]
        , test "the admin categories save sends online too" <|
            \_ ->
                withOfferId <|
                    \offerId ->
                        Shop.updateProductCategories
                            { id = offerId, categories = [], online = False }
                            Shop.idSelectionSet
                            |> Graphql.Document.serializeMutation
                            |> String.contains "online: false"
                            |> Expect.equal True
        ]


createMutation : Bool -> String
createMutation online =
    Shop.createProduct
        { title = "Bolo"
        , description = Markdown.empty
        , images = []
        , categories = []
        , price = 10
        , stockTracking = Shop.NoTracking
        , online = online
        , offerType = OfferType.Simple
        }
        Shop.idSelectionSet
        |> Graphql.Document.serializeMutation


withOfferId : (Shop.Id -> Expect.Expectation) -> Expect.Expectation
withOfferId expectation =
    Url.fromString "https://cambiatus.test/7"
        |> Maybe.andThen (Url.Parser.parse Shop.idUrlParser)
        |> Maybe.map expectation
        |> Maybe.withDefault (Expect.fail "could not build a Shop.Id")
