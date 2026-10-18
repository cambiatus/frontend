module Shop.OfferTypeTests exposing (all)

import Cambiatus.Enum.OfferType as OfferType exposing (OfferType)
import Expect
import Form
import Graphql.Document
import Markdown exposing (Markdown)
import Page.Shop.Editor as Editor
import Shop
import Shop.Where
import Test exposing (Test, describe, test)
import Translation
import Url
import Url.Parser


all : Test
all =
    describe "Shop offer type"
        [ test "editor encodes SERVICE when Serviço picked" <|
            \_ ->
                createMutation (pickedInEditor OfferType.Service)
                    |> Expect.all
                        [ String.contains "type: SERVICE" >> Expect.equal True
                        , String.contains "id:" >> Expect.equal False
                        ]
        , test "editor encodes SIMPLE when Produto picked" <|
            \_ ->
                createMutation (pickedInEditor OfferType.Simple)
                    |> String.contains "type: SIMPLE"
                    |> Expect.equal True
        , test "a new offer defaults to Produto" <|
            \_ ->
                parseMainInformation
                    (Editor.mainInformationInput
                        { offerType = OfferType.Simple, title = "Bolo", description = longEnough }
                    )
                    |> Expect.equal (Ok OfferType.Simple)
        , test "editing an existing service preselects Serviço" <|
            \_ ->
                parseMainInformation
                    (Editor.mainInformationInput
                        { offerType = OfferType.Service, title = "Aula de violão", description = longEnough }
                    )
                    |> Expect.equal (Ok OfferType.Service)
        , test "an existing BASKET is sent unchanged, not coerced to SIMPLE" <|
            \_ ->
                createMutation
                    (parseMainInformation
                        (Editor.mainInformationInput
                            { offerType = OfferType.Basket, title = "Cesta", description = longEnough }
                        )
                        |> Result.withDefault OfferType.Simple
                    )
                    |> String.contains "type: BASKET"
                    |> Expect.equal True
        , test "updating an offer sends type and the id" <|
            \_ ->
                withOfferId <|
                    \offerId ->
                        Shop.updateProduct
                            { id = offerId
                            , title = "Aula"
                            , description = Markdown.empty
                            , images = []
                            , categories = []
                            , price = 10
                            , stockTracking = Shop.NoTracking
                            , online = False
                            , offerType = OfferType.Service
                            }
                            Shop.idSelectionSet
                            |> Graphql.Document.serializeMutation
                            |> Expect.all
                                [ String.contains "type: SERVICE" >> Expect.equal True
                                , String.contains "id: 7" >> Expect.equal True
                                ]
        , test "the admin categories-only save leaves the type alone" <|
            \_ ->
                withOfferId <|
                    \offerId ->
                        Shop.updateProductCategories
                            { id = offerId, categories = [], online = False }
                            Shop.idSelectionSet
                            |> Graphql.Document.serializeMutation
                            |> String.contains "type:"
                            |> Expect.equal False
        , test "the shop filter sends the type to products" <|
            \_ ->
                Shop.productsQuery
                    { user = Nothing
                    , categories = []
                    , where_ = Shop.Where.Anywhere
                    , offerType = Just OfferType.Service
                    }
                    |> Graphql.Document.serializeQuery
                    |> String.contains "type: SERVICE"
                    |> Expect.equal True
        , test "the shop filter sends no type for All" <|
            \_ ->
                Shop.productsQuery
                    { user = Nothing
                    , categories = []
                    , where_ = Shop.Where.Anywhere
                    , offerType = Nothing
                    }
                    |> Graphql.Document.serializeQuery
                    |> String.contains "type:"
                    |> Expect.equal False
        ]


{-| What the editor's form outputs after the seller picks `chosen`.
-}
pickedInEditor : OfferType -> OfferType
pickedInEditor chosen =
    Editor.mainInformationInput { offerType = chosen, title = "Bolo", description = longEnough }
        |> parseMainInformation
        |> Result.withDefault OfferType.Basket


parseMainInformation : Editor.MainInformationFormInput -> Result () OfferType
parseMainInformation input =
    Form.parse (Editor.mainInformationForm translators)
        (Form.init input)
        { onError = always (Err ())
        , onSuccess = .offerType >> Ok
        }


translators : Translation.Translators
translators =
    { t = identity, tr = \key _ -> key }


longEnough : Markdown
longEnough =
    Markdown.fromTranslation { translators | t = always "# A description long enough" } ""


createMutation : OfferType -> String
createMutation offerType =
    Shop.createProduct
        { title = "Bolo"
        , description = Markdown.empty
        , images = []
        , categories = []
        , price = 10
        , stockTracking = Shop.NoTracking
        , online = False
        , offerType = offerType
        }
        Shop.idSelectionSet
        |> Graphql.Document.serializeMutation


withOfferId : (Shop.Id -> Expect.Expectation) -> Expect.Expectation
withOfferId expectation =
    Url.fromString "https://cambiatus.test/7"
        |> Maybe.andThen (Url.Parser.parse Shop.idUrlParser)
        |> Maybe.map expectation
        |> Maybe.withDefault (Expect.fail "could not build a Shop.Id")
