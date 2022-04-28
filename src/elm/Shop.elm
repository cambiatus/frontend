module Shop exposing
    ( Filter(..)
    , Id
    , Product
    , ProductPreview
    , StockTracking(..)
    , createProduct
    , deleteProduct
    , encodeTransferSale
    , getAvailableUnits
    , hasUnitTracking
    , idSelectionSet
    , idToString
    , idUrlParser
    , isOutOfStock
    , productPreviewQuery
    , productQuery
    , productSelectionSet
    , productsQuery
    , updateProduct
    )

import Avatar
import Cambiatus.Mutation as Mutation
import Cambiatus.Object
import Cambiatus.Object.Product
import Cambiatus.Object.ProductPreview
import Cambiatus.Query as Query
import Eos exposing (Symbol)
import Eos.Account as Eos
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Json.Encode as Encode exposing (Value)
import Markdown exposing (Markdown)
import Profile
import Url.Parser



-- Sale


type alias Product =
    { id : Id
    , title : String
    , description : Markdown
    , creatorId : Eos.Name
    , price : Float
    , symbol : Symbol
    , image : Maybe String
    , stockTracking : StockTracking
    , creator : Profile.Minimal
    }



-- ID


type Id
    = Id Int


idToString : Id -> String
idToString (Id id) =
    String.fromInt id


idUrlParser : Url.Parser.Parser (Id -> a) a
idUrlParser =
    Url.Parser.int
        |> Url.Parser.map Id


idSelectionSet : SelectionSet Id Cambiatus.Object.Product
idSelectionSet =
    SelectionSet.map Id Cambiatus.Object.Product.id


encodeId : Id -> Value
encodeId (Id id) =
    Encode.int id


type StockTracking
    = NoTracking
    | UnitTracking { availableUnits : Int }


type alias ProductPreview =
    { symbol : Symbol
    , creator : Profile.Minimal
    , description : Markdown
    , id : Id
    , image : Maybe String
    , price : Float
    , title : String
    }


type Filter
    = UserSales
    | All



-- Type used when transfering to a sale


type alias TransferSale =
    { id : Id
    , from : Eos.Name
    , to : Eos.Name
    , quantity : Eos.Asset
    , units : Int
    }


encodeTransferSale : TransferSale -> Value
encodeTransferSale t =
    Encode.object
        [ ( "sale_id", encodeId t.id )
        , ( "from", Eos.encodeName t.from )
        , ( "to", Eos.encodeName t.to )
        , ( "quantity", Eos.encodeAsset t.quantity )
        , ( "units", Encode.int t.units )
        ]



-- PRODUCT GRAPHQL API


productSelectionSet : SelectionSet Product Cambiatus.Object.Product
productSelectionSet =
    SelectionSet.succeed
        (\id title description creatorId price symbol image maybeUnits trackStock creator ->
            { id = id
            , title = title
            , description = description
            , creatorId = creatorId
            , price = price
            , symbol = symbol
            , image = image
            , stockTracking =
                if trackStock then
                    case maybeUnits of
                        Nothing ->
                            NoTracking

                        Just units ->
                            UnitTracking { availableUnits = units }

                else
                    NoTracking
            , creator = creator
            }
        )
        |> with idSelectionSet
        |> with Cambiatus.Object.Product.title
        |> with (Markdown.selectionSet Cambiatus.Object.Product.description)
        |> with (Eos.nameSelectionSet Cambiatus.Object.Product.creatorId)
        |> with Cambiatus.Object.Product.price
        |> with (Eos.symbolSelectionSet Cambiatus.Object.Product.communityId)
        |> with (detectEmptyString Cambiatus.Object.Product.image)
        |> with Cambiatus.Object.Product.units
        |> with Cambiatus.Object.Product.trackStock
        |> with (Cambiatus.Object.Product.creator Profile.minimalSelectionSet)


productPreviewSelectionSet : SelectionSet ProductPreview Cambiatus.Object.ProductPreview
productPreviewSelectionSet =
    SelectionSet.succeed ProductPreview
        |> with (Eos.symbolSelectionSet Cambiatus.Object.ProductPreview.communityId)
        |> with
            (Eos.nameSelectionSet Cambiatus.Object.ProductPreview.creatorId
                |> SelectionSet.map productPreviewProfile
            )
        |> with (Markdown.selectionSet Cambiatus.Object.ProductPreview.description)
        |> with (SelectionSet.map Id Cambiatus.Object.ProductPreview.id)
        |> with (detectEmptyString Cambiatus.Object.ProductPreview.image)
        |> with Cambiatus.Object.ProductPreview.price
        |> with Cambiatus.Object.ProductPreview.title


detectEmptyString : SelectionSet (Maybe String) typeLock -> SelectionSet (Maybe String) typeLock
detectEmptyString =
    SelectionSet.map
        (\selection ->
            case selection of
                Just "" ->
                    Nothing

                _ ->
                    selection
        )


productPreviewProfile : Eos.Name -> Profile.Minimal
productPreviewProfile accountName =
    { account = accountName
    , name = accountName |> Eos.nameToString |> Just
    , avatar = Avatar.empty
    , email = Nothing
    , bio = Nothing
    , contacts = []
    }


productQuery : Id -> SelectionSet (Maybe Product) RootQuery
productQuery (Id saleId) =
    Query.product { id = saleId } productSelectionSet


productPreviewQuery : Id -> SelectionSet ProductPreview RootQuery
productPreviewQuery (Id productId) =
    Query.productPreview { id = productId } productPreviewSelectionSet


productsQuery : Filter -> Eos.Name -> Symbol -> SelectionSet (List Product) RootQuery
productsQuery filter accName communityId =
    case filter of
        UserSales ->
            let
                args =
                    \_ ->
                        { filters = Present { account = Eos.nameToString accName, inStock = Absent }
                        }
            in
            Query.products args { communityId = Eos.symbolToString communityId } productSelectionSet

        All ->
            let
                args =
                    { communityId = Eos.symbolToString communityId
                    }
            in
            Query.products identity args productSelectionSet


createProduct :
    { symbol : Symbol
    , title : String
    , description : Markdown
    , images : List String
    , price : Float
    , stockTracking : StockTracking
    }
    -> SelectionSet decodesTo Cambiatus.Object.Product
    -> SelectionSet (Maybe decodesTo) RootMutation
createProduct options selectionSet =
    upsert
        { id = Nothing
        , symbol = options.symbol
        , title = options.title
        , description = options.description
        , images = options.images
        , price = options.price
        , stockTracking = options.stockTracking
        }
        selectionSet


{-| Images will be overwritten with whatever is passed in here. If you want to
keep the existing images, you must include them in the `images` field.
-}
updateProduct :
    { id : Id
    , symbol : Symbol
    , title : String
    , description : Markdown
    , images : List String
    , price : Float
    , stockTracking : StockTracking
    }
    -> SelectionSet decodesTo Cambiatus.Object.Product
    -> SelectionSet (Maybe decodesTo) RootMutation
updateProduct options selectionSet =
    upsert
        { id = Just options.id
        , symbol = options.symbol
        , title = options.title
        , description = options.description
        , images = options.images
        , price = options.price
        , stockTracking = options.stockTracking
        }
        selectionSet


upsert :
    { id : Maybe Id
    , symbol : Symbol
    , title : String
    , description : Markdown
    , images : List String
    , price : Float
    , stockTracking : StockTracking
    }
    -> SelectionSet decodesTo Cambiatus.Object.Product
    -> SelectionSet (Maybe decodesTo) RootMutation
upsert { id, symbol, title, description, images, price, stockTracking } =
    Mutation.product
        (\_ ->
            { id =
                case id of
                    Nothing ->
                        OptionalArgument.Absent

                    Just (Id unwrappedId) ->
                        OptionalArgument.Present unwrappedId
            , communityId = OptionalArgument.Present (Eos.symbolToString symbol)
            , title = OptionalArgument.Present title
            , description = OptionalArgument.Present (Markdown.toRawString description)
            , images = OptionalArgument.Present images
            , price = OptionalArgument.Present price
            , trackStock =
                case stockTracking of
                    NoTracking ->
                        OptionalArgument.Present False

                    UnitTracking _ ->
                        OptionalArgument.Present True
            , units =
                case stockTracking of
                    NoTracking ->
                        Absent

                    UnitTracking { availableUnits } ->
                        Present availableUnits
            }
        )


deleteProduct :
    Id
    -> SelectionSet decodesTo Cambiatus.Object.DeleteStatus
    -> SelectionSet (Maybe decodesTo) RootMutation
deleteProduct (Id id) =
    Mutation.deleteProduct { id = id }



-- HELPER FUNCTIONS


isOutOfStock : Product -> Bool
isOutOfStock product =
    case product.stockTracking of
        NoTracking ->
            False

        UnitTracking { availableUnits } ->
            availableUnits == 0


hasUnitTracking : Product -> Bool
hasUnitTracking product =
    case product.stockTracking of
        NoTracking ->
            False

        UnitTracking _ ->
            True


getAvailableUnits : Product -> Maybe Int
getAvailableUnits product =
    case product.stockTracking of
        NoTracking ->
            Nothing

        UnitTracking { availableUnits } ->
            Just availableUnits
