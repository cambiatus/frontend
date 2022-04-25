module Shop exposing
    ( Filter(..)
    , Product
    , ProductId
    , ProductPreview
    , encodeTransferSale
    , productPreviewQuery
    , productQuery
    , productSelectionSet
    , productsQuery
    )

import Avatar
import Cambiatus.Object
import Cambiatus.Object.Product
import Cambiatus.Object.ProductPreview
import Cambiatus.Query as Query
import Eos exposing (Symbol)
import Eos.Account as Eos
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Json.Encode as Encode exposing (Value)
import Markdown exposing (Markdown)
import Profile



-- Sale


type alias Product =
    { id : Int
    , title : String
    , description : Markdown
    , creatorId : Eos.Name
    , price : Float
    , symbol : Symbol
    , image : Maybe String
    , units : Int
    , trackStock : Bool
    , stockTracking : StockTracking
    , creator : Profile.Minimal
    }


type StockTracking
    = NoTracking
    | UnitTracking { availableUnits : Int }


type alias ProductPreview =
    { symbol : Symbol
    , creator : Profile.Minimal
    , description : Markdown
    , id : Int
    , image : Maybe String
    , price : Float
    , title : String
    }


type alias ProductId =
    Int


type Filter
    = UserSales
    | All



-- Type used when transfering to a sale


type alias TransferSale =
    { id : Int
    , from : Eos.Name
    , to : Eos.Name
    , quantity : Eos.Asset
    , units : Int
    }


encodeTransferSale : TransferSale -> Value
encodeTransferSale t =
    Encode.object
        [ ( "sale_id", Encode.int t.id )
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
            , units = 0
            , trackStock = trackStock
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
        |> with Cambiatus.Object.Product.id
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
        |> with Cambiatus.Object.ProductPreview.id
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


productQuery : Int -> SelectionSet (Maybe Product) RootQuery
productQuery saleId =
    Query.product { id = saleId } productSelectionSet


productPreviewQuery : Int -> SelectionSet ProductPreview RootQuery
productPreviewQuery productId =
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
