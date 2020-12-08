module Shop exposing
    ( Filter(..)
    , Product
    , ProductId
    , decodeTargetValueToFilter
    , encodeTransferSale
    , productQuery
    , productsQuery
    )

import Cambiatus.Object
import Cambiatus.Object.Product
import Cambiatus.Query as Query
import Eos exposing (Symbol)
import Eos.Account as Eos
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html.Events exposing (targetValue)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Profile exposing (Profile)



-- Sale


type alias Product =
    { id : Int
    , title : String
    , description : String
    , creatorId : Eos.Name
    , price : Float
    , symbol : Symbol
    , image : Maybe String
    , units : Int
    , trackStock : Bool
    , creator : Profile
    }


type alias ProductId =
    String


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


decodeTargetValueToFilter : ( String, String ) -> Decoder Filter
decodeTargetValueToFilter ( all, user ) =
    let
        transform val =
            if val == all then
                Decode.succeed All

            else if val == user then
                Decode.succeed UserSales

            else
                Decode.fail ("Invalid filter: " ++ val)
    in
    Decode.andThen transform targetValue



-- PRODUCT GRAPHQL API


productSelection : SelectionSet Product Cambiatus.Object.Product
productSelection =
    SelectionSet.succeed Product
        |> with Cambiatus.Object.Product.id
        |> with Cambiatus.Object.Product.title
        |> with Cambiatus.Object.Product.description
        |> with (Eos.nameSelectionSet Cambiatus.Object.Product.creatorId)
        |> with Cambiatus.Object.Product.price
        |> with (Eos.symbolSelectionSet Cambiatus.Object.Product.communityId)
        |> with Cambiatus.Object.Product.image
        |> with Cambiatus.Object.Product.units
        |> with Cambiatus.Object.Product.trackStock
        |> with (Cambiatus.Object.Product.creator Profile.selectionSet)


productQuery : Int -> SelectionSet (Maybe Product) RootQuery
productQuery saleId =
    Query.product { id = saleId } productSelection


productsQuery : Filter -> Eos.Name -> Symbol -> SelectionSet (List Product) RootQuery
productsQuery filter accName communityId =
    case filter of
        UserSales ->
            let
                args =
                    \_ ->
                        { filters = Present { account = Eos.nameToString accName }
                        }
            in
            Query.products args { communityId = Eos.symbolToString communityId } productSelection

        All ->
            let
                args =
                    { communityId = Eos.symbolToString communityId
                    }
            in
            Query.products identity args productSelection
