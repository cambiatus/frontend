module Shop exposing (Filter(..), Sale, SaleId, decodeTargetValueToFilter, encodeTransferSale, saleQuery, salesQuery)

import Avatar exposing (Avatar)
import Bespiral.Object
import Bespiral.Object.Profile as Creator
import Bespiral.Object.Sale
import Bespiral.Object.SaleHistory
import Bespiral.Query
import Eos exposing (Symbol)
import Eos.Account as Eos
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html.Events exposing (targetValue)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import User exposing (User)



-- Sale


type alias Sale =
    { id : Int
    , title : String
    , description : String
    , creatorId : Eos.Name
    , createdEosAccount : Eos.Name
    , price : Float
    , symbol : Symbol
    , rateCount : Maybe Int
    , image : Maybe String
    , units : Int
    , trackStock : Bool
    , creator : User
    }


type alias SaleAvatar =
    { avatar : Avatar }


type alias SaleId =
    String


type Filter
    = UserSales
    | MyCommunities
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


decodeTargetValueToFilter : ( String, String, String ) -> Decoder Filter
decodeTargetValueToFilter ( communities, all, user ) =
    let
        transform val =
            if val == communities then
                Decode.succeed MyCommunities

            else if val == all then
                Decode.succeed All

            else if val == user then
                Decode.succeed UserSales

            else
                Decode.fail ("Invalid filter: " ++ val)
    in
    Decode.andThen transform targetValue



-- SALE GRAPHQL API


salesSelection : SelectionSet Sale Bespiral.Object.Sale
salesSelection =
    SelectionSet.succeed Sale
        |> with Bespiral.Object.Sale.id
        |> with Bespiral.Object.Sale.title
        |> with Bespiral.Object.Sale.description
        |> with (Eos.nameSelectionSet Bespiral.Object.Sale.creatorId)
        |> with (Eos.nameSelectionSet Bespiral.Object.Sale.createdEosAccount)
        |> with Bespiral.Object.Sale.price
        |> with (Eos.symbolSelectionSet Bespiral.Object.Sale.communityId)
        |> SelectionSet.hardcoded (Just <| 0)
        |> with Bespiral.Object.Sale.image
        |> with Bespiral.Object.Sale.units
        |> with Bespiral.Object.Sale.trackStock
        |> with (Bespiral.Object.Sale.creator User.selectionSet)


saleQuery : Int -> SelectionSet (Maybe Sale) RootQuery
saleQuery saleId =
    let
        args =
            { input = { id = saleId } }
    in
    Bespiral.Query.sale
        args
        salesSelection


salesQuery : Filter -> Eos.Name -> SelectionSet (List Sale) RootQuery
salesQuery filter accName =
    case filter of
        UserSales ->
            let
                accString =
                    Eos.nameToString accName

                args =
                    { input = { account = Present accString, all = Absent, communities = Absent } }
            in
            Bespiral.Query.sales
                args
                salesSelection

        MyCommunities ->
            let
                accString =
                    Eos.nameToString accName

                args =
                    { input = { account = Absent, all = Absent, communities = Present accString } }
            in
            Bespiral.Query.sales
                args
                salesSelection

        All ->
            let
                accString =
                    Eos.nameToString accName

                args =
                    { input = { account = Absent, all = Present accString, communities = Absent } }
            in
            Bespiral.Query.sales
                args
                salesSelection
