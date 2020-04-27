module Shop exposing
    ( Filter(..)
    , Sale
    , SaleId
    , decodeTargetValueToFilter
    , encodeTransferSale
    , saleQuery
    , salesQuery
    )

import Avatar exposing (Avatar)
import Cambiatus.Object
import Cambiatus.Object.Profile as Creator
import Cambiatus.Object.Sale
import Cambiatus.Object.SaleHistory
import Cambiatus.Query
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
    , creator : Profile
    }


type alias SaleAvatar =
    { avatar : Avatar }


type alias SaleId =
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



-- SALE GRAPHQL API


salesSelection : SelectionSet Sale Cambiatus.Object.Sale
salesSelection =
    SelectionSet.succeed Sale
        |> with Cambiatus.Object.Sale.id
        |> with Cambiatus.Object.Sale.title
        |> with Cambiatus.Object.Sale.description
        |> with (Eos.nameSelectionSet Cambiatus.Object.Sale.creatorId)
        |> with (Eos.nameSelectionSet Cambiatus.Object.Sale.createdEosAccount)
        |> with Cambiatus.Object.Sale.price
        |> with (Eos.symbolSelectionSet Cambiatus.Object.Sale.communityId)
        |> SelectionSet.hardcoded (Just <| 0)
        |> with Cambiatus.Object.Sale.image
        |> with Cambiatus.Object.Sale.units
        |> with Cambiatus.Object.Sale.trackStock
        |> with (Cambiatus.Object.Sale.creator Profile.selectionSet)


saleQuery : Int -> SelectionSet (Maybe Sale) RootQuery
saleQuery saleId =
    let
        args =
            { input = { id = saleId } }
    in
    Cambiatus.Query.sale
        args
        salesSelection


salesQuery : Filter -> Eos.Name -> Symbol -> SelectionSet (List Sale) RootQuery
salesQuery filter accName communityId =
    case filter of
        UserSales ->
            let
                accString =
                    Eos.nameToString accName

                args =
                    { input =
                        { account = Present accString
                        , all = Absent
                        , communities = Absent
                        , communityId = Present (Eos.symbolToString communityId)
                        }
                    }
            in
            Cambiatus.Query.sales
                args
                salesSelection

        All ->
            let
                accString =
                    Eos.nameToString accName

                args =
                    { input =
                        { account = Absent
                        , all = Present accString
                        , communities = Absent
                        , communityId = Present (Eos.symbolToString communityId)
                        }
                    }
            in
            Cambiatus.Query.sales
                args
                salesSelection
