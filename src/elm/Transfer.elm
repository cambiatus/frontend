module Transfer exposing
    ( ConnectionTransfer
    , QueryTransfers
    , Transfer
    , communityFilter
    , encodeEosActionData
    , getTotalCount
    , getTransfers
    , metadataConnectionSelectionSet
    , transferConnectionSelectionSet
    , transferItemSelectionSet
    , transferQuery
    , transfersQuery
    , userFilter
    )

import Api.Relay exposing (Edge, MetadataConnection, PageConnection, PageInfo, PaginationArgs, pageInfoSelectionSet)
import Avatar exposing (Avatar)
import Cambiatus.Object
import Cambiatus.Object.Community
import Cambiatus.Object.Profile
import Cambiatus.Object.Transfer
import Cambiatus.Object.TransferConnection
import Cambiatus.Object.TransferEdge
import Cambiatus.Query
import Cambiatus.Scalar exposing (DateTime(..))
import Eos exposing (Symbol)
import Eos.Account as Eos
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Json.Encode as Encode exposing (Value)
import Profile exposing (Profile)


type TransferFilter
    = RegularUser Eos.Name
    | Community Symbol


userFilter : Eos.Name -> TransferFilter
userFilter name =
    RegularUser name


communityFilter : Symbol -> TransferFilter
communityFilter sym =
    Community sym


type alias ProfileArgs =
    { input :
        { account : OptionalArgument String
        }
    }


type alias CommunityArgs =
    { symbol : OptionalArgument String
    }


type alias Transfer =
    { id : Int
    , to : Profile
    , from : Profile
    , value : Float
    , memo : Maybe String
    , symbol : Symbol
    , community : Cmm
    , blockTime : DateTime
    }


type alias TransferUser =
    { avatar : Avatar
    , userName : Maybe String
    , account : Eos.Name
    }


type alias Cmm =
    { name : String
    }


type alias EdgeTransfer =
    Edge Transfer


type alias ConnectionTransfer =
    PageConnection Transfer


type alias QueryTransfers =
    { transfers : Maybe ConnectionTransfer
    }


type alias EosActionData =
    { from : Eos.Name
    , to : Eos.Name
    , value : Eos.Asset
    , memo : String
    }


encodeEosActionData : EosActionData -> Value
encodeEosActionData data =
    Encode.object
        [ ( "from", Eos.encodeName data.from )
        , ( "to", Eos.encodeName data.to )
        , ( "quantity", Eos.encodeAsset data.value )
        , ( "memo", Encode.string data.memo )
        ]



-- GRAPHQL API


profileNameSelectionSet : SelectionSet (Maybe String) typeLock -> SelectionSet (Maybe String) typeLock
profileNameSelectionSet =
    SelectionSet.map (\t -> t)


transferItemSelectionSet : SelectionSet Transfer Cambiatus.Object.Transfer
transferItemSelectionSet =
    SelectionSet.succeed Transfer
        |> with Cambiatus.Object.Transfer.id
        |> with (Cambiatus.Object.Transfer.to Profile.selectionSet)
        |> with (Cambiatus.Object.Transfer.from Profile.selectionSet)
        |> with Cambiatus.Object.Transfer.amount
        |> with Cambiatus.Object.Transfer.memo
        |> with (Eos.symbolSelectionSet Cambiatus.Object.Transfer.communityId)
        |> with
            (Cambiatus.Object.Transfer.community
                (SelectionSet.succeed Cmm
                    |> with Cambiatus.Object.Community.name
                )
            )
        |> with Cambiatus.Object.Transfer.createdAt


transferEdgeSelectionSet : SelectionSet EdgeTransfer Cambiatus.Object.TransferEdge
transferEdgeSelectionSet =
    SelectionSet.succeed Edge
        |> with Cambiatus.Object.TransferEdge.cursor
        |> with (Cambiatus.Object.TransferEdge.node transferItemSelectionSet)


transferConnectionSelectionSet : SelectionSet ConnectionTransfer Cambiatus.Object.TransferConnection
transferConnectionSelectionSet =
    SelectionSet.succeed PageConnection
        |> with (Cambiatus.Object.TransferConnection.edges transferEdgeSelectionSet)
        |> with (Cambiatus.Object.TransferConnection.pageInfo pageInfoSelectionSet)


metadataConnectionSelectionSet : SelectionSet MetadataConnection Cambiatus.Object.TransferConnection
metadataConnectionSelectionSet =
    SelectionSet.succeed MetadataConnection
        |> with Cambiatus.Object.TransferConnection.totalCount
        |> with Cambiatus.Object.TransferConnection.fetchedCount


profileTransfersSelectionSet : (PaginationArgs -> PaginationArgs) -> SelectionSet QueryTransfers Cambiatus.Object.Profile
profileTransfersSelectionSet paginateArgs =
    let
        transfers =
            Cambiatus.Object.Profile.transfers
                paginateArgs
                transferConnectionSelectionSet
    in
    SelectionSet.succeed QueryTransfers
        |> with transfers


communityTransfersSelectionSet : (PaginationArgs -> PaginationArgs) -> SelectionSet QueryTransfers Cambiatus.Object.Community
communityTransfersSelectionSet paginateArgs =
    let
        transfers =
            Cambiatus.Object.Community.transfers
                paginateArgs
                transferConnectionSelectionSet
    in
    SelectionSet.succeed QueryTransfers
        |> with transfers


transfersQuery : TransferFilter -> (PaginationArgs -> PaginationArgs) -> SelectionSet (Maybe QueryTransfers) RootQuery
transfersQuery input paginateArgs =
    case input of
        RegularUser name ->
            profileTransfersSelectionSet paginateArgs
                |> Cambiatus.Query.profile { input = { account = Present (Eos.nameToString name) } }

        Community symbol ->
            communityTransfersSelectionSet paginateArgs
                |> Cambiatus.Query.community { symbol = Eos.symbolToString symbol }


getTransfers : Maybe { t | transfers : Maybe ConnectionTransfer } -> List Transfer
getTransfers maybeObj =
    let
        toMaybeConn : Maybe { t | transfers : Maybe ConnectionTransfer } -> Maybe ConnectionTransfer
        toMaybeConn maybeObj_ =
            Maybe.andThen
                (\a -> a.transfers)
                maybeObj_

        toMaybeEdges : Maybe ConnectionTransfer -> Maybe (List (Maybe EdgeTransfer))
        toMaybeEdges maybeConn =
            Maybe.andThen
                (\a -> a.edges)
                maybeConn

        toEdges : Maybe (List (Maybe EdgeTransfer)) -> List (Maybe EdgeTransfer)
        toEdges maybeEdges =
            Maybe.withDefault
                []
                maybeEdges

        toMaybeNodes : List (Maybe EdgeTransfer) -> List (Maybe Transfer)
        toMaybeNodes edges =
            List.map
                (\a ->
                    Maybe.andThen
                        (\b ->
                            b.node
                        )
                        a
                )
                edges

        toNodes : List (Maybe Transfer) -> List Transfer
        toNodes maybeNodes =
            List.filterMap
                identity
                maybeNodes
    in
    maybeObj
        |> toMaybeConn
        |> toMaybeEdges
        |> toEdges
        |> toMaybeNodes
        |> toNodes


getTotalCount : Maybe { t | transfers : Maybe MetadataConnection } -> Maybe Int
getTotalCount maybeObj =
    let
        toMaybeConn : Maybe { t | transfers : Maybe MetadataConnection } -> Maybe MetadataConnection
        toMaybeConn maybeObj_ =
            Maybe.andThen
                (\obj ->
                    obj.transfers
                )
                maybeObj_

        toMaybeTotal : Maybe MetadataConnection -> Maybe Int
        toMaybeTotal maybeConn =
            case maybeConn of
                Just conn ->
                    conn.totalCount

                Nothing ->
                    Nothing
    in
    maybeObj
        |> toMaybeConn
        |> toMaybeTotal


transferQuery : Int -> SelectionSet (Maybe Transfer) RootQuery
transferQuery tID =
    let
        args =
            { input = { id = tID } }
    in
    Cambiatus.Query.transfer args transferItemSelectionSet
