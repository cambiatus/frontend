module Transfer exposing (ConnectionTransfer, QueryTransfers, Transfer, communityFilter, encodeEosActionData, getTotalCount, getTransfers, metadataConnectionSelectionSet, transferConnectionSelectionSet, transferItemSelectionSet, transferQuery, transfersQuery, userFilter)

import Api.Relay exposing (Edge, MetadataConnection, PageConnection, PageInfo, PaginationArgs, pageInfoSelectionSet)
import Avatar exposing (Avatar)
import Bespiral.Object
import Bespiral.Object.Community
import Bespiral.Object.Profile
import Bespiral.Object.Transfer
import Bespiral.Object.TransferConnection
import Bespiral.Object.TransferEdge
import Bespiral.Query
import Bespiral.Scalar exposing (DateTime(..))
import Eos exposing (Symbol)
import Eos.Account as Eos
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Json.Encode as Encode exposing (Value)
import User exposing (User)


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
    , to : User
    , from : User
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


transferItemSelectionSet : SelectionSet Transfer Bespiral.Object.Transfer
transferItemSelectionSet =
    SelectionSet.succeed Transfer
        |> with Bespiral.Object.Transfer.id
        |> with (Bespiral.Object.Transfer.to User.selectionSet)
        |> with (Bespiral.Object.Transfer.from User.selectionSet)
        |> with Bespiral.Object.Transfer.amount
        |> with Bespiral.Object.Transfer.memo
        |> with (Eos.symbolSelectionSet Bespiral.Object.Transfer.communityId)
        |> with
            (Bespiral.Object.Transfer.community
                (SelectionSet.succeed Cmm
                    |> with Bespiral.Object.Community.name
                )
            )
        |> with Bespiral.Object.Transfer.createdAt


transferEdgeSelectionSet : SelectionSet EdgeTransfer Bespiral.Object.TransferEdge
transferEdgeSelectionSet =
    SelectionSet.succeed Edge
        |> with Bespiral.Object.TransferEdge.cursor
        |> with (Bespiral.Object.TransferEdge.node transferItemSelectionSet)


transferConnectionSelectionSet : SelectionSet ConnectionTransfer Bespiral.Object.TransferConnection
transferConnectionSelectionSet =
    SelectionSet.succeed PageConnection
        |> with (Bespiral.Object.TransferConnection.edges transferEdgeSelectionSet)
        |> with (Bespiral.Object.TransferConnection.pageInfo pageInfoSelectionSet)


metadataConnectionSelectionSet : SelectionSet MetadataConnection Bespiral.Object.TransferConnection
metadataConnectionSelectionSet =
    SelectionSet.succeed MetadataConnection
        |> with Bespiral.Object.TransferConnection.totalCount
        |> with Bespiral.Object.TransferConnection.fetchedCount


profileTransfersSelectionSet : (PaginationArgs -> PaginationArgs) -> SelectionSet QueryTransfers Bespiral.Object.Profile
profileTransfersSelectionSet paginateArgs =
    let
        transfers =
            Bespiral.Object.Profile.transfers
                paginateArgs
                transferConnectionSelectionSet
    in
    SelectionSet.succeed QueryTransfers
        |> with transfers


communityTransfersSelectionSet : (PaginationArgs -> PaginationArgs) -> SelectionSet QueryTransfers Bespiral.Object.Community
communityTransfersSelectionSet paginateArgs =
    let
        transfers =
            Bespiral.Object.Community.transfers
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
                |> Bespiral.Query.profile { input = { account = Present (Eos.nameToString name) } }

        Community symbol ->
            communityTransfersSelectionSet paginateArgs
                |> Bespiral.Query.community { symbol = Eos.symbolToString symbol }


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
    Bespiral.Query.transfer args transferItemSelectionSet
