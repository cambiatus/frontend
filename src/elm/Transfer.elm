module Transfer exposing
    ( ConnectionTransfer
    , EdgeTransfer
    , QueryTransfers
    , Transfer
    , encodeEosActionData
    , getTransfers
    , transferConnectionSelectionSet
    , transferQuery
    , transferSucceedSubscription
    , transfersUserQuery
    )

import Api.Relay exposing (Edge, PageConnection, pageInfoSelectionSet)
import Cambiatus.Object
import Cambiatus.Object.Community
import Cambiatus.Object.Subdomain
import Cambiatus.Object.Transfer
import Cambiatus.Object.TransferConnection
import Cambiatus.Object.TransferEdge
import Cambiatus.Object.User as User
import Cambiatus.Query
import Cambiatus.Scalar exposing (DateTime(..))
import Cambiatus.Subscription as Subscription
import Eos exposing (symbolToString)
import Eos.Account as Eos
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Json.Encode as Encode exposing (Value)
import Profile


type alias Transfer =
    { id : Int
    , to : Profile.Model
    , from : Profile.Model
    , value : Float
    , memo : Maybe String
    , communityId : CommunityId
    , community : Cmm
    , blockTime : DateTime
    , createdTx : String
    }


type alias CommunityId =
    String


type alias Cmm =
    { symbol : Eos.Symbol
    , subdomain : String
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


transferItemSelectionSet : SelectionSet Transfer Cambiatus.Object.Transfer
transferItemSelectionSet =
    SelectionSet.succeed Transfer
        |> with Cambiatus.Object.Transfer.id
        |> with (Cambiatus.Object.Transfer.to Profile.selectionSet)
        |> with (Cambiatus.Object.Transfer.from Profile.selectionSet)
        |> with Cambiatus.Object.Transfer.amount
        |> with Cambiatus.Object.Transfer.memo
        |> with
            (Cambiatus.Object.Transfer.community
                Cambiatus.Object.Community.name
            )
        |> with (Cambiatus.Object.Transfer.community communitySelectionSet)
        |> with Cambiatus.Object.Transfer.createdAt
        |> with Cambiatus.Object.Transfer.createdTx


communitySelectionSet : SelectionSet Cmm Cambiatus.Object.Community
communitySelectionSet =
    SelectionSet.succeed Cmm
        |> with (Eos.symbolSelectionSet Cambiatus.Object.Community.symbol)
        |> with
            (Cambiatus.Object.Community.subdomain Cambiatus.Object.Subdomain.name
                |> SelectionSet.map (Maybe.withDefault "")
            )


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


profileTransfersSelectionSet : (User.TransfersOptionalArguments -> User.TransfersOptionalArguments) -> SelectionSet QueryTransfers Cambiatus.Object.User
profileTransfersSelectionSet paginateArgs =
    let
        transfers =
            User.transfers
                paginateArgs
                transferConnectionSelectionSet
    in
    SelectionSet.succeed QueryTransfers
        |> with transfers


transfersUserQuery : Eos.Name -> (User.TransfersOptionalArguments -> User.TransfersOptionalArguments) -> SelectionSet (Maybe QueryTransfers) RootQuery
transfersUserQuery name paginateArgs =
    profileTransfersSelectionSet paginateArgs
        |> Cambiatus.Query.user { account = Eos.nameToString name }


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
            List.map (\a -> Maybe.andThen (\b -> b.node) a) edges

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


transferQuery : Int -> SelectionSet (Maybe Transfer) RootQuery
transferQuery tID =
    let
        args =
            { id = tID }
    in
    Cambiatus.Query.transfer args transferItemSelectionSet


transferSucceedSubscription : Eos.Symbol -> String -> String -> SelectionSet Transfer Graphql.Operation.RootSubscription
transferSucceedSubscription symbol fromAccount toAccount =
    let
        args =
            { input =
                { from = fromAccount
                , to = toAccount
                , symbol = symbolToString symbol |> String.toUpper
                }
            }
    in
    Subscription.transfersucceed args transferItemSelectionSet
