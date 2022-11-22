module Transfer exposing
    ( ConnectionTransfer
    , CreatedTx
    , EdgeTransfer
    , ProfileSummaries
    , QueryTransfers
    , Transfer
    , createdTxToString
    , encodeEosActionData
    , encodeEosActionWithMarkdown
    , getTransfers
    , transferConnectionSelectionSet
    , transferQuery
    , transferSucceedSubscription
    , transfersUserQuery
    , updateProfileSummaries
    , view
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
import Cambiatus.Scalar exposing (DateTime)
import Cambiatus.Subscription as Subscription
import Eos exposing (symbolToString)
import Eos.Account as Eos
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, div, p, span, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Json.Encode as Encode exposing (Value)
import Markdown exposing (Markdown)
import Profile
import Profile.Summary
import Session.LoggedIn as LoggedIn


type alias Transfer =
    { id : Int
    , to : Profile.Model
    , from : Profile.Model
    , value : Float
    , memo : Maybe Markdown
    , communityId : CommunityId
    , community : Cmm
    , blockTime : DateTime
    , createdTx : CreatedTx
    }


type CreatedTx
    = CreatedTx String


createdTxToString : CreatedTx -> String
createdTxToString (CreatedTx tx) =
    tx


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
    , memo : Markdown
    }


encodeEosActionData : EosActionData -> Value
encodeEosActionData data =
    Encode.object
        [ ( "from", Eos.encodeName data.from )
        , ( "to", Eos.encodeName data.to )
        , ( "quantity", Eos.encodeAsset data.value )
        , ( "memo", Markdown.encode data.memo )
        ]


type alias EosActionDataWithMarkdown =
    { from : Eos.Name
    , to : Eos.Name
    , value : Eos.Asset
    , memo : Markdown
    }


encodeEosActionWithMarkdown : EosActionDataWithMarkdown -> Value
encodeEosActionWithMarkdown data =
    Encode.object
        [ ( "from", Eos.encodeName data.from )
        , ( "to", Eos.encodeName data.to )
        , ( "quantity", Eos.encodeAsset data.value )
        , ( "memo", Markdown.encode data.memo )
        ]



-- GRAPHQL API


transferItemSelectionSet : SelectionSet Transfer Cambiatus.Object.Transfer
transferItemSelectionSet =
    SelectionSet.succeed Transfer
        |> with Cambiatus.Object.Transfer.id
        |> with (Cambiatus.Object.Transfer.to Profile.selectionSet)
        |> with (Cambiatus.Object.Transfer.from Profile.selectionSet)
        |> with Cambiatus.Object.Transfer.amount
        |> with (Markdown.maybeSelectionSet Cambiatus.Object.Transfer.memo)
        |> with
            (Cambiatus.Object.Transfer.community
                Cambiatus.Object.Community.name
            )
        |> with (Cambiatus.Object.Transfer.community communitySelectionSet)
        |> with Cambiatus.Object.Transfer.createdAt
        |> with (SelectionSet.map CreatedTx Cambiatus.Object.Transfer.createdTx)


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


transferSucceedSubscription : Eos.Symbol -> { from : Eos.Name, to : Eos.Name } -> SelectionSet Transfer Graphql.Operation.RootSubscription
transferSucceedSubscription symbol { from, to } =
    let
        args =
            { input =
                { from = Eos.nameToString from
                , to = Eos.nameToString to
                , symbol = symbolToString symbol |> String.toUpper
                }
            }
    in
    Subscription.transfersucceed args transferItemSelectionSet



-- VIEWS


type alias ProfileSummaries =
    { left : Profile.Summary.Model
    , right : Profile.Summary.Model
    }


updateProfileSummaries : ProfileSummaries -> Bool -> Profile.Summary.Msg -> ProfileSummaries
updateProfileSummaries profileSummaries isLeft profileSummaryMsg =
    if isLeft then
        { profileSummaries | left = Profile.Summary.update profileSummaryMsg profileSummaries.left }

    else
        { profileSummaries | right = Profile.Summary.update profileSummaryMsg profileSummaries.right }


view :
    LoggedIn.Model
    -> Transfer
    -> Profile.Summary.Model
    -> (Profile.Summary.Msg -> msg)
    -> msg
    -> List (Html.Attribute msg)
    -> Html msg
view loggedIn transfer profileSummary profileSummaryToMsg onClickMsg attrs =
    let
        { t } =
            loggedIn.shared.translators

        ( otherProfile, isFromUser ) =
            if transfer.from.account == loggedIn.accountName then
                ( transfer.to, True )

            else
                ( transfer.from, False )
    in
    div
        (class "flex hover:bg-gray-100 p-4 cursor-pointer"
            :: onClick onClickMsg
            :: attrs
        )
        [ profileSummary
            |> Profile.Summary.withoutName
            |> Profile.Summary.withImageSize "w-14 h-14"
            |> Profile.Summary.view loggedIn.shared.translators loggedIn.accountName otherProfile
            |> Html.map profileSummaryToMsg
        , div [ class "ml-4 w-full overflow-ellipsis overflow-hidden" ]
            [ p
                [ class "font-sm flex flex-wrap"
                , classList
                    [ ( "text-gray-333", isFromUser )
                    , ( "text-indigo-500", not isFromUser )
                    ]
                ]
                [ span [ class "mr-1" ]
                    [ if isFromUser then
                        text <| t "transfer.sent_to"

                      else
                        text <| t "transfer.received_from"
                    ]
                , span [ class "font-bold" ]
                    [ text <|
                        Maybe.withDefault (Eos.nameToString otherProfile.account)
                            otherProfile.name
                    ]
                ]
            , p
                [ classList
                    [ ( "text-gray-333", isFromUser )
                    , ( "text-indigo-500", not isFromUser )
                    ]
                ]
                [ span [ class "font-bold text-lg" ]
                    [ text <|
                        Eos.formatSymbolAmount loggedIn.shared.translators
                            transfer.community.symbol
                            transfer.value
                    ]
                , text " "
                , span [ class "text-sm" ] [ text <| Eos.symbolToSymbolCodeString transfer.community.symbol ]
                ]
            , case transfer.memo of
                Nothing ->
                    text ""

                Just memo ->
                    Markdown.view [ class "text-sm text-gray-900 break-all" ] memo
            ]
        ]
