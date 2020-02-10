module Notification exposing (History, MintData, Model, Notification, NotificationType(..), SaleHistoryData, TransferData, addNotification, init, markAsReadMutation, notificationHistoryQuery, readAll)

import Bespiral.Mutation as Mutation
import Bespiral.Object
import Bespiral.Object.Community as Community
import Bespiral.Object.Mint as Mint
import Bespiral.Object.NotificationHistory as NotificationHistory
import Bespiral.Object.Sale as Sale
import Bespiral.Object.SaleHistory as SaleHistory
import Bespiral.Object.Transfer as Transfer
import Bespiral.Query as Query
import Bespiral.Scalar exposing (DateTime(..))
import Bespiral.Union
import Bespiral.Union.NotificationType
import Community exposing (Balance, Community)
import Eos exposing (Symbol)
import Eos.Account as Eos
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Profile exposing (Profile)
import Transfer exposing (transferItemSelectionSet)



-- INIT


init : Model
init =
    { readNotifications = []
    , unreadNotifications = []
    , hasUnread = False
    }



-- MODEL


type alias Model =
    { readNotifications : List Notification
    , unreadNotifications : List Notification
    , hasUnread : Bool
    }


type alias Notification =
    { title : String
    , description : String
    , class : String
    , unread : Bool
    , link : Maybe String
    }


type alias History =
    { id : Int
    , type_ : String
    , isRead : Bool
    , payload : NotificationType
    , insertedAt : DateTime
    , recipientId : Eos.Name
    }


type alias TransferData =
    { amount : Float
    , fromId : String
    , toId : String
    , memo : Maybe String
    , symbol : String
    , community : Community
    }


type alias SaleHistoryData =
    { amount : Float
    , community : Community
    , fromId : Eos.Name
    , toId : Eos.Name
    , communityId : String
    , sale : Sale
    }


type alias MintData =
    { quantity : Float
    , memo : Maybe String
    , community : Community
    }


type alias Community =
    { logo : String
    , symbol : String
    }


type alias Sale =
    { id : Int
    , title : String
    , image : Maybe String
    }


type NotificationType
    = Transfer TransferData
    | SaleHistory SaleHistoryData
    | Mint MintData


addNotification : Notification -> Model -> Model
addNotification notification model =
    { model
        | unreadNotifications = notification :: model.unreadNotifications
        , hasUnread = True
    }


readAll : Model -> Model
readAll model =
    { model
        | hasUnread = False
        , unreadNotifications = []
        , readNotifications = model.readNotifications ++ model.unreadNotifications
    }


markAsReadMutation : Int -> SelectionSet History RootMutation
markAsReadMutation id =
    Mutation.readNotification
        { input = { id = id } }
        notificationHistorySelectionSet


notificationHistoryQuery : Eos.Name -> SelectionSet (List History) RootQuery
notificationHistoryQuery account =
    Query.notificationHistory
        { account = Eos.nameToString account }
        notificationHistorySelectionSet


notificationHistorySelectionSet =
    SelectionSet.succeed History
        |> with NotificationHistory.id
        |> with NotificationHistory.type_
        |> with NotificationHistory.isRead
        |> with (NotificationHistory.payload typeUnionSelectionSet)
        |> with NotificationHistory.insertedAt
        |> with (Eos.nameSelectionSet NotificationHistory.recipientId)


typeUnionSelectionSet : SelectionSet NotificationType Bespiral.Union.NotificationType
typeUnionSelectionSet =
    Bespiral.Union.NotificationType.fragments
        { onTransfer = SelectionSet.map Transfer transferSelectionSet
        , onSaleHistory = SelectionSet.map SaleHistory saleHistorySelectionSet
        , onMint = SelectionSet.map Mint mintSelectionSet
        }


mintSelectionSet : SelectionSet MintData Bespiral.Object.Mint
mintSelectionSet =
    SelectionSet.succeed MintData
        |> with Mint.quantity
        |> with Mint.memo
        |> with (Mint.community logoSelectionSet)


transferSelectionSet : SelectionSet TransferData Bespiral.Object.Transfer
transferSelectionSet =
    SelectionSet.succeed TransferData
        |> with Transfer.amount
        |> with Transfer.fromId
        |> with Transfer.toId
        |> with Transfer.memo
        |> with Transfer.communityId
        |> with (Transfer.community logoSelectionSet)


saleHistorySelectionSet =
    SelectionSet.succeed SaleHistoryData
        |> with SaleHistory.amount
        |> with (SaleHistory.community logoSelectionSet)
        |> with (Eos.nameSelectionSet SaleHistory.fromId)
        |> with (Eos.nameSelectionSet SaleHistory.toId)
        |> with SaleHistory.communityId
        |> with
            (SaleHistory.sale
                (SelectionSet.succeed Sale
                    |> with Sale.id
                    |> with Sale.title
                    |> with Sale.image
                )
            )


logoSelectionSet =
    SelectionSet.succeed Community
        |> with Community.logo
        |> with Community.symbol
