module Notification exposing (History, Model, Notification, NotificationType(..), SaleHistoryData, TransferData, addNotification, init, notificationHistoryQuery, readAll)

import Account exposing (Profile)
import Bespiral.Object
import Bespiral.Object.Community as Community
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
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
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
    { type_ : String
    , isRead : Bool
    , payload : NotificationType
    , insertedAt : DateTime
    , recipientId : Eos.Name
    }


type alias TransferData =
    { amount : String
    , fromId : String
    , toId : String
    , memo : Maybe String
    }


type alias SaleHistoryData =
    { amount : Float
    , community : Community
    , fromId : Eos.Name
    , toId : Eos.Name
    , communityId : String
    , sale : Sale
    }


type alias Community =
    { logo : String }


type alias Sale =
    { title : String }


type NotificationType
    = Transfer TransferData
    | SaleHistory SaleHistoryData


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


notificationHistoryQuery : Eos.Name -> SelectionSet (List History) RootQuery
notificationHistoryQuery account =
    Query.notificationHistory
        { account = Eos.nameToString account }
        notificationHistorySelectionSet


notificationHistorySelectionSet =
    SelectionSet.succeed History
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
        }


transferSelectionSet : SelectionSet TransferData Bespiral.Object.Transfer
transferSelectionSet =
    SelectionSet.succeed TransferData
        |> with Transfer.amount
        |> with Transfer.fromId
        |> with Transfer.toId
        |> with Transfer.memo


saleHistorySelectionSet =
    SelectionSet.succeed SaleHistoryData
        |> with SaleHistory.amount
        |> with (SaleHistory.community logoSelectionSet)
        |> with (Eos.nameSelectionSet SaleHistory.fromId)
        |> with (Eos.nameSelectionSet SaleHistory.toId)
        |> with SaleHistory.communityId
        |> with (SaleHistory.sale (SelectionSet.succeed Sale |> with Sale.title))


logoSelectionSet =
    SelectionSet.succeed Community
        |> with Community.logo
