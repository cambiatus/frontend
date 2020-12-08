module Notification exposing
    ( History
    , MintData
    , Model
    , Notification
    , NotificationType(..)
    , OrderData
    , TransferData
    , addNotification
    , init
    , markAsReadMutation
    , notificationHistoryQuery
    , readAll
    )

import Cambiatus.Mutation as Mutation
import Cambiatus.Object
import Cambiatus.Object.Community as Community
import Cambiatus.Object.Mint as Mint
import Cambiatus.Object.NotificationHistory as NotificationHistory
import Cambiatus.Object.Order as Order
import Cambiatus.Object.Product as Product
import Cambiatus.Object.Transfer as Transfer
import Cambiatus.Query as Query
import Cambiatus.Scalar exposing (DateTime(..))
import Cambiatus.Union
import Cambiatus.Union.NotificationType
import Eos
import Eos.Account as Eos
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Transfer



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
    { id : Int
    , amount : Float
    , fromId : String
    , toId : String
    , memo : Maybe String
    , symbol : Eos.Symbol
    , community : Community
    }


type alias OrderData =
    { amount : Float
    , community : Community
    , fromId : Eos.Name
    , toId : Eos.Name
    , communityId : String
    , product : Product
    }


type alias MintData =
    { quantity : Float
    , memo : Maybe String
    , community : Community
    }


type alias Community =
    { logo : String
    , symbol : Eos.Symbol
    }


type alias Product =
    { id : Int
    , title : String
    , image : Maybe String
    }


type NotificationType
    = Transfer TransferData
    | SaleHistory OrderData
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


notificationHistorySelectionSet : SelectionSet History Cambiatus.Object.NotificationHistory
notificationHistorySelectionSet =
    SelectionSet.succeed History
        |> with NotificationHistory.id
        |> with NotificationHistory.type_
        |> with NotificationHistory.isRead
        |> with (NotificationHistory.payload typeUnionSelectionSet)
        |> with NotificationHistory.insertedAt
        |> with (Eos.nameSelectionSet NotificationHistory.recipientId)


typeUnionSelectionSet : SelectionSet NotificationType Cambiatus.Union.NotificationType
typeUnionSelectionSet =
    Cambiatus.Union.NotificationType.fragments
        { onTransfer = SelectionSet.map Transfer transferSelectionSet
        , onOrder = SelectionSet.map SaleHistory saleHistorySelectionSet
        , onMint = SelectionSet.map Mint mintSelectionSet
        }


mintSelectionSet : SelectionSet MintData Cambiatus.Object.Mint
mintSelectionSet =
    SelectionSet.succeed MintData
        |> with Mint.quantity
        |> with Mint.memo
        |> with (Mint.community logoSelectionSet)


transferSelectionSet : SelectionSet TransferData Cambiatus.Object.Transfer
transferSelectionSet =
    SelectionSet.succeed TransferData
        |> with Transfer.id
        |> with Transfer.amount
        |> with Transfer.fromId
        |> with Transfer.toId
        |> with Transfer.memo
        |> with (Eos.symbolSelectionSet Transfer.communityId)
        |> with (Transfer.community logoSelectionSet)


saleHistorySelectionSet : SelectionSet OrderData Cambiatus.Object.Order
saleHistorySelectionSet =
    SelectionSet.succeed OrderData
        |> with Order.amount
        |> with (Order.community logoSelectionSet)
        |> with (Eos.nameSelectionSet Order.fromId)
        |> with (Eos.nameSelectionSet Order.toId)
        |> with Order.communityId
        |> with
            (Order.product
                (SelectionSet.succeed Product
                    |> with Product.id
                    |> with Product.title
                    |> with Product.image
                )
            )


logoSelectionSet : SelectionSet Community Cambiatus.Object.Community
logoSelectionSet =
    SelectionSet.succeed Community
        |> with Community.logo
        |> with (Eos.symbolSelectionSet Community.symbol)
