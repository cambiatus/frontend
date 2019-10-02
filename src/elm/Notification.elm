module Notification exposing (History, Model, Notification, addNotification, init, notificationHistoryQuery, readAll)

import Bespiral.Object.NotificationHistory as NotificationHistory
import Bespiral.Query as Query
import Bespiral.Scalar exposing (DateTime(..))
import Eos.Account as Eos
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)



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
    { recipient : Eos.Name
    , type_ : String
    , isRead : Bool
    , payload : String
    , insertedAt : DateTime
    }


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
        |> with (Eos.nameSelectionSet NotificationHistory.recipientId)
        |> with NotificationHistory.type_
        |> with NotificationHistory.isRead
        |> with NotificationHistory.payload
        |> with NotificationHistory.insertedAt
