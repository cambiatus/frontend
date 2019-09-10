module Notification exposing (Model, Notification, addNotification, init, readAll)

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



-- UPDATE
--type Msg
--    = Add Notification
--    | ReadAll
--update : Msg -> Model -> ( Model, Cmd msg )
--update msg model =
--    case msg of
--        Add notification ->
--            ( addNotification notification model
--            , Cmd.none
--            )
--        ReadAll ->
--            ( { model
--                | hasUnread = False
--                , unreadNotifications = []
--                , readNotifications = model.readNotifications ++ model.unreadNotifications
--              }
--            , Cmd.none
--            )


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
