module Page.Notification exposing (..)

import Api.Graphql
import Graphql.Http
import Html exposing (..)
import Notification exposing (History)
import Session.LoggedIn as LoggedIn exposing (External(..))
import UpdateResult as UR



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init ({ shared } as loggedIn) =
    ( initModel loggedIn
    , Api.Graphql.query shared (Notification.notificationHistoryQuery loggedIn.accountName) CompletedLoadNotificationHistory
    )



-- MODEL


type alias Model =
    { status : Status
    }


initModel : LoggedIn.Model -> Model
initModel loggedIn =
    { status = Loading }


type Status
    = Loading
    | LoadingFailed (Graphql.Http.Error (List History))
    | Loaded (List History)



-- VIEW


view : LoggedIn.Model -> Model -> Html Msg
view loggedIn model =
    div [] [ text "" ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = CompletedLoadNotificationHistory (Result (Graphql.Http.Error (List History)) (List History))


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedLoadNotificationHistory r ->
            [ "CompletedLoadNotificationHistory", UR.resultToString r ]
