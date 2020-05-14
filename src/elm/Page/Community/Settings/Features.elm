module Page.Community.Settings.Features exposing (Model, Msg, init, msgToString, update, view)

import Api.Graphql
import Community
import Eos exposing (Symbol)
import Graphql.Http
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Page
import Page.Community.Settings.Settings
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import UpdateResult as UR


init : LoggedIn.Model -> Symbol -> ( Model, Cmd Msg )
init { shared } symbol =
    ( initModel symbol
    , Cmd.none
    )


initModel : Symbol -> Model
initModel symbol =
    { status = Loading
    , settings = Nothing
    , symbol = symbol
    }


type alias Model =
    { status : Status
    , settings : Maybe Settings
    , symbol : Symbol
    }


type alias Settings =
    { actions : Bool
    , shop : Bool
    }


type Status
    = Loading
    | LoadingFailed (Graphql.Http.Error (Maybe Settings))
    | Loaded Settings


type SettingStatus
    = Enabled
    | Disabled


type Msg
    = CompletedLoad (Result (Graphql.Http.Error (Maybe Settings)) (Maybe Settings))


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


view : LoggedIn.Model -> Model -> Html Msg
view loggedIn model =
    div []
        [ Page.viewHeader loggedIn "Features" (Route.CommunitySettings model.symbol)
        , div
            [ class "grid"
            , style "grid-template" """
                                     """
            ]
            []
        ]


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        CompletedLoad (Ok (Just settings)) ->
            UR.init { model | settings = Just settings }

        CompletedLoad (Ok Nothing) ->
            -- TODO: community not found
            UR.init model

        CompletedLoad (Err err) ->
            UR.init { model | status = LoadingFailed err }
                |> UR.logGraphqlError msg err


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedLoad r ->
            [ "CompletedLoad", UR.resultToString r ]
