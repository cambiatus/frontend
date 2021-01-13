module Page.Profile.Public exposing (Model, Msg, Status, init, initModel, jsAddressToMsg, msgToString, update, view)

import Api.Graphql
import Eos.Account as Eos
import Graphql.Http
import Html exposing (Html, div)
import Json.Decode exposing (Value)
import Page
import Page.Profile exposing (ProfilePage(..), viewUserInfo)
import Profile exposing (Model)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..), FeedbackStatus(..))
import UpdateResult as UR


init : LoggedIn.Model -> String -> ( Model, Cmd Msg )
init loggedIn accountName =
    let
        profileQuery =
            Api.Graphql.query loggedIn.shared
                (Profile.query (Eos.stringToName accountName))
                CompletedProfileLoad
    in
    ( initModel
    , profileQuery
    )


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = CompletedProfileLoad (Result (Graphql.Http.Error (Maybe Profile.Model)) (Maybe Profile.Model))


type alias Model =
    Status


type Status
    = Loading
    | LoadingFailed (Graphql.Http.Error (Maybe Profile.Model))
    | Loaded Profile.Model
    | NotFound


initModel : Model
initModel =
    Loading


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn status =
    let
        t =
            loggedIn.shared.translators.t

        title =
            case status of
                Loaded profile ->
                    Maybe.withDefault "" profile.name

                _ ->
                    ""

        content =
            case status of
                Loading ->
                    Page.fullPageLoading loggedIn.shared

                Loaded profile ->
                    div []
                        [ Page.viewHeader loggedIn (t "menu.profile") Route.Dashboard
                        , viewUserInfo loggedIn
                            profile
                            Public
                        ]

                NotFound ->
                    Page.fullPageNotFound (t "error.unknown") (t "error.pageNotFound")

                LoadingFailed err ->
                    Page.fullPageNotFound (t "error.unknown") (Page.errorToString err)
    in
    { title = title
    , content = content
    }



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg _ _ =
    case msg of
        CompletedProfileLoad (Ok Nothing) ->
            UR.init NotFound

        CompletedProfileLoad (Ok (Just profile)) ->
            UR.init (Loaded profile)

        CompletedProfileLoad (Err err) ->
            UR.init (LoadingFailed err)
                |> UR.logGraphqlError msg err


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr _ =
    case addr of
        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedProfileLoad _ ->
            [ "CompletedProfileLoad" ]
