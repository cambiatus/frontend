module Page.Profile.Public exposing (Model, Msg, Status, init, msgToString, update, view)

import Api.Graphql
import Eos.Account as Eos
import Graphql.Http
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Page
import Page.Profile exposing (ProfilePage(..), viewUserInfo)
import Profile
import RemoteData exposing (RemoteData)
import Session.LoggedIn as LoggedIn exposing (External(..))
import UpdateResult as UR


init : LoggedIn.Model -> String -> ( Model, Cmd Msg )
init loggedIn accountName =
    let
        profileQuery =
            Api.Graphql.query loggedIn.shared
                (Just loggedIn.authToken)
                (Profile.query (Eos.stringToName accountName))
                CompletedProfileLoad
    in
    ( initModel
    , profileQuery
    )


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = CompletedProfileLoad (RemoteData (Graphql.Http.Error (Maybe Profile.Model)) (Maybe Profile.Model))


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
                    div [ class "flex-grow flex flex-col" ]
                        [ Page.viewHeader loggedIn (t "menu.profile")
                        , viewUserInfo loggedIn
                            profile
                            Public
                            (text "")
                        ]

                NotFound ->
                    Page.fullPageNotFound (t "error.unknown") (t "error.pageNotFound")

                LoadingFailed err ->
                    Page.fullPageGraphQLError (t "error.unknown") err
    in
    { title = title
    , content = content
    }



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model _ =
    case msg of
        CompletedProfileLoad (RemoteData.Success Nothing) ->
            UR.init NotFound

        CompletedProfileLoad (RemoteData.Success (Just profile)) ->
            UR.init (Loaded profile)

        CompletedProfileLoad (RemoteData.Failure err) ->
            UR.init (LoadingFailed err)
                |> UR.logGraphqlError msg err

        CompletedProfileLoad _ ->
            UR.init model


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedProfileLoad r ->
            [ "CompletedProfileLoad", UR.remoteDataToString r ]
