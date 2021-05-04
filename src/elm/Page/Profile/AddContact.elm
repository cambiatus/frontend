module Page.Profile.AddContact exposing
    ( Model
    , Msg
    , init
    , msgToString
    , update
    , view
    )

import Api.Graphql
import Graphql.Http
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Http
import Page
import Profile
import Profile.Contact as Contact
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn
import UpdateResult as UR
import View.Feedback as Feedback


type alias Model =
    RemoteData String Contact.Model


init : LoggedIn.Model -> ( Model, Cmd Msg )
init ({ shared } as loggedIn) =
    case loggedIn.profile of
        LoggedIn.Loaded profile ->
            ( profile.contacts
                |> Contact.initMultiple
                |> RemoteData.Success
            , Cmd.none
            )

        LoggedIn.LoadingFailed _ _ ->
            ( RemoteData.Failure (shared.translators.t "contact_form.profile_loading_failed")
            , Cmd.none
            )

        _ ->
            let
                profileQuery =
                    Api.Graphql.query loggedIn.shared
                        (Just loggedIn.authToken)
                        (Profile.query loggedIn.accountName)
                        CompletedProfileLoad
            in
            ( RemoteData.Loading, profileQuery )


type Msg
    = GotContactMsg Contact.Msg
    | CompletedProfileLoad (RemoteData (Graphql.Http.Error (Maybe Profile.Model)) (Maybe Profile.Model))


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model ({ shared, authToken } as loggedIn) =
    case msg of
        CompletedProfileLoad (RemoteData.Success (Just profile_)) ->
            Contact.initMultiple profile_.contacts
                |> RemoteData.Success
                |> UR.init

        CompletedProfileLoad (RemoteData.Success Nothing) ->
            RemoteData.Failure (shared.translators.t "Something went wrong")
                |> UR.init

        CompletedProfileLoad (RemoteData.Failure _) ->
            RemoteData.Failure (shared.translators.t "Something went wrong")
                |> UR.init

        CompletedProfileLoad RemoteData.Loading ->
            UR.init RemoteData.Loading

        CompletedProfileLoad RemoteData.NotAsked ->
            UR.init RemoteData.NotAsked

        GotContactMsg subMsg ->
            case ( model, LoggedIn.profile loggedIn ) of
                ( RemoteData.Success contactModel, Just profile_ ) ->
                    let
                        ( newModel, cmd, newContacts ) =
                            Contact.update subMsg
                                contactModel
                                shared
                                authToken
                                profile_.contacts

                        actOnNewContacts updateResult =
                            case newContacts of
                                Contact.WithContacts successMessage contacts shouldRedirect ->
                                    let
                                        newProfile =
                                            { profile_ | contacts = contacts }
                                    in
                                    updateResult
                                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Success successMessage)
                                        |> UR.addExt (LoggedIn.UpdatedLoggedIn { loggedIn | profile = LoggedIn.Loaded newProfile })
                                        |> (if shouldRedirect then
                                                UR.addCmd (Route.replaceUrl shared.navKey Route.Profile)

                                            else
                                                identity
                                           )

                                Contact.WithError errorMessage ->
                                    updateResult
                                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure errorMessage)

                                Contact.NotAsked ->
                                    updateResult
                    in
                    newModel
                        |> RemoteData.Success
                        |> UR.init
                        |> UR.addCmd (Cmd.map GotContactMsg cmd)
                        |> actOnNewContacts

                _ ->
                    UR.init model


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view ({ shared } as loggedIn) model =
    { title = shared.translators.t "contact_form.options"
    , content =
        div [ class "bg-white pb-8" ]
            [ Page.viewHeader loggedIn (shared.translators.t "contact_form.options") Route.Profile
            , case model of
                RemoteData.Success contacts ->
                    Contact.view shared.translators contacts
                        |> Html.map GotContactMsg

                RemoteData.Loading ->
                    Page.fullPageLoading shared

                RemoteData.Failure err ->
                    Page.fullPageError err Http.NetworkError

                RemoteData.NotAsked ->
                    text ""
            ]
    }


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedProfileLoad r ->
            [ "CompletedProfileLoad", UR.remoteDataToString r ]

        GotContactMsg _ ->
            [ "GotContactMsg" ]
