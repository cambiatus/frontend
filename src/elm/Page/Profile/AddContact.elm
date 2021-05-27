module Page.Profile.AddContact exposing
    ( Model
    , Msg
    , init
    , msgToString
    , receiveBroadcast
    , update
    , view
    )

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
init loggedIn =
    ( RemoteData.Loading
    , LoggedIn.maybeInitWith CompletedLoadProfile .profile loggedIn
    )


type Msg
    = GotContactMsg Contact.Msg
    | CompletedLoadProfile Profile.Model


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model ({ shared, authToken } as loggedIn) =
    case msg of
        CompletedLoadProfile profile ->
            profile.contacts
                |> Contact.initMultiple
                |> RemoteData.Success
                |> UR.init

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
                                    case loggedIn.profile of
                                        RemoteData.Success profile ->
                                            updateResult
                                                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Success successMessage)
                                                |> UR.addExt
                                                    ({ profile | contacts = contacts }
                                                        |> LoggedIn.ProfileLoaded
                                                        |> LoggedIn.ExternalBroadcast
                                                    )
                                                |> (if shouldRedirect then
                                                        UR.addCmd (Route.replaceUrl shared.navKey Route.Profile)

                                                    else
                                                        identity
                                                   )

                                        _ ->
                                            updateResult
                                                |> UR.logImpossible msg [ "WithContacts", "NoProfile" ]

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
            [ Page.viewHeader loggedIn (shared.translators.t "contact_form.options")
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


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.ProfileLoaded profile ->
            Just (CompletedLoadProfile profile)

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedLoadProfile _ ->
            [ "CompletedLoadProfile" ]

        GotContactMsg _ ->
            [ "GotContactMsg" ]
