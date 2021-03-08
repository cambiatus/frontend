module Page.Profile.AddContact exposing
    ( Model
    , Msg
    , init
    , msgToString
    , update
    , view
    )

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Page
import Profile.Contact as Contact
import Route
import Session.LoggedIn as LoggedIn
import UpdateResult as UR


type alias Model =
    Contact.Model


init : LoggedIn.Model -> ( Model, Cmd Msg )
init _ =
    ( Contact.init False, Cmd.none )


type Msg
    = GotContactMsg Contact.Msg


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model { shared, authToken } =
    case msg of
        GotContactMsg subMsg ->
            let
                ( newModel, cmd, newContacts ) =
                    Contact.update subMsg model shared authToken

                actOnNewContacts updateResult =
                    case newContacts of
                        Contact.WithContacts successMessage _ ->
                            updateResult
                                |> UR.addExt (LoggedIn.ShowFeedback LoggedIn.Success successMessage)
                                |> UR.addCmd (Route.replaceUrl shared.navKey Route.Profile)

                        Contact.WithError errorMessage ->
                            updateResult
                                |> UR.addExt (LoggedIn.ShowFeedback LoggedIn.Failure errorMessage)

                        Contact.NotAsked ->
                            updateResult
            in
            UR.init newModel
                |> UR.addCmd (Cmd.map GotContactMsg cmd)
                |> actOnNewContacts


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view ({ shared } as loggedIn) model =
    { title = "Add contact options"
    , content =
        div [ class "bg-white pb-8" ]
            [ Page.viewHeader loggedIn "Contact Options" Route.Profile
            , Contact.view shared.translators model
                |> Html.map GotContactMsg
            ]
    }


msgToString : Msg -> List String
msgToString msg =
    case msg of
        GotContactMsg _ ->
            [ "GotContactMsg" ]
