module Page.Profile.AddKyc exposing
    ( Model
    , Msg(..)
    , UpdateResult
    , init
    , msgToString
    , update
    , view
    )

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Page
import Profile exposing (Model)
import Profile.EditKycForm as KycForm
import RemoteData
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import UpdateResult as UR
import View.Feedback as Feedback



-- MODEL


type alias Model =
    KycForm.Model



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init _ =
    ( KycForm.init
    , Cmd.none
    )



-- UPDATE


type Msg
    = FormMsg KycForm.Msg


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    let
        { t } =
            loggedIn.shared.translators
    in
    case msg of
        FormMsg kycFormMsg ->
            let
                formUpdateResult =
                    KycForm.update
                        loggedIn.shared
                        model
                        kycFormMsg
                        |> UR.fromChild identity
                            FormMsg
                            LoggedIn.addFeedback
                            model
            in
            case kycFormMsg of
                KycForm.Submitted formOutput ->
                    formUpdateResult
                        |> UR.addExt (LoggedIn.UpdatedLoggedIn { loggedIn | profile = RemoteData.Loading })
                        |> UR.addExt
                            (KycForm.saveKycData loggedIn formOutput
                                |> LoggedIn.mapExternal FormMsg
                            )

                KycForm.Saved result ->
                    case result of
                        RemoteData.Failure _ ->
                            formUpdateResult
                                |> UR.addExt (ShowFeedback Feedback.Failure (t "error.unknown"))

                        _ ->
                            formUpdateResult
                                |> UR.addCmd
                                    (Route.Profile loggedIn.accountName
                                        |> Route.replaceUrl loggedIn.shared.navKey
                                    )
                                |> UR.addExt (LoggedIn.ReloadResource LoggedIn.ProfileResource)
                                |> UR.addExt (ShowFeedback Feedback.Success (t "community.kyc.add.success"))

                _ ->
                    formUpdateResult



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        { t } =
            loggedIn.shared.translators

        pageTitle =
            t "community.kyc.add.title"

        content =
            case loggedIn.profile of
                RemoteData.Loading ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.NotAsked ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.Failure e ->
                    Page.fullPageGraphQLError pageTitle e

                RemoteData.Success _ ->
                    div [ class "bg-white" ]
                        [ Page.viewHeader loggedIn pageTitle
                        , div [ class "px-4" ]
                            [ KycForm.view
                                loggedIn.shared.translators
                                model
                                |> Html.map FormMsg
                            ]
                        ]
    in
    { title = pageTitle
    , content = content
    }



-- INTEROP


msgToString : Msg -> List String
msgToString msg =
    case msg of
        FormMsg _ ->
            [ "FormMsg" ]
