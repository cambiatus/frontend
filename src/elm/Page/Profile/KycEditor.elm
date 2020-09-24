module Page.Profile.KycEditor exposing
    ( Model
    , Msg(..)
    , UpdateResult
    , init
    , msgToString
    , update
    , view
    )

import Api.Graphql
import Graphql.Http
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Http
import Page
import Profile exposing (Profile)
import Profile.EditKycForm as KycForm
import Route
import Session.LoggedIn as LoggedIn exposing (External(..), FeedbackStatus(..), ProfileStatus(..))
import UpdateResult as UR



-- MODEL


type alias Model =
    { status : ProfileStatus
    , kycForm : KycForm.Model
    }



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    let
        profileQuery =
            Api.Graphql.query loggedIn.shared
                (Profile.query loggedIn.accountName)
                CompletedProfileLoad
    in
    ( { status = Loading loggedIn.accountName
      , kycForm = KycForm.init
      }
    , profileQuery
    )



-- UPDATE


type Msg
    = FormMsg KycForm.Msg
    | CompletedProfileLoad (Result (Graphql.Http.Error (Maybe Profile)) (Maybe Profile))


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    let
        { t } =
            loggedIn.shared.translators
    in
    case msg of
        CompletedProfileLoad (Ok Nothing) ->
            UR.init model

        CompletedProfileLoad (Ok (Just profile)) ->
            case profile.kyc of
                Just _ ->
                    -- Users with already filled KYC data are restricted from seeing this page.
                    model
                        |> UR.init
                        |> UR.addCmd
                            (Route.Profile
                                |> Route.replaceUrl loggedIn.shared.navKey
                            )
                        |> UR.addExt
                            (ShowFeedback
                                Failure
                                (t "community.kyc.add.restricted")
                            )

                Nothing ->
                    { model
                        | status = Loaded profile
                    }
                        |> UR.init

        CompletedProfileLoad (Err err) ->
            UR.init { model | status = LoadingFailed loggedIn.accountName err }
                |> UR.logGraphqlError msg err

        FormMsg kycFormMsg ->
            let
                newModel =
                    { model
                        | kycForm =
                            KycForm.update
                                loggedIn.shared.translators
                                model.kycForm
                                kycFormMsg
                    }
            in
            case kycFormMsg of
                KycForm.Submitted _ ->
                    let
                        isFormValid =
                            List.isEmpty newModel.kycForm.validationErrors
                    in
                    if isFormValid then
                        newModel
                            |> UR.init
                            |> UR.addCmd
                                (KycForm.saveKycData loggedIn
                                    newModel.kycForm
                                    |> Cmd.map FormMsg
                                )

                    else
                        newModel
                            |> UR.init

                KycForm.Saved _ ->
                    case newModel.kycForm.serverError of
                        Just error ->
                            newModel
                                |> UR.init
                                |> UR.addExt (ShowFeedback Failure error)

                        Nothing ->
                            model
                                |> UR.init
                                |> UR.addCmd
                                    (Route.Profile
                                        |> Route.replaceUrl loggedIn.shared.navKey
                                    )
                                |> UR.addExt
                                    (ShowFeedback
                                        Success
                                        (t "community.kyc.add.success")
                                    )

                _ ->
                    newModel |> UR.init



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        { t } =
            loggedIn.shared.translators

        pageTitle =
            t "community.kyc.add.title"

        content =
            case model.status of
                Loading _ ->
                    Page.fullPageLoading

                LoadingFailed _ _ ->
                    Page.fullPageError pageTitle Http.Timeout

                Loaded _ ->
                    div [ class "bg-white" ]
                        [ Page.viewHeader loggedIn pageTitle Route.Profile
                        , div [ class "px-4" ]
                            [ KycForm.view
                                loggedIn.shared.translators
                                model.kycForm
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
        CompletedProfileLoad r ->
            [ "CompletedProfileLoad", UR.resultToString r ]

        FormMsg _ ->
            [ "FormMsg" ]
