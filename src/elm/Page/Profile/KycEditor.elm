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
import Html exposing (Html, p)
import Kyc exposing (ProfileKyc)
import Profile
import Profile.EditKycForm as KycForm
import Route
import Session.LoggedIn as LoggedIn exposing (External(..), FeedbackStatus(..))
import UpdateResult as UR



-- INIT


type Msg
    = NoOp
    | FormMsg KycForm.Msg
    | KycDataSaved (Result (Graphql.Http.Error (Maybe ProfileKyc)) (Maybe ProfileKyc))


type alias Model =
    { kycForm : KycForm.Model }


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( { kycForm = KycForm.initKycForm }
    , Cmd.none
    )


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        NoOp ->
            model |> UR.init

        FormMsg kycFormMsg ->
            case kycFormMsg of
                KycForm.KycFormSubmitted f ->
                    let
                        newModel =
                            { model | kycForm = KycForm.update model.kycForm kycFormMsg }
                    in
                    if List.isEmpty newModel.kycForm.problems then
                        newModel
                            |> UR.init
                            |> UR.addCmd
                                (saveKycData loggedIn
                                    { documentType = newModel.kycForm.document.value
                                    , document = newModel.kycForm.documentNumber
                                    , userType = "natural"
                                    , phone = newModel.kycForm.phoneNumber
                                    , isVerified = False
                                    }
                                )

                    else
                        newModel
                            |> UR.init

                _ ->
                    { model | kycForm = KycForm.update model.kycForm kycFormMsg }
                        |> UR.init

        KycDataSaved resp ->
            let
                { t } =
                    loggedIn.shared.translators
            in
            case resp of
                Ok _ ->
                    model
                        |> UR.init
                        |> UR.addCmd
                            (Route.Profile
                                |> Route.replaceUrl loggedIn.shared.navKey
                            )
                        |> UR.addExt (ShowFeedback Success (t "KYC were saved. Now you have full access to the community!"))

                Err err ->
                    let
                        f =
                            model.kycForm

                        errorForm =
                            { f | serverError = Just "Sorry, couldn't save the form. Please, check your data and try again." }
                    in
                    { model | kycForm = errorForm }
                        |> UR.init


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        content =
            KycForm.view
                loggedIn.shared.translators
                model.kycForm
                |> Html.map FormMsg
    in
    { title = "Edit KYC page"
    , content = content
    }


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        FormMsg _ ->
            [ "FormMsg" ]

        KycDataSaved _ ->
            [ "KycDataSaved" ]


saveKycData : LoggedIn.Model -> ProfileKyc -> Cmd Msg
saveKycData { accountName, shared } data =
    Api.Graphql.mutation shared
        (Profile.upsertKycMutation accountName data)
        KycDataSaved
