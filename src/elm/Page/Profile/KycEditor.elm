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
            let
                newModel =
                    { model | kycForm = KycForm.update model.kycForm kycFormMsg }
            in
            case kycFormMsg of
                KycForm.Submitted _ ->
                    if List.isEmpty newModel.kycForm.validationErrors then
                        newModel
                            |> UR.init
                            |> UR.addCmd
                                (KycForm.saveKycData loggedIn
                                    { documentType = newModel.kycForm.document.value
                                    , document = newModel.kycForm.documentNumber
                                    , userType = "natural"
                                    , phone = newModel.kycForm.phoneNumber
                                    , isVerified = False
                                    }
                                    |> Cmd.map FormMsg
                                )

                    else
                        newModel
                            |> UR.init

                KycForm.Saved _ ->
                    let
                        { t } =
                            loggedIn.shared.translators
                    in
                    case newModel.kycForm.serverError of
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
                                        (t "KYC were saved. Now you have full access to the community!")
                                    )

                        Just error ->
                            newModel
                                |> UR.init
                                |> UR.addExt (ShowFeedback Failure error)

                _ ->
                    newModel |> UR.init


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
