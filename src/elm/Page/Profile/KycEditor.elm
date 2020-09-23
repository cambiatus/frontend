module Page.Profile.KycEditor exposing
    ( Model
    , Msg(..)
    , UpdateResult
    , init
    , msgToString
    , update
    , view
    )

import Html exposing (Html)
import Profile.EditKycForm as KycForm
import Route
import Session.LoggedIn as LoggedIn exposing (External(..), FeedbackStatus(..))
import UpdateResult as UR



-- MODEL


type alias Model =
    { kycForm : KycForm.Model }



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init _ =
    ( { kycForm = KycForm.init }
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
                newModel =
                    { model | kycForm = KycForm.update model.kycForm kycFormMsg }
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
                                        (t "KYC were saved. Now you have full access to the community!")
                                    )

                _ ->
                    newModel |> UR.init



-- VIEW


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



-- INTEROP


msgToString : Msg -> List String
msgToString msg =
    case msg of
        FormMsg _ ->
            [ "FormMsg" ]
