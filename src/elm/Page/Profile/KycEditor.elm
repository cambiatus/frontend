module Page.Profile.KycEditor exposing
    ( Model
    , Msg(..)
    , UpdateResult
    , init
    , msgToString
    , update
    , view
    )

import Graphql.Http
import Html exposing (Html, p)
import Kyc exposing (ProfileKyc)
import Profile.EditKycForm as KycForm
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
            { model | kycForm = KycForm.update model.kycForm kycFormMsg }
                |> UR.init

        KycDataSaved resp ->
            model
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
