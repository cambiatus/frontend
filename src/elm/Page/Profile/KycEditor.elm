module Page.Profile.KycEditor exposing
    ( Model
    , Msg(..)
    , UpdateResult
    , init
    , msgToString
    , update
    , view
    )

import Html exposing (Html, p)
import Profile.EditKycForm as KycForm exposing (..)
import Session.LoggedIn as LoggedIn exposing (External(..), FeedbackStatus(..))
import UpdateResult as UR



-- INIT


type Msg
    = NoOp
    | FormMsg KycForm.Msg


type alias Model =
    { kycForm : KycForm.Form }


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
            { model | kycForm = updateKycForm model.kycForm kycFormMsg }
                |> UR.init


updateKycForm : KycForm.Form -> KycForm.Msg -> KycForm.Form
updateKycForm kycForm kycMsg =
    case kycMsg of
        DocumentTypeChanged val ->
            { kycForm
                | document = valToDoc val
                , documentNumber = ""
                , problems = []
            }

        DocumentNumberEntered n ->
            let
                trim : Int -> String -> String -> String
                trim desiredLength oldNum newNum =
                    let
                        corrected =
                            if String.all Char.isDigit newNum then
                                newNum

                            else
                                oldNum
                    in
                    if String.length corrected > desiredLength then
                        String.slice 0 desiredLength corrected

                    else
                        corrected

                trimmedNumber =
                    if String.startsWith "0" n then
                        kycForm.documentNumber

                    else
                        trim kycForm.document.maxLength kycForm.documentNumber n
            in
            { kycForm | documentNumber = trimmedNumber }

        PhoneNumberEntered p ->
            { kycForm | phoneNumber = p }

        KycFormSubmitted f ->
            f


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
