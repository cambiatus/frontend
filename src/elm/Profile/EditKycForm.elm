module Profile.EditKycForm exposing
    ( Model
    , Msg(..)
    , init
    , saveKycData
    , update
    , view
    )

import Api.Graphql
import Graphql.Http
import Html exposing (Html, button, div, form, input, label, option, p, select, text)
import Html.Attributes exposing (attribute, class, maxlength, placeholder, selected, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Kyc exposing (ProfileKyc)
import Kyc.CostaRica.CedulaDeIdentidad as CedulaDeIdentidad
import Kyc.CostaRica.Dimex as Dimex
import Kyc.CostaRica.Nite as Nite
import Kyc.CostaRica.Phone as Phone
import Profile
import Session.LoggedIn as LoggedIn exposing (External(..), FeedbackStatus(..))
import Session.Shared exposing (Translators)
import Validate exposing (Validator, ifBlank, validate)


type Msg
    = DocumentTypeChanged String
    | DocumentNumberEntered String
    | PhoneNumberEntered String
    | Submitted Model
    | Saved (Result (Graphql.Http.Error (Maybe ProfileKyc)) (Maybe ProfileKyc))


type CostaRicaDoc
    = CedulaDoc
    | DimexDoc
    | NiteDoc


type alias Doc =
    { docType : CostaRicaDoc
    , isValid : String -> Bool
    , title : String
    , value : String
    , maxLength : Int
    , placeholderText : String
    }


type KycFormField
    = DocumentNumber
    | PhoneNumber


type alias Model =
    { document : Doc
    , documentNumber : String
    , phoneNumber : String
    , validationErrors : List ( KycFormField, String )
    , serverError : Maybe String
    }


kycValidator : Translators -> (String -> Bool) -> Validator ( KycFormField, String ) Model
kycValidator { t } documentValidator =
    let
        ifInvalidNumber subjectToString error =
            Validate.ifFalse (\subject -> documentValidator (subjectToString subject)) error

        ifInvalidPhoneNumber subjectToString error =
            Validate.ifFalse (\subject -> Phone.isValid (subjectToString subject)) error
    in
    Validate.all
        [ Validate.firstError
            [ ifBlank .documentNumber ( DocumentNumber, t "error.required" )
            , ifInvalidNumber .documentNumber ( DocumentNumber, t "register.form.document.errorInvalid" )
            ]
        , Validate.firstError
            [ ifBlank .phoneNumber ( PhoneNumber, t "error.required" )
            , ifInvalidPhoneNumber .phoneNumber ( PhoneNumber, t "error.phone" )
            ]
        ]


init : Model
init =
    { document = valToDoc "cedula_de_identidad"
    , documentNumber = ""
    , phoneNumber = ""
    , validationErrors = []
    , serverError = Nothing
    }


valToDoc : String -> Doc
valToDoc v =
    case v of
        "DIMEX" ->
            { docType = DimexDoc
            , isValid = Dimex.isValid
            , title = "register.form.document.dimex.label"
            , value = "dimex"
            , maxLength = 12
            , placeholderText = "register.form.document.dimex.placeholder"
            }

        "NITE" ->
            { docType = NiteDoc
            , isValid = Nite.isValid
            , title = "register.form.document.nite.label"
            , value = "nite"
            , maxLength = 10
            , placeholderText = "register.form.document.nite.placeholder"
            }

        _ ->
            { docType = CedulaDoc
            , isValid = CedulaDeIdentidad.isValid
            , title = "register.form.document.cedula_de_identidad.label"
            , value = "cedula_de_identidad"
            , maxLength = 11
            , placeholderText = "register.form.document.cedula_de_identidad.placeholder"
            }


view : Translators -> Model -> Html Msg
view { t } model =
    let
        { document, documentNumber, phoneNumber, validationErrors } =
            model

        { docType, placeholderText, maxLength, isValid, title } =
            document

        showProblem field =
            let
                isFieldError ( fieldWithError, _ ) =
                    fieldWithError == field
            in
            case List.filter isFieldError validationErrors of
                h :: _ ->
                    div [ class "form-error" ]
                        [ text (Tuple.second h) ]

                [] ->
                    text ""
    in
    div [ class "md:max-w-sm md:mx-auto py-6" ]
        [ form
            [ onSubmit (Submitted model) ]
            [ div [ class "form-field mb-6" ]
                [ label [ class "input-label block" ]
                    [ text (t "register.form.document.type")
                    ]
                , select
                    [ onInput DocumentTypeChanged
                    , class "form-select"
                    ]
                    [ option
                        [ value "cedula_de_identidad"
                        , selected (docType == CedulaDoc)
                        ]
                        [ text (t "register.form.document.cedula_de_identidad.label") ]
                    , option
                        [ value "DIMEX"
                        , selected (docType == DimexDoc)
                        ]
                        [ text (t "register.form.document.dimex.label") ]
                    , option
                        [ value "NITE"
                        , selected (docType == NiteDoc)
                        ]
                        [ text (t "register.form.document.nite.label") ]
                    ]
                ]
            , div [ class "form-field mb-6" ]
                [ label [ class "input-label block" ]
                    [ text (t title) ]
                , input
                    [ type_ "text"
                    , class "form-input"
                    , attribute "inputmode" "numeric"
                    , onInput DocumentNumberEntered
                    , value documentNumber
                    , maxlength maxLength
                    , placeholder (t placeholderText)
                    ]
                    []
                , showProblem DocumentNumber
                ]
            , div [ class "form-field mb-10" ]
                [ label [ class "input-label block" ]
                    [ text (t "register.form.phone.label") ]
                , input
                    [ type_ "tel"
                    , class "form-input"
                    , value phoneNumber
                    , attribute "inputmode" "numeric"
                    , onInput PhoneNumberEntered
                    , maxlength 8
                    , placeholder (t "register.form.phone.placeholder")
                    ]
                    []
                , showProblem PhoneNumber
                ]
            , div []
                [ button
                    [ class "button w-full button-primary" ]
                    [ text (t "profile.edit.submit") ]
                ]
            ]
        ]


update : Translators -> Model -> Msg -> Model
update translators model msg =
    let
        { t } =
            translators
    in
    case msg of
        DocumentTypeChanged val ->
            { model
                | document = valToDoc val
                , documentNumber = ""
                , validationErrors = []
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
                        model.documentNumber

                    else
                        trim model.document.maxLength model.documentNumber n
            in
            { model | documentNumber = trimmedNumber }

        PhoneNumberEntered p ->
            { model | phoneNumber = p }

        Submitted m ->
            let
                formValidator =
                    kycValidator translators m.document.isValid

                errors =
                    case validate formValidator m of
                        Ok _ ->
                            []

                        Err errs ->
                            errs
            in
            { m | validationErrors = errors }

        Saved (Ok _) ->
            model

        Saved (Err _) ->
            let
                errorForm =
                    { model | serverError = Just (t "error.unknown") }
            in
            errorForm


modelToProfileKyc : Model -> ProfileKyc
modelToProfileKyc model =
    { documentType = model.document.value
    , document = model.documentNumber
    , userType = "natural"
    , phone = model.phoneNumber
    , isVerified = False
    }


saveKycData : LoggedIn.Model -> Model -> Cmd Msg
saveKycData { accountName, shared } model =
    let
        data =
            modelToProfileKyc model
    in
    Api.Graphql.mutation shared
        (Profile.upsertKycMutation accountName data)
        Saved
