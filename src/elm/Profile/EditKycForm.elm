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
import Html exposing (Html, button, div, form, p, text)
import Html.Attributes exposing (class, maxlength)
import Html.Events exposing (onSubmit)
import Kyc exposing (ProfileKyc)
import Kyc.CostaRica.CedulaDeIdentidad as CedulaDeIdentidad
import Kyc.CostaRica.Dimex as Dimex
import Kyc.CostaRica.Nite as Nite
import Kyc.CostaRica.Phone as Phone
import Profile
import RemoteData exposing (RemoteData)
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Translators)
import Validate exposing (Validator, ifBlank, validate)
import View.Form.Input as Input
import View.Form.Select as Select


type Msg
    = DocumentTypeChanged CostaRicaDoc
    | DocumentNumberEntered String
    | PhoneNumberEntered String
    | Submitted Model
    | Saved (RemoteData (Graphql.Http.Error (Maybe ProfileKyc)) (Maybe ProfileKyc))


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
    { document = initDoc CedulaDoc
    , documentNumber = ""
    , phoneNumber = ""
    , validationErrors = []
    , serverError = Nothing
    }


initDoc : CostaRicaDoc -> Doc
initDoc docType =
    case docType of
        DimexDoc ->
            { docType = DimexDoc
            , isValid = Dimex.isValid
            , title = "register.form.document.dimex.label"
            , value = "dimex"
            , maxLength = 12
            , placeholderText = "register.form.document.dimex.placeholder"
            }

        NiteDoc ->
            { docType = NiteDoc
            , isValid = Nite.isValid
            , title = "register.form.document.nite.label"
            , value = "nite"
            , maxLength = 10
            , placeholderText = "register.form.document.nite.placeholder"
            }

        CedulaDoc ->
            { docType = CedulaDoc
            , isValid = CedulaDeIdentidad.isValid
            , title = "register.form.document.cedula_de_identidad.label"
            , value = "cedula_de_identidad"
            , maxLength = 9
            , placeholderText = "register.form.document.cedula_de_identidad.placeholder"
            }


docToString : CostaRicaDoc -> String
docToString docType =
    case docType of
        DimexDoc ->
            "DIMEX"

        NiteDoc ->
            "NITE"

        CedulaDoc ->
            "cedula_de_identidad"


view : Translators -> Model -> Html Msg
view ({ t } as translators) model =
    let
        { document, documentNumber, phoneNumber, validationErrors } =
            model

        { docType, placeholderText, maxLength, title } =
            document

        isFieldError field ( fieldWithError, _ ) =
            fieldWithError == field

        problemsForField field =
            List.filter (isFieldError field) validationErrors
                |> List.map Tuple.second
    in
    div [ class "md:max-w-sm md:mx-auto py-6" ]
        [ form
            [ onSubmit (Submitted model) ]
            [ Select.init
                { id = "document_type_select"
                , label = t "register.form.document.type"
                , onInput = DocumentTypeChanged
                , firstOption = { value = CedulaDoc, label = t "register.form.document.cedula_de_identidad.label" }
                , value = docType
                , valueToString = docToString
                , disabled = False
                , problems = Nothing
                }
                |> Select.withOptions
                    [ { value = DimexDoc, label = t "register.form.document.dimex.label" }
                    , { value = NiteDoc, label = t "register.form.document.nite.label" }
                    ]
                |> Select.toHtml
            , Input.init
                { label = t title
                , id = "document_number_field"
                , onInput = DocumentNumberEntered
                , disabled = False
                , value = documentNumber
                , placeholder = Just (t placeholderText)
                , problems = Just (problemsForField DocumentNumber)
                , translators = translators
                }
                |> Input.asNumeric
                |> Input.withAttrs [ maxlength maxLength ]
                |> Input.toHtml
            , Input.init
                { label = t "register.form.phone.label"
                , id = "phone_number_field"
                , onInput = PhoneNumberEntered
                , disabled = False
                , value = phoneNumber
                , placeholder = Just (t "register.form.phone.placeholder")
                , problems = Just (problemsForField PhoneNumber)
                , translators = translators
                }
                |> Input.withType Input.Telephone
                |> Input.asNumeric
                |> Input.withAttrs [ maxlength 8 ]
                |> Input.toHtml
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
        DocumentTypeChanged doc ->
            { model
                | document = initDoc doc
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

        Saved (RemoteData.Success _) ->
            model

        Saved (RemoteData.Failure _) ->
            let
                errorForm =
                    { model | serverError = Just (t "error.unknown") }
            in
            errorForm

        Saved _ ->
            model


modelToProfileKyc : Model -> ProfileKyc
modelToProfileKyc model =
    { documentType = model.document.value
    , document = model.documentNumber
    , userType = "natural"
    , phone = model.phoneNumber
    , isVerified = False
    }


saveKycData : LoggedIn.Model -> Model -> Cmd Msg
saveKycData { shared, authToken } model =
    let
        data =
            modelToProfileKyc model
    in
    Api.Graphql.mutation shared
        (Just authToken)
        (Profile.upsertKycMutation data)
        Saved
