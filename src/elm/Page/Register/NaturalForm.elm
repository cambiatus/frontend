module Page.Register.NaturalForm exposing (Document(..), Model, Msg(..), init, update, validator, view)

import Html exposing (Html, button, text)
import Html.Events exposing (onSubmit)
import Kyc.CostaRica.Phone as KycPhone
import Page.Login exposing (Model)
import Page.Register.Common exposing (..)
import Session.Shared exposing (Translators)
import Validate exposing (Validator)
import View.Form.Input



--- MODEL


type alias Model =
    { document : String
    , documentType : Document
    , name : String
    , email : String
    , phone : String
    , username : String
    , account : String
    , problems : List ( Field, String )
    }


type Field
    = Document
    | DocumentType
    | Name
    | Email
    | Phone
    | Username
    | Account


init : Model
init =
    { document = ""
    , documentType = SSN
    , name = ""
    , email = ""
    , phone = ""
    , username = ""
    , account = ""
    , problems = []
    }


type Document
    = SSN
    | DIMEX
    | NITE


documentTypeToString : Document -> String
documentTypeToString document =
    case document of
        SSN ->
            "ssn"

        DIMEX ->
            "dimex"

        NITE ->
            "nite"



--- MSG


type Msg
    = EnteredDocument String
    | EnteredDocumentType String
    | EnteredName String
    | EnteredEmail String
    | EnteredPhone String
    | EnteredAccount String
    | ValidateForm



--- VIEW


view : Translators -> Model -> Html Msg
view translators model =
    let
        formTranslationString =
            "register.form"

        documentTranslationString =
            formTranslationString ++ ".document." ++ documentTypeToString model.documentType
    in
    Html.form [ onSubmit ValidateForm ]
        [ viewTitleForStep translators 1
        , viewSelectField "Document Type"
            (documentTypeToString model.documentType)
            EnteredDocumentType
            [ { value = "ssn", label = translators.t "register.form.document.ssn.label" }
            , { value = "dimex", label = translators.t "register.form.document.dimex.label" }
            , { value = "nite", label = translators.t "register.form.document.nite.label" }
            ]
            translators
        , View.Form.Input.init
            { id = "document"
            , label = translators.t (documentTranslationString ++ ".label")
            , onInput = EnteredDocument
            , disabled = False
            , value = model.document
            , placeholder = Just (translators.t (documentTranslationString ++ ".placeholder"))
            , problems = fieldProblems Document model.problems
            , translators = translators
            }
            |> View.Form.Input.withCounter
                (documentTranslationString
                    ++ ".maximum"
                    |> translators.t
                    |> String.toInt
                    |> Maybe.withDefault 10
                )
            |> View.Form.Input.toHtml
        , View.Form.Input.init
            { id = "name"
            , label = "Company Name"
            , disabled = False
            , onInput = EnteredName
            , placeholder = Just "Ex.: Cambiatus"
            , problems = fieldProblems Name model.problems
            , translators = translators
            , value = model.name
            }
            |> View.Form.Input.toHtml
        , View.Form.Input.init
            { id = "email"
            , label = translators.t (formTranslationString ++ ".email.label")
            , onInput = EnteredEmail
            , disabled = False
            , value = model.email
            , placeholder = Just (translators.t (formTranslationString ++ ".email.placeholder"))
            , problems = fieldProblems Email model.problems
            , translators = translators
            }
            |> View.Form.Input.toHtml
        , View.Form.Input.init
            { id = "phone"
            , label = translators.t (formTranslationString ++ ".phone.label")
            , onInput = EnteredPhone
            , disabled = False
            , value = model.phone
            , placeholder = Just (translators.t (formTranslationString ++ ".phone.placeholder"))
            , problems = fieldProblems Phone model.problems
            , translators = translators
            }
            |> View.Form.Input.toHtml
        , View.Form.Input.init
            { id = "account"
            , label = translators.t (formTranslationString ++ ".account.label")
            , onInput = EnteredAccount
            , disabled = False
            , value = model.account
            , placeholder = Just (translators.t (formTranslationString ++ ".account.placeholder"))
            , problems = fieldProblems Account model.problems
            , translators = translators
            }
            |> View.Form.Input.withCounter 12
            |> View.Form.Input.toHtml
        , button [] [ text "Validate" ]
        ]



--- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        EnteredName name ->
            { model | name = name }

        EnteredDocument document ->
            { model | document = document }

        EnteredDocumentType documentType ->
            { model
                | documentType =
                    case documentType of
                        "ssn" ->
                            SSN

                        "dimex" ->
                            DIMEX

                        "nite" ->
                            NITE

                        _ ->
                            SSN
            }

        EnteredEmail email ->
            { model | email = email }

        EnteredPhone phone ->
            { model | phone = phone }

        EnteredAccount account ->
            { model | account = account }

        ValidateForm ->
            { model
                | problems =
                    case Validate.validate validator model of
                        Ok _ ->
                            []

                        Err err ->
                            err
            }


validator : Validator ( Field, String ) Model
validator =
    let
        ifInvalidPhoneNumber subjectToString error =
            Validate.ifFalse (\subject -> KycPhone.isValid (subjectToString subject)) error
    in
    Validate.all
        [ Validate.firstError
            [ Validate.ifBlank .document ( Document, "required" ) ]
        , Validate.firstError
            [ Validate.ifBlank .name ( Name, "required" )
            ]
        , Validate.firstError
            [ Validate.ifBlank .email ( Email, "required" )
            , Validate.ifInvalidEmail .email (\_ -> ( Email, "invalid e-mail" ))
            ]
        , Validate.firstError
            [ Validate.ifBlank .account ( Account, "required" )
            , Validate.ifTrue (\f -> String.length f.account < 12) ( Account, "too short" )
            , Validate.ifTrue (\f -> String.length f.account > 12) ( Account, "too long" )
            , Validate.ifFalse (\f -> String.all Char.isAlphaNum f.account) ( Account, "charerror" )
            ]
        , Validate.firstError
            [ Validate.ifBlank .phone ( Phone, "required" )
            , ifInvalidPhoneNumber .phone ( Phone, "Please, use a valid phone number." )
            ]
        ]
