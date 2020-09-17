module Page.Register.NaturalForm exposing (Document(..), Field(..), Model, Msg(..), documentTypeToString, init, update, validator, view)

import Html exposing (Html)
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
    , account : String
    , problems : List ( Field, String )
    }


type Field
    = Document
    | DocumentType
    | Name
    | Email
    | Phone
    | Account


init : { a | account : Maybe String, email : Maybe String, phone : Maybe String } -> Model
init options =
    { document = ""
    , documentType = NITE
    , name = ""
    , email = Maybe.withDefault "" options.email
    , phone = Maybe.withDefault "" options.phone
    , account = Maybe.withDefault "" options.account
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
            "cedula_de_identidad"

        DIMEX ->
            "dimex"

        NITE ->
            "nite"



--- MSG


type Msg
    = EnteredDocument Int String
    | EnteredDocumentType String
    | EnteredName String
    | EnteredEmail String
    | EnteredPhone String
    | EnteredAccount String



--- VIEW


view : Translators -> Model -> Html Msg
view translators model =
    let
        formTranslationString =
            "register.form"

        documentTranslationString =
            formTranslationString ++ ".document." ++ documentTypeToString model.documentType

        documentMaximum =
            documentTranslationString
                ++ ".maximum"
                |> translators.t
                |> String.toInt
                |> Maybe.withDefault 10
    in
    Html.div []
        [ viewSelectField "Document Type"
            (documentTypeToString model.documentType)
            True
            EnteredDocumentType
            [ { value = "ssn", label = translators.t "register.form.document.cedula_de_identidad.label" }
            , { value = "dimex", label = translators.t "register.form.document.dimex.label" }
            , { value = "nite", label = translators.t "register.form.document.nite.label" }
            ]
            translators
        , View.Form.Input.init
            { id = "document"
            , label = translators.t (documentTranslationString ++ ".label")
            , onInput = EnteredDocument documentMaximum
            , disabled = False
            , value = model.document
            , placeholder = Just (translators.t (documentTranslationString ++ ".placeholder"))
            , problems = fieldProblems Document model.problems
            , translators = translators
            }
            |> View.Form.Input.withCounter documentMaximum
            |> View.Form.Input.toHtml
        , View.Form.Input.init
            { id = "name"
            , label = translators.t (formTranslationString ++ ".name.label")
            , disabled = False
            , onInput = EnteredName
            , placeholder = Just (translators.t (formTranslationString ++ ".name.placeholder"))
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
        ]



--- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        EnteredName name ->
            { model | name = name }

        EnteredDocument maximum document ->
            { model
                | document =
                    if String.length document <= maximum && not (containsLetters document) then
                        document

                    else
                        model.document
            }

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
                , document = ""
            }

        EnteredEmail email ->
            { model | email = email }

        EnteredPhone phone ->
            { model | phone = phone }

        EnteredAccount account ->
            { model
                | account =
                    if String.length account > 12 || (account |> containsNumberGreaterThan 5) then
                        model.account

                    else
                        account
            }


validator : Translators -> Validator ( Field, String ) Model
validator { t } =
    let
        ifInvalidPhoneNumber subjectToString error =
            Validate.ifFalse (\subject -> KycPhone.isValid (subjectToString subject)) error
    in
    Validate.all
        [ Validate.ifBlank .document ( Document, t "error.required" )
        , Validate.ifBlank .name ( Name, t "error.required" )
        , Validate.ifBlank .account ( Account, t "error.required" )
        , Validate.firstError
            [ Validate.ifBlank .email ( Email, t "error.required" )
            , Validate.ifInvalidEmail .email (\_ -> ( Email, t "error.email" ))
            ]
        , Validate.firstError
            [ Validate.ifBlank .account ( Account, t "error.required" )
            , Validate.ifTrue (\f -> String.length f.account < 12) ( Account, t "error.validator.text.exactly" )
            , Validate.ifTrue (\f -> String.length f.account > 12) ( Account, t "error.validator.text.exactly" )
            , Validate.ifFalse (\f -> String.all Char.isAlphaNum f.account) ( Account, t "error.invalidChar" )
            ]
        , Validate.firstError
            [ Validate.ifBlank .phone ( Phone, t "error.required" )
            , ifInvalidPhoneNumber .phone ( Phone, t "error.phone" )
            ]
        ]
