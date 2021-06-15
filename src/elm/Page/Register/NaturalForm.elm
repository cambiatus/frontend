module Page.Register.NaturalForm exposing (Document(..), Field(..), Model, Msg(..), documentTypeToString, init, update, validator, view)

import Html exposing (Html)
import Html.Attributes exposing (autocomplete, classList)
import Kyc.CostaRica.Phone as KycPhone
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
    , problems : List ( Field, String, ProblemEvent )
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
    , documentType = SSN
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
    | EnteredDocumentType Document
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
        [ viewSelectField
            { id = "document_type_select"
            , label = translators.t "register.form.document.type"
            , onInput = EnteredDocumentType
            , options =
                [ { value = DIMEX
                  , label = translators.t "register.form.document.dimex.label"
                  }
                , { value = NITE
                  , label = translators.t "register.form.document.nite.label"
                  }
                , { value = SSN
                  , label = translators.t "register.form.document.cedula_de_identidad.label"
                  }
                ]
            , value = model.documentType
            , valueToString = documentTypeToString
            , enabled = True
            , problems = fieldProblems DocumentType model.problems
            }
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
            |> View.Form.Input.withCounter 8
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
            |> (let
                    hasErrors =
                        List.any (\( f, _, evt ) -> f == Account && evt == OnInput) model.problems
                in
                View.Form.Input.withAttrs
                    [ autocomplete False
                    , classList
                        [ ( "shake-invalid", hasErrors )
                        , ( "field-with-error", hasErrors )
                        ]
                    ]
               )
            |> View.Form.Input.withCounter 12
            |> View.Form.Input.toHtml
        ]



--- UPDATE


update : Translators -> Msg -> Model -> Model
update translators msg model =
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
                | documentType = documentType
                , document = ""
            }

        EnteredEmail email ->
            { model | email = email }

        EnteredPhone phone ->
            { model
                | phone =
                    if String.length phone <= 8 && not (containsLetters phone) then
                        phone

                    else
                        model.phone
            }

        EnteredAccount account ->
            let
                formProblemsWithoutAccount =
                    model.problems
                        |> List.filter (\( field, _, _ ) -> field /= Account)

                ( preparedAccountName, errorMsg ) =
                    validateAccountName translators account model.account
            in
            { model
                | account = preparedAccountName
                , problems =
                    case errorMsg of
                        Nothing ->
                            formProblemsWithoutAccount

                        Just e ->
                            formProblemsWithoutAccount ++ [ ( Account, e, OnInput ) ]
            }


validator : Translators -> Validator ( Field, String, ProblemEvent ) Model
validator { t, tr } =
    let
        ifInvalidPhoneNumber subjectToString error =
            Validate.ifFalse (\subject -> KycPhone.isValid (subjectToString subject)) error
    in
    Validate.all
        [ Validate.ifBlank .document ( Document, t "error.required", OnSubmit )
        , Validate.ifBlank .name ( Name, t "error.required", OnSubmit )
        , Validate.ifBlank .account ( Account, t "error.required", OnSubmit )
        , Validate.firstError
            [ Validate.ifBlank .email ( Email, t "error.required", OnSubmit )
            , Validate.ifInvalidEmail .email (\_ -> ( Email, t "error.email", OnSubmit ))
            ]
        , Validate.firstError
            [ Validate.ifBlank .account ( Account, t "error.required", OnSubmit )
            , Validate.ifTrue (\f -> String.length f.account < 12) ( Account, tr "error.validator.text.exactly" [ ( "base", "12" ) ], OnSubmit )
            , Validate.ifTrue (\f -> String.length f.account > 12) ( Account, tr "error.validator.text.exactly" [ ( "base", "12" ) ], OnSubmit )
            , Validate.ifFalse (\f -> String.all Char.isAlphaNum f.account) ( Account, t "error.invalidChar", OnSubmit )
            ]
        , Validate.firstError
            [ Validate.ifBlank .phone ( Phone, t "error.required", OnSubmit )
            , ifInvalidPhoneNumber .phone ( Phone, t "error.phone", OnSubmit )
            ]
        ]
