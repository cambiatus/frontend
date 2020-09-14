module Page.Register.NaturalForm exposing (Document(..), Field(..), Model, Msg(..), documentTypeToString, init, update, validator, view)

import Address
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
    , username : String
    , account : String
    , street : String
    , zip : String
    , number : String
    , problems : List ( Field, String )
    , state : ( String, String )
    , city : ( String, String )
    , district : ( String, String )
    , country : Address.Country
    , states : List Address.State
    , cities : List Address.City
    , districts : List Address.Neighborhood
    }


type Field
    = Document
    | DocumentType
    | Name
    | Email
    | Phone
    | Username
    | Account
    | Street
    | Zip
    | Number
    | State


init : Address.Country -> Model
init country =
    { document = ""
    , documentType = SSN
    , name = ""
    , email = ""
    , phone = ""
    , username = ""
    , account = ""
    , street = ""
    , zip = ""
    , number = ""
    , problems = []
    , state = ( "", "" )
    , city = ( "", "" )
    , district = ( "", "" )
    , country = country
    , states = country.states
    , cities = []
    , districts = []
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
    | EnteredCity String
    | EnteredDistrict String
    | EnteredState String
    | EnteredStreet String
    | EnteredZip String
    | EnteredNumber String



--- VIEW


view : Translators -> Model -> Html Msg
view translators model =
    let
        formTranslationString =
            "register.form"

        documentTranslationString =
            formTranslationString ++ ".document." ++ documentTypeToString model.documentType
    in
    Html.div []
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
        , viewSelectField (translators.t "register.form.state")
            model.document
            EnteredState
            (List.map (\state -> { value = state.name, label = state.name }) model.country.states)
            translators
        , viewSelectField (translators.t "register.form.city")
            model.document
            EnteredCity
            (List.map (\city -> { value = city.name, label = city.name }) model.cities)
            translators
        , viewSelectField (translators.t "register.form.district")
            model.document
            EnteredDistrict
            (List.map (\district -> { value = district.name, label = district.name }) model.districts)
            translators
        , View.Form.Input.init
            { id = "street"
            , label = translators.t (formTranslationString ++ ".street.label")
            , onInput = EnteredStreet
            , disabled = False
            , value = model.street
            , placeholder = Just (translators.t (formTranslationString ++ ".street.placeholder"))
            , problems = fieldProblems Street model.problems
            , translators = translators
            }
            |> View.Form.Input.toHtml
        , View.Form.Input.init
            { id = "number"
            , label = translators.t (formTranslationString ++ ".number.label")
            , onInput = EnteredNumber
            , disabled = False
            , value = model.number
            , placeholder = Just (translators.t (formTranslationString ++ ".number.placeholder"))
            , problems = fieldProblems Number model.problems
            , translators = translators
            }
            |> View.Form.Input.toHtml
        , View.Form.Input.init
            { id = "zip"
            , label = translators.t (formTranslationString ++ ".zip.label")
            , onInput = EnteredZip
            , disabled = False
            , value = model.zip
            , placeholder = Just (translators.t (formTranslationString ++ ".zip.placeholder"))
            , problems = fieldProblems Zip model.problems
            , translators = translators
            }
            |> View.Form.Input.toHtml
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
            { model
                | account =
                    if String.length account > 12 then
                        model.account

                    else
                        account
            }

        EnteredState str ->
            { model
                | state = findId str model.country.states
                , cities = getCities model.country.states str
            }

        EnteredCity str ->
            { model
                | city = findId str model.cities
                , districts = getDistricts model.cities str
            }

        EnteredDistrict str ->
            { model | district = findId str model.districts }

        EnteredStreet str ->
            { model | street = str }

        EnteredZip str ->
            { model | zip = str }

        EnteredNumber str ->
            { model | number = str }


validator : Translators -> Validator ( Field, String ) Model
validator { t } =
    let
        ifInvalidPhoneNumber subjectToString error =
            Validate.ifFalse (\subject -> KycPhone.isValid (subjectToString subject)) error
    in
    Validate.all
        [ Validate.ifBlank .document ( Document, t "error.required" )
        , Validate.ifBlank .name ( Name, t "error.required" )
        , Validate.ifBlank .username ( Username, t "error.required" )
        , Validate.ifBlank .account ( Username, t "error.required" )
        , Validate.ifBlank .zip ( Username, t "error.required" )
        , Validate.ifBlank .number ( Username, t "error.required" )
        , ifEmptyTuple .state ( State, t "error.required" )
        , ifEmptyTuple .city ( State, t "error.required" )
        , ifEmptyTuple .district ( State, t "error.required" )
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
