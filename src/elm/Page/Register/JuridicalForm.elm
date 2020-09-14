module Page.Register.JuridicalForm exposing (CompanyType(..), Field(..), Model, Msg(..), companyTypeToString, init, update, validator, view)

import Address
import Html exposing (Html)
import Kyc.CostaRica.Phone as KycPhone
import Page.Register.Common exposing (..)
import Session.Shared exposing (Translators)
import Validate exposing (Validator)
import View.Form.Input



--- MODEL


type alias Model =
    { companyType : CompanyType
    , document : String
    , name : String
    , email : String
    , phone : String
    , username : String
    , state : ( String, String )
    , city : ( String, String )
    , district : ( String, String )
    , account : String
    , street : String
    , zip : String
    , number : String
    , problems : List ( Field, String )
    , country : Address.Country
    , states : List Address.State
    , cities : List Address.City
    , districts : List Address.Neighborhood
    }


type Field
    = CompanyType
    | Document
    | Name
    | Email
    | Phone
    | Username
    | State
    | City
    | District
    | Account
    | Street
    | Zip
    | Number


init : Address.Country -> Model
init country =
    { companyType = MIPYME
    , document = ""
    , name = ""
    , email = ""
    , phone = ""
    , username = ""
    , state = ( "", "" )
    , city = ( "", "" )
    , district = ( "", "" )
    , account = ""
    , street = ""
    , zip = ""
    , number = ""
    , problems = []
    , country = country
    , states = country.states
    , cities = []
    , districts = []
    }


type CompanyType
    = MIPYME
    | Corporation


companyTypeToString : CompanyType -> String
companyTypeToString type_ =
    case type_ of
        MIPYME ->
            "mipyme"

        Corporation ->
            "corporation"



--- MSG


type Msg
    = EnteredDocument String
    | EnteredType String
    | EnteredName String
    | EnteredEmail String
    | EnteredPhone String
    | EnteredState String
    | EnteredCity String
    | EnteredDistrict String
    | EnteredAccount String
    | EnteredStreet String
    | EnteredZip String
    | EnteredNumber String



--- VIEW


view : Translators -> Model -> Html Msg
view translators model =
    let
        formTranslationString =
            "register.form"

        companyTranslationString =
            formTranslationString ++ ".company." ++ companyTypeToString model.companyType
    in
    Html.div []
        [ viewTitleForStep translators 1
        , viewSelectField (translators.t "register.form.company_type")
            (companyTypeToString model.companyType)
            EnteredType
            [ { value = "mipyme", label = translators.t "register.form.company.mipyme.label" }
            , { value = "corporation", label = translators.t "register.form.company.corporation.label" }
            ]
            translators
        , View.Form.Input.init
            { id = "document"
            , label = translators.t (companyTranslationString ++ ".label")
            , onInput = EnteredDocument
            , disabled = False
            , value = model.document
            , placeholder = Just (translators.t (companyTranslationString ++ ".placeholder"))
            , problems = fieldProblems Document model.problems
            , translators = translators
            }
            |> View.Form.Input.withCounter
                (companyTranslationString
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
update msg form =
    case msg of
        EnteredDocument document ->
            { form | document = document }

        EnteredType type_ ->
            { form
                | companyType =
                    case type_ of
                        "corporation" ->
                            Corporation

                        "mipyme" ->
                            MIPYME

                        _ ->
                            Corporation
            }

        EnteredName str ->
            { form | name = str }

        EnteredEmail str ->
            { form | email = str }

        EnteredPhone str ->
            { form | phone = str }

        EnteredState str ->
            { form
                | state = findId str form.country.states
                , cities = getCities form.country.states str
            }

        EnteredCity str ->
            { form
                | city = findId str form.cities
                , districts = getDistricts form.cities str
            }

        EnteredDistrict str ->
            { form | district = findId str form.districts }

        EnteredAccount str ->
            { form
                | account =
                    if String.length str > 12 then
                        form.account

                    else
                        str
            }

        EnteredStreet str ->
            { form | street = str }

        EnteredZip str ->
            { form | zip = str }

        EnteredNumber str ->
            { form | number = str }


validator : Translators -> Validator ( Field, String ) Model
validator { t } =
    let
        ifInvalidPhoneNumber subjectToString error =
            Validate.ifFalse (\subject -> KycPhone.isValid (subjectToString subject)) error
    in
    Validate.all
        [ Validate.ifBlank .name ( Name, t "error.required" )
        , Validate.ifBlank .document ( Document, t "error.required" )
        , Validate.ifBlank .username ( Document, t "error.required" )
        , Validate.ifBlank .account ( Document, t "error.required" )
        , Validate.ifBlank .zip ( Document, t "error.required" )
        , Validate.ifBlank .number ( Document, t "error.required" )
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
