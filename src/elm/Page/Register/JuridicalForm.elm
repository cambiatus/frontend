module Page.Register.JuridicalForm exposing (CompanyType(..), Model, Msg(..), init, update, validator, view)

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
    , state : String
    , city : String
    , district : String
    , account : String
    , problems : List ( Field, String )
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


init : Model
init =
    { companyType = MIPYME
    , document = ""
    , name = ""
    , email = ""
    , phone = ""
    , username = ""
    , state = ""
    , city = ""
    , district = ""
    , account = ""
    , problems = []
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
            [ { value = "mipyme", label = translators.t "register.form.company.mipyme.label" }
            , { value = "corporation", label = translators.t "register.form.company.corporation.label" }
            ]
            translators
        , viewSelectField (translators.t "register.form.city")
            model.document
            EnteredCity
            [ { value = "mipyme", label = translators.t "register.form.company.mipyme.label" }
            , { value = "corporation", label = translators.t "register.form.company.corporation.label" }
            ]
            translators
        , viewSelectField (translators.t "register.form.district")
            model.document
            EnteredDistrict
            [ { value = "mipyme", label = translators.t "register.form.company.mipyme.label" }
            , { value = "corporation", label = translators.t "register.form.company.corporation.label" }
            ]
            translators
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
            { form | state = str }

        EnteredCity str ->
            { form | city = str }

        EnteredDistrict str ->
            { form | district = str }

        EnteredAccount str ->
            { form | account = str }


validator : Validator ( Field, String ) Model
validator =
    let
        ifInvalidPhoneNumber subjectToString error =
            Validate.ifFalse (\subject -> KycPhone.isValid (subjectToString subject)) error
    in
    Validate.all
        [ Validate.firstError
            [ Validate.ifBlank .name ( Name, "required" )
            ]
        , Validate.firstError
            [ Validate.ifBlank .document ( Document, "required" )
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
