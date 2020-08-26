module Page.Register.JuridicalForm exposing (CompanyType(..), Model, Msg(..), init, update, view)

import Html exposing (Html)
import Page.Register.Common exposing (..)
import Session.Shared exposing (Translators)
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
    }


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
    }


type CompanyType
    = MIPYME
    | Corporation



--- MSG


type Msg
    = EnteredDocument String
    | EnteredType String
    | EnteredName String
    | EnteredEmail String
    | EnteredPhone String
    | EnteredUsername String
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
    in
    Html.form []
        [ viewTitleForStep translators 1
        , viewSelectField (translators.t "register.form.company_type")
            model.document
            EnteredType
            [ { value = "mipyme", label = translators.t "register.form.company.mipyme.label" }
            , { value = "corporation", label = translators.t "register.form.company.corporation.label" }
            ]
            translators
        , documentInput translators EnteredDocument model.document "corporation" (formTranslationString ++ ".company.")
        , View.Form.Input.init
            { id = "name"
            , label = "Company Name"
            , disabled = False
            , onInput = EnteredName
            , placeholder = Just "Ex.: Cambiatus"
            , problems = Nothing
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
            , problems = Nothing
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
            , problems = Nothing
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
            , problems = Nothing
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

        EnteredUsername str ->
            { form | username = str }

        EnteredState str ->
            { form | state = str }

        EnteredCity str ->
            { form | city = str }

        EnteredDistrict str ->
            { form | district = str }

        EnteredAccount str ->
            { form | account = str }
