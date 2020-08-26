module Page.Register.NaturalForm exposing (Document(..), Model, Msg(..), init, update, view)

import Html exposing (Html)
import Page.Login exposing (Model)
import Page.Register.Common exposing (..)
import Session.Shared exposing (Translators)
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
    }


init : Model
init =
    { document = ""
    , documentType = SSN
    , name = ""
    , email = ""
    , phone = ""
    , username = ""
    , account = ""
    }


type Document
    = SSN
    | DIMEX
    | NITE



--- MSG


type Msg
    = EnteredDocument String
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
    in
    Html.form []
        [ viewTitleForStep translators 1
        , viewSelectField "Document Type"
            model.document
            EnteredDocumentType
            [ { value = "ssn", label = translators.t "register.form.document.ssn.label" }
            , { value = "dimex", label = translators.t "register.form.document.dimex.label" }
            , { value = "nite", label = translators.t "register.form.document.nite.label" }
            ]
            translators
        , documentInput translators EnteredDocument model.document "" (formTranslationString ++ ".document.")
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
