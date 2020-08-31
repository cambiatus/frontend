module Page.Register.DefaultForm exposing (Model, Msg(..), init, update, validator, view)

import Html exposing (Html)
import Page.Register.Common exposing (fieldProblems, viewTitleForStep)
import Session.Shared exposing (Translators)
import Validate exposing (Validator)
import View.Form.Input



--- MODEL


type alias Model =
    { name : String
    , account : String
    , email : String
    , problems : List ( Field, String )
    }


type Field
    = Name
    | Account
    | Email


init : Model
init =
    { name = ""
    , account = ""
    , email = ""
    , problems = []
    }



--- MSG


type Msg
    = EnteredName String
    | EnteredAccount String
    | EnteredEmail String


view : Translators -> Model -> Html Msg
view translators model =
    let
        formTranslationString =
            "register.form"
    in
    Html.div []
        [ viewTitleForStep translators 1
        , View.Form.Input.init
            { id = "name"
            , label = translators.t (formTranslationString ++ ".name.label")
            , onInput = EnteredName
            , disabled = False
            , value = model.name
            , placeholder = Just (translators.t (formTranslationString ++ ".name.placeholder"))
            , problems = fieldProblems Name model.problems
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
        ]



--- UPDATE


update : Msg -> Model -> Model
update msg form =
    case msg of
        EnteredName name ->
            { form | name = name }

        EnteredAccount account ->
            { form | account = account }

        EnteredEmail email ->
            { form | email = email }


validator : Validator ( Field, String ) Model
validator =
    Validate.all
        [ Validate.firstError
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
        ]
