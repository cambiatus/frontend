module Page.Register.DefaultForm exposing (Field(..), Model, Msg(..), init, update, validator, view)

import Html exposing (Html)
import Page.Register.Common exposing (containsNumberGreaterThan, fieldProblems)
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
        [ View.Form.Input.init
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
            { form
                | account =
                    if String.length account > 12 || (account |> containsNumberGreaterThan 5) then
                        form.account

                    else
                        account
            }

        EnteredEmail email ->
            { form | email = email }


validator : Translators -> Validator ( Field, String ) Model
validator { t } =
    Validate.all
        [ Validate.firstError
            [ Validate.ifBlank .name ( Name, t "error.required" )
            ]
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
        ]
