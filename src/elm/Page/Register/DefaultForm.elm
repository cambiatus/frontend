module Page.Register.DefaultForm exposing (Field(..), Model, Msg(..), init, update, validator, view)

import Html exposing (Html)
import Html.Attributes exposing (autocomplete, classList)
import Page.Register.Common exposing (ProblemEvent(..), fieldProblems, validateAccountName)
import Session.Shared exposing (Translators)
import Validate exposing (Validator)
import View.Form.Input



--- MODEL


type alias Model =
    { name : String
    , account : String
    , email : String
    , problems : List ( Field, String, ProblemEvent )
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


update : Translators -> Msg -> Model -> Model
update translators msg form =
    case msg of
        EnteredName name ->
            { form | name = name }

        EnteredAccount account ->
            let
                formProblemsWithoutAccount =
                    form.problems
                        |> List.filter (\( field, _, _ ) -> field /= Account)

                ( preparedAccountName, errorMsg ) =
                    validateAccountName translators account form.account
            in
            { form
                | account = preparedAccountName
                , problems =
                    case errorMsg of
                        Nothing ->
                            formProblemsWithoutAccount

                        Just e ->
                            formProblemsWithoutAccount ++ [ ( Account, e, OnInput ) ]
            }

        EnteredEmail email ->
            { form | email = email }


validator : Translators -> Validator ( Field, String, ProblemEvent ) Model
validator { t, tr } =
    Validate.all
        [ Validate.firstError
            [ Validate.ifBlank .name ( Name, t "error.required", OnSubmit )
            ]
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
        ]
