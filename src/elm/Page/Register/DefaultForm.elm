module Page.Register.DefaultForm exposing (Field(..), Model, Msg(..), init, update, validator, view)

import Html exposing (Html)
import Page.Register.Common exposing (containsNumberGreaterThan, fieldProblems)
import Regex
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
            let
                ( updatedAccountName, updatedFormProblems ) =
                    validateAccountName account form
            in
            { form
                | account = updatedAccountName
                , problems = updatedFormProblems
            }

        EnteredEmail email ->
            { form | email = email }


validateAccountName : String -> Model -> ( String, List ( Field, String ) )
validateAccountName enteredAccountName { account, problems } =
    let
        formProblemsWithoutAccount =
            problems
                |> List.filter (\( field, _ ) -> field /= Account)

        preparedAccountName =
            enteredAccountName
                |> String.trim
                |> String.toLower
                |> String.left 12

        validAccountName : Regex.Regex
        validAccountName =
            Maybe.withDefault Regex.never <|
                Regex.fromString "^[a-z1-5]{0,12}$"

        isAccountNameValid : Bool
        isAccountNameValid =
            preparedAccountName
                |> Regex.contains validAccountName
    in
    if isAccountNameValid then
        ( preparedAccountName, formProblemsWithoutAccount )

    else
        ( account
        , formProblemsWithoutAccount
            ++ [ ( Account, "'" ++ String.right 1 enteredAccountName ++ "error.invalidChar" ) ]
        )


validator : Translators -> Validator ( Field, String ) Model
validator { t, tr } =
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
            , Validate.ifTrue (\f -> String.length f.account < 12) ( Account, tr "error.validator.text.exactly" [ ( "base", "12" ) ] )
            , Validate.ifTrue (\f -> String.length f.account > 12) ( Account, tr "error.validator.text.exactly" [ ( "base", "12" ) ] )
            , Validate.ifFalse (\f -> String.all Char.isAlphaNum f.account) ( Account, t "error.invalidChar" )
            ]
        ]
