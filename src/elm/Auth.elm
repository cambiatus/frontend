module Auth exposing
    ( ExternalMsg(..)
    , Model
    , Msg
    , PrivateKeyLogin
    , init
    , initRegister
    , isAuth
    , jsAddressToMsg
    , maybePrivateKey
    , msgToString
    , subscriptions
    , update
    , view
    , viewFieldLabel
    )

import Api
import Asset.Icon as Icon
import Browser.Dom as Dom
import Browser.Events
import Eos.Account as Eos
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import I18Next exposing (t)
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import List.Extra as LE
import Log
import Profile exposing (Profile)
import Regex
import Route
import Session.Shared as Shared exposing (Shared)
import Task
import UpdateResult as UR
import Utils
import Validate exposing (Validator, fromValid, ifNotInt, validate)



-- INIT


init : Shared -> Model
init shared =
    case shared.maybeAccount of
        Just ( _, True ) ->
            { initModel | status = LoginWithPin }

        _ ->
            initModel


initRegister : String -> Model
initRegister pk =
    { initModel | status = LoggedInWithPin pk }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map PressedEnter (Browser.Events.onKeyDown Utils.decodeEnterKeyDown)



-- MODEL


type alias Model =
    { status : Status
    , loginError : Maybe String
    , form : PrivateKeyLogin
    , pinVisibility : Bool
    , problems : List Problem
    }


initModel : Model
initModel =
    { status = Options LoginStepPassphrase
    , loginError = Nothing
    , form = initPrivateKeyLogin
    , pinVisibility = True
    , problems = []
    }


type Status
    = Options LoginStep
    | LoginWithPrivateKey PrivateKeyLogin
    | LoginWithPrivateKeyAccounts (List Eos.Name) PrivateKeyLogin
    | LoggingInWithPrivateKeyAccounts (List Eos.Name) PrivateKeyLogin
    | LoggedInWithPrivateKey PrivateKey
    | LoginWithPin
    | LoggingInWithPin
    | LoggedInWithPin PrivateKey


type alias PrivateKeyLogin =
    { privateKey : String
    , usePin : Maybe String
    , enteredPin : String
    , enteredPinConf : String
    }


initPrivateKeyLogin : PrivateKeyLogin
initPrivateKeyLogin =
    { privateKey = ""
    , usePin = Nothing
    , enteredPin = ""
    , enteredPinConf = ""
    }


type PinField
    = PinInput
    | PinConfInput


{-| Validator for checking entered 12 words.
-}
passphraseValidator : Validator Problem PrivateKeyLogin
passphraseValidator =
    Validate.all
        [ Validate.firstError
            [ Validate.ifBlank .privateKey (InvalidEntry Passphrase "error.required")
            , Validate.fromErrors
                (\form ->
                    let
                        oneOrMoreSpaces =
                            Maybe.withDefault Regex.never <|
                                Regex.fromString "\\s+"

                        letter =
                            Maybe.withDefault Regex.never <|
                                Regex.fromString "[a-zA-Z]"

                        passphraseAsList =
                            form.privateKey
                                |> String.trim
                                |> Regex.split oneOrMoreSpaces

                        passphraseHasTwelveWords =
                            List.length passphraseAsList == 12

                        passphraseHasOnlyLetters =
                            let
                                onlyLetters s =
                                    List.length (Regex.find letter s) == String.length s
                            in
                            List.all onlyLetters passphraseAsList

                        passphraseHasWordsWithAtLeastTwoLetters =
                            List.all (\w -> String.length w > 2) passphraseAsList
                    in
                    if not passphraseHasTwelveWords then
                        [ InvalidEntry Passphrase "Please, enter 12 words (TR)" ]

                    else if not passphraseHasOnlyLetters then
                        [ InvalidEntry Passphrase "Please, use only letters (TR)" ]

                    else if not passphraseHasWordsWithAtLeastTwoLetters then
                        [ InvalidEntry Passphrase "All words should have at least 3 letters (TR)" ]

                    else
                        []
                )
            ]
        ]


{-| Validates PIN field and related PIN confirmation field.
-}
pinValidator : Validator ( ValidatedField, String ) PrivateKeyLogin
pinValidator =
    Validate.all
        [ Validate.firstError
            [ Validate.ifBlank .enteredPin ( Pin, "Pin can't be blank." )
            , Validate.ifBlank .enteredPinConf ( PinConfirmation, "Conf Pin can't be blank." )
            , Validate.fromErrors
                (\form ->
                    if form.enteredPin == form.enteredPinConf then
                        []

                    else
                        [ ( PinConfirmation, "PIN must match." ) ]
                )
            ]
        ]


viewFieldErrors : ValidatedField -> List ( ValidatedField, String ) -> Html msg
viewFieldErrors field errors =
    let
        isFieldType error =
            field == Tuple.first error

        fieldErrors =
            errors
                |> List.filter isFieldType
                |> List.map Tuple.second
    in
    b [ class "text-red" ]
        [ text (String.join "; " fieldErrors)
        ]


encodePrivateKeyLogin : PrivateKeyLogin -> Value
encodePrivateKeyLogin pk =
    Encode.object
        [ ( "privateKey", Encode.string pk.privateKey )
        , ( "usePin"
          , case pk.usePin of
                Nothing ->
                    Encode.null

                Just pin ->
                    Encode.string pin
          )
        ]


type alias PrivateKey =
    String


isAuth : Model -> Bool
isAuth model =
    case model.status of
        LoggedInWithPin _ ->
            True

        LoggedInWithPrivateKey _ ->
            True

        _ ->
            False


maybePrivateKey : Model -> Maybe String
maybePrivateKey model =
    case model.status of
        LoggedInWithPin pk ->
            Just pk

        LoggedInWithPrivateKey pk ->
            Just pk

        _ ->
            Nothing



-- VIEW


view : Bool -> Shared -> Model -> List (Html Msg)
view isModal shared model =
    case model.status of
        Options loginStep ->
            viewLoginSteps isModal shared model loginStep

        LoginWithPrivateKey _ ->
            viewLoginSteps isModal shared model LoginStepPassphrase

        LoginWithPrivateKeyAccounts accounts form ->
            viewMultipleAccount accounts form False isModal shared model

        LoggingInWithPrivateKeyAccounts accounts form ->
            viewMultipleAccount accounts form True isModal shared model

        LoggedInWithPrivateKey _ ->
            viewLoginSteps isModal shared model LoginStepPassphrase

        LoginWithPin ->
            case shared.maybeAccount of
                Just ( accountName, True ) ->
                    viewLoginWithPin accountName False isModal shared model

                _ ->
                    viewLoginSteps isModal shared model LoginStepPassphrase

        LoggingInWithPin ->
            case shared.maybeAccount of
                Just ( accountName, True ) ->
                    viewLoginWithPin accountName True isModal shared model

                _ ->
                    viewLoginSteps isModal shared model LoginStepPassphrase

        LoggedInWithPin _ ->
            case shared.maybeAccount of
                Just ( accountName, True ) ->
                    viewLoginWithPin accountName True isModal shared model

                _ ->
                    viewLoginSteps isModal shared model LoginStepPassphrase


type LoginStep
    = LoginStepPassphrase
    | LoginStepPIN


viewLoginSteps : Bool -> Shared -> Model -> LoginStep -> List (Html Msg)
viewLoginSteps isModal shared model loginStep =
    let
        text_ s =
            Html.text (t shared.translations s)

        errors =
            case loginStep of
                LoginStepPassphrase ->
                    List.map (\err -> viewFieldProblem shared Passphrase err) model.problems

                LoginStepPIN ->
                    List.map (\err -> viewFieldProblem shared Pin err) model.problems

        viewLoginPassphrase =
            div [ class "temp-passphrase-step" ]
                [ div [ class "card__auth__input" ]
                    [ img [ src "images/login_key.svg" ] []
                    , viewFieldLabel shared "auth.login.wordsMode.input" "privateKey" Nothing
                    , textarea
                        [ class "input"
                        , id "privateKey"
                        , value model.form.privateKey
                        , onInput EnteredPassphrase
                        , required True
                        , autocomplete False
                        ]
                        []
                    , ul [] errors
                    ]
                , if not isModal then
                    a [ Route.href (Route.Register Nothing Nothing), class "card__auth__prompt" ]
                        [ span [] [ text_ "auth.login.register" ]
                        , span [ class "card__auth__login__mode" ] [ text_ "auth.login.registerLink" ]
                        ]

                  else
                    text ""
                , button
                    [ class "btn btn--primary btn--login"
                    , onClick ClickedViewLoginPinStep
                    ]
                    [ text_ "Next" ]
                ]

        viewLoginPin =
            div [ class "temp-pin-step" ]
                [ div [ onClick ClickedViewOptions ] [ text "← Back to 12 words" ]
                , div [ class "text-center" ]
                    [ img
                        [ class "inline"
                        , src "images/login_pin.svg"
                        ]
                        []
                    ]
                , viewPinForm model shared PinInput
                , viewPinForm model shared PinConfInput
                , button
                    [ class "btn btn--primary btn--login"
                    , onClick (SubmittedLoginPrivateKey model.form)
                    ]
                    [ text_ "auth.login.submit" ]
                ]
    in
    [ div [ class "" ]
        [ viewAuthError shared model.loginError
        ]
    , case loginStep of
        LoginStepPassphrase ->
            viewLoginPassphrase

        LoginStepPIN ->
            viewLoginPin
    ]


viewLoginWithPrivateKeyLogin : PrivateKeyLogin -> Bool -> Bool -> Shared -> Model -> List (Html Msg)
viewLoginWithPrivateKeyLogin form isDisabled isModal shared model =
    let
        text_ s =
            Html.text (t shared.translations s)
    in
    [ div [ class "card__login-header" ]
        [ h2 [ class "card__title" ]
            [ text_ "auth.loginPrivatekey" ]
        , viewAuthError shared model.loginError
        , button
            [ class "card__close-btn"
            , onClick ClickedViewOptions
            , type_ "button"
            , disabled isDisabled
            , title (t shared.translations "menu.cancel")
            ]
            [ Icon.close "" ]
        ]
    , Html.form
        [ onSubmit (SubmittedLoginPrivateKey form) ]
        [ div [ class "input-group" ]
            [ input
                [ class "input input--login flex100"
                , type_ "text"
                , value form.privateKey
                , onInput EnteredPassphrase
                , placeholder (t shared.translations "auth.loginPrivatekeyPlaceholder")
                , required True
                , disabled isDisabled
                ]
                []
            , button
                [ class "btn btn--primary btn--login flex000"
                , disabled isDisabled
                ]
                [ text_ "auth.login" ]
            ]
        ]
    ]


viewMultipleAccount : List Eos.Name -> PrivateKeyLogin -> Bool -> Bool -> Shared -> Model -> List (Html Msg)
viewMultipleAccount accounts form isDisabled isModal shared model =
    let
        text_ s =
            Html.text (t shared.translations s)

        btnClass =
            class "btn btn--outline btn--login"
    in
    div [ class "card__login-header" ]
        [ h2 [ class "card__title" ]
            [ text_ "auth.chooseAccount" ]
        , viewAuthError shared model.loginError
        , button
            [ class "card__close-btn"
            , onClick ClickedViewOptions
            , type_ "button"
            , disabled isDisabled
            , title (t shared.translations "menu.cancel")
            ]
            [ Icon.close "" ]
        ]
        :: List.map
            (\a ->
                button
                    [ btnClass
                    , disabled isDisabled
                    , onClick (ClickedPrivateKeyAccount a form)
                    ]
                    [ text (Eos.nameToString a) ]
            )
            accounts


viewLoginWithPin : Eos.Name -> Bool -> Bool -> Shared -> Model -> List (Html Msg)
viewLoginWithPin accountName isDisabled isModal shared model =
    let
        text_ s =
            Html.text (t shared.translations s)

        tr id_ replaces =
            I18Next.tr shared.translations I18Next.Curly id_ replaces
    in
    [ div [ class "card__login-header" ]
        [ p [ class "card__pin__prompt" ]
            [ text (tr "auth.loginPin" [ ( "accountName", Eos.nameToString accountName ) ]) ]
        , viewAuthError shared model.loginError
        ]
    , Html.form
        [ class "card__pin__input__group"
        , onSubmit SubmittedLoginPIN
        ]
        [ viewPinForm model shared PinInput
        , button
            [ class "btn btn--primary btn--login flex000"
            , disabled isDisabled
            ]
            [ text_ "auth.login.submit" ]
        ]
    ]


viewAuthTabs : Shared -> Html msg
viewAuthTabs { translations } =
    let
        text_ : String -> Html msg
        text_ s =
            text (t translations s)
    in
    div [ class "card__auth__tabs__login" ]
        [ div [ class "disabled" ]
            [ a [ Route.href (Route.Register Nothing Nothing) ]
                [ p [] [ text_ "auth.login.registerTab" ] ]
            ]
        , div [ class "enabled" ] [ p [] [ text_ "auth.login.loginTab" ] ]
        ]


viewAuthError : Shared -> Maybe String -> Html Msg
viewAuthError shared maybeLoginError =
    case maybeLoginError of
        Nothing ->
            text ""

        Just error ->
            let
                tr =
                    t shared.translations "error.accountNotFound"
            in
            div [ class "bg-red border-lg rounded p-4 mt-2" ]
                [ p [ class "text-white" ] [ text error ]
                ]


toggleViewPin : Model -> Html Msg
toggleViewPin model =
    button [ class "", onClick TogglePinVisibility ]
        [ if model.pinVisibility then
            img [ src "/icons/eye-show.svg" ] []

          else
            img [ src "/icons/eye-close.svg" ] []
        ]


viewFieldLabel : Shared -> String -> String -> Maybe (Html msg) -> Html msg
viewFieldLabel { translations } tSuffix id_ viewToggleHiddenSymbols =
    let
        labelText : String
        labelText =
            t translations (tSuffix ++ ".label")
    in
    label [ for id_ ]
        [ span [ class "text-white text-body" ] [ Html.text labelText ]
        , Maybe.withDefault (text "") viewToggleHiddenSymbols
        ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg ExternalMsg


type Msg
    = Ignored
    | ClickedViewOptions
    | ClickedViewLoginPinStep
    | EnteredPassphrase String
    | SubmittedLoginPrivateKey PrivateKeyLogin
    | GotMultipleAccountsLogin (List Eos.Name)
    | ClickedPrivateKeyAccount Eos.Name PrivateKeyLogin
    | GotPrivateKeyLogin (Result String ( Eos.Name, String ))
    | SubmittedLoginPIN
    | GotPinLogin (Result String ( Eos.Name, String ))
    | CompletedLoadProfile Status Eos.Name (Result Http.Error Profile)
    | CompletedCreateProfile Status Eos.Name (Result Http.Error Profile)
    | TogglePinVisibility
    | PressedEnter Bool
    | EnteredPinDigit String
    | EnteredPin String
    | EnteredPinConf String


type ExternalMsg
    = ClickedCancel
    | CompletedAuth Profile
    | UpdatedShared Shared


type ValidationError
    = PinTooShort
    | PinTooLong
    | PinInvalidChars
    | PinConfirmDontMatch


validationErrorToString : Shared -> ValidationError -> String
validationErrorToString shared error =
    let
        t s =
            I18Next.t shared.translations s

        tr str values =
            I18Next.tr shared.translations I18Next.Curly str values
    in
    case error of
        PinTooShort ->
            tr "error.tooShort" [ ( "minLength", "6" ) ]

        PinTooLong ->
            tr "error.tooLong" [ ( "maxLength", "6" ) ]

        PinInvalidChars ->
            t "register.form.pinCharError"

        PinConfirmDontMatch ->
            t "register.form.pinConfirmError"


update : Msg -> Shared -> Model -> Bool -> UpdateResult
update msg shared model showAuthModal =
    case msg of
        EnteredPin data ->
            let
                _ =
                    Debug.log "EnterdPin data" data

                otherProblems =
                    model.problems
                        |> List.filter
                            (\p ->
                                case p of
                                    InvalidEntry Pin _ ->
                                        False

                                    _ ->
                                        True
                            )

                pinErrors =
                    if data == "" then
                        [ InvalidEntry Pin (validationErrorToString shared PinTooShort) ]

                    else
                        []

                currentForm =
                    model.form
            in
            { model
                | form = { currentForm | enteredPin = data }
                , problems = otherProblems ++ pinErrors
            }
                |> UR.init

        EnteredPinConf data ->
            let
                otherProblems =
                    model.problems
                        |> List.filter
                            (\p ->
                                case p of
                                    InvalidEntry PinConfirmation _ ->
                                        False

                                    _ ->
                                        True
                            )

                pinConfProbs =
                    if data == "" then
                        [ InvalidEntry PinConfirmation (validationErrorToString shared PinTooShort) ]

                    else
                        []

                currentForm =
                    model.form
            in
            { model
                | form = { currentForm | enteredPinConf = data }
                , problems = pinConfProbs ++ otherProblems
            }
                |> UR.init

        Ignored ->
            UR.init model

        ClickedViewOptions ->
            UR.init
                { model
                    | loginError = Nothing
                    , status = Options LoginStepPassphrase
                }

        ClickedViewLoginPinStep ->
            case validate passphraseValidator model.form of
                Ok _ ->
                    { model
                        | loginError = Nothing
                        , problems = []
                        , status = Options LoginStepPIN
                    }
                        |> UR.init

                Err errors ->
                    { model | problems = errors }
                        |> UR.init

        EnteredPassphrase phrase ->
            let
                currentForm =
                    model.form

                newForm =
                    { currentForm
                        | privateKey = phrase
                    }
            in
            { model
                | form = newForm
                , problems = []
            }
                |> UR.init

        SubmittedLoginPrivateKey form ->
            case form.enteredPin of
                "" ->
                    { model | loginError = Just "Please fill in all the PIN digits" }
                        |> UR.init

                _ ->
                    let
                        pinString =
                            form.enteredPin

                        newForm =
                            { form | usePin = Just pinString }
                    in
                    { model | form = newForm }
                        |> UR.init
                        |> UR.addPort
                            { responseAddress = SubmittedLoginPrivateKey form
                            , responseData = Encode.null
                            , data =
                                Encode.object
                                    [ ( "name", Encode.string "loginWithPrivateKey" )
                                    , ( "form", encodePrivateKeyLogin newForm )
                                    ]
                            }

        GotMultipleAccountsLogin accounts ->
            UR.init
                { model
                    | status =
                        case model.status of
                            _ ->
                                model.status
                }

        ClickedPrivateKeyAccount accountName form ->
            UR.init
                { model
                    | status =
                        case model.status of
                            LoginWithPrivateKeyAccounts accounts frm ->
                                LoggingInWithPrivateKeyAccounts accounts frm

                            _ ->
                                model.status
                }
                |> UR.addPort
                    { responseAddress = ClickedPrivateKeyAccount accountName form
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "loginWithPrivateKeyAccount" )
                            , ( "accountName", Eos.encodeName accountName )
                            , ( "form", encodePrivateKeyLogin form )
                            ]
                    }

        GotPrivateKeyLogin (Ok ( accountName, privateKey )) ->
            UR.init model
                |> UR.addCmd (Api.signIn shared accountName (CompletedLoadProfile (LoggedInWithPrivateKey privateKey) accountName))

        GotPrivateKeyLogin (Err err) ->
            UR.init
                { model
                    | loginError = Just err
                    , status =
                        case model.status of
                            LoggingInWithPrivateKeyAccounts accounts form ->
                                LoginWithPrivateKeyAccounts accounts form

                            _ ->
                                model.status
                }

        SubmittedLoginPIN ->
            case model.form.enteredPin of
                "" ->
                    { model | loginError = Just "Please fill in all the PIN digits" }
                        |> UR.init

                _ ->
                    let
                        pinString =
                            model.form.enteredPin
                    in
                    UR.init { model | status = LoggingInWithPin }
                        |> UR.addPort
                            { responseAddress = SubmittedLoginPIN
                            , responseData = Encode.null
                            , data =
                                Encode.object
                                    [ ( "name", Encode.string "loginWithPin" )
                                    , ( "pin", Encode.string pinString )
                                    ]
                            }

        GotPinLogin (Ok ( accountName, privateKey )) ->
            UR.init model
                |> UR.addCmd (Api.signIn shared accountName (CompletedLoadProfile (LoggedInWithPin privateKey) accountName))

        GotPinLogin (Err err) ->
            UR.init
                { model
                    | loginError = Just err
                    , status =
                        case model.status of
                            LoggingInWithPin ->
                                LoginWithPin

                            _ ->
                                model.status
                }

        CompletedLoadProfile newStatus accountName (Ok profile) ->
            UR.init { model | status = newStatus }
                |> UR.addExt (CompletedAuth profile)
                |> UR.addPort
                    { responseAddress = CompletedLoadProfile newStatus accountName (Ok profile)
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "chatCredentials" )
                            , ( "container", Encode.string "chat-manager" )
                            , ( "credentials", Profile.encodeProfileChat profile )
                            , ( "notificationAddress"
                              , Encode.list Encode.string [ "GotPageMsg", "GotLoggedInMsg", "ReceivedNotification" ]
                              )
                            ]
                    }

        CompletedLoadProfile newStatus accountName (Err err) ->
            case err of
                Http.BadStatus 404 ->
                    UR.init model
                        |> UR.addCmd
                            (Api.signUp shared
                                { name = ""
                                , email = ""
                                , account = accountName
                                , invitationId = Nothing
                                }
                                (CompletedCreateProfile newStatus accountName)
                            )

                _ ->
                    loginFailed err model

        CompletedCreateProfile newStatus accountName (Ok _) ->
            UR.init model
                |> UR.addCmd (Api.signIn shared accountName (CompletedLoadProfile newStatus accountName))

        CompletedCreateProfile _ _ (Err err) ->
            loginFailed err model

        EnteredPinDigit data ->
            let
                currentForm =
                    model.form

                newPin =
                    model.form.enteredPin
            in
            { model | form = { currentForm | enteredPin = newPin } }
                |> UR.init
                |> UR.addPort
                    { responseAddress = Ignored
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "pos", Encode.int -999 )
                            , ( "data", Encode.string data )
                            , ( "iswithinif", Encode.bool True )
                            ]
                    }

        TogglePinVisibility ->
            { model | pinVisibility = not model.pinVisibility } |> UR.init

        PressedEnter val ->
            if val && showAuthModal then
                UR.init model
                    |> UR.addCmd
                        (Task.succeed (SubmittedLoginPrivateKey model.form)
                            |> Task.perform identity
                        )

            else
                UR.init model


loginFailed : Http.Error -> Model -> UpdateResult
loginFailed httpError model =
    UR.init
        { model
            | loginError =
                case httpError of
                    Http.BadStatus code ->
                        Just (String.fromInt code)

                    _ ->
                        Just "Auth failed"
            , status =
                case model.status of
                    LoggingInWithPrivateKeyAccounts accounts form ->
                        LoginWithPrivateKeyAccounts accounts form

                    LoggingInWithPin ->
                        LoginWithPin

                    _ ->
                        Options LoginStepPassphrase
        }
        |> UR.addCmd (Log.httpError httpError)
        |> UR.addPort
            { responseAddress = Ignored
            , responseData = Encode.null
            , data =
                Encode.object
                    [ ( "name", Encode.string "logout" )
                    , ( "container", Encode.string "chat-manager" )
                    ]
            }


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "SubmittedLoginPrivateKey" :: [] ->
            decodeAccountNameOrStringError GotPrivateKeyLogin val

        "ClickedPrivateKeyAccount" :: [] ->
            decodeAccountNameOrStringError GotPrivateKeyLogin val

        "SubmittedLoginPIN" :: [] ->
            decodeAccountNameOrStringError GotPinLogin val

        _ ->
            Nothing


decodeAccountNameOrStringError : (Result String ( Eos.Name, String ) -> Msg) -> Value -> Maybe Msg
decodeAccountNameOrStringError toMsg value =
    Decode.decodeValue
        (Decode.oneOf
            [ Decode.succeed Tuple.pair
                |> Decode.required "accountName" Eos.nameDecoder
                |> Decode.required "privateKey" Decode.string
                |> Decode.map (Ok >> toMsg)
            , Decode.field "accountNames" (Decode.list Eos.nameDecoder)
                |> Decode.map GotMultipleAccountsLogin
            , Decode.field "error" Decode.string
                |> Decode.map (Err >> toMsg)
            ]
        )
        value
        |> Result.toMaybe


msgToString : Msg -> List String
msgToString msg =
    case msg of
        Ignored ->
            [ "Ignored" ]

        ClickedViewOptions ->
            [ "ClickedViewOptions" ]

        ClickedViewLoginPinStep ->
            [ "ClickedViewLoginPinStep" ]

        EnteredPassphrase _ ->
            [ "EnteredPrivateKey" ]

        SubmittedLoginPrivateKey _ ->
            [ "SubmittedLoginPrivateKey" ]

        GotMultipleAccountsLogin _ ->
            [ "GotMultipleAccountsLogin" ]

        ClickedPrivateKeyAccount _ _ ->
            [ "ClickedPrivateKeyAccount" ]

        GotPrivateKeyLogin r ->
            [ "GotPrivateKeyLogin", UR.resultToString r ]

        SubmittedLoginPIN ->
            [ "SubmittedLoginPIN" ]

        GotPinLogin r ->
            [ "GotPinLogin", UR.resultToString r ]

        CompletedLoadProfile _ _ r ->
            [ "CompletedLoadProfile", UR.resultToString r ]

        CompletedCreateProfile _ _ r ->
            [ "CompletedCreateProfile", UR.resultToString r ]

        EnteredPinDigit _ ->
            [ "EnteredPinDigit" ]

        EnteredPin _ ->
            [ "EnteredPin" ]

        EnteredPinConf _ ->
            [ "EnteredPinConf" ]

        TogglePinVisibility ->
            [ "TogglePinVisibility" ]

        PressedEnter _ ->
            [ "PressedEnter" ]


{-| Call this function under the field to render related validation problems.
-}
viewFieldProblem : Shared -> ValidatedField -> Problem -> Html msg
viewFieldProblem shared field problem =
    let
        t s =
            I18Next.t shared.translations s
    in
    case problem of
        ServerError _ ->
            text ""

        InvalidEntry f str ->
            if f == field then
                li [ class "field__error" ] [ text (t str) ]

            else
                text ""


viewPinForm : Model -> Shared -> PinField -> Html Msg
viewPinForm model shared inputType =
    let
        pinPrompt =
            case inputType of
                PinInput ->
                    "auth.pin"

                PinConfInput ->
                    "auth.pinConfirmation"

        errors =
            case inputType of
                PinInput ->
                    List.map (\err -> viewFieldProblem shared Pin err) model.problems

                PinConfInput ->
                    List.map (\err -> viewFieldProblem shared PinConfirmation err) model.problems

        val =
            case inputType of
                PinInput ->
                    model.form.enteredPin

                PinConfInput ->
                    model.form.enteredPinConf

        msg =
            case inputType of
                PinInput ->
                    EnteredPin

                PinConfInput ->
                    EnteredPinConf
    in
    div [ class "card__auth__pin__section" ]
        [ viewFieldLabel shared pinPrompt "pin_input_0" Nothing
        , div []
            [ input
                [ class ""
                , maxlength 6
                , value val
                , onInput msg
                , required True
                , autocomplete False
                , attribute "inputmode" "numeric"
                ]
                []
            ]
        , ul [] errors
        ]


type ValidatedField
    = Passphrase
    | Pin
    | PinConfirmation


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String
