module Auth exposing
    ( ExternalMsg(..)
    , LoginFormData
    , Model
    , Msg
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
import Html exposing (Html, a, button, div, h2, img, input, label, li, p, span, strong, text, textarea, ul)
import Html.Attributes exposing (attribute, autocomplete, class, disabled, for, id, maxlength, pattern, placeholder, required, src, title, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import I18Next exposing (t)
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import List.Extra as LE
import Log
import Profile exposing (Profile)
import Route
import Session.Shared exposing (Shared)
import Task
import UpdateResult as UR
import Utils
import Validate exposing (Validator, validate)


{-| Authentication of a user with Passphrase or Private Key.

  - Passphrase consists of 12 unique words given to the user during the registration. Only users knows this phrase.
  - Private Key (PK) is a hashed analogue of a Passphrase.
  - PIN is used to encrypt the Passphrase/PK in the browser. Each time the user logs-in the new PIN is created.

User may use Passphrase and PK interchangeable for logging in, although we push the user forward to use the Passphrase
because it's more convenient for humans.

-}



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
    Sub.map KeyPressed (Browser.Events.onKeyDown Utils.decodeEnterKeyDown)



-- MODEL


type alias Model =
    { status : Status
    , loginError : Maybe String
    , form : LoginFormData
    , pinVisibility : Bool
    , pinConfirmationVisibility : Bool
    , problems : List Problem
    }


initModel : Model
initModel =
    { status = Options LoginStepPassphrase
    , loginError = Nothing
    , form = initLoginFormData
    , pinVisibility = True
    , pinConfirmationVisibility = True
    , problems = []
    }


type Status
    = Options LoginStep
    | LoginWithPrivateKeyAccounts (List Eos.Name) LoginFormData
    | LoggingInWithPrivateKeyAccounts (List Eos.Name) LoginFormData
    | LoggedInWithPrivateKey PrivateKey
    | LoginWithPin
    | LoggingInWithPin
    | LoggedInWithPin PrivateKey


type alias LoginFormData =
    { passphrase : String
    , usePin : Maybe String
    , enteredPin : String
    , enteredPinConfirmation : String
    }


initLoginFormData : LoginFormData
initLoginFormData =
    { passphrase = ""
    , usePin = Nothing
    , enteredPin = ""
    , enteredPinConfirmation = ""
    }


type PinField
    = PinInput
    | PinConfirmationInput


passphraseValidator : Validator Problem LoginFormData
passphraseValidator =
    Validate.fromErrors
        (\form ->
            let
                words : List String
                words =
                    String.words form.passphrase

                has12words : Bool
                has12words =
                    List.length words == 12

                allWordsConsistOnlyOfLetters : Bool
                allWordsConsistOnlyOfLetters =
                    let
                        onlyLetters s =
                            String.length (String.filter Char.isAlpha s) == String.length s
                    in
                    List.all onlyLetters words

                allWordsHaveAtLeastThreeLetters : Bool
                allWordsHaveAtLeastThreeLetters =
                    List.all (\w -> String.length w > 2) words

                trPrefix s =
                    "auth.login.wordsMode.input." ++ s
            in
            {- These rules force user to use 12 words instead of PK. -}
            if not has12words then
                [ InvalidEntry Passphrase (trPrefix "notPassphraseError") ]

            else if not allWordsConsistOnlyOfLetters then
                [ InvalidEntry Passphrase (trPrefix "notLatinLettersError") ]

            else if not allWordsHaveAtLeastThreeLetters then
                [ InvalidEntry Passphrase (trPrefix "atLeastThreeLettersError") ]

            else
                []
        )


pinValidator : Validator Problem LoginFormData
pinValidator =
    Validate.fromErrors
        (\form ->
            let
                pin =
                    form.enteredPin

                pinConfirmed =
                    form.enteredPinConfirmation

                hasCorrectLength p =
                    String.length p == 6

                hasOnlyDigits p =
                    String.all Char.isDigit p

                isPinConfirmedCorrectly =
                    pin == pinConfirmed
            in
            if not (hasCorrectLength pin) || not (hasOnlyDigits pin) then
                [ InvalidEntry Pin "auth.pin.shouldHaveSixDigitsError" ]

            else if not (hasCorrectLength pinConfirmed) || not (hasOnlyDigits pinConfirmed) then
                [ InvalidEntry PinConfirmation "auth.pin.shouldHaveSixDigitsError" ]

            else if not isPinConfirmedCorrectly then
                [ InvalidEntry PinConfirmation "auth.pinConfirmation.differsFromPinError" ]

            else
                []
        )


encodeLoginFormData : LoginFormData -> Value
encodeLoginFormData formData =
    Encode.object
        [ ( "privateKey", Encode.string formData.passphrase )
        , ( "usePin"
          , case formData.usePin of
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

        LoginWithPrivateKeyAccounts accounts form ->
            viewMultipleAccount accounts form False shared model

        LoggingInWithPrivateKeyAccounts accounts form ->
            viewMultipleAccount accounts form True shared model

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

        tr : String -> I18Next.Replacements -> String
        tr =
            I18Next.tr shared.translations I18Next.Curly

        errors =
            case loginStep of
                LoginStepPassphrase ->
                    List.map (viewFieldProblem shared Passphrase) model.problems

                LoginStepPIN ->
                    List.map (viewFieldProblem shared Pin) model.problems

        illustration fileName =
            img [ class "h-40 mx-auto mt-8 mb-7", src ("images/" ++ fileName) ] []

        buttonClass =
            "button button-primary min-w-full"

        labelText : String -> Html msg
        labelText tSuffix =
            text <| t shared.translations (tSuffix ++ ".label")

        pClass =
            "text-white text-body mb-5"

        viewPassphrase =
            let
                passphraseId =
                    "passphrase"
            in
            div []
                [ illustration "login_key.svg"
                , p [ class pClass ]
                    [ span [ class "text-green text-caption tracking-wide uppercase block mb-1" ] [ text ("Welcome back" ++ ",") ]
                    , span [ class "text-white block leading-relaxed" ] [ text "Enter your 12 words that you've saved on PDF in your device" ]
                    ]
                , viewFieldLabel shared "auth.login.wordsMode.input" passphraseId Nothing
                , textarea
                    [ class "form-textarea h-19 min-w-full block"
                    , id passphraseId
                    , value model.form.passphrase
                    , onInput EnteredPassphrase
                    , required True
                    , autocomplete False
                    ]
                    []
                , div [ class "input-label pr-1 text-white font-bold mt-1 text-right" ]
                    [ text <|
                        let
                            passphraseWordsCount =
                                String.fromInt <| List.length (List.filter (not << String.isEmpty) <| String.words model.form.passphrase)

                            _ =
                                Debug.log "passphraseWordsCount"
                        in
                        tr
                            "edit.input_counter"
                            [ ( "current", passphraseWordsCount )
                            , ( "max", "12" )
                            ]
                    ]
                , ul [ class "form-error-on-dark-bg absolute" ] errors
                , if not isModal then
                    p [ class "text-white text-body text-center mt-16 mb-4 block" ]
                        [ text_ "auth.login.register"
                        , a [ Route.href (Route.Register Nothing Nothing), class "text-orange-300 underline" ] [ text_ "auth.login.registerLink" ]
                        ]

                  else
                    text ""
                , button
                    [ class buttonClass
                    , onClick ClickedViewLoginPinStep
                    ]
                    [ text_ "dashboard.next" ]
                ]

        viewCreatePin =
            let
                trPrefix s =
                    "auth.pin.instruction." ++ s
            in
            div []
                [ illustration "login_pin.svg"
                , p [ class pClass ]
                    [ text_ (trPrefix "nowCreate")
                    , text " "
                    , strong [] [ text_ (trPrefix "sixDigitPin") ]
                    , text ". "
                    , text_ (trPrefix "thePin")
                    , text " "
                    , strong [] [ text_ (trPrefix "notPassword") ]
                    , text " "
                    , text_ (trPrefix "eachLogin")
                    ]
                , viewPinField model shared PinInput
                , div [ class "h-10" ] []
                , viewPinField model shared PinConfirmationInput
                , div [ class "h-20" ] []
                , button
                    [ class buttonClass
                    , onClick (SubmittedLoginPrivateKey model.form)
                    ]
                    [ text_ "auth.login.submit" ]
                ]
    in
    [ viewAuthError shared model.loginError
    , case loginStep of
        LoginStepPassphrase ->
            viewPassphrase

        LoginStepPIN ->
            viewCreatePin
    ]


viewMultipleAccount : List Eos.Name -> LoginFormData -> Bool -> Shared -> Model -> List (Html Msg)
viewMultipleAccount accounts form isDisabled shared model =
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
        [ viewPinField model shared PinInput
        , button
            [ class "btn btn--primary btn--login flex000"
            , disabled isDisabled
            ]
            [ text_ "auth.login.submit" ]
        ]
    ]


viewAuthError : Shared -> Maybe String -> Html Msg
viewAuthError shared maybeLoginError =
    case maybeLoginError of
        Nothing ->
            text ""

        Just error ->
            div [ class "bg-red border-lg rounded p-4 mt-2" ]
                [ p [ class "text-white" ]
                    [ text (t shared.translations error) ]
                ]


toggleViewPin : Bool -> String -> String -> Msg -> Html Msg
toggleViewPin isVisible showLabel hideLabel msg =
    button
        [ class "absolute mt-3 uppercase text-xs right-0 mr-3 text-orange-300"
        , onClick msg
        , attribute "tabindex" "-1"
        ]
        [ if isVisible then
            text hideLabel

          else
            text showLabel
        ]


viewFieldLabel : Shared -> String -> String -> Maybe (Html msg) -> Html msg
viewFieldLabel { translations } tSuffix id_ viewToggleHiddenSymbols =
    let
        labelText : String
        labelText =
            t translations (tSuffix ++ ".label")
    in
    label [ for id_, class "block" ]
        [ span [ class "text-green tracking-wide uppercase text-caption block mb-1" ]
            [ text <| labelText ]
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
    | SubmittedLoginPrivateKey LoginFormData
    | GotMultipleAccountsLogin (List Eos.Name)
    | ClickedPrivateKeyAccount Eos.Name LoginFormData
    | GotPrivateKeyLogin (Result String ( Eos.Name, String ))
    | SubmittedLoginPIN
    | GotPinLogin (Result String ( Eos.Name, String ))
    | CompletedLoadProfile Status Eos.Name (Result Http.Error Profile)
    | CompletedCreateProfile Status Eos.Name (Result Http.Error Profile)
    | TogglePinVisibility
    | TogglePinConfirmationVisibility
    | KeyPressed Bool
    | EnteredPin String
    | EnteredPinConf String


type ExternalMsg
    = ClickedCancel
    | CompletedAuth Profile
    | UpdatedShared Shared


trimPinNumber : Int -> String -> String -> String
trimPinNumber desiredLength oldPin newPin =
    let
        correctedPIN =
            if String.all Char.isDigit newPin then
                newPin

            else
                oldPin
    in
    if String.length correctedPIN > desiredLength then
        String.slice 0 desiredLength correctedPIN

    else
        correctedPIN


update : Msg -> Shared -> Model -> Bool -> UpdateResult
update msg shared model showAuthModal =
    case msg of
        EnteredPin pin ->
            let
                currentForm =
                    model.form
            in
            { model
                | form =
                    { currentForm
                        | enteredPin = trimPinNumber 6 currentForm.enteredPin pin
                    }
                , loginError = Nothing -- show validation errors only when form submitted
                , problems = []
            }
                |> UR.init

        EnteredPinConf pin ->
            let
                currentForm =
                    model.form
            in
            { model
                | form =
                    { currentForm
                        | enteredPinConfirmation = trimPinNumber 6 currentForm.enteredPinConfirmation pin
                    }
                , loginError = Nothing -- show validation errors only when form is submitted
                , problems = []
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
                    let
                        passphraseWithOnlySpaces =
                            -- Make sure that we have only spaces as a word separators (e.g. line brakes won't work)
                            String.join " " <| String.words model.form.passphrase

                        currentForm =
                            model.form

                        newForm =
                            { currentForm | passphrase = passphraseWithOnlySpaces }
                    in
                    { model
                        | loginError = Nothing
                        , form = newForm
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
                        | passphrase = phrase
                    }
            in
            { model
                | form = newForm
                , problems = []
            }
                |> UR.init

        SubmittedLoginPrivateKey form ->
            case validate pinValidator form of
                Ok _ ->
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
                                    , ( "form", encodeLoginFormData newForm )
                                    ]
                            }

                Err errors ->
                    { model | problems = errors }
                        |> UR.init

        GotMultipleAccountsLogin _ ->
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
                            , ( "form", encodeLoginFormData form )
                            ]
                    }

        GotPrivateKeyLogin (Ok ( accountName, privateKey )) ->
            UR.init model
                |> UR.addCmd
                    (Api.signIn
                        shared
                        accountName
                        (CompletedLoadProfile (LoggedInWithPrivateKey privateKey) accountName)
                    )

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

        CompletedLoadProfile newStatus _ (Ok profile) ->
            UR.init { model | status = newStatus }
                |> UR.addExt (CompletedAuth profile)

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

        TogglePinVisibility ->
            { model | pinVisibility = not model.pinVisibility } |> UR.init

        TogglePinConfirmationVisibility ->
            { model | pinConfirmationVisibility = not model.pinConfirmationVisibility } |> UR.init

        KeyPressed isEnter ->
            if isEnter then
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

        EnteredPin _ ->
            [ "EnteredPin" ]

        EnteredPinConf _ ->
            [ "EnteredPinConf" ]

        TogglePinVisibility ->
            [ "TogglePinVisibility" ]

        TogglePinConfirmationVisibility ->
            [ "TogglePinConfirmationVisibility" ]

        KeyPressed _ ->
            [ "KeyPressed" ]


{-| Call this function under the field to render related validation problems.
-}
viewFieldProblem : Shared -> ValidatedField -> Problem -> Html msg
viewFieldProblem { translations } field problem =
    let
        t s =
            I18Next.t translations s
    in
    case problem of
        ServerError _ ->
            text ""

        InvalidEntry f str ->
            if f == field then
                li [] [ text (t str) ]

            else
                text ""


viewPinField : Model -> Shared -> PinField -> Html Msg
viewPinField ({ form, problems } as model) shared inputType =
    let
        pinPrompt =
            case inputType of
                PinInput ->
                    case shared.maybeAccount of
                        Nothing ->
                            "auth.pin"

                        _ ->
                            -- Popup with PIN input for logged-in user has different label
                            "auth.pinPopup"

                PinConfirmationInput ->
                    "auth.pinConfirmation"

        errors =
            case inputType of
                PinInput ->
                    List.map (viewFieldProblem shared Pin) problems

                PinConfirmationInput ->
                    List.map (viewFieldProblem shared PinConfirmation) problems

        val =
            case inputType of
                PinInput ->
                    form.enteredPin

                PinConfirmationInput ->
                    form.enteredPinConfirmation

        inputId =
            case inputType of
                PinInput ->
                    "pinInput"

                PinConfirmationInput ->
                    "pinInputConfirmation"

        msg =
            case inputType of
                PinInput ->
                    EnteredPin

                PinConfirmationInput ->
                    EnteredPinConf

        toggleVisibilityMsg =
            case inputType of
                PinInput ->
                    TogglePinVisibility

                PinConfirmationInput ->
                    TogglePinConfirmationVisibility

        isVisible =
            case inputType of
                PinInput ->
                    model.pinVisibility

                PinConfirmationInput ->
                    model.pinConfirmationVisibility

        t =
            I18Next.t shared.translations

        tr : String -> I18Next.Replacements -> String
        tr =
            I18Next.tr shared.translations I18Next.Curly
    in
    div [ class "relative" ]
        [ viewFieldLabel shared pinPrompt inputId Nothing
        , input
            [ class "form-input min-w-full tracking-widest"
            , type_ <|
                if isVisible then
                    -- `"text"` is used here because with `"number"` field restrictions for the PIN
                    -- don't apply after toggling visibility (see `trimPinNumber` function).
                    "text"

                else
                    "password"
            , id inputId
            , placeholder "******"
            , maxlength 6
            , value val
            , onInput msg
            , required True
            , autocomplete False
            , attribute "inputmode" "numeric"
            ]
            []
        , div [ class "input-label pr-1 text-right text-white font-bold mt-1 absolute right-0" ]
            [ text <|
                tr
                    "edit.input_counter"
                    [ ( "current", String.fromInt <| String.length val )
                    , ( "max", "6" )
                    ]
            ]
        , toggleViewPin isVisible (t "auth.pin.toggle.show") (t "auth.pin.toggle.hide") toggleVisibilityMsg
        , ul [ class "form-error-on-dark-bg absolute" ] errors
        ]


type ValidatedField
    = Passphrase
    | Pin
    | PinConfirmation


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String
