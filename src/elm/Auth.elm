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

import Api.Graphql
import Asset.Icon as Icon
import Browser.Events
import Eos.Account as Eos
import Graphql.Http
import Html exposing (Html, a, button, div, h2, img, label, li, p, span, strong, text, textarea, ul)
import Html.Attributes exposing (autocomplete, class, disabled, for, id, placeholder, required, src, title, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import I18Next exposing (t)
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Log
import Profile exposing (Profile)
import Route
import Session.Shared exposing (Shared)
import Task
import UpdateResult as UR
import Utils
import Validate exposing (Validator, validate)
import View.Pin as Pin


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
    , problems : List ( Field, String )
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


type Field
    = Passphrase
    | Pin
    | PinConfirmation


passphraseValidator : Validator ( Field, String ) LoginFormData
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
                [ ( Passphrase, trPrefix "notPassphraseError" ) ]

            else if not allWordsConsistOnlyOfLetters then
                [ ( Passphrase, trPrefix "notLatinLettersError" ) ]

            else if not allWordsHaveAtLeastThreeLetters then
                [ ( Passphrase, trPrefix "atLeastThreeLettersError" ) ]

            else
                []
        )


pinValidator : Validator ( Field, String ) LoginFormData
pinValidator =
    Validate.fromErrors
        (\form ->
            let
                pin =
                    form.enteredPin

                pinConfirmed =
                    form.enteredPinConfirmation

                isPinConfirmedCorrectly =
                    pin == pinConfirmed
            in
            if not (Pin.isValid pin) then
                [ ( Pin, "auth.pin.shouldHaveSixDigitsError" ) ]

            else if not (Pin.isValid pinConfirmed) then
                [ ( PinConfirmation, "auth.pin.shouldHaveSixDigitsError" ) ]

            else if not isPinConfirmedCorrectly then
                [ ( PinConfirmation, "auth.pinConfirmation.differsFromPinError" ) ]

            else
                []
        )


encodeLoginFormData : LoginFormData -> Value
encodeLoginFormData formData =
    Encode.object
        [ ( "passphrase", Encode.string formData.passphrase )
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
                Just ( _, True ) ->
                    viewLoginWithPin False shared model

                _ ->
                    viewLoginSteps isModal shared model LoginStepPassphrase

        LoggingInWithPin ->
            case shared.maybeAccount of
                Just ( _, True ) ->
                    viewLoginWithPin True shared model

                _ ->
                    viewLoginSteps isModal shared model LoginStepPassphrase

        LoggedInWithPin _ ->
            case shared.maybeAccount of
                Just ( _, True ) ->
                    viewLoginWithPin True shared model

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

        illustration fileName =
            img [ class "h-40 mx-auto mt-8 mb-7", src ("images/" ++ fileName) ] []

        buttonClass =
            "button button-primary min-w-full mb-8"

        pClass =
            "text-white text-body mb-5"

        viewPassphrase =
            let
                passphraseId =
                    "passphrase"

                isPassphraseError ( problemType, _ ) =
                    case problemType of
                        Passphrase ->
                            True

                        _ ->
                            False

                passphraseErrors =
                    List.filter isPassphraseError model.problems

                errors =
                    List.map (\( _, errorDescription ) -> li [] [ text_ errorDescription ]) passphraseErrors

                passphraseWordsCount =
                    model.form.passphrase
                        |> String.words
                        |> List.filter (not << String.isEmpty)
                        |> List.length
                        |> String.fromInt
            in
            [ div [ class "sf-content" ]
                [ illustration "login_key.svg"
                , p [ class pClass ]
                    [ span [ class "text-green text-caption tracking-wide uppercase block mb-1" ]
                        [ text_ "menu.my_communities" ]
                    , span [ class "text-white block leading-relaxed" ]
                        [ text_ "auth.login.wordsMode.input.description" ]
                    ]
                , viewFieldLabel shared "auth.login.wordsMode.input" passphraseId
                , div [ class "relative" ]
                    [ textarea
                        [ class "form-textarea h-19 min-w-full block"
                        , placeholder (t shared.translations "auth.login.wordsMode.input.placeholder")
                        , class <|
                            if not (List.isEmpty passphraseErrors) then
                                "field-with-error"

                            else
                                ""
                        , id passphraseId
                        , value model.form.passphrase
                        , onInput EnteredPassphrase
                        , required True
                        , autocomplete False
                        ]
                        []
                    , div [ class "input-label pr-1 absolute right-0 text-white font-bold mt-1 text-right" ]
                        [ text <|
                            tr
                                "edit.input_counter"
                                [ ( "current", passphraseWordsCount )
                                , ( "max", "12" )
                                ]
                        ]
                    ]
                , ul [ class "form-error-on-dark-bg absolute" ] errors
                ]
            , div [ class "sf-footer" ]
                [ if not isModal then
                    p [ class "text-white text-body text-center mt-16 mb-6 block" ]
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
            ]

        viewCreatePin =
            let
                trPrefix s =
                    "auth.pin.instruction." ++ s
            in
            [ div [ class "sf-content" ]
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
                , viewPin model shared
                , viewPinConfirmation model shared
                ]
            , div [ class "sf-footer" ]
                [ button
                    [ class buttonClass
                    , class "mt-10"
                    , onClick (SubmittedLoginPrivateKey model.form)
                    ]
                    [ text_ "auth.login.submit" ]
                ]
            ]

        stepBody =
            case loginStep of
                LoginStepPassphrase ->
                    viewPassphrase

                LoginStepPIN ->
                    viewCreatePin
    in
    [ div [ class "mx-4 mt-4 md:max-w-sm md:mx-auto" ]
        [ viewAuthError shared model.loginError ]
    , div [ class "sf-wrapper w-full px-4 md:max-w-sm md:mx-auto md:pt-20 md:px-0" ]
        stepBody
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


{-| Popup asking the logged-in user to enter the PIN when needed.
-}
viewLoginWithPin : Bool -> Shared -> Model -> List (Html Msg)
viewLoginWithPin isDisabled shared model =
    let
        text_ s =
            Html.text (t shared.translations s)
    in
    [ div []
        [ p [ class "modal-header px-0" ]
            [ text_ "auth.login.modalFormTitle"
            ]
        , p [ class "text-sm" ]
            [ text_ "auth.login.enterPinToContinue" ]
        , viewAuthError shared model.loginError
        ]
    , Html.form
        [ onSubmit SubmittedLoginPIN
        ]
        [ viewPin model shared
        , button
            [ class "button button-primary w-full"
            , disabled isDisabled
            ]
            [ text_ "auth.login.continue" ]
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


viewFieldLabel : Shared -> String -> String -> Html msg
viewFieldLabel { translations } tSuffix id_ =
    let
        labelText : String
        labelText =
            t translations (tSuffix ++ ".label")
    in
    label
        [ class "block"
        , for id_
        ]
        [ span [ class "text-green tracking-wide uppercase text-caption block mb-1" ]
            [ text <| labelText ]
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
    | CompletedLoadProfile Status Eos.Name (Result (Graphql.Http.Error (Maybe Profile)) (Maybe Profile))
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


update : Msg -> Shared -> Model -> UpdateResult
update msg shared model =
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
                    (Api.Graphql.query shared
                        (Profile.query accountName)
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
            if String.isEmpty model.form.enteredPin then
                { model | loginError = Just "Please fill in all the PIN digits" }
                    |> UR.init

            else
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
                |> UR.addCmd
                    (Api.Graphql.query shared
                        (Profile.query accountName)
                        (CompletedLoadProfile (LoggedInWithPrivateKey privateKey) accountName)
                    )

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
            case profile of
                Just p ->
                    UR.init { model | status = newStatus }
                        |> UR.addExt (CompletedAuth p)

                Nothing ->
                    UR.init model

        CompletedLoadProfile _ _ (Err err) ->
            loginFailedGraphql err model

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


loginFailedGraphql : Graphql.Http.Error (Maybe Profile) -> Model -> UpdateResult
loginFailedGraphql httpError model =
    UR.init
        { model
            | loginError =
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
        |> UR.addCmd (Log.graphqlError httpError)
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


viewPin : Model -> Shared -> Html Msg
viewPin ({ form } as model) shared =
    let
        pinLabel =
            case shared.maybeAccount of
                Just _ ->
                    -- Popup with PIN input for logged-in user has different label
                    I18Next.t shared.translations "auth.pinPopup.label"

                Nothing ->
                    I18Next.t shared.translations "auth.pin.label"

        isPinError ( problemType, _ ) =
            case problemType of
                Pin ->
                    True

                _ ->
                    False

        errors =
            List.filter isPinError model.problems
                |> List.map (\( _, err ) -> err)
    in
    Pin.view
        shared
        { labelText = pinLabel
        , inputId = "pinInput"
        , inputValue = form.enteredPin
        , onInputMsg = EnteredPin
        , onToggleMsg = TogglePinVisibility
        , isVisible = model.pinVisibility
        , errors = errors
        }


viewPinConfirmation : Model -> Shared -> Html Msg
viewPinConfirmation ({ form } as model) shared =
    let
        isPinConfirmError ( problemType, _ ) =
            case problemType of
                PinConfirmation ->
                    True

                _ ->
                    False

        errors =
            List.filter isPinConfirmError model.problems
                |> List.map (\( _, err ) -> err)
    in
    Pin.view
        shared
        { labelText = I18Next.t shared.translations "auth.pinConfirmation.label"
        , inputId = "pinInputConfirmation"
        , inputValue = form.enteredPinConfirmation
        , onInputMsg = EnteredPinConf
        , onToggleMsg = TogglePinConfirmationVisibility
        , isVisible = model.pinConfirmationVisibility
        , errors = errors
        }
