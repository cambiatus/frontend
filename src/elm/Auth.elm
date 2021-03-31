module Auth exposing
    ( ExternalMsg(..)
    , LoginFormData
    , Model
    , Msg
    , SignInResponse
    , init
    , isAuth
    , jsAddressToMsg
    , maybePrivateKey
    , msgToString
    , signIn
    , subscriptions
    , update
    , view
    , viewFieldLabel
    )

import Api.Graphql
import Browser.Dom as Dom
import Browser.Events
import Cambiatus.Mutation
import Cambiatus.Object.Session
import Eos.Account as Eos
import Graphql.Http
import Graphql.Operation exposing (RootMutation)
import Graphql.OptionalArgument as OptionalArgument
import Graphql.SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, a, button, div, form, img, label, li, p, span, strong, text, textarea, ul)
import Html.Attributes exposing (autocomplete, autofocus, class, disabled, for, id, placeholder, required, src, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Log
import Ports
import Profile exposing (Model)
import RemoteData exposing (RemoteData)
import Route
import Session.Shared exposing (Shared, Translators)
import Task
import UpdateResult as UR
import Utils
import Validate exposing (Validator, validate)
import View.Feedback as Feedback
import View.Form
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
            initModel WithoutPrivateKey

        _ ->
            initModel (Unauthenticated LoginStepPassphrase)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map KeyPressed (Browser.Events.onKeyDown Utils.decodeEnterKeyDown)



-- MODEL


type alias Model =
    { status : Status
    , form : LoginFormData
    , error : Maybe String
    , isSigningIn : Bool
    , pinVisibility : Bool
    , pinConfirmationVisibility : Bool
    , problems : List ( Field, String )
    }


initModel : Status -> Model
initModel status =
    { status = status
    , form = initLoginFormData
    , error = Nothing
    , isSigningIn = False
    , pinVisibility = True
    , pinConfirmationVisibility = True
    , problems = []
    }


{-| Represents the state of the user's authentication. A user can be:

  - Unauthenticated - The user needs to see a form to insert the 12 words and create a PIN
  - Authenticated, but without theprivate key - This means the user is logged in,
    but we don't have their PIN, so we can't get their private key. If we ever need
    the user's private key, we should prompt them for their PIN
  - Authenticated, with private key - This means the user is logged in and we
    have access to their private key. The user can perform actions that need
    authentication without having to type in their PIN

-}
type Status
    = Unauthenticated LoginStep
    | WithoutPrivateKey
    | WithPrivateKey PrivateKey


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

                allWordsHaveAtLeastThreeLetters : Bool
                allWordsHaveAtLeastThreeLetters =
                    List.all (\w -> String.length w > 2) words

                trPrefix s =
                    "auth.login.wordsMode.input." ++ s
            in
            {- These rules force user to use 12 words instead of PK. -}
            if not has12words then
                [ ( Passphrase, trPrefix "notPassphraseError" ) ]

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
        WithPrivateKey _ ->
            True

        -- TODO - Should `WithoutPrivateKey` be considered authed here? (Check usage in other modules)
        _ ->
            False


maybePrivateKey : Model -> Maybe String
maybePrivateKey model =
    case model.status of
        WithPrivateKey pk ->
            Just pk

        _ ->
            Nothing



-- VIEW


view : Shared -> Model -> List (Html Msg)
view shared model =
    case model.status of
        Unauthenticated loginStep ->
            viewUnauthenticated shared model loginStep

        WithoutPrivateKey ->
            viewAuthenticated shared model

        WithPrivateKey _ ->
            viewAuthenticated shared model


type LoginStep
    = LoginStepPassphrase
    | LoginStepPIN


{-| The view that is presented to an unauthenticated user (the login page)
-}
viewUnauthenticated : Shared -> Model -> LoginStep -> List (Html Msg)
viewUnauthenticated shared model loginStep =
    let
        { t, tr } =
            shared.translators

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
                    List.map (\( _, errorDescription ) -> li [] [ text (t errorDescription) ]) passphraseErrors

                passphraseWordsCount =
                    model.form.passphrase
                        |> String.words
                        |> List.filter (not << String.isEmpty)
                        |> List.length
                        |> String.fromInt
            in
            [ form [ class "sf-content" ]
                [ illustration "login_key.svg"
                , p [ class pClass ]
                    [ span [ class "text-green text-caption tracking-wide uppercase block mb-1" ]
                        [ text (t "menu.my_communities") ]
                    , span [ class "text-white block leading-relaxed" ]
                        [ text (t "auth.login.wordsMode.input.description") ]
                    ]
                , viewFieldLabel shared.translators "auth.login.wordsMode.input" passphraseId
                , div [ class "relative" ]
                    [ textarea
                        [ class "form-textarea h-19 min-w-full block"
                        , placeholder (t "auth.login.wordsMode.input.placeholder")
                        , View.Form.noGrammarly
                        , autofocus True
                        , class <|
                            if not (List.isEmpty passphraseErrors) then
                                "field-with-error"

                            else
                                ""
                        , id passphraseId
                        , value model.form.passphrase
                        , onInput EnteredPassphrase
                        , onSubmit ClickedNextStep
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
                [ p [ class "text-white text-body text-center mt-16 mb-6 block" ]
                    [ text (t "auth.login.register")
                    , a [ Route.href (Route.Register Nothing Nothing), class "text-orange-300 underline" ]
                        [ text (t "auth.login.registerLink")
                        ]
                    ]
                , button
                    [ class buttonClass
                    , onClick ClickedNextStep
                    ]
                    [ text (t "dashboard.next") ]
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
                    [ text (t (trPrefix "nowCreate"))
                    , text " "
                    , strong [] [ text (t (trPrefix "sixDigitPin")) ]
                    , text ". "
                    , text (t <| trPrefix "thePin")
                    , text " "
                    , strong [] [ text <| t (trPrefix "notPassword") ]
                    , text " "
                    , text <| t (trPrefix "eachLogin")
                    ]
                , viewPin model shared
                , viewPinConfirmation model shared
                ]
            , div [ class "sf-footer" ]
                [ button
                    [ class buttonClass
                    , class "mt-10"
                    , disabled model.isSigningIn
                    , onClick (SubmittedForm model.form)
                    ]
                    [ text <|
                        t
                            (if model.isSigningIn then
                                "auth.login.submitting"

                             else
                                "auth.login.submit"
                            )
                    ]
                ]
            ]

        stepBody =
            case loginStep of
                LoginStepPassphrase ->
                    viewPassphrase

                LoginStepPIN ->
                    viewCreatePin
    in
    [ div [ class "sf-wrapper w-full px-4 md:max-w-sm md:mx-auto md:pt-20 md:px-0" ]
        stepBody
    ]


{-| Modal that asks for the user's PIN whenever needed. They must be authenticated
already (`WithPrivateKey` or `WithoutPrivateKey`)
-}
viewAuthenticated : Shared -> Model -> List (Html Msg)
viewAuthenticated shared model =
    -- TODO - Better visualization for when isSigningIn
    let
        { t } =
            shared.translators

        isDisabled =
            model.isSigningIn
                || (case model.status of
                        WithPrivateKey _ ->
                            True

                        WithoutPrivateKey ->
                            False

                        Unauthenticated _ ->
                            True
                   )
    in
    [ div []
        [ p
            [ class "modal-header px-0"
            ]
            [ text <| t "auth.login.modalFormTitle"
            ]
        , p [ class "text-sm" ]
            [ text <| t "auth.login.enterPinToContinue" ]
        ]
    , Html.form [ onSubmit SubmittedLoginPIN ]
        [ viewPin model shared
        , button
            [ class "button button-primary w-full"
            , disabled isDisabled
            ]
            [ text <| t "auth.login.continue" ]
        ]
    ]


viewFieldLabel : Translators -> String -> String -> Html msg
viewFieldLabel { t } tSuffix id_ =
    label
        [ class "block"
        , for id_
        ]
        [ span [ class "text-green tracking-wide uppercase text-caption block mb-1" ]
            [ text <| t (tSuffix ++ ".label") ]
        ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg ExternalMsg


type Msg
    = Ignored
      -- Input
    | EnteredPassphrase String
    | EnteredPin String
    | EnteredPinConf String
    | TogglePinVisibility
    | TogglePinConfirmationVisibility
    | KeyPressed Bool
      -- Submission
    | ClickedNextStep
    | SubmittedForm LoginFormData
    | SubmittedLoginPIN
    | CompletedSignIn Status (RemoteData (Graphql.Http.Error (Maybe SignInResponse)) (Maybe SignInResponse))
      -- Response
    | GotMultipleAccountsLogin (List Eos.Name)
    | ClickedPrivateKeyAccount Eos.Name LoginFormData
    | GotPrivateKeyLogin (Result String ( Eos.Name, String ))
    | GotPinLogin (Result String ( Eos.Name, String ))


type alias SignInResponse =
    { user : Profile.Model
    , token : String
    }


type ExternalMsg
    = ClickedCancel
    | CompletedAuth SignInResponse Model
    | SetFeedback Feedback.Model


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
    let
        { t } =
            shared.translators
    in
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
                , problems = []
            }
                |> UR.init

        Ignored ->
            UR.init model

        ClickedNextStep ->
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
                        | form = newForm
                        , problems = []
                        , status = Unauthenticated LoginStepPIN
                    }
                        |> UR.init
                        |> UR.addCmd
                            (Dom.focus "pinInput"
                                |> Task.attempt (\_ -> Ignored)
                            )

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
            case model.status of
                Unauthenticated LoginStepPassphrase ->
                    { model
                        | form = newForm
                        , problems = []
                    }
                        |> UR.init

                _ ->
                    UR.init model

        SubmittedForm form ->
            case validate pinValidator form of
                Ok _ ->
                    let
                        pinString =
                            form.enteredPin

                        newForm =
                            { form | usePin = Just pinString }
                    in
                    { model
                        | form = newForm
                        , isSigningIn = True
                    }
                        |> UR.init
                        |> UR.addPort
                            { responseAddress = SubmittedForm form
                            , responseData = Encode.null
                            , data =
                                Encode.object
                                    [ ( "name", Encode.string "loginWithPrivateKey" )
                                    , ( "form", encodeLoginFormData newForm )
                                    ]
                            }

                Err errors ->
                    { model
                        | problems = errors
                        , isSigningIn = False
                    }
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
            UR.init model
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
            model
                |> UR.init
                |> UR.addCmd
                    (Api.Graphql.mutation shared
                        Nothing
                        (signIn accountName shared Nothing)
                        (CompletedSignIn (WithPrivateKey privateKey))
                    )

        GotPrivateKeyLogin (Err err) ->
            model
                |> loginFailed
                |> addError (t err)

        CompletedSignIn status (RemoteData.Success (Just ({ token } as signInResponse))) ->
            let
                newModel =
                    { model | status = status }
            in
            newModel
                |> UR.init
                |> UR.addCmd (Ports.storeAuthToken token)
                |> UR.addExt (CompletedAuth signInResponse newModel)

        CompletedSignIn _ (RemoteData.Success Nothing) ->
            model
                |> loginFailed
                |> addError (t "error.unknown")

        CompletedSignIn _ (RemoteData.Failure err) ->
            loginFailedGraphql err model

        CompletedSignIn _ _ ->
            UR.init model

        SubmittedLoginPIN ->
            let
                pinString =
                    model.form.enteredPin
            in
            UR.init { model | isSigningIn = True }
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
                    (Api.Graphql.mutation shared
                        Nothing
                        (signIn accountName shared Nothing)
                        (CompletedSignIn (WithPrivateKey privateKey))
                    )

        GotPinLogin (Err err) ->
            model
                |> loginFailed
                |> addError (t err)

        TogglePinVisibility ->
            { model | pinVisibility = not model.pinVisibility } |> UR.init

        TogglePinConfirmationVisibility ->
            { model | pinConfirmationVisibility = not model.pinConfirmationVisibility } |> UR.init

        KeyPressed isEnter ->
            if isEnter then
                case model.status of
                    Unauthenticated LoginStepPassphrase ->
                        UR.init model
                            |> UR.addCmd
                                (Task.succeed ClickedNextStep
                                    |> Task.perform identity
                                )

                    Unauthenticated LoginStepPIN ->
                        UR.init model
                            |> UR.addCmd
                                (Task.succeed (SubmittedForm model.form)
                                    |> Task.perform identity
                                )

                    _ ->
                        UR.init model

            else
                UR.init model


signIn : Eos.Name -> Shared -> Maybe String -> SelectionSet (Maybe SignInResponse) RootMutation
signIn accountName shared maybeInvitationId =
    Cambiatus.Mutation.signIn
        (\opts -> { opts | invitationId = OptionalArgument.fromMaybe maybeInvitationId })
        { account = Eos.nameToString accountName
        , password = shared.graphqlSecret
        }
        (Graphql.SelectionSet.succeed SignInResponse
            |> with (Cambiatus.Object.Session.user Profile.selectionSet)
            |> with Cambiatus.Object.Session.token
        )


addError : String -> UpdateResult -> UpdateResult
addError error uResult =
    let
        isModal =
            case uResult.model.status of
                WithPrivateKey _ ->
                    True

                WithoutPrivateKey ->
                    True

                Unauthenticated _ ->
                    False
    in
    if isModal then
        uResult
            |> UR.mapModel (\m -> { m | error = Just error })

    else
        uResult
            |> UR.addExt (SetFeedback (Feedback.Shown Feedback.Failure error))


loginFailed : Model -> UpdateResult
loginFailed model =
    { model
        | status =
            case model.status of
                WithoutPrivateKey ->
                    WithoutPrivateKey

                Unauthenticated _ ->
                    Unauthenticated LoginStepPassphrase

                WithPrivateKey _ ->
                    WithoutPrivateKey
        , form = initLoginFormData
        , isSigningIn = False
    }
        |> UR.init


loginFailedGraphql : Graphql.Http.Error e -> Model -> UpdateResult
loginFailedGraphql httpError model =
    model
        |> loginFailed
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
        -- TODO - I18N
        |> addError "Auth failed"


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

        CompletedSignIn _ _ ->
            [ "CompletedSignIn" ]

        ClickedNextStep ->
            [ "ClickedViewLoginPinStep" ]

        EnteredPassphrase _ ->
            [ "EnteredPrivateKey" ]

        SubmittedForm _ ->
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
                    shared.translators.t "auth.pinPopup.label"

                Nothing ->
                    shared.translators.t "auth.pin.label"

        isPinError ( problemType, _ ) =
            case problemType of
                Pin ->
                    True

                _ ->
                    False

        errors =
            List.filter isPinError model.problems
                |> List.map (\( _, err ) -> err)

        modalErrors =
            case ( model.status, model.error ) of
                ( WithPrivateKey _, Just error ) ->
                    [ error ]

                ( WithoutPrivateKey, Just error ) ->
                    [ error ]

                _ ->
                    []
    in
    Pin.view
        shared
        { labelText = pinLabel
        , inputId = "pinInput"
        , inputValue = form.enteredPin
        , onInputMsg = EnteredPin
        , onToggleMsg = TogglePinVisibility
        , isVisible = model.pinVisibility
        , errors = modalErrors ++ errors
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
        { labelText = shared.translators.t "auth.pinConfirmation.label"
        , inputId = "pinInputConfirmation"
        , inputValue = form.enteredPinConfirmation
        , onInputMsg = EnteredPinConf
        , onToggleMsg = TogglePinConfirmationVisibility
        , isVisible = model.pinConfirmationVisibility
        , errors = errors
        }
