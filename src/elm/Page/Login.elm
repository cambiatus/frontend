module Page.Login exposing (Model, Msg, init, jsAddressToMsg, msgToString, update, view)

{-| This module is responsible for signing a user in, and showing the "Login" page.

For more information on the authentication architecture, checkout the Auth module.

The login process has two steps: `EnteringPassphrase` and `EnteringPin`.

First, the user enters the passphrase that was generated during the registering
process, and we check if that's valid. If so, we go on to the next step, and have
the user create a PIN. We store all the data we're going to need in cookies
and in our application.

The passphrase is a sequence of 12 english words that uniquely identifies the user,
and the PIN is a 6-digit sequence that we use to encrypt the passphrase, generating
the Private Key (PK), which can be used to sign EOS transactions.

In order to get an auth token from the backend, we use asymmetric cryptography:

1.  We request a phrase from the backend
2.  We sign the phrase with the user's private key
3.  We call the `signIn` mutation on GraphQL, passing the signed phrase as password
4.  The backend can use the user's public key to verify if the phrase was signed with the user's private key. If it is, we get back an auth token

-}

import Api.Graphql
import Browser.Dom as Dom
import Eos.Account as Eos
import Form
import Form.Text
import Form.Validate
import Graphql.Http
import Html exposing (Html, a, br, button, div, img, p, span, strong, text)
import Html.Attributes exposing (autocomplete, autofocus, class, classList, rows, spellcheck, src, type_)
import Html.Attributes.Aria exposing (ariaHidden)
import Html.Events exposing (on, onClick)
import Html.Keyed
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline
import Json.Encode as Encode exposing (Value)
import Log
import RemoteData exposing (RemoteData)
import Route
import Session.Guest as Guest
import Session.Shared exposing (Shared)
import Set exposing (Set)
import Task
import UpdateResult as UR
import View.Feedback as Feedback
import View.Pin as Pin



-- INIT


init : Guest.Model -> UpdateResult
init guest =
    let
        requestBip39 =
            case guest.shared.bip39 of
                Session.Shared.Bip39Loaded _ ->
                    identity

                Session.Shared.Bip39NotLoaded ->
                    UR.addPort
                        { responseAddress = GotBip39 { english = Set.empty, portuguese = Set.empty, spanish = Set.empty }
                        , responseData = Encode.null
                        , data = Encode.object [ ( "name", Encode.string "getBip39" ) ]
                        }
    in
    EnteringPassphrase initPassphraseModel
        |> UR.init
        |> requestBip39


initPassphraseModel : PassphraseModel
initPassphraseModel =
    { hasPasted = False
    , form = Form.init { passphrase = "" }
    }


initPinModel : Bool -> String -> ( PinModel, Cmd PinMsg )
initPinModel pinVisibility passphrase =
    let
        ( pinModel, pinCmd ) =
            Pin.init
                { label = "auth.pin.label"
                , id = "pinInput"
                , withConfirmation = True
                , submitLabel = "auth.login.submit"
                , submittingLabel = "auth.login.submitting"
                , pinVisibility = pinVisibility
                , lastKnownPin = Nothing
                }
    in
    ( { status = InputtingPin
      , passphrase = passphrase
      , pinModel = pinModel
      }
    , Cmd.map GotPinComponentMsg pinCmd
    )



-- MODEL


type Model
    = EnteringPassphrase PassphraseModel
    | EnteringPin PinModel


type alias PassphraseModel =
    { hasPasted : Bool
    , form : Form.Model PassphraseInput
    }


type alias PassphraseInput =
    { passphrase : String }


passphraseForm : Shared -> { hasPasted : Bool, hasTriedSubmitting : Bool } -> Form.Form PassphraseMsg PassphraseInput Passphrase
passphraseForm ({ translators } as shared) { hasPasted, hasTriedSubmitting } =
    let
        { t } =
            translators

        viewPasteButton =
            if shared.canReadClipboard then
                button
                    [ class "absolute bottom-4 left-1/2 -translate-x-1/2 button z-10"
                    , classList
                        [ ( "button-secondary", not hasPasted )
                        , ( "button-primary", hasPasted )
                        ]
                    , type_ "button"
                    , onClick ClickedPaste
                    ]
                    [ if hasPasted then
                        text (t "auth.login.wordsMode.input.pasted")

                      else
                        text (t "auth.login.wordsMode.input.paste")
                    ]

            else
                text ""

        viewHighlights currentText =
            div
                [ class "absolute inset-px pointer-events-none"
                , ariaHidden True
                ]
                [ div
                    [ Html.Attributes.id "highlights"
                    , class "p-4 overflow-auto h-full scrollbar-hidden"
                    ]
                    (currentText
                        |> String.split "\n"
                        |> List.map
                            (\line ->
                                if String.isEmpty line then
                                    br [] []

                                else
                                    p []
                                        (line
                                            |> String.split " "
                                            |> List.map
                                                (\word ->
                                                    if String.isEmpty word then
                                                        span [ class "inline-block" ] [ text " " ]

                                                    else
                                                        span
                                                            [ case shared.bip39 of
                                                                Session.Shared.Bip39NotLoaded ->
                                                                    class "transition-colors"

                                                                Session.Shared.Bip39Loaded bip39Wordlist ->
                                                                    if
                                                                        hasTriedSubmitting
                                                                            && not (Set.member word bip39Wordlist.english)
                                                                            && not (Set.member word bip39Wordlist.portuguese)
                                                                            && not (Set.member word bip39Wordlist.spanish)
                                                                    then
                                                                        class "bg-red/60 text-white"

                                                                    else
                                                                        class "transition-colors"
                                                            , class "pointer-events-none rounded-sm px-0.5 -mx-0.5"
                                                            ]
                                                            [ text word ]
                                                )
                                            |> List.intersperse (span [] [ text " " ])
                                        )
                            )
                    )
                ]
    in
    Form.succeed identity
        |> Form.withDecoration (viewIllustration "login_key.svg")
        |> Form.withDecoration
            (p [ class "text-white mb-6" ]
                [ span [ class "font-bold block" ]
                    [ text (t "menu.welcome_to") ]
                , span [ class "block" ]
                    [ text (t "auth.login.wordsMode.input.description") ]
                ]
            )
        |> Form.with
            (Form.introspect
                (\values ->
                    Form.Text.init
                        { label = t "auth.login.wordsMode.input.label"
                        , id = "passphrase-input"
                        }
                        |> Form.Text.withInputElement (Form.Text.TextareaInput { submitOnEnter = True })
                        |> Form.Text.withPlaceholder (t "auth.login.wordsMode.input.placeholder")
                        |> Form.Text.withExtraAttrs
                            [ class "min-w-full block p-4 relative bg-transparent z-10 caret-black text-transparent"
                            , classList [ ( "pb-18", shared.canReadClipboard ) ]
                            , rows 2
                            , autofocus True
                            , autocomplete False
                            , spellcheck False
                            , on "scroll"
                                (Decode.at [ "target", "scrollTop" ] Decode.int
                                    |> Decode.map (\scrollTop -> ScrolledTextArea { scrollTop = scrollTop })
                                )
                            ]
                        |> Form.Text.withInputContainerAttrs [ class "bg-white rounded" ]
                        |> Form.Text.withCounter (Form.Text.CountWords 12)
                        |> Form.Text.withCounterAttrs [ class "!text-white" ]
                        |> Form.Text.withLabelAttrs [ class "text-white" ]
                        |> Form.Text.withErrorAttrs [ class "form-error-on-dark-bg" ]
                        |> Form.Text.withElements
                            [ viewPasteButton
                            , viewHighlights values.passphrase
                            ]
                        |> Form.textField
                            { parser =
                                Form.Validate.succeed
                                    >> passphraseValidator shared.bip39
                                    >> Form.Validate.validate translators
                            , value = .passphrase
                            , update = \passphrase input -> { input | passphrase = passphrase }
                            , externalError = always Nothing
                            }
                )
            )


type Passphrase
    = Passphrase String


type alias PinModel =
    { status : PinStatus
    , passphrase : String
    , pinModel : Pin.Model
    }


type PinStatus
    = InputtingPin
    | GettingAccountName
    | LoggingIn
        { accountName : Eos.Name
        , pin : String
        , privateKey : Eos.PrivateKey
        }



-- VIEW


view : Guest.Model -> Model -> { title : String, content : Html Msg }
view guest model =
    { title =
        guest.shared.translators.t "auth.login.loginTab"
    , content =
        div [ class "bg-purple-500 flex-grow flex flex-col justify-center md:block" ]
            [ Html.Keyed.node "div"
                [ class "flex flex-col flex-grow justify-between w-full p-4 md:max-w-sm md:mx-auto md:pt-20 md:px-0" ]
                (case model of
                    EnteringPassphrase passphraseModel ->
                        viewPassphrase guest passphraseModel
                            |> List.map (Html.map GotPassphraseMsg)
                            |> List.indexedMap (\index node -> ( "passphrase-" ++ String.fromInt index, node ))

                    EnteringPin pinModel ->
                        viewPin guest pinModel
                            |> List.map (Html.map GotPinMsg)
                            |> List.indexedMap (\index node -> ( "pin-" ++ String.fromInt index, node ))
                )
            ]
    }


viewPassphrase : Guest.Model -> PassphraseModel -> List (Html PassphraseMsg)
viewPassphrase ({ shared } as guest) model =
    let
        { t } =
            shared.translators

        showRegisterLink =
            RemoteData.map .hasAutoInvite guest.community
                |> RemoteData.withDefault False
    in
    [ Form.view [ class "flex flex-col justify-center" ]
        shared.translators
        (\submitButton ->
            [ if showRegisterLink then
                p [ class "text-white text-center mb-6 block" ]
                    [ text (t "auth.login.register")
                    , a [ Route.href (Route.Register Nothing Nothing), class "text-orange-300 underline" ]
                        [ text (t "auth.login.registerLink")
                        ]
                    ]

              else
                text ""
            , submitButton
                [ class "button button-primary min-w-full"
                ]
                [ text (t "dashboard.continue") ]
            ]
        )
        (passphraseForm shared { hasPasted = model.hasPasted, hasTriedSubmitting = Form.isShowingAllErrors model.form })
        model.form
        { toMsg = GotPassphraseFormMsg
        , onSubmit = ClickedNextStep
        }
    ]


viewPin : Guest.Model -> PinModel -> List (Html PinMsg)
viewPin { shared } model =
    let
        trPrefix s =
            "auth.pin.instruction." ++ s

        { t } =
            shared.translators

        isInputtingPin =
            case model.status of
                InputtingPin ->
                    True

                _ ->
                    False
    in
    [ viewIllustration "login_pin.svg"
    , p [ class "text-white" ]
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
    , model.pinModel
        |> Pin.withBackgroundColor Pin.Dark
        |> Pin.withDisabled (not isInputtingPin)
        |> Pin.view shared.translators
        |> Html.map GotPinComponentMsg
    ]


viewIllustration : String -> Html msg
viewIllustration fileName =
    img [ class "h-40 mx-auto mb-6", src ("/images/" ++ fileName) ] []



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg Guest.External


type alias PassphraseUpdateResult =
    UR.UpdateResult PassphraseModel PassphraseMsg PassphraseExternalMsg


type alias PinUpdateResult =
    UR.UpdateResult PinModel PinMsg PinExternalMsg


type Msg
    = WentToPin Passphrase
    | GotPassphraseMsg PassphraseMsg
    | GotPinMsg PinMsg
    | GotBip39 { english : Set String, portuguese : Set String, spanish : Set String }


type PassphraseMsg
    = PassphraseIgnored
    | ClickedPaste
    | GotClipboardResponse ClipboardResponse
    | ScrolledTextArea { scrollTop : Int }
    | GotPassphraseFormMsg (Form.Msg PassphraseInput)
    | ClickedNextStep Passphrase


type ClipboardResponse
    = Denied
    | NotSupported
    | WithContent String
    | WithError


type PassphraseExternalMsg
    = FinishedEnteringPassphrase Passphrase
    | PassphraseGuestExternal Guest.External


type PinMsg
    = PinIgnored
    | SubmittedPinWithSuccess String
    | GotAccountName (Result String { accountName : Eos.Name, privateKey : Eos.PrivateKey, pin : String })
    | GeneratedAuthPhrase (RemoteData (Graphql.Http.Error Api.Graphql.Phrase) Api.Graphql.Phrase)
    | SignedAuthPhrase Api.Graphql.Password
    | GotSignInResult Eos.PrivateKey String (RemoteData (Graphql.Http.Error Api.Graphql.SignInResponse) Api.Graphql.SignInResponse)
    | GotPinComponentMsg Pin.Msg


type PinExternalMsg
    = PinGuestExternal Guest.External
    | RevertProcess


update : Msg -> Model -> Guest.Model -> UpdateResult
update msg model guest =
    case ( msg, model ) of
        ( GotPassphraseMsg passphraseMsg, EnteringPassphrase passphraseModel ) ->
            updateWithPassphrase passphraseMsg passphraseModel guest
                |> UR.map EnteringPassphrase
                    GotPassphraseMsg
                    (\ext ur ->
                        case ext of
                            FinishedEnteringPassphrase validPassphrase ->
                                UR.addMsg (WentToPin validPassphrase) ur

                            PassphraseGuestExternal guestExternal ->
                                UR.addExt guestExternal ur
                    )

        ( WentToPin (Passphrase passphrase), EnteringPassphrase _ ) ->
            let
                ( pinModel, pinCmd ) =
                    initPinModel guest.shared.pinVisibility passphrase
            in
            pinModel
                |> EnteringPin
                |> UR.init
                |> UR.addCmd (Cmd.map GotPinMsg pinCmd)

        ( GotPinMsg pinMsg, EnteringPin pinModel ) ->
            updateWithPin pinMsg pinModel guest
                |> UR.map EnteringPin
                    GotPinMsg
                    (\ext ur ->
                        case ext of
                            PinGuestExternal guestExternal ->
                                UR.addExt guestExternal ur

                            RevertProcess ->
                                initPassphraseModel
                                    |> EnteringPassphrase
                                    |> UR.setModel ur
                    )

        ( GotBip39 bip39, _ ) ->
            let
                oldShared =
                    guest.shared
            in
            model
                |> UR.init
                |> UR.addExt (Guest.UpdatedShared { oldShared | bip39 = Session.Shared.Bip39Loaded bip39 })

        -- Impossible Msgs
        ( GotPassphraseMsg _, EnteringPin _ ) ->
            UR.init model
                |> UR.logIncompatibleMsg msg
                    Nothing
                    { moduleName = "Page.Login"
                    , function = "update"
                    }
                    []

        ( WentToPin _, EnteringPin _ ) ->
            UR.init model
                |> UR.logIncompatibleMsg msg
                    Nothing
                    { moduleName = "Page.Login"
                    , function = "update"
                    }
                    []

        ( GotPinMsg _, EnteringPassphrase _ ) ->
            UR.init model
                |> UR.logIncompatibleMsg msg
                    Nothing
                    { moduleName = "Page.Login"
                    , function = "update"
                    }
                    []


updateWithPassphrase : PassphraseMsg -> PassphraseModel -> Guest.Model -> PassphraseUpdateResult
updateWithPassphrase msg model { shared } =
    case msg of
        PassphraseIgnored ->
            UR.init model

        ClickedPaste ->
            UR.init model
                |> UR.addPort
                    { responseAddress = ClickedPaste
                    , responseData = Encode.null
                    , data = Encode.object [ ( "name", Encode.string "readClipboard" ) ]
                    }
                |> UR.addCmd
                    (Dom.focus "passphrase-input"
                        |> Task.attempt (\_ -> PassphraseIgnored)
                    )
                |> UR.addExt
                    (Feedback.Hidden
                        |> Guest.SetFeedback
                        |> PassphraseGuestExternal
                    )

        GotClipboardResponse Denied ->
            { model | hasPasted = False }
                |> UR.init
                |> UR.addExt
                    (Feedback.Visible Feedback.Failure (shared.translators.t "error.clipboard.permissionDenied")
                        |> Guest.SetFeedback
                        |> PassphraseGuestExternal
                    )

        GotClipboardResponse NotSupported ->
            { model | hasPasted = False }
                |> UR.init
                |> UR.addExt
                    (Feedback.Visible Feedback.Failure (shared.translators.t "error.clipboard.paste.notSupported")
                        |> Guest.SetFeedback
                        |> PassphraseGuestExternal
                    )
                |> UR.logEvent
                    { username = Nothing
                    , message = "Clipboard API not supported"
                    , tags = [ Log.TypeTag Log.UnsupportedFeature ]
                    , location = { moduleName = "Page.Login", function = "updateWithPassphrase" }
                    , contexts = []
                    , transaction = msg
                    , level = Log.Warning
                    }

        GotClipboardResponse WithError ->
            { model | hasPasted = False }
                |> UR.init
                |> UR.addExt
                    (Feedback.Visible Feedback.Failure (shared.translators.t "error.unknown")
                        |> Guest.SetFeedback
                        |> PassphraseGuestExternal
                    )

        GotClipboardResponse (WithContent content) ->
            { model
                | hasPasted = True
                , form =
                    Form.updateValues (\oldForm -> { oldForm | passphrase = content })
                        model.form
            }
                |> UR.init
                |> UR.addCmd
                    (Dom.focus "passphrase-input"
                        |> Task.attempt (\_ -> PassphraseIgnored)
                    )
                |> UR.addExt
                    (Feedback.Hidden
                        |> Guest.SetFeedback
                        |> PassphraseGuestExternal
                    )

        ScrolledTextArea { scrollTop } ->
            UR.init model
                |> UR.addCmd
                    (Dom.setViewportOf "highlights" 0 (toFloat scrollTop)
                        |> Task.attempt (\_ -> PassphraseIgnored)
                    )

        GotPassphraseFormMsg subMsg ->
            Form.update shared subMsg model.form
                |> UR.fromChild
                    (\newForm -> { model | form = newForm })
                    GotPassphraseFormMsg
                    (Guest.SetFeedback >> PassphraseGuestExternal >> UR.addExt)
                    model

        ClickedNextStep passphrase ->
            model
                |> UR.init
                |> UR.addExt (FinishedEnteringPassphrase passphrase)


updateWithPin : PinMsg -> PinModel -> Guest.Model -> PinUpdateResult
updateWithPin msg model ({ shared } as guest) =
    case msg of
        PinIgnored ->
            UR.init model

        SubmittedPinWithSuccess pin ->
            { model | status = GettingAccountName }
                |> UR.init
                |> UR.addPort
                    { responseAddress = SubmittedPinWithSuccess pin
                    , responseData = Encode.string pin
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "getAccountFrom12Words" )
                            , ( "passphrase", Encode.string model.passphrase )
                            ]
                    }

        GotAccountName (Ok userData) ->
            { model | status = LoggingIn userData }
                |> UR.init
                |> UR.addCmd (Api.Graphql.askForPhrase shared userData.accountName GeneratedAuthPhrase)

        GotAccountName (Err err) ->
            { model | status = InputtingPin }
                |> UR.init
                |> UR.addExt
                    (Feedback.Visible Feedback.Failure (shared.translators.t err)
                        |> Guest.SetFeedback
                        |> PinGuestExternal
                    )
                |> UR.addExt RevertProcess

        GeneratedAuthPhrase (RemoteData.Success phrase) ->
            case model.status of
                LoggingIn { privateKey } ->
                    model
                        |> UR.init
                        |> UR.addPort (Api.Graphql.signPhrasePort msg privateKey phrase)

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Generated auth phrase, but wasn't logging in"
                            Nothing
                            { moduleName = "Page.Login"
                            , function = "updateWithPin"
                            }
                            []

        GeneratedAuthPhrase (RemoteData.Failure err) ->
            model
                |> UR.init
                |> UR.addExt
                    (Feedback.Visible Feedback.Failure (shared.translators.t "auth.failed_generating_phrase")
                        |> Guest.SetFeedback
                        |> PinGuestExternal
                    )
                |> UR.logGraphqlError msg
                    Nothing
                    "Got an error when generating auth phrase for login"
                    { moduleName = "Page.Login"
                    , function = "updateWithPin"
                    }
                    []
                    err
                |> UR.addExt RevertProcess

        GeneratedAuthPhrase _ ->
            UR.init model

        SignedAuthPhrase signedPhrase ->
            case model.status of
                LoggingIn userData ->
                    model
                        |> UR.init
                        |> UR.addCmd
                            (Api.Graphql.signIn shared
                                { account = userData.accountName
                                , password = signedPhrase
                                , invitationId = guest.maybeInvitation
                                }
                                (GotSignInResult userData.privateKey userData.pin)
                            )

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Signed auth phrase, but wasn't logging in"
                            Nothing
                            { moduleName = "Page.Login"
                            , function = "updateWithPin"
                            }
                            []

        GotSignInResult privateKey pin (RemoteData.Success signInResponse) ->
            UR.init model
                |> UR.addCmd (Api.Graphql.storeToken signInResponse.token)
                |> UR.addPort
                    { responseAddress = PinIgnored
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "login" )
                            , ( "privateKey", Eos.encodePrivateKey privateKey )
                            , ( "passphrase", Encode.string model.passphrase )
                            , ( "accountName", Eos.encodeName signInResponse.profile.account )
                            , ( "pin", Encode.string pin )
                            ]
                    }
                |> UR.addExt
                    (Guest.LoggedIn
                        { pin = pin
                        , privateKey = privateKey
                        , signInResponse = signInResponse
                        }
                        |> PinGuestExternal
                    )

        GotSignInResult _ _ (RemoteData.Failure err) ->
            UR.init model
                |> UR.logGraphqlError msg
                    Nothing
                    "Got an error when trying to login"
                    { moduleName = "Page.Login", function = "updateWithPin" }
                    []
                    err
                |> UR.addPort
                    { responseAddress = PinIgnored
                    , responseData = Encode.null
                    , data = Encode.object [ ( "name", Encode.string "logout" ) ]
                    }
                |> UR.addExt
                    (Feedback.Visible Feedback.Failure (shared.translators.t "auth.failed")
                        |> Guest.SetFeedback
                        |> PinGuestExternal
                    )
                |> UR.addExt RevertProcess

        GotSignInResult _ _ RemoteData.NotAsked ->
            UR.init model

        GotSignInResult _ _ RemoteData.Loading ->
            UR.init model

        GotPinComponentMsg subMsg ->
            Pin.update shared subMsg model.pinModel
                |> UR.fromChild (\pinModel -> { model | pinModel = pinModel })
                    GotPinComponentMsg
                    (\ext ur ->
                        case ext of
                            Pin.SendFeedback feedback ->
                                UR.addExt (PinGuestExternal (Guest.SetFeedback feedback)) ur

                            Pin.SubmitPin pin ->
                                let
                                    ( newShared, submitCmd ) =
                                        Pin.postSubmitAction ur.model.pinModel
                                            pin
                                            shared
                                            SubmittedPinWithSuccess
                                in
                                ur
                                    |> UR.addCmd submitCmd
                                    |> UR.addExt (PinGuestExternal (Guest.UpdatedShared newShared))
                    )
                    model



-- UTILS


passphraseValidator : Session.Shared.Bip39Status -> Form.Validate.Validator String -> Form.Validate.Validator Passphrase
passphraseValidator maybeBip39Wordlist =
    let
        has12Words passphrase =
            if List.length (String.words passphrase) >= 12 then
                String.words passphrase
                    |> List.take 12
                    |> String.join " "
                    |> Ok

            else
                Err (\translators_ -> translators_.t "auth.login.wordsMode.input.notPassphraseError")

        isBip39 passphrase =
            case maybeBip39Wordlist of
                Session.Shared.Bip39NotLoaded ->
                    Ok passphrase

                Session.Shared.Bip39Loaded bip39Wordlist ->
                    case
                        String.words passphrase
                            |> List.filter
                                (\word ->
                                    not (Set.member word bip39Wordlist.english)
                                        && not (Set.member word bip39Wordlist.portuguese)
                                        && not (Set.member word bip39Wordlist.spanish)
                                )
                    of
                        firstWord :: [] ->
                            Err (\translators_ -> translators_.tr "auth.login.wordsMode.input.invalid_word" [ ( "word", firstWord ) ])

                        firstWord :: otherWords ->
                            Err
                                (\translators_ ->
                                    translators_.tr "auth.login.wordsMode.input.invalid_words"
                                        [ ( "words"
                                          , String.join ", " (firstWord :: otherWords)
                                          )
                                        ]
                                )

                        [] ->
                            Ok passphrase

        wordsHave3Letters passphrase =
            if
                String.words passphrase
                    |> List.all (\word -> String.length word > 2)
            then
                Ok passphrase

            else
                Err (\translators_ -> translators_.t "auth.login.wordsMode.input.atLeastThreeLettersError")
    in
    Form.Validate.map String.toLower
        >> Form.Validate.custom has12Words
        >> Form.Validate.custom wordsHave3Letters
        >> Form.Validate.custom isBip39
        >> Form.Validate.map Passphrase


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "GotPassphraseMsg" :: "ClickedPaste" :: [] ->
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.field "isDenied" Decode.bool
                        |> Decode.map (\_ -> Denied)
                    , Decode.field "notSupported" Decode.bool
                        |> Decode.map (\_ -> NotSupported)
                    , Decode.field "clipboardContent" Decode.string
                        |> Decode.map WithContent
                    , Decode.field "error" Decode.string
                        |> Decode.map (\_ -> WithError)
                    ]
                    |> Decode.map (GotPassphraseMsg << GotClipboardResponse)
                )
                val
                |> Result.toMaybe

        "GotPinMsg" :: "SubmittedPinWithSuccess" :: [] ->
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.succeed (\pin account privateKey -> { pin = pin, accountName = account, privateKey = privateKey })
                        |> DecodePipeline.required "addressData" Decode.string
                        |> DecodePipeline.required "accountName" Eos.nameDecoder
                        |> DecodePipeline.required "privateKey" Eos.privateKeyDecoder
                        |> Decode.map (Ok >> GotAccountName >> GotPinMsg)
                    , Decode.field "error" Decode.string
                        |> Decode.map (Err >> GotAccountName >> GotPinMsg)
                    ]
                )
                val
                |> Result.toMaybe

        "GotPinMsg" :: "GeneratedAuthPhrase" :: _ ->
            Api.Graphql.decodeSignedPhrasePort (SignedAuthPhrase >> GotPinMsg)
                val

        "GotPinMsg" :: "PinIgnored" :: [] ->
            Just (GotPinMsg PinIgnored)

        "GotBip39" :: [] ->
            let
                decodeLanguage key =
                    Decode.field key (Decode.list Decode.string)
                        |> Decode.map Set.fromList
            in
            Decode.decodeValue
                (Decode.map3
                    (\english portuguese spanish ->
                        { english = english
                        , portuguese = portuguese
                        , spanish = spanish
                        }
                    )
                    (decodeLanguage "english")
                    (decodeLanguage "portuguese")
                    (decodeLanguage "spanish")
                )
                val
                |> Result.map GotBip39
                |> Result.toMaybe

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        WentToPin _ ->
            [ "WentToPin" ]

        GotPassphraseMsg passphraseMsg ->
            "GotPassphraseMsg" :: passphraseMsgToString passphraseMsg

        GotPinMsg pinMsg ->
            "GotPinMsg" :: pinMsgToString pinMsg

        GotBip39 _ ->
            [ "GotBip39" ]


passphraseMsgToString : PassphraseMsg -> List String
passphraseMsgToString msg =
    case msg of
        PassphraseIgnored ->
            [ "PassphraseIgnored" ]

        ClickedPaste ->
            [ "ClickedPaste" ]

        GotClipboardResponse _ ->
            [ "GotClipboardResponse" ]

        ScrolledTextArea _ ->
            [ "ScrolledTextArea" ]

        GotPassphraseFormMsg subMsg ->
            "GotPassphraseFormMsg" :: Form.msgToString subMsg

        ClickedNextStep _ ->
            [ "ClickedNextStep" ]


pinMsgToString : PinMsg -> List String
pinMsgToString msg =
    case msg of
        PinIgnored ->
            [ "PinIgnored" ]

        SubmittedPinWithSuccess _ ->
            [ "SubmittedPinWithSuccess" ]

        GotAccountName r ->
            [ "GotAccountName", UR.resultToString r ]

        GeneratedAuthPhrase _ ->
            [ "GeneratedAuthPhrase" ]

        SignedAuthPhrase _ ->
            [ "SignedAuthPhrase" ]

        GotSignInResult _ _ r ->
            [ "GotSignInResult", UR.remoteDataToString r ]

        GotPinComponentMsg subMsg ->
            "GotPinComponentMsg" :: Pin.msgToString subMsg
