module Page.Login exposing (Model, Msg, init, jsAddressToMsg, msgToString, update, view)

{-| This module is responsible for signing a user in, and showing the "Login" page.

For more information on the authentication architecture, checkout the Auth module.

The login process has two steps: `EnteringPassphrase` and `EnteringPin`.

First, the user enters the passphrase that was generated during the registering
process, and we check if that's valid. If so, we go on to the next step, and have
the user create a PIN. We store all the data we're going to need in localStorage
and in our application, and then perform a `signIn` mutation to get an auth token
from the backend.

The passphrase is a sequence of 12 english words that uniquely identifies the user,
and the PIN is a 6-digit sequence that we use to encrypt the passphrase, generating
the Private Key (PK), which can be used to sign EOS transactions.

-}

import Api.Graphql
import Auth
import Browser.Dom as Dom
import Dict
import Eos.Account as Eos
import Form
import Form.Text
import Form.Validate
import Graphql.Http
import Html exposing (Html, a, button, div, img, p, span, strong, text)
import Html.Attributes exposing (autocomplete, autofocus, class, classList, rows, src, type_)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline
import Json.Encode as Encode exposing (Value)
import Log
import Ports
import RemoteData exposing (RemoteData)
import Route
import Session.Guest as Guest
import Session.Shared exposing (Shared)
import Task
import UpdateResult as UR
import View.Feedback as Feedback
import View.Pin as Pin



-- INIT


init : Guest.Model -> ( Model, Cmd Msg )
init _ =
    ( EnteringPassphrase initPassphraseModel
    , Cmd.none
    )


initPassphraseModel : PassphraseModel
initPassphraseModel =
    { hasPasted = False
    , form = Form.init { passphrase = "" }
    }


initPinModel : Bool -> String -> PinModel
initPinModel pinVisibility passphrase =
    { isSigningIn = False
    , passphrase = passphrase
    , pinModel =
        Pin.init
            { label = "auth.pin.label"
            , id = "pinInput"
            , withConfirmation = True
            , submitLabel = "auth.login.submit"
            , submittingLabel = "auth.login.submitting"
            , pinVisibility = pinVisibility
            }
    }



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


passphraseForm : Shared -> { hasPasted : Bool } -> Form.Form PassphraseMsg PassphraseInput Passphrase
passphraseForm ({ translators } as shared) { hasPasted } =
    let
        { t } =
            translators

        viewPasteButton =
            if shared.canReadClipboard then
                button
                    [ class "absolute bottom-4 left-1/2 transform -translate-x-1/2 button"
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
            (Form.Text.init
                { label = t "auth.login.wordsMode.input.label"
                , id = "passphrase-input"
                }
                |> Form.Text.withInputElement (Form.Text.TextareaInput { submitOnEnter = True })
                |> Form.Text.withPlaceholder (t "auth.login.wordsMode.input.placeholder")
                |> Form.Text.withExtraAttrs
                    [ class "min-w-full block p-4"
                    , classList [ ( "pb-18", shared.canReadClipboard ) ]
                    , rows 2
                    , autofocus True
                    , autocomplete False
                    ]
                |> Form.Text.withCounter (Form.Text.CountWords 12)
                |> Form.Text.withCounterAttrs [ class "!text-white" ]
                |> Form.Text.withLabelAttrs [ class "text-white" ]
                |> Form.Text.withErrorAttrs [ class "form-error-on-dark-bg" ]
                |> Form.Text.withElements [ viewPasteButton ]
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> passphraseValidator
                            >> Form.Validate.validate translators
                    , value = .passphrase
                    , update = \passphrase input -> { input | passphrase = passphrase }
                    , externalError = always Nothing
                    }
            )


type Passphrase
    = Passphrase String


type alias PinModel =
    { isSigningIn : Bool
    , passphrase : String
    , pinModel : Pin.Model
    }



-- VIEW


view : Guest.Model -> Model -> { title : String, content : Html Msg }
view guest model =
    { title =
        guest.shared.translators.t "auth.login.loginTab"
    , content =
        div [ class "bg-purple-500 flex-grow flex flex-col justify-center md:block" ]
            [ div [ class "flex flex-col flex-grow justify-between w-full p-4 md:max-w-sm md:mx-auto md:pt-20 md:px-0" ]
                (case model of
                    EnteringPassphrase passphraseModel ->
                        viewPassphrase guest passphraseModel
                            |> List.map (Html.map GotPassphraseMsg)

                    EnteringPin pinModel ->
                        viewPin guest pinModel
                            |> List.map (Html.map GotPinMsg)
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
        (passphraseForm shared { hasPasted = model.hasPasted })
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
    in
    [ viewIllustration "login_pin.svg"
    , p [ class "text-white mb-6" ]
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


type PassphraseMsg
    = PassphraseIgnored
    | ClickedPaste
    | GotClipboardResponse ClipboardResponse
    | GotPassphraseFormMsg (Form.Msg PassphraseInput)
    | ClickedNextStep Passphrase


type ClipboardResponse
    = Denied
    | NotSupported
    | WithContent String
    | WithError String


type PassphraseExternalMsg
    = FinishedEnteringPassphrase Passphrase
    | PassphraseGuestExternal Guest.External


type PinMsg
    = PinIgnored
    | SubmittedPinWithSuccess String
    | GotSubmitResult (Result String ( Eos.Name, Eos.PrivateKey ))
    | GotSignInResult Eos.PrivateKey (RemoteData (Graphql.Http.Error (Maybe Auth.SignInResponse)) (Maybe Auth.SignInResponse))
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
                                ur
                                    |> UR.addCmd
                                        (Task.succeed validPassphrase
                                            |> Task.perform WentToPin
                                        )

                            PassphraseGuestExternal guestExternal ->
                                UR.addExt guestExternal ur
                    )

        ( WentToPin (Passphrase passphrase), EnteringPassphrase _ ) ->
            initPinModel guest.shared.pinVisibility passphrase
                |> EnteringPin
                |> UR.init
                |> UR.addCmd
                    (Dom.focus "pinInput"
                        |> Task.attempt (\_ -> GotPinMsg PinIgnored)
                    )

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
                    (Dom.focus "passphrase"
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

        GotClipboardResponse (WithError error) ->
            { model | hasPasted = False }
                |> UR.init
                |> UR.addExt
                    (Feedback.Visible Feedback.Failure (shared.translators.t "error.unknown")
                        |> Guest.SetFeedback
                        |> PassphraseGuestExternal
                    )
                |> UR.logEvent
                    { username = Nothing
                    , message = "Got error when pasting from clipboard"
                    , tags = [ Log.TypeTag Log.UnknownError ]
                    , location = { moduleName = "Page.Login", function = "updateWithPassphrase" }
                    , contexts =
                        [ { name = "Error"
                          , extras = Dict.fromList [ ( "message", Encode.string error ) ]
                          }
                        ]
                    , transaction = msg
                    , level = Log.Warning
                    }

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
            { model | isSigningIn = True }
                |> UR.init
                |> UR.addPort
                    { responseAddress = SubmittedPinWithSuccess pin
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "login" )
                            , ( "passphrase", Encode.string model.passphrase )
                            , ( "pin", Encode.string pin )
                            ]
                    }

        GotSubmitResult (Ok ( accountName, privateKey )) ->
            UR.init model
                |> UR.addCmd
                    (Api.Graphql.mutation shared
                        Nothing
                        (Auth.signIn accountName shared guest.maybeInvitation)
                        (GotSignInResult privateKey)
                    )

        GotSubmitResult (Err err) ->
            UR.init model
                |> UR.addExt
                    (Feedback.Visible Feedback.Failure (shared.translators.t err)
                        |> Guest.SetFeedback
                        |> PinGuestExternal
                    )
                |> UR.addExt RevertProcess

        GotSignInResult privateKey (RemoteData.Success (Just signInResponse)) ->
            UR.init model
                |> UR.addCmd (Ports.storeAuthToken signInResponse.token)
                |> UR.addExt (Guest.LoggedIn privateKey signInResponse |> PinGuestExternal)

        GotSignInResult _ (RemoteData.Success Nothing) ->
            UR.init model
                |> UR.addExt
                    (Feedback.Visible Feedback.Failure (shared.translators.t "error.unknown")
                        |> Guest.SetFeedback
                        |> PinGuestExternal
                    )
                |> UR.addPort
                    { responseAddress = PinIgnored
                    , responseData = Encode.null
                    , data = Encode.object [ ( "name", Encode.string "logout" ) ]
                    }
                |> UR.logImpossible msg
                    "Got a sign in response with Nothing"
                    Nothing
                    { moduleName = "Page.Login", function = "updateWithPin" }
                    []

        GotSignInResult _ (RemoteData.Failure err) ->
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

        GotSignInResult _ RemoteData.NotAsked ->
            UR.init model

        GotSignInResult _ RemoteData.Loading ->
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


passphraseValidator : Form.Validate.Validator String -> Form.Validate.Validator Passphrase
passphraseValidator =
    let
        has12Words passphrase =
            if List.length (String.words passphrase) >= 12 then
                String.words passphrase
                    |> List.take 12
                    |> String.join " "
                    |> Ok

            else
                Err (\translators_ -> translators_.t "auth.login.wordsMode.input.notPassphraseError")

        wordsHave3Letters passphrase =
            if
                String.words passphrase
                    |> List.all (\word -> String.length word > 2)
            then
                Ok passphrase

            else
                Err (\translators_ -> translators_.t "auth.login.wordsMode.input.atLeastThreeLettersError")
    in
    Form.Validate.custom has12Words
        >> Form.Validate.custom wordsHave3Letters
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
                        |> Decode.map WithError
                    ]
                    |> Decode.map (GotPassphraseMsg << GotClipboardResponse)
                )
                val
                |> Result.toMaybe

        "GotPinMsg" :: "SubmittedPinWithSuccess" :: _ :: [] ->
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.succeed Tuple.pair
                        |> DecodePipeline.required "accountName" Eos.nameDecoder
                        |> DecodePipeline.required "privateKey" Eos.privateKeyDecoder
                        |> Decode.map (Ok >> GotSubmitResult >> GotPinMsg)
                    , Decode.field "error" Decode.string
                        |> Decode.map (Err >> GotSubmitResult >> GotPinMsg)
                    ]
                )
                val
                |> Result.toMaybe

        "GotPinMsg" :: "PinIgnored" :: [] ->
            Just (GotPinMsg PinIgnored)

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


passphraseMsgToString : PassphraseMsg -> List String
passphraseMsgToString msg =
    case msg of
        PassphraseIgnored ->
            [ "PassphraseIgnored" ]

        ClickedPaste ->
            [ "ClickedPaste" ]

        GotClipboardResponse _ ->
            [ "GotClipboardResponse" ]

        GotPassphraseFormMsg subMsg ->
            "GotPassphraseFormMsg" :: Form.msgToString subMsg

        ClickedNextStep _ ->
            [ "ClickedNextStep" ]


pinMsgToString : PinMsg -> List String
pinMsgToString msg =
    case msg of
        PinIgnored ->
            [ "PinIgnored" ]

        SubmittedPinWithSuccess pin ->
            [ "SubmittedPinWithSuccess", pin ]

        GotSubmitResult r ->
            [ "GotSubmitResult", UR.resultToString r ]

        GotSignInResult _ r ->
            [ "GotSignInResult", UR.remoteDataToString r ]

        GotPinComponentMsg subMsg ->
            "GotPinComponentMsg" :: Pin.msgToString subMsg
