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
import Eos.Account as Eos
import Graphql.Http
import Html exposing (Html, a, button, div, form, img, p, span, strong, text)
import Html.Attributes exposing (autocomplete, autofocus, class, classList, required, rows, src, type_)
import Html.Events exposing (keyCode, onClick, preventDefaultOn)
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline
import Json.Encode as Encode exposing (Value)
import Ports
import RemoteData exposing (RemoteData)
import Route
import Session.Guest as Guest
import Task
import UpdateResult as UR
import Validate exposing (Validator)
import View.Feedback as Feedback
import View.Form
import View.Form.Input as Input
import View.Form.InputCounter as InputCounter
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
    , passphrase = ""
    , problems = []
    }


initPinModel : String -> PinModel
initPinModel passphrase =
    { isSigningIn = False
    , passphrase = passphrase
    , pinModel =
        Pin.init
            { label = "auth.pin.label"
            , id = "pinInput"
            , withConfirmation = True
            , submitLabel = "auth.login.submit"
            , submittingLabel = "auth.login.submitting"
            }
    }



-- MODEL


type Model
    = EnteringPassphrase PassphraseModel
    | EnteringPin PinModel


type alias PassphraseModel =
    { hasPasted : Bool
    , passphrase : String
    , problems : List String
    }


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
            [ div [ class "sf-wrapper flex-grow w-full px-4 md:max-w-sm md:mx-auto md:pt-20 md:px-0" ]
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
        { t, tr } =
            shared.translators

        enterKeyCode =
            13

        viewPasteButton =
            if shared.canReadClipboard then
                button
                    [ class "absolute bottom-0 left-0 button m-2"
                    , classList
                        [ ( "button-secondary", not model.hasPasted )
                        , ( "button-primary", model.hasPasted )
                        ]
                    , type_ "button"
                    , onClick ClickedPaste
                    ]
                    [ if model.hasPasted then
                        text (t "auth.login.wordsMode.input.pasted")

                      else
                        text (t "auth.login.wordsMode.input.paste")
                    ]

            else
                text ""

        showRegisterLink =
            RemoteData.map .hasAutoInvite guest.community
                |> RemoteData.withDefault False

        communityName =
            case guest.community of
                RemoteData.Success community ->
                    community.name

                _ ->
                    ""
    in
    [ form [ class "sf-content flex flex-col flex-grow justify-center" ]
        [ viewIllustration "login_key.svg"
        , p [ class "text-white text-body mb-5" ]
            [ span [ class "text-green text-caption tracking-wide uppercase block mb-1" ]
                [ text (tr "menu.welcome_to" [ ( "community_name", communityName ) ]) ]
            , span [ class "text-white block leading-relaxed" ]
                [ text (t "auth.login.wordsMode.input.description") ]
            ]
        , Input.init
            { label = t "auth.login.wordsMode.input.label"
            , id = "passphrase"
            , onInput = EnteredPassphrase
            , disabled = False
            , value = model.passphrase
            , placeholder = Just <| t "auth.login.wordsMode.input.placeholder"
            , problems =
                model.problems
                    |> List.map t
                    |> Just
            , translators = shared.translators
            }
            |> Input.withInputType Input.TextArea
            |> Input.withAttrs
                [ class "min-w-full block text-base"
                , classList
                    [ ( "field-with-error", not (List.isEmpty model.problems) )
                    , ( "pb-16", shared.canReadClipboard )
                    ]
                , rows 2
                , View.Form.noGrammarly
                , autofocus True
                , required True
                , autocomplete False
                , preventDefaultOn "keydown"
                    (keyCode
                        |> Decode.map
                            (\code ->
                                if code == enterKeyCode then
                                    ( ClickedNextStep, True )

                                else
                                    ( PassphraseIgnored, False )
                            )
                    )
                ]
            |> Input.withCounter 12
            |> Input.withCounterType InputCounter.CountWords
            |> Input.withCounterAttrs [ class "text-white" ]
            |> Input.withErrorAttrs [ class "form-error-on-dark-bg" ]
            |> Input.withElements [ viewPasteButton ]
            |> Input.toHtml
        ]
    , div [ class "sf-footer" ]
        [ if showRegisterLink then
            p [ class "text-white text-body text-center mb-6 block" ]
                [ text (t "auth.login.register")
                , a [ Route.href (Route.Register Nothing Nothing), class "text-orange-300 underline" ]
                    [ text (t "auth.login.registerLink")
                    ]
                ]

          else
            text ""
        , button
            [ class "button button-primary min-w-full mb-8"
            , onClick ClickedNextStep
            ]
            [ text (t "dashboard.next") ]
        ]
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
    , p [ class "text-white text-body mb-5" ]
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
    , Pin.withAttrs [ class "mb-8" ] model.pinModel
        |> Pin.view shared.translators
        |> Html.map GotPinComponentMsg
    ]


viewIllustration : String -> Html msg
viewIllustration fileName =
    img [ class "h-40 mx-auto mt-8 mb-7", src ("/images/" ++ fileName) ] []



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg Guest.External


type alias PassphraseUpdateResult =
    UR.UpdateResult PassphraseModel PassphraseMsg PassphraseExternalMsg


type alias PinUpdateResult =
    UR.UpdateResult PinModel PinMsg PinExternalMsg


type Msg
    = WentToPin (Validate.Valid PassphraseModel)
    | GotPassphraseMsg PassphraseMsg
    | GotPinMsg PinMsg


type PassphraseMsg
    = PassphraseIgnored
    | ClickedPaste
    | GotClipboardResponse ClipboardResponse
    | EnteredPassphrase String
    | ClickedNextStep


type ClipboardResponse
    = Denied
    | NotSupported
    | WithContent String
    | WithError String


type PassphraseExternalMsg
    = FinishedEnteringPassphrase (Validate.Valid PassphraseModel)
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

        ( WentToPin validPassphrase, EnteringPassphrase _ ) ->
            Validate.fromValid validPassphrase
                |> .passphrase
                |> initPinModel
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
                |> UR.logImpossible msg [ "CreatingPin" ]

        ( WentToPin _, EnteringPin _ ) ->
            UR.init model
                |> UR.logImpossible msg [ "CreatingPin" ]

        ( GotPinMsg _, EnteringPassphrase _ ) ->
            UR.init model
                |> UR.logImpossible msg [ "CreatingPassphrase" ]


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
                |> UR.logImpossible msg [ "NotSupported" ]
                |> UR.addExt
                    (Feedback.Visible Feedback.Failure (shared.translators.t "error.clipboard.paste.notSupported")
                        |> Guest.SetFeedback
                        |> PassphraseGuestExternal
                    )

        GotClipboardResponse (WithError error) ->
            { model | hasPasted = False }
                |> UR.init
                |> UR.logImpossible msg [ "WithError", error ]
                |> UR.addExt
                    (Feedback.Visible Feedback.Failure (shared.translators.t "error.unknown")
                        |> Guest.SetFeedback
                        |> PassphraseGuestExternal
                    )

        GotClipboardResponse (WithContent content) ->
            { model
                | passphrase =
                    String.trim content
                        |> String.words
                        |> List.take 12
                        |> String.join " "
                , hasPasted = True
                , problems = []
            }
                |> UR.init
                |> UR.addExt
                    (Feedback.Hidden
                        |> Guest.SetFeedback
                        |> PassphraseGuestExternal
                    )

        EnteredPassphrase passphrase ->
            { model
                | passphrase =
                    if List.length (String.words passphrase) >= 12 then
                        String.words passphrase
                            |> List.take 12
                            |> String.join " "

                    else
                        passphrase
                , hasPasted = False
                , problems = []
            }
                |> UR.init

        ClickedNextStep ->
            case Validate.validate passphraseValidator model of
                Ok validModel ->
                    { model | problems = [] }
                        |> UR.init
                        |> UR.addExt (FinishedEnteringPassphrase validModel)

                Err errors ->
                    { model | problems = errors }
                        |> UR.init


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
                |> UR.logImpossible msg [ "NoSignInResponse" ]

        GotSignInResult _ (RemoteData.Failure err) ->
            UR.init model
                |> UR.logGraphqlError msg err
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
            let
                ( pinModel, submitStatus ) =
                    Pin.update subMsg model.pinModel
            in
            { model | pinModel = pinModel }
                |> UR.init
                |> UR.addCmd (Pin.maybeSubmitCmd submitStatus SubmittedPinWithSuccess)



-- UTILS


passphraseValidator : Validator String PassphraseModel
passphraseValidator =
    Validate.fromErrors
        (\model ->
            let
                words =
                    String.words model.passphrase

                has12Words =
                    List.length words == 12

                allWordsHaveAtLeastThreeLetters =
                    List.all (\w -> String.length w > 2) words

                trPrefix s =
                    "auth.login.wordsMode.input." ++ s
            in
            if not has12Words then
                [ trPrefix "notPassphraseError" ]

            else if not allWordsHaveAtLeastThreeLetters then
                [ trPrefix "atLeastThreeLettersError" ]

            else
                []
        )


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

        EnteredPassphrase _ ->
            [ "EnteredPassphrase" ]

        ClickedNextStep ->
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
