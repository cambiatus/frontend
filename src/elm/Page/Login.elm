module Page.Login exposing (Model, Msg, init, jsAddressToMsg, msgToString, subscriptions, update, view)

import Browser.Dom as Dom
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, a, button, div, form, img, label, li, p, span, strong, text, textarea, ul)
import Html.Attributes exposing (autocomplete, autofocus, class, classList, disabled, for, id, placeholder, required, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Route
import Session.Guest as Guest exposing (External(..))
import Session.Shared exposing (Shared, Translators)
import Task
import UpdateResult as UR
import Utils
import Validate exposing (Validator)
import View.Form
import View.Pin as Pin



-- TODO - Try to use View.Form
-- INIT


init : Guest.Model -> ( Model, Cmd Msg )
init _ =
    ( CreatingPassphrase initPassphraseModel
    , Cmd.none
    )


initPassphraseModel : PassphraseModel
initPassphraseModel =
    { hasPasted = False
    , passphrase = ""
    , problems = []
    }


initPinModel : PinModel
initPinModel =
    { isSigningIn = False
    , pin = ""
    , pinConfirmation = ""
    , isPinVisible = True
    , isPinConfirmationVisible = True
    , problems = []
    , confirmationProblems = []
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map KeyPressed (onKeyDown Utils.decodeEnterKeyDown)



-- MODEL


type Model
    = CreatingPassphrase PassphraseModel
    | CreatingPin PinModel


type alias PassphraseModel =
    { hasPasted : Bool
    , passphrase : String
    , problems : List String
    }


type alias PinModel =
    { isSigningIn : Bool
    , pin : String
    , pinConfirmation : String
    , isPinVisible : Bool
    , isPinConfirmationVisible : Bool
    , problems : List String
    , confirmationProblems : List String
    }



-- VIEW


view : Guest.Model -> Model -> { title : String, content : Html Msg }
view guest model =
    { title =
        guest.shared.translators.t "auth.login.loginTab"
    , content =
        div [ class "bg-purple-500 flex-grow flex flex-wrap md:block" ]
            [ div [ class "sf-wrapper w-full px-4 md:max-w-sm md:mx-auto md:pt-20 md:px-0" ]
                [ case model of
                    CreatingPassphrase passphraseModel ->
                        viewCreatingPassphrase guest passphraseModel
                            |> Html.map GotPassphraseMsg

                    CreatingPin pinModel ->
                        viewCreatingPin guest pinModel
                            |> Html.map GotPinMsg
                ]
            ]
    }


viewCreatingPassphrase : Guest.Model -> PassphraseModel -> Html PassphraseMsg
viewCreatingPassphrase { shared } model =
    let
        { t, tr } =
            shared.translators

        passphraseId =
            "passphrase"

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

        errors =
            model.problems
                |> List.map (\error -> li [] [ text (t error) ])

        passphraseWordsCount =
            model.passphrase
                |> String.words
                |> List.filter (not << String.isEmpty)
                |> List.length
                |> String.fromInt
    in
    div []
        [ form [ class "sf-content" ]
            [ viewIllustration "login_key.svg"
            , p [ class "text-white text-body mb-5" ]
                [ span [ class "text-green text-caption tracking-wide uppercase block mb-1" ]
                    [ text (t "menu.my_communities") ]
                , span [ class "text-white block leading-relaxed" ]
                    [ text (t "auth.login.wordsMode.input.description") ]
                ]
            , viewFieldLabel shared.translators "auth.login.wordsMode.input" passphraseId
            , div [ class "relative" ]
                [ textarea
                    [ class "form-textarea min-h-19 min-w-full block"
                    , classList [ ( "pb-16", shared.canReadClipboard ) ]
                    , placeholder (t "auth.login.wordsMode.input.placeholder")
                    , View.Form.noGrammarly
                    , autofocus True
                    , class <|
                        if List.isEmpty model.problems then
                            ""

                        else
                            "field-with-error"
                    , id passphraseId
                    , value model.passphrase
                    , onInput EnteredPassphrase
                    , onSubmit ClickedNextStep
                    , required True
                    , autocomplete False
                    ]
                    []
                , viewPasteButton
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
                [ class "button button-primary min-w-full mb-8"
                , onClick ClickedNextStep
                ]
                [ text (t "dashboard.next") ]
            ]
        ]


viewCreatingPin : Guest.Model -> PinModel -> Html PinMsg
viewCreatingPin { shared } model =
    let
        trPrefix s =
            "auth.pin.instruction." ++ s

        { t } =
            shared.translators
    in
    div []
        [ div [ class "sf-content" ]
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
            , viewPin model shared
            , viewPinConfirmation model shared
            ]
        , div [ class "sf-footer" ]
            [ button
                [ class "button button-primary min-w-full mb-8"
                , class "mt-10"
                , disabled model.isSigningIn

                -- , onClick (SubmittedForm model.form)
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


viewPin : PinModel -> Shared -> Html PinMsg
viewPin model shared =
    Pin.view
        shared
        { labelText = shared.translators.t "auth.pin.label"
        , inputId = "pinInput"
        , inputValue = model.pin
        , onInputMsg = EnteredPin
        , onToggleMsg = ToggledPinVisibility
        , isVisible = model.isPinVisible
        , errors = model.problems
        }


viewPinConfirmation : PinModel -> Shared -> Html PinMsg
viewPinConfirmation model shared =
    Pin.view
        shared
        { labelText = shared.translators.t "auth.pinConfirmation.label"
        , inputId = "pinInputConfirmation"
        , inputValue = model.pinConfirmation
        , onInputMsg = EnteredPinConfirmation
        , onToggleMsg = ToggledPinConfirmationVisibility
        , isVisible = model.isPinConfirmationVisible
        , errors = model.confirmationProblems
        }


viewFieldLabel : Translators -> String -> String -> Html msg
viewFieldLabel { t } tSuffix id_ =
    label
        [ class "block"
        , for id_
        ]
        [ span [ class "text-green tracking-wide uppercase text-caption block mb-1" ]
            [ text <| t (tSuffix ++ ".label") ]
        ]


viewIllustration : String -> Html msg
viewIllustration fileName =
    img [ class "h-40 mx-auto mt-8 mb-7", src ("images/" ++ fileName) ] []



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg External


type alias PassphraseUpdateResult =
    UR.UpdateResult PassphraseModel PassphraseMsg PassphraseExternalMsg


type alias PinUpdateResult =
    UR.UpdateResult PinModel PinMsg ()


type Msg
    = KeyPressed Bool
    | WentToPin
    | GotPassphraseMsg PassphraseMsg
    | GotPinMsg PinMsg


type PassphraseMsg
    = Ignored
    | ClickedPaste
    | GotClipboardContent (Maybe String)
    | EnteredPassphrase String
    | ClickedNextStep


type PassphraseExternalMsg
    = FinishedEnteringPassphrase


type PinMsg
    = EnteredPin String
    | EnteredPinConfirmation String
    | ToggledPinVisibility
    | ToggledPinConfirmationVisibility


update : Msg -> Model -> Guest.Model -> UpdateResult
update msg model _ =
    case ( msg, model ) of
        ( KeyPressed isEnter, CreatingPassphrase _ ) ->
            let
                cmd =
                    if isEnter then
                        GotPassphraseMsg ClickedNextStep
                            |> Task.succeed
                            |> Task.perform identity

                    else
                        Cmd.none
            in
            UR.init model
                |> UR.addCmd cmd

        ( KeyPressed _, CreatingPin _ ) ->
            UR.init model

        ( GotPassphraseMsg passphraseMsg, CreatingPassphrase passphraseModel ) ->
            updateWithPassphrase passphraseMsg passphraseModel
                |> UR.map CreatingPassphrase
                    GotPassphraseMsg
                    (\ext ur ->
                        case ext of
                            FinishedEnteringPassphrase ->
                                ur
                                    |> UR.addCmd
                                        (Task.succeed WentToPin
                                            |> Task.perform identity
                                        )
                    )

        ( WentToPin, CreatingPassphrase _ ) ->
            initPinModel
                |> CreatingPin
                |> UR.init

        ( GotPinMsg pinMsg, CreatingPin pinModel ) ->
            updateWithPin pinMsg pinModel
                -- TODO
                |> UR.map CreatingPin GotPinMsg (\ext ur -> ur)

        -- Impossible Msgs
        ( GotPassphraseMsg _, CreatingPin _ ) ->
            UR.init model
                |> UR.logImpossible msg [ "CreatingPin" ]

        ( WentToPin, CreatingPin _ ) ->
            UR.init model
                |> UR.logImpossible msg [ "CreatingPin" ]

        ( GotPinMsg _, CreatingPassphrase _ ) ->
            UR.init model
                |> UR.logImpossible msg [ "CreatingPassphrase" ]


updateWithPassphrase : PassphraseMsg -> PassphraseModel -> PassphraseUpdateResult
updateWithPassphrase msg model =
    case msg of
        Ignored ->
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
                        |> Task.attempt (\_ -> Ignored)
                    )

        GotClipboardContent (Just content) ->
            { model
                | passphrase = String.trim content
                , hasPasted = True
                , problems = []
            }
                |> UR.init

        GotClipboardContent Nothing ->
            UR.init model
                |> UR.logImpossible msg [ "ClipboardApiNotSupported" ]

        EnteredPassphrase passphrase ->
            { model
                | passphrase = passphrase
                , hasPasted = False
            }
                |> UR.init

        ClickedNextStep ->
            case Validate.validate passphraseValidator model of
                Ok _ ->
                    { model | problems = [] }
                        |> UR.init
                        |> UR.addExt FinishedEnteringPassphrase

                Err errors ->
                    { model | problems = errors }
                        |> UR.init


updateWithPin : PinMsg -> PinModel -> PinUpdateResult
updateWithPin msg model =
    case msg of
        EnteredPin pin ->
            { model | pin = pin }
                |> UR.init

        EnteredPinConfirmation pinConfirmation ->
            { model | pinConfirmation = pinConfirmation }
                |> UR.init

        ToggledPinVisibility ->
            { model | isPinVisible = not model.isPinVisible }
                |> UR.init

        ToggledPinConfirmationVisibility ->
            { model | isPinConfirmationVisible = not model.isPinConfirmationVisible }
                |> UR.init



-- UTILS


passphraseValidator : Validator String PassphraseModel
passphraseValidator =
    Validate.fromErrors
        (\form ->
            let
                words =
                    String.words form.passphrase

                has12words =
                    List.length words == 12

                allWordsHaveAtLeastThreeLetters =
                    List.all (\w -> String.length w > 2) words

                trPrefix s =
                    "auth.login.wordsMode.input." ++ s
            in
            if not has12words then
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
                (Decode.succeed (GotPassphraseMsg << GotClipboardContent)
                    |> Decode.required "clipboardContent" (Decode.nullable Decode.string)
                )
                val
                |> Result.toMaybe

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        KeyPressed _ ->
            [ "KeyPressed" ]

        WentToPin ->
            [ "WentToPin" ]

        GotPassphraseMsg passphraseMsg ->
            "GotPassphraseMsg" :: passphraseMsgToString passphraseMsg

        GotPinMsg pinMsg ->
            "GotPinMsg" :: pinMsgToString pinMsg


passphraseMsgToString : PassphraseMsg -> List String
passphraseMsgToString msg =
    case msg of
        Ignored ->
            [ "Ignored" ]

        ClickedPaste ->
            [ "ClickedPaste" ]

        GotClipboardContent _ ->
            [ "GotClipboardContent" ]

        EnteredPassphrase _ ->
            [ "EnteredPassphrase" ]

        ClickedNextStep ->
            [ "ClickedNextStep" ]


pinMsgToString : PinMsg -> List String
pinMsgToString msg =
    case msg of
        EnteredPin _ ->
            [ "EnteredPin" ]

        EnteredPinConfirmation _ ->
            [ "EnteredPinConfirmation" ]

        ToggledPinVisibility ->
            [ "ToggledPinVisibility" ]

        ToggledPinConfirmationVisibility ->
            [ "ToggledPinConfirmationVisibility" ]
