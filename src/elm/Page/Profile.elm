module Page.Profile exposing (Model, Msg, init, jsAddressToMsg, msgToString, update, view)

import Api.Graphql
import Eos.Account
import Graphql.Http
import Html exposing (Html, button, div, input, label, p, span, text)
import Html.Attributes exposing (checked, class, for, id, name, style, type_)
import Html.Events exposing (onClick, onInput, stopPropagationOn)
import Http
import I18Next exposing (Translations, t)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Page
import Page.PublicProfile as PublicProfile
import Profile exposing (Profile)
import PushSubscription exposing (PushSubscription)
import Session.LoggedIn as LoggedIn exposing (External(..), FeedbackStatus(..))
import Task
import UpdateResult as UR



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    let
        profileQuery =
            Api.Graphql.query loggedIn.shared
                (Profile.query loggedIn.accountName)
                CompletedProfileLoad
    in
    ( initModel loggedIn
    , Cmd.batch
        [ profileQuery
        , Task.succeed CheckPushPref |> Task.perform identity
        ]
    )



-- MODEL


type alias Model =
    { status : Status
    , pinModal : ModalStatus
    , newPin : String
    , pushNotifications : Bool
    }


initModel : LoggedIn.Model -> Model
initModel _ =
    { status = Loading
    , pinModal = Hidden
    , newPin = ""
    , pushNotifications = False
    }


type Status
    = Loading
    | LoadingFailed (Graphql.Http.Error (Maybe Profile))
    | Loaded Profile


type ModalStatus
    = Hidden
    | Shown



-- VIEW


view : LoggedIn.Model -> Model -> Html Msg
view loggedIn model =
    case model.status of
        Loading ->
            Page.fullPageLoading

        LoadingFailed _ ->
            Page.fullPageError (t loggedIn.shared.translations "profile.title") Http.Timeout

        Loaded profile ->
            view_ model loggedIn profile


view_ : Model -> LoggedIn.Model -> Profile -> Html Msg
view_ model loggedIn profile =
    let
        ipfsUrl =
            loggedIn.shared.endpoints.ipfs

        text_ str =
            t loggedIn.shared.translations str

        downloadAction =
            case LoggedIn.maybePrivateKey loggedIn of
                Just privateKey ->
                    DownloadPdf privateKey

                Nothing ->
                    case loggedIn.shared.maybeAccount of
                        Just ( _, True ) ->
                            ClickedViewPrivateKeyAuth

                        _ ->
                            Ignored
    in
    div [ class "grid gap-6" ]
        [ PublicProfile.view_ loggedIn profile False
        , div [ class "bg-white" ]
            [ div [ class "container mx-auto p-4 px-8" ]
                [ viewAction (text_ "profile.12words.title") [ viewButton (text_ "profile.12words.button") downloadAction ]
                , viewAction (text_ "profile.pin.title") [ viewButton (text_ "profile.pin.button") ClickedChangePin ]
                , viewAction (text_ "notifications.title")
                    [ toggleView loggedIn.shared.translations model.pushNotifications RequestPush "notifications"
                    ]
                ]
            ]
        , viewModal model.pinModal loggedIn.shared.translations
        ]


viewModal : ModalStatus -> Translations -> Html Msg
viewModal status translations =
    let
        text_ str =
            t translations str
    in
    case status of
        Shown ->
            div
                [ class "modal container"
                , stopPropagationOn "click" (Decode.succeed ( Ignored, True ))
                , style "align-items" "stretch"
                ]
                [ div [ class "modal-bg", onClick ClickedCloseChangePin ] []
                , div [ class "modal-content overflow-auto" ]
                    [ div [ class "display flex flex-col justify-around h-full" ]
                        [ div []
                            [ p [ class "w-full font-medium text-heading text-2xl mb-2" ] [ text "Change PIN" ]
                            , p [ class "text-sm" ] [ text (text_ "profile.changePin") ]
                            ]
                        , div []
                            [ label [ class "input-label", for "newPin" ] [ text (text_ "profile.newPin") ]
                            , input [ id "newPin", class "input w-full mb-4", type_ "text", onInput EnteredPin ] []
                            ]
                        , button [ class "button button-primary w-full", onClick ChangedPin ] [ text "Change" ]
                        ]
                    ]
                ]

        Hidden ->
            text ""


viewButton : String -> Msg -> Html Msg
viewButton label msg =
    button
        [ class "uppercase border border-solid border-gray-500 rounded-full py-2 px-5 leading-none text-orange-300 font-medium"
        , onClick msg
        ]
        [ text label
        ]


viewAction : String -> List (Html Msg) -> Html Msg
viewAction label contents =
    div
        [ class "grid grid-cols-2 grid-rows-1"
        , style "grid-template-areas" "'key value'"
        ]
        [ span
            [ class "text-sm py-2 leading-6"
            , style "grid-area" "key"
            ]
            [ text label ]
        , span
            [ class "text-indigo-500 font-medium text-sm text-right py-2 leading-6"
            , style "grid-area" "value"
            ]
            contents
        ]


toggleView : Translations -> Bool -> Msg -> String -> Html Msg
toggleView translations isEnabled toggleFunction inputId =
    let
        translate =
            t translations

        classes =
            class "flex items-center"

        statusText =
            if isEnabled then
                translate "settings.features.enabled"

            else
                translate "settings.features.disabled"
    in
    div [ class "form-switch inline-block align-middle" ]
        [ input
            [ type_ "checkbox"
            , id inputId
            , name inputId
            , class "form-switch-checkbox"
            , checked isEnabled
            , onClick toggleFunction
            ]
            []
        , label [ class "form-switch-label", for inputId ] []
        ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = Ignored
    | CompletedProfileLoad (Result (Graphql.Http.Error (Maybe Profile)) (Maybe Profile))
    | ClickedChangePin
    | DownloadPdf String
    | ClickedCloseChangePin
    | ClickedViewPrivateKeyAuth
    | ChangedPin
    | EnteredPin String
    | GotPushPreference Bool
    | RequestPush
    | CheckPushPref
    | GotPushSub PushSubscription
    | CompletedPushUpload (Result (Graphql.Http.Error ()) ())


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    let
        downloadPdfPort passphrase =
            UR.addPort
                { responseAddress = Ignored
                , responseData = Encode.null
                , data =
                    Encode.object
                        [ ( "name", Encode.string "printAuthPdf" )
                        , ( "passphrase", Encode.string passphrase )
                        ]
                }
    in
    case msg of
        Ignored ->
            UR.init model

        CompletedProfileLoad (Ok Nothing) ->
            UR.init model

        CompletedProfileLoad (Ok (Just profile)) ->
            UR.init { model | status = Loaded profile }

        CompletedProfileLoad (Err err) ->
            UR.init { model | status = LoadingFailed err }
                |> UR.logGraphqlError msg err

        ClickedChangePin ->
            UR.init { model | pinModal = Shown }

        ChangedPin ->
            UR.init model
                |> (case LoggedIn.maybePrivateKey loggedIn of
                        Just key ->
                            UR.addPort
                                { responseAddress = ClickedCloseChangePin
                                , responseData = Encode.null
                                , data =
                                    Encode.object
                                        [ ( "name", Encode.string "changePin" )
                                        , ( "decryptedKey", Encode.string key )
                                        , ( "pin", Encode.string model.newPin )
                                        , ( "accountName"
                                          , loggedIn.accountName
                                                |> Eos.Account.nameToString
                                                |> Encode.string
                                          )
                                        ]
                                }

                        Nothing ->
                            UR.addExt (Just ChangedPin |> LoggedIn.RequiredAuthentication)
                   )

        EnteredPin newPin ->
            UR.init { model | newPin = newPin }

        ClickedCloseChangePin ->
            UR.init { model | pinModal = Hidden }

        ClickedViewPrivateKeyAuth ->
            case LoggedIn.maybePrivateKey loggedIn of
                Nothing ->
                    UR.init model
                        |> UR.addExt
                            (Just ClickedViewPrivateKeyAuth
                                |> RequiredAuthentication
                            )

                Just key ->
                    model
                        |> UR.init
                        |> downloadPdfPort key

        DownloadPdf passphrase ->
            model
                |> UR.init
                |> downloadPdfPort passphrase

        GotPushPreference val ->
            { model | pushNotifications = val }
                |> UR.init

        CheckPushPref ->
            model
                |> UR.init
                |> UR.addPort
                    { responseAddress = GotPushPreference False
                    , responseData = Encode.null
                    , data =
                        Encode.object [ ( "name", Encode.string "checkPushPref" ) ]
                    }

        RequestPush ->
            if model.pushNotifications then
                model
                    |> UR.init
                    |> UR.addPort
                        { responseAddress = GotPushPreference False
                        , responseData = Encode.null
                        , data =
                            Encode.object [ ( "name", Encode.string "disablePushPref" ) ]
                        }

            else
                model
                    |> UR.init
                    |> UR.addPort
                        { responseAddress = RequestPush
                        , responseData = Encode.null
                        , data =
                            Encode.object
                                [ ( "name", Encode.string "requestPushPermission" ) ]
                        }

        GotPushSub push ->
            model
                |> UR.init
                |> UR.addCmd
                    (uploadPushSubscription loggedIn push)

        CompletedPushUpload res ->
            case res of
                Ok _ ->
                    model
                        |> UR.init
                        |> UR.addPort
                            { responseAddress = CompletedPushUpload res
                            , responseData = Encode.null
                            , data =
                                Encode.object
                                    [ ( "name", Encode.string "completedPushUpload" ) ]
                            }

                Err err ->
                    model
                        |> UR.init
                        |> UR.logGraphqlError msg err


decodePushPref : Value -> Maybe Msg
decodePushPref val =
    Decode.decodeValue (Decode.field "isSet" Decode.bool) val
        |> Result.map GotPushPreference
        |> Result.toMaybe


uploadPushSubscription : LoggedIn.Model -> PushSubscription -> Cmd Msg
uploadPushSubscription { accountName, shared } data =
    Api.Graphql.mutation shared
        (PushSubscription.activatePushMutation accountName data)
        CompletedPushUpload


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "ClickedCloseChangePin" :: [] ->
            Just ClickedCloseChangePin

        "RequestPush" :: _ ->
            let
                push =
                    Decode.decodeValue (Decode.field "sub" Decode.string) val
                        |> Result.andThen (Decode.decodeString Decode.value)
                        |> Result.andThen (Decode.decodeValue PushSubscription.decode)
            in
            case push of
                Ok res ->
                    Just (GotPushSub res)

                Err _ ->
                    -- TODO: Handle PushSubscription Decode error
                    Nothing

        "ChangedPin" :: [] ->
            Just ChangedPin

        "GotPushPreference" :: _ ->
            decodePushPref val

        "CompletedPushUpload" :: _ ->
            decodePushPref val

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        Ignored ->
            [ "Ignored" ]

        CompletedProfileLoad r ->
            [ "CompletedProfileLoad", UR.resultToString r ]

        DownloadPdf r ->
            [ "DownloadPdf" ]

        ClickedChangePin ->
            [ "ClickedChangePin" ]

        ClickedCloseChangePin ->
            [ "ClickedCloseChangePin" ]

        ClickedViewPrivateKeyAuth ->
            [ "ClickedViewPrivateKeyAuth" ]

        ChangedPin ->
            [ "ChangedPin" ]

        EnteredPin r ->
            [ "EnteredPin" ]

        GotPushPreference r ->
            [ "GotPushPreference" ]

        RequestPush ->
            [ "RequestPush" ]

        CheckPushPref ->
            [ "CheckPushPref" ]

        GotPushSub _ ->
            [ "GotPushSub" ]

        CompletedPushUpload _ ->
            [ "CompletedPushUpload" ]
