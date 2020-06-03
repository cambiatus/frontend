module Page.Profile exposing (Model, Msg, init, jsAddressToMsg, msgToString, update, view)

import Api
import Api.Graphql
import Asset.Icon as Icon
import Avatar exposing (Avatar)
import Browser.Events
import Dict exposing (Dict)
import Eos as Eos exposing (Symbol)
import Eos.Account as Eos
import File exposing (File)
import Graphql.Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import I18Next exposing (t)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Page
import Page.PublicProfile as PublicProfile
import Profile exposing (Profile, ProfileForm, decode)
import PushSubscription exposing (PushSubscription)
import Session.LoggedIn as LoggedIn exposing (External(..), FeedbackStatus(..))
import Task
import UpdateResult as UR
import Utils



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
        ]
    )



-- MODEL


type alias Model =
    { status : Status
    , pinModal : ModalStatus
    , newPin : String
    }


initModel : LoggedIn.Model -> Model
initModel _ =
    { status = Loading
    , pinModal = Hidden
    , newPin = ""
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
            text (t loggedIn.shared.translations str)

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
                [ viewAction "My 12 words" [ viewButton "Download" downloadAction ]
                , viewAction "My security PIN" [ viewButton "Change" ClickedChangePin ]
                , viewAction "Notifications" [ text "hi" ]
                ]
            ]
        , viewModal model.pinModal
        ]


viewModal : ModalStatus -> Html Msg
viewModal status =
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
                            , p [ class "text-sm" ] [ text "Change your pin!" ]
                            ]
                        , div []
                            [ label [ class "input-label", for "newPin" ] [ text "New security pin" ]
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


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
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
                |> UR.addPort
                    { responseAddress = ChangedPin
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "loginWithPin" )
                            , ( "pin", Encode.string model.newPin )
                            ]
                    }

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

                Just _ ->
                    UR.init model

        DownloadPdf passPhrase ->
            model
                |> UR.init
                |> UR.addPort
                    { responseAddress = Ignored
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "printAuthPdf" )
                            , ( "passphrase", Encode.string passPhrase )
                            ]
                    }


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
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
