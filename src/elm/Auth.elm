module Auth exposing
    ( ExternalMsg(..)
    , Model
    , Msg
    , hasPrivateKey
    , init
    , jsAddressToMsg
    , maybePrivateKey
    , msgToString
    , removePrivateKey
    , update
    , view
    )

{-| Authentication of a user with Passphrase or Private Key.

  - Passphrase consists of 12 unique words given to the user during the registration. Only users knows this phrase.
  - Private Key (PK) is a hashed analogue of a Passphrase.
  - PIN is used to encrypt the Passphrase/PK in the browser. Each time the user logs-in the new PIN is created.

This module handles already logged in users, so we already know we have all the data we need in localStorage.
For more information on the login process, checkout the Login module.

After having logged in at least once, the data we need is stored in localStorage.
If we need to prove the user is authenticated (and haven't done so in this session),
we can request the user to type in the PIN they created when signing in the last time,
so we can retrieve their Private Key from localStorage.

This module concerns mostly with authenticating with Private Key. That means it
handles authentication for EOS, not for our Graphql API. That is mainly dealt with
in the `LoggedIn` and `Api.Graphql` modules, but since that depends on the user's private key, this
is also related to it.

-}

import Dict
import Eos.Account as Eos
import Html exposing (Html, p, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipeline
import Json.Encode as Encode exposing (Value)
import Log
import Session.Shared exposing (Shared)
import UpdateResult as UR
import View.Feedback as Feedback
import View.Pin as Pin



-- INIT


init : Bool -> Maybe Eos.PrivateKey -> ( Model, Cmd Msg )
init pinVisibility maybePrivateKey_ =
    let
        status =
            case maybePrivateKey_ of
                Nothing ->
                    WithoutPrivateKey

                Just privateKey ->
                    WithPrivateKey privateKey

        ( pinModel, pinCmd ) =
            initPinModel pinVisibility status
    in
    ( { status = status
      , error = Nothing
      , pinModel = pinModel
      }
    , pinCmd
    )



-- MODEL


type alias Model =
    { status : Status
    , error : Maybe String
    , pinModel : Pin.Model
    }


initPinModel : Bool -> Status -> ( Pin.Model, Cmd Msg )
initPinModel pinVisibility status =
    let
        ( pinModel, pinCmd ) =
            Pin.init
                { label = "auth.pinPopup.label"
                , id = "pinPopup"
                , withConfirmation = False
                , submitLabel = "auth.login.continue"
                , submittingLabel = "auth.login.continue"
                , pinVisibility = pinVisibility
                }
    in
    ( pinModel
        |> Pin.withDisabled
            (case status of
                WithoutPrivateKey ->
                    False

                WithPrivateKey _ ->
                    True
            )
    , Cmd.map GotPinMsg pinCmd
    )


{-| Represents the state of the user's authentication. A user can be:

  - Authenticated, but without the private key - This means the user is logged in,
    but we don't have their PIN, so we can't get their private key. If we ever need
    the user's private key, we should prompt them for their PIN
  - Authenticated, with private key - This means the user is logged in and we
    have access to their private key. The user can perform actions that need
    authentication without having to type in their PIN

-}
type Status
    = WithoutPrivateKey
    | WithPrivateKey Eos.PrivateKey


hasPrivateKey : Model -> Bool
hasPrivateKey model =
    case model.status of
        WithPrivateKey _ ->
            True

        WithoutPrivateKey ->
            False


removePrivateKey : Model -> Model
removePrivateKey model =
    { model | status = WithoutPrivateKey }


maybePrivateKey : Model -> Maybe Eos.PrivateKey
maybePrivateKey model =
    case model.status of
        WithPrivateKey pk ->
            Just pk

        WithoutPrivateKey ->
            Nothing



-- VIEW


{-| Modal that asks for the user's PIN whenever needed. They must be authenticated
already (`WithPrivateKey` or `WithoutPrivateKey`)
-}
view : Shared -> Model -> List (Html Msg)
view shared model =
    let
        { t } =
            shared.translators
    in
    [ p [ class "text-sm" ]
        [ text <| t "auth.login.enterPinToContinue" ]
    , Pin.view shared.translators model.pinModel
        |> Html.map GotPinMsg
    ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg ExternalMsg


type Msg
    = GotPinMsg Pin.Msg
    | SubmittedPin String
    | GotPrivateKey (Result String ( Eos.Name, Eos.PrivateKey ))


type ExternalMsg
    = CompletedAuth Eos.Name Model
    | UpdatedShared Shared
    | SetFeedback Feedback.Model


update : Msg -> Shared -> Model -> UpdateResult
update msg shared model =
    case msg of
        GotPinMsg subMsg ->
            Pin.update shared subMsg model.pinModel
                |> UR.fromChild (\pinModel -> { model | pinModel = pinModel })
                    GotPinMsg
                    (\ext ur ->
                        case ext of
                            Pin.SendFeedback feedback ->
                                UR.addExt (SetFeedback feedback) ur

                            Pin.SubmitPin pin ->
                                let
                                    ( newShared, submitCmd ) =
                                        Pin.postSubmitAction ur.model.pinModel
                                            pin
                                            shared
                                            SubmittedPin
                                in
                                ur
                                    |> UR.addCmd submitCmd
                                    |> UR.addExt (UpdatedShared newShared)
                    )
                    model

        SubmittedPin pin ->
            { model | pinModel = Pin.withDisabled True model.pinModel }
                |> UR.init
                |> UR.addPort
                    { responseAddress = SubmittedPin pin
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "getPrivateKey" )
                            , ( "pin", Encode.string pin )
                            ]
                    }

        GotPrivateKey (Ok ( accountName, privateKey )) ->
            let
                newModel =
                    { model
                        | status = WithPrivateKey privateKey
                        , pinModel = Pin.withDisabled False model.pinModel
                    }
            in
            newModel
                |> UR.init
                |> UR.addExt (CompletedAuth accountName newModel)

        GotPrivateKey (Err err) ->
            model
                |> authFailed err
                |> UR.logEvent
                    { username = Nothing
                    , message = "Got an error when submitting PIN"
                    , tags = [ Log.TypeTag Log.UnknownError ]
                    , location =
                        { moduleName = "Auth"
                        , function = "update"
                        }
                    , contexts =
                        [ { name = "Error"
                          , extras = Dict.fromList [ ( "errorValue", Encode.string err ) ]
                          }
                        ]
                    , transaction = msg
                    , level = Log.Error
                    }


authFailed : String -> Model -> UpdateResult
authFailed error model =
    { model
        | status = WithoutPrivateKey
        , error = Nothing
        , pinModel =
            model.pinModel
                |> Pin.withProblem Pin.Pin error
    }
        |> UR.init


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "SubmittedPin" :: [] ->
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.succeed Tuple.pair
                        |> DecodePipeline.required "accountName" Eos.nameDecoder
                        |> DecodePipeline.required "privateKey" Eos.privateKeyDecoder
                        |> Decode.map (Ok >> GotPrivateKey)
                    , Decode.field "error" Decode.string
                        |> Decode.map (Err >> GotPrivateKey)
                    ]
                )
                val
                |> Result.toMaybe

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        SubmittedPin _ ->
            [ "SubmittedPin" ]

        GotPrivateKey r ->
            [ "GotPrivateKey", UR.resultToString r ]

        GotPinMsg subMsg ->
            "GotPinMsg" :: Pin.msgToString subMsg
