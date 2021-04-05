module Auth exposing
    ( ExternalMsg(..)
    , Model
    , Msg
    , SignInResponse
    , hasPrivateKey
    , init
    , jsAddressToMsg
    , maybePrivateKey
    , msgToString
    , signIn
    , update
    , view
    , viewFieldLabel
    )

import Api.Graphql
import Cambiatus.Mutation
import Cambiatus.Object.Session
import Eos.Account as Eos
import Graphql.Http
import Graphql.Operation exposing (RootMutation)
import Graphql.OptionalArgument as OptionalArgument
import Graphql.SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, button, div, label, p, span, text)
import Html.Attributes exposing (class, disabled, for)
import Html.Events exposing (onSubmit)
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Log
import Ports
import Profile exposing (Model)
import RemoteData exposing (RemoteData)
import Session.Shared exposing (Shared, Translators)
import UpdateResult as UR
import View.Feedback as Feedback
import View.Pin as Pin


{-| Authentication of a user with Passphrase or Private Key.

  - Passphrase consists of 12 unique words given to the user during the registration. Only users knows this phrase.
  - Private Key (PK) is a hashed analogue of a Passphrase.
  - PIN is used to encrypt the Passphrase/PK in the browser. Each time the user logs-in the new PIN is created.

User may use Passphrase and PK interchangeable for logging in, although we push the user forward to use the Passphrase
because it's more convenient for humans.

-}



-- INIT


init : Maybe PrivateKey -> Model
init maybePrivateKey_ =
    case maybePrivateKey_ of
        Nothing ->
            initModel WithoutPrivateKey

        Just privateKey ->
            initModel (WithPrivateKey privateKey)



-- MODEL


type alias Model =
    { status : Status
    , pin : String
    , error : Maybe String
    , isSigningIn : Bool
    , pinVisibility : Bool
    }


initModel : Status -> Model
initModel status =
    { status = status
    , pin = ""
    , error = Nothing
    , isSigningIn = False
    , pinVisibility = True
    }


{-| Represents the state of the user's authentication. A user can be:

  - Authenticated, but without theprivate key - This means the user is logged in,
    but we don't have their PIN, so we can't get their private key. If we ever need
    the user's private key, we should prompt them for their PIN
  - Authenticated, with private key - This means the user is logged in and we
    have access to their private key. The user can perform actions that need
    authentication without having to type in their PIN

-}
type Status
    = WithoutPrivateKey
    | WithPrivateKey PrivateKey


type alias PrivateKey =
    String


hasPrivateKey : Model -> Bool
hasPrivateKey model =
    case model.status of
        WithPrivateKey _ ->
            True

        WithoutPrivateKey ->
            False


maybePrivateKey : Model -> Maybe String
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
    -- TODO - Better visualization for when isSigningIn
    let
        { t } =
            shared.translators

        isDisabled =
            model.isSigningIn || hasPrivateKey model
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
    | EnteredPin String
    | TogglePinVisibility
      -- Submission
    | SubmittedLoginPIN
    | CompletedSignIn Status (RemoteData (Graphql.Http.Error (Maybe SignInResponse)) (Maybe SignInResponse))
      -- Response
    | GotMultipleAccountsLogin (List Eos.Name)
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
            { model | pin = trimPinNumber 6 model.pin pin, error = Nothing }
                |> UR.init

        Ignored ->
            UR.init model

        GotMultipleAccountsLogin _ ->
            UR.init
                { model
                    | status =
                        case model.status of
                            _ ->
                                model.status
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
            model
                |> loginFailed
                |> UR.addCmd (Log.graphqlError err)
                |> UR.addPort
                    { responseAddress = Ignored
                    , responseData = Encode.null
                    , data = Encode.object [ ( "name", Encode.string "logout" ) ]
                    }
                |> addError (t "auth.failed")

        CompletedSignIn _ _ ->
            UR.init model

        SubmittedLoginPIN ->
            let
                pinString =
                    model.pin
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
    uResult
        |> UR.mapModel (\m -> { m | error = Just error })


loginFailed : Model -> UpdateResult
loginFailed model =
    { model
        | status =
            case model.status of
                WithoutPrivateKey ->
                    WithoutPrivateKey

                WithPrivateKey _ ->
                    WithoutPrivateKey
        , pin = ""
        , error = Nothing
        , isSigningIn = False
    }
        |> UR.init


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "SubmittedLoginPIN" :: [] ->
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.succeed Tuple.pair
                        |> Decode.required "accountName" Eos.nameDecoder
                        |> Decode.required "privateKey" Decode.string
                        |> Decode.map (Ok >> GotPinLogin)

                    -- TODO - Is this still needed?
                    , Decode.field "accountNames" (Decode.list Eos.nameDecoder)
                        |> Decode.map GotMultipleAccountsLogin
                    , Decode.field "error" Decode.string
                        |> Decode.map (Err >> GotPinLogin)
                    ]
                )
                val
                |> Result.toMaybe

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        Ignored ->
            [ "Ignored" ]

        CompletedSignIn _ _ ->
            [ "CompletedSignIn" ]

        GotMultipleAccountsLogin _ ->
            [ "GotMultipleAccountsLogin" ]

        GotPrivateKeyLogin r ->
            [ "GotPrivateKeyLogin", UR.resultToString r ]

        SubmittedLoginPIN ->
            [ "SubmittedLoginPIN" ]

        GotPinLogin r ->
            [ "GotPinLogin", UR.resultToString r ]

        EnteredPin _ ->
            [ "EnteredPin" ]

        TogglePinVisibility ->
            [ "TogglePinVisibility" ]


viewPin : Model -> Shared -> Html Msg
viewPin model shared =
    let
        pinLabel =
            shared.translators.t "auth.pinPopup.label"

        errors =
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
        , inputValue = model.pin
        , onInputMsg = EnteredPin
        , onToggleMsg = TogglePinVisibility
        , isVisible = model.pinVisibility
        , errors = errors
        }
