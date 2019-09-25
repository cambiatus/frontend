module Flags exposing (AuthPreference(..), Endpoints, Environment(..), Flags, decode, default, defaultEndpoints)

import Eos
import Eos.Account as Eos
import Json.Decode as Decode exposing (Decoder, int, nullable, string)
import Json.Decode.Pipeline as Decode exposing (hardcoded, optional, required)


type alias Flags =
    { environment : Environment
    , language : String
    , maybeAccount : Maybe ( Eos.Name, Bool )
    , authPreference : Maybe AuthPreference
    , endpoints : Endpoints
    , logo : String
    , logoMobile : String
    , now : Int
    }


default : Flags
default =
    { environment = Development
    , language = "en-US"
    , maybeAccount = Nothing
    , authPreference = Nothing
    , endpoints = defaultEndpoints
    , logo = "/images/logo-cambiatus.svg"
    , logoMobile = "/images/logo-cambiatus-mobile.svg"
    , now = 0
    }


decode : Decoder Flags
decode =
    Decode.succeed Flags
        |> required "env" decodeEnvironment
        |> required "language" string
        |> Decode.custom
            (Decode.succeed
                (Maybe.map2 (\acc auth -> ( acc, auth )))
                |> optional "accountName" (nullable Eos.nameDecoder) Nothing
                |> optional "isPinAvailable"
                    (nullable Decode.bool)
                    Nothing
            )
        |> optional "authPreference" decodeAuthPreference Nothing
        |> required "endpoints" decodeEndpoints
        |> required "logo" Decode.string
        |> required "logoMobile" Decode.string
        |> required "now" Decode.int


type alias Endpoints =
    { eosio : String
    , api : String
    , chat : String
    , graphql : String
    , ipfs : String
    }


defaultEndpoints : Endpoints
defaultEndpoints =
    { eosio = "https://eosio.cambiatus.io"
    , api = "https://api.cambiatus.io"
    , chat = "https://app.cambiatus.io/chat"
    , graphql = "https://api.cambiatus.io/api/graph"
    , ipfs = "https://ipfs.cambiatus.io/ipfs"
    }


decodeEndpoints : Decoder Endpoints
decodeEndpoints =
    Decode.succeed Endpoints
        |> required "eosio" string
        |> required "api" string
        |> required "chat" string
        |> required "graphql" string
        |> required "ipfs" string


type Environment
    = Development
    | Production


decodeEnvironment : Decoder Environment
decodeEnvironment =
    Decode.map
        (\env ->
            if env == "development" then
                Development

            else
                Production
        )
        string


type AuthPreference
    = Pin
    | Scatter


decodeAuthPreference : Decoder (Maybe AuthPreference)
decodeAuthPreference =
    Decode.map
        (\s ->
            case s of
                "pin" ->
                    Just Pin

                "scatter" ->
                    Just Scatter

                _ ->
                    Nothing
        )
        Decode.string
        |> nullable
        |> Decode.map (Maybe.withDefault Nothing)
