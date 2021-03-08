module Flags exposing
    ( Endpoints
    , Environment(..)
    , Flags
    , decode
    , default
    , defaultEndpoints
    )

import Eos exposing (Symbol)
import Eos.Account as Eos
import Json.Decode as Decode exposing (Decoder, nullable, string)
import Json.Decode.Pipeline as Decode exposing (optional, required)


type alias Flags =
    { environment : Environment
    , language : String
    , maybeAccount : Maybe ( Eos.Name, Bool )
    , endpoints : Endpoints
    , logo : String
    , logoMobile : String
    , now : Int
    , allowCommunityCreation : Bool
    , selectedCommunity : Symbol
    , tokenContract : String
    , communityContract : String
    , graphqlSecret : String
    , authToken : Maybe String
    }


default : Flags
default =
    { environment = Development
    , language = "en-US"
    , maybeAccount = Nothing
    , endpoints = defaultEndpoints
    , logo = "/images/logo-cambiatus.png"
    , logoMobile = "/images/logo-cambiatus-mobile.svg"
    , now = 0
    , allowCommunityCreation = True
    , selectedCommunity = Eos.cambiatusSymbol
    , tokenContract = "bes.token"
    , communityContract = "bes.cmm"
    , graphqlSecret = ""
    , authToken = Nothing
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
        |> required "endpoints" decodeEndpoints
        |> required "logo" Decode.string
        |> required "logoMobile" Decode.string
        |> required "now" Decode.int
        |> required "allowCommunityCreation" Decode.bool
        |> required "selectedCommunity" Eos.symbolDecoder
        |> required "tokenContract" Decode.string
        |> required "communityContract" Decode.string
        |> required "graphqlSecret" Decode.string
        |> required "authToken" (Decode.nullable Decode.string)


type alias Endpoints =
    { eosio : String
    , api : String
    , graphql : String
    }


defaultEndpoints : Endpoints
defaultEndpoints =
    { eosio = "https://eosio.cambiatus.io"
    , api = "https://api.cambiatus.io"
    , graphql = "https://api.cambiatus.io/api/graph"
    }


decodeEndpoints : Decoder Endpoints
decodeEndpoints =
    Decode.succeed Endpoints
        |> required "eosio" string
        |> required "api" string
        |> required "graphql" string


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
