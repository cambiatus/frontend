module Flags exposing
    ( Endpoints
    , Environment(..)
    , Flags
    , decode
    , default
    )

import Eos
import Eos.Account as Eos
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as DecodePipeline exposing (optional, required)


type alias Flags =
    { environment : Environment
    , language : String
    , maybeAccount : Maybe ( Eos.Name, Bool )
    , endpoints : Endpoints
    , logo : String
    , logoMobile : String
    , now : Int
    , allowCommunityCreation : Bool
    , tokenContract : String
    , communityContract : String
    , graphqlSecret : String
    , authToken : Maybe String
    , canReadClipboard : Bool
    , useSubdomain : Bool
    , selectedCommunity : Maybe Eos.Symbol
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
    , tokenContract = "bes.token"
    , communityContract = "bes.cmm"
    , graphqlSecret = ""
    , authToken = Nothing
    , canReadClipboard = False
    , useSubdomain = True
    , selectedCommunity = Nothing
    }


decode : Decoder Flags
decode =
    Decode.succeed Flags
        |> required "env" decodeEnvironment
        |> required "language" Decode.string
        |> DecodePipeline.custom
            (Decode.succeed
                (Maybe.map2 (\acc auth -> ( acc, auth )))
                |> optional "accountName" (Decode.nullable Eos.nameDecoder) Nothing
                |> optional "isPinAvailable"
                    (Decode.nullable Decode.bool)
                    Nothing
            )
        |> required "endpoints" decodeEndpoints
        |> required "logo" Decode.string
        |> required "logoMobile" Decode.string
        |> required "now" Decode.int
        |> required "allowCommunityCreation" Decode.bool
        |> required "tokenContract" Decode.string
        |> required "communityContract" Decode.string
        |> required "graphqlSecret" Decode.string
        |> required "authToken" (Decode.nullable Decode.string)
        |> required "canReadClipboard" Decode.bool
        |> required "useSubdomain" Decode.bool
        |> required "selectedCommunity" (Decode.nullable Eos.symbolDecoder)


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
        |> required "eosio" Decode.string
        |> required "api" Decode.string
        |> required "graphql" Decode.string


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
        Decode.string
