module Flags exposing
    ( Endpoints
    , Flags
    , decode
    , default
    )

import Api.Graphql
import Eos
import Eos.Account as Eos
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)


type alias Flags =
    { language : String
    , maybeAccount : Maybe Eos.Name
    , endpoints : Endpoints
    , logo : String
    , logoMobile : String
    , now : Int
    , allowCommunityCreation : Bool
    , tokenContract : String
    , communityContract : String
    , graphqlSecret : String
    , authToken : Maybe Api.Graphql.Token
    , hasUsedPKAuth : Bool
    , canReadClipboard : Bool
    , useSubdomain : Bool
    , selectedCommunity : Maybe Eos.Symbol
    , pinVisibility : Bool
    , hasSeenSponsorModal : Bool
    }


default : Flags
default =
    { language = "en-US"
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
    , hasUsedPKAuth = False
    , canReadClipboard = False
    , useSubdomain = True
    , selectedCommunity = Nothing
    , pinVisibility = False
    , hasSeenSponsorModal = False
    }


decode : Decoder Flags
decode =
    Decode.succeed Flags
        |> required "language" Decode.string
        |> required "accountName" (Decode.nullable Eos.nameDecoder)
        |> required "endpoints" decodeEndpoints
        |> required "logo" Decode.string
        |> required "logoMobile" Decode.string
        |> required "now" Decode.int
        |> required "allowCommunityCreation" Decode.bool
        |> required "tokenContract" Decode.string
        |> required "communityContract" Decode.string
        |> required "graphqlSecret" Decode.string
        |> required "authToken" (Decode.nullable Api.Graphql.tokenDecoder)
        |> required "hasUsedPKAuth" Decode.bool
        |> required "canReadClipboard" Decode.bool
        |> required "useSubdomain" Decode.bool
        |> required "selectedCommunity" (Decode.nullable Eos.symbolDecoder)
        |> required "pinVisibility" Decode.bool
        |> required "hasSeenSponsorModal" Decode.bool


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
