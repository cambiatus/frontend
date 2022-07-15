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
import Version exposing (Version)


type alias Flags =
    { language : String
    , version : Version
    , maybeAccount : Maybe Eos.Name
    , endpoints : Endpoints
    , logo : String
    , logoMobile : String
    , now : Int
    , allowCommunityCreation : Bool
    , tokenContract : String
    , communityContract : String
    , authToken : Maybe Api.Graphql.Token
    , canReadClipboard : Bool
    , canShare : Bool
    , useSubdomain : Bool
    , selectedCommunity : Maybe Eos.Symbol
    , pinVisibility : Bool
    , hasSeenSponsorModal : Bool
    }


default : Flags
default =
    { language = "en-US"
    , version = Version.default
    , maybeAccount = Nothing
    , endpoints = defaultEndpoints
    , logo = "/images/logo-cambiatus.png"
    , logoMobile = "/images/logo-cambiatus-mobile.svg"
    , now = 0
    , allowCommunityCreation = True
    , tokenContract = "bes.token"
    , communityContract = "bes.cmm"
    , authToken = Nothing
    , canReadClipboard = False
    , canShare = False
    , useSubdomain = True
    , selectedCommunity = Nothing
    , pinVisibility = False
    , hasSeenSponsorModal = False
    }


decode : Decoder Flags
decode =
    Decode.succeed Flags
        |> required "language" Decode.string
        |> required "version" Version.decode
        |> required "accountName" (Decode.nullable Eos.nameDecoder)
        |> required "endpoints" decodeEndpoints
        |> required "logo" Decode.string
        |> required "logoMobile" Decode.string
        |> required "now" Decode.int
        |> required "allowCommunityCreation" Decode.bool
        |> required "tokenContract" Decode.string
        |> required "communityContract" Decode.string
        |> required "authToken" (Decode.nullable Api.Graphql.tokenDecoder)
        |> required "canReadClipboard" Decode.bool
        |> required "canShare" Decode.bool
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
