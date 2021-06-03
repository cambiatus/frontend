module Token exposing
    ( CreateTokenData
    , ExpiryOptsData
    , TokenType(..)
    , UpdateTokenData
    , createTokenDataDecoder
    , encodeCreateTokenData
    , encodeExpiryOpts
    , encodeUpdateTokenData
    , expiryOptsDataDecoder
    , tokenTypeSelectionSet
    , tokenTypeToString
    , updateTokenDataDecoder
    )

import Cambiatus.Object
import Cambiatus.Object.Community as Community
import Eos
import Eos.Account as Eos
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)



-- TYPES


type alias CreateTokenData =
    { creator : Eos.Name
    , maxSupply : Eos.Asset
    , minBalance : Eos.Asset
    , tokenType : TokenType
    }


type alias UpdateTokenData =
    { maxSupply : Eos.Asset
    , minBalance : Eos.Asset
    }


type alias ExpiryOptsData =
    { currency : Eos.Symbol
    , naturalExpirationPeriod : Int
    , juridicalExpirationPeriod : Int
    , renovationAmount : Eos.Asset
    }


type TokenType
    = Mcc
    | Expiry



-- ENCODING AND DECODING


encodeUpdateTokenData : UpdateTokenData -> Value
encodeUpdateTokenData c =
    Encode.object
        [ ( "max_supply", Eos.encodeAsset c.maxSupply )
        , ( "min_balance", Eos.encodeAsset c.minBalance )
        ]


updateTokenDataDecoder : Decoder UpdateTokenData
updateTokenDataDecoder =
    Decode.succeed UpdateTokenData
        |> required "max_supply" Eos.decodeAsset
        |> required "min_balance" Eos.decodeAsset


encodeTokenType : TokenType -> Value
encodeTokenType =
    tokenTypeToString >> Encode.string


tokenTypeDecoder : Decoder TokenType
tokenTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\typeString ->
                case typeString of
                    "mcc" ->
                        Decode.succeed Mcc

                    "expiry" ->
                        Decode.succeed Expiry

                    _ ->
                        Decode.fail "Expected token type to be mcc or expiry"
            )


encodeCreateTokenData : CreateTokenData -> Value
encodeCreateTokenData c =
    Encode.object
        [ ( "issuer", Eos.encodeName c.creator )
        , ( "max_supply", Eos.encodeAsset c.maxSupply )
        , ( "min_balance", Eos.encodeAsset c.minBalance )
        , ( "type", encodeTokenType c.tokenType )
        ]


createTokenDataDecoder : Decoder CreateTokenData
createTokenDataDecoder =
    Decode.succeed CreateTokenData
        |> required "issuer" Eos.nameDecoder
        |> required "max_supply" Eos.decodeAsset
        |> required "min_balance" Eos.decodeAsset
        |> required "type" tokenTypeDecoder


encodeExpiryOpts : ExpiryOptsData -> Value
encodeExpiryOpts expiryOptsData =
    Encode.object
        [ ( "currency", Eos.encodeSymbol expiryOptsData.currency )
        , ( "natural_expiration_period", Encode.int expiryOptsData.naturalExpirationPeriod )
        , ( "juridical_expiration_period", Encode.int expiryOptsData.juridicalExpirationPeriod )
        , ( "renovation_amount", Eos.encodeAsset expiryOptsData.renovationAmount )
        ]


expiryOptsDataDecoder : Decoder ExpiryOptsData
expiryOptsDataDecoder =
    Decode.succeed ExpiryOptsData
        |> required "currency" Eos.symbolDecoder
        |> required "natural_expiration_period" Decode.int
        |> required "juridical_expiration_period" Decode.int
        |> required "renovation_amount" Eos.decodeAsset



-- GRAPHQL


tokenTypeSelectionSet : SelectionSet (Maybe TokenType) Cambiatus.Object.Community
tokenTypeSelectionSet =
    Community.type_
        |> SelectionSet.map (Maybe.andThen tokenTypeFromString)



-- UTILS


tokenTypeToString : TokenType -> String
tokenTypeToString tokenType =
    case tokenType of
        Mcc ->
            "mcc"

        Expiry ->
            "expiry"


tokenTypeFromString : String -> Maybe TokenType
tokenTypeFromString stringTokenType =
    case stringTokenType of
        "mcc" ->
            Just Mcc

        "expiry" ->
            Just Expiry

        _ ->
            Nothing
