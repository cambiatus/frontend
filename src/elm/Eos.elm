module Eos exposing (Action, Asset, Authorization, EosBool(..), Network, Symbol, TableQuery, Transaction, bespiralSymbol, boolToEosBool, decodeAmountToFloat, decodeAsset, encodeAction, encodeAsset, encodeAuthorization, encodeEosBool, encodeNetwork, encodeSymbol, encodeTableQuery, encodeTransaction, symbolDecoder, symbolFromString, symbolSelectionSet, symbolToString, symbolUrlParser)

import Eos.Account as Account exposing (Account, PermissionName)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Url.Parser



-- NETWORK


type alias Network =
    { blockchain : String
    , host : String
    , port_ : Int
    , protocol : String
    , chainId : String
    }


encodeNetwork : Network -> Value
encodeNetwork network =
    Encode.object
        [ ( "blockchain", Encode.string network.blockchain )
        , ( "host", Encode.string network.host )
        , ( "port", Encode.int network.port_ )
        , ( "protocol", Encode.string network.protocol )
        , ( "chainId", Encode.string network.chainId )
        ]



-- TRANSACTION


type alias Transaction =
    { actions : List Action
    }



-- TODO: you'll receive one authorization for all the actions, just encode each action with the same authorization


encodeTransaction : Transaction -> Value
encodeTransaction transaction =
    Encode.object
        [ ( "name", Encode.string "eosTransaction" )
        , ( "actions", Encode.list encodeAction transaction.actions )
        ]



-- ACTION


type alias Action =
    { accountName : String
    , name : String
    , data : Value
    , authorization : Authorization
    }


encodeAction : Action -> Value
encodeAction action =
    Encode.object
        [ ( "account", Encode.string action.accountName )
        , ( "name", Encode.string action.name )
        , ( "authorization", encodeAuthorization action.authorization )
        , ( "data", action.data )
        ]



-- AUTHORIZATION


type alias Authorization =
    { actor : Account.Name
    , permissionName : PermissionName
    }


encodeAuthorization : Authorization -> Value
encodeAuthorization authorization =
    Encode.list
        (\a ->
            Encode.object
                [ ( "actor", Account.encodeName a.actor )
                , ( "permission", Account.encodePermissionName a.permissionName )
                ]
        )
        [ authorization ]



-- ASSET


type alias Asset =
    { amount : Float
    , symbol : Symbol
    }


encodeAsset : Asset -> Value
encodeAsset asset =
    String.fromFloat asset.amount
        ++ " "
        ++ symbolToString asset.symbol
        |> Encode.string


decodeAsset : Decoder Asset
decodeAsset =
    Decode.string
        |> Decode.andThen
            (\s ->
                let
                    value =
                        amountToFloat s
                            |> Maybe.map Decode.succeed
                            |> Maybe.withDefault (Decode.fail "Fail to decode asset amount")

                    symbol =
                        amountToSymbol s
                            |> Maybe.map Decode.succeed
                            |> Maybe.withDefault (Decode.fail "Fail to decode asset symbol")
                in
                Decode.map2
                    Asset
                    value
                    symbol
            )


decodeAmountToFloat : Decoder Float
decodeAmountToFloat =
    Decode.string
        |> Decode.andThen
            (\s ->
                amountToFloat s
                    |> Maybe.map Decode.succeed
                    |> Maybe.withDefault (Decode.fail "Fail to decode amount")
            )


amountToFloat : String -> Maybe Float
amountToFloat s =
    String.split " " s
        |> List.head
        |> Maybe.andThen String.toFloat



-- SYMBOL


type Symbol
    = Symbol String


symbolDecoder : Decoder Symbol
symbolDecoder =
    Decode.map Symbol Decode.string


encodeSymbol : Symbol -> Value
encodeSymbol (Symbol symbol) =
    Encode.string symbol


symbolToString : Symbol -> String
symbolToString (Symbol symbol) =
    symbol


symbolFromString : String -> Maybe Symbol
symbolFromString str =
    if String.length str == 3 || String.length str == 4 then
        Just (Symbol (String.toUpper str))

    else
        Nothing


symbolUrlParser : Url.Parser.Parser (Symbol -> a) a
symbolUrlParser =
    Url.Parser.custom "SYMBOL" symbolFromString


symbolSelectionSet : SelectionSet String typeLock -> SelectionSet Symbol typeLock
symbolSelectionSet =
    SelectionSet.map Symbol


bespiralSymbol : Symbol
bespiralSymbol =
    Symbol "BES"


amountToSymbol : String -> Maybe Symbol
amountToSymbol s =
    String.split " " s
        |> List.reverse
        |> List.head
        |> Maybe.andThen symbolFromString



-- EosBool


type EosBool
    = EosTrue
    | EosFalse


boolToEosBool : Bool -> EosBool
boolToEosBool b =
    if b then
        EosTrue

    else
        EosFalse


encodeEosBool : EosBool -> Value
encodeEosBool eosBool =
    case eosBool of
        EosTrue ->
            Encode.int 1

        EosFalse ->
            Encode.int 0



-- Table Query


type alias TableQuery =
    { code : String
    , scope : String
    , table : String
    , limit : Int
    }


encodeTableQuery : TableQuery -> Value
encodeTableQuery query =
    Encode.object
        [ ( "code", Encode.string query.code )
        , ( "scope", Encode.string query.scope )
        , ( "table", Encode.string query.table )
        , ( "limit", Encode.int query.limit )
        , ( "json", Encode.bool True )
        ]
