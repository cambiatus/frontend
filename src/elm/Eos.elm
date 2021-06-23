module Eos exposing
    ( Action
    , Asset
    , Authorization
    , EosBool(..)
    , Symbol
    , TableQuery
    , Transaction
    , assetToString
    , boolToEosBool
    , cambiatusSymbol
    , decodeAsset
    , encodeAsset
    , encodeEosBool
    , encodeSymbol
    , encodeTableQuery
    , encodeTransaction
    , eosBoolDecoder
    , eosBoolToBool
    , formatSymbolAmount
    , getSymbolPrecision
    , maxSymbolLength
    , minSymbolLength
    , symbolDecoder
    , symbolFromString
    , symbolSelectionSet
    , symbolToString
    , symbolToSymbolCodeString
    )

import Eos.Account as Account exposing (PermissionName)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Utils



-- TRANSACTION


type alias Transaction =
    List Action


encodeTransaction : Transaction -> Value
encodeTransaction transaction =
    Encode.object
        [ ( "name", Encode.string "eosTransaction" )
        , ( "actions", Encode.list encodeAction transaction )
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


assetToString : Asset -> String
assetToString asset =
    formatSymbolAmount asset.symbol asset.amount
        ++ " "
        ++ symbolToSymbolCodeString asset.symbol


encodeAsset : Asset -> Value
encodeAsset asset =
    String.fromFloat asset.amount
        ++ " "
        ++ symbolToSymbolCodeString asset.symbol
        |> Encode.string


decodeAsset : Decoder Asset
decodeAsset =
    Decode.string
        |> Decode.andThen
            (\s ->
                let
                    value =
                        assetStringToFloat s
                            |> Maybe.map Decode.succeed
                            |> Maybe.withDefault (Decode.fail "Fail to decode asset amount")

                    symbol =
                        getSymbolFromAssetString s
                            |> Maybe.map Decode.succeed
                            |> Maybe.withDefault (Decode.fail "Fail to decode asset symbol")
                in
                Decode.map2
                    Asset
                    value
                    symbol
            )


assetStringToFloat : String -> Maybe Float
assetStringToFloat s =
    String.split " " s
        |> List.head
        |> Maybe.andThen String.toFloat


getSymbolFromAssetString : String -> Maybe Symbol
getSymbolFromAssetString s =
    let
        assetArr =
            String.split " " s

        symbol : Maybe String
        symbol =
            assetArr |> List.reverse |> List.head

        precision : Maybe Int
        precision =
            assetArr
                |> List.head
                |> Maybe.andThen (\amount -> Just (String.split "." amount))
                |> Maybe.andThen
                    (\amountArr ->
                        case amountArr of
                            [ _, p ] ->
                                Just (String.length p)

                            _ ->
                                Just 0
                    )
    in
    case ( symbol, precision ) of
        ( Just symbolString, Just p ) ->
            Just (Symbol symbolString p)

        _ ->
            Nothing



-- SYMBOL


{-| Symbol is composed of a 3 to 4 alphanumeric chars long and an integer, used for informing precision
On EOS symbols are displayed like so: `4,EOS` or `0,CMB`


# Definition

@docs Symbol

-}
type Symbol
    = Symbol String Int


getSymbolPrecision : Symbol -> Int
getSymbolPrecision (Symbol _ precision) =
    precision


symbolDecoder : Decoder Symbol
symbolDecoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                if string == "undefined" then
                    Decode.fail "Cannot decode 'undefined' symbol, check Javascript"

                else
                    case symbolFromString string of
                        Just symbol ->
                            Decode.succeed symbol

                        Nothing ->
                            Decode.fail "Cannot decode given symbol"
            )


encodeSymbol : Symbol -> Value
encodeSymbol symbol =
    Encode.string (symbolToString symbol)


symbolToString : Symbol -> String
symbolToString (Symbol symbol precision) =
    [ String.fromInt precision, ",", symbol ]
        |> String.concat


formatSymbolAmount : Symbol -> Float -> String
formatSymbolAmount (Symbol _ precision) amount =
    Utils.formatFloat amount precision


symbolToSymbolCodeString : Symbol -> String
symbolToSymbolCodeString (Symbol s _) =
    s


minSymbolLength : Int
minSymbolLength =
    3


maxSymbolLength : Int
maxSymbolLength =
    7


symbolFromString : String -> Maybe Symbol
symbolFromString str =
    let
        details =
            String.split "," str |> List.take 2

        maybePrecision : Maybe Int
        maybePrecision =
            List.head details
                |> Maybe.andThen String.toInt

        maybeSymbolCode : Maybe String
        maybeSymbolCode =
            details |> List.reverse |> List.head
    in
    case ( maybeSymbolCode, maybePrecision ) of
        ( Just symbolCode, Just precision ) ->
            if
                String.all Char.isAlpha symbolCode
                    && (String.length symbolCode >= minSymbolLength || String.length symbolCode <= maxSymbolLength)
            then
                Just (Symbol (String.toUpper symbolCode) precision)

            else
                Nothing

        _ ->
            Nothing


symbolSelectionSet : SelectionSet String typeLock -> SelectionSet Symbol typeLock
symbolSelectionSet field =
    SelectionSet.succeed Symbol
        |> with
            (field
                |> SelectionSet.mapOrFail
                    (\s ->
                        case String.split "," s |> List.reverse |> List.head of
                            Just e ->
                                Ok e

                            Nothing ->
                                Err "Can't parse symbol"
                    )
            )
        |> with
            (field
                |> SelectionSet.mapOrFail
                    (\s ->
                        case String.split "," s |> List.head |> Maybe.andThen String.toInt of
                            Just e ->
                                Ok e

                            Nothing ->
                                Err "Can't parse symbol"
                    )
            )


cambiatusSymbol : Symbol
cambiatusSymbol =
    Symbol "CMB" 0


type EosBool
    = EosTrue
    | EosFalse


boolToEosBool : Bool -> EosBool
boolToEosBool b =
    if b then
        EosTrue

    else
        EosFalse


eosBoolToBool : EosBool -> Bool
eosBoolToBool eosBool =
    case eosBool of
        EosTrue ->
            True

        EosFalse ->
            False


encodeEosBool : EosBool -> Value
encodeEosBool eosBool =
    case eosBool of
        EosTrue ->
            Encode.int 1

        EosFalse ->
            Encode.int 0


intToEosBool : Int -> EosBool
intToEosBool v =
    if v == 1 then
        EosTrue

    else
        EosFalse


eosBoolDecoder : Decoder EosBool
eosBoolDecoder =
    Decode.int
        |> Decode.map intToEosBool



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
