module Eos.Account exposing
    ( Name
    , PermissionName
    , PrivateKey
    , encodeName
    , encodePermissionName
    , nameDecoder
    , nameQueryUrlParser
    , nameSelectionSet
    , nameToString
    , privateKeyDecoder
    , privateKeyToString
    , samplePermission
    , stringToName
    , viewName
    )

import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)



-- NAME


type Name
    = Name String


stringToName : String -> Name
stringToName string =
    Name string


encodeName : Name -> Value
encodeName (Name name) =
    Encode.string name


nameDecoder : Decoder Name
nameDecoder =
    Decode.map Name Decode.string


viewName : Name -> Html msg
viewName (Name name) =
    Html.text name


nameToString : Name -> String
nameToString (Name name) =
    name


nameQueryUrlParser : String -> Name
nameQueryUrlParser =
    Name


nameSelectionSet : SelectionSet String typeLock -> SelectionSet Name typeLock
nameSelectionSet =
    SelectionSet.map Name



-- PERMISSION


type PermissionName
    = PermissionName String


encodePermissionName : PermissionName -> Value
encodePermissionName (PermissionName permissionName) =
    Encode.string permissionName


samplePermission : PermissionName
samplePermission =
    PermissionName "active"



-- PRIVATE KEY


type PrivateKey
    = PrivateKey String


privateKeyDecoder : Decoder PrivateKey
privateKeyDecoder =
    Decode.map PrivateKey Decode.string


privateKeyToString : PrivateKey -> String
privateKeyToString (PrivateKey pk) =
    pk
