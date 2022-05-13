module Eos.Account exposing
    ( Name
    , PermissionName
    , PrivateKey
    , encodeName
    , encodePermissionName
    , encodePrivateKey
    , errorToString
    , fromString
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


{-| DEPRECATED. Use fromString instead, or Form.Validate.eosAccount

This function is deprecated because we shouldn't allow any string to be a name,
otherwise we don't get the benefits of it being an opaque type.

-}
stringToName : String -> Name
stringToName string =
    Name string


type Error
    = InvalidCharacters Char
    | TooShort
    | TooLong


fromString : String -> Result Error Name
fromString string =
    let
        sanitizedString =
            string
                |> String.trim
                |> String.toLower

        isCharacterAllowed : Char -> Bool
        isCharacterAllowed char =
            Char.isAlpha char || List.member char [ '1', '2', '3', '4', '5' ]
    in
    case String.filter (not << isCharacterAllowed) sanitizedString |> String.toList |> List.head of
        Just firstInvalidCharacter ->
            Err (InvalidCharacters firstInvalidCharacter)

        Nothing ->
            if String.length sanitizedString < minNameLength then
                Err TooShort

            else if String.length sanitizedString > maxNameLength then
                Err TooLong

            else
                Ok (Name sanitizedString)


maxNameLength : Int
maxNameLength =
    12


minNameLength : Int
minNameLength =
    -- If changing this value, also fix `errorToString`'s validation message on
    -- the TooShort and TooLong cases
    12


errorToString :
    { translators | tr : String -> List ( String, String ) -> String }
    -> Error
    -> String
errorToString { tr } error =
    case error of
        InvalidCharacters firstInvalidCharacter ->
            tr "error.notAllowedChar" [ ( "char", String.fromChar firstInvalidCharacter ) ]

        TooShort ->
            tr "error.validator.text.exactly" [ ( "base", String.fromInt minNameLength ) ]

        TooLong ->
            tr "error.validator.text.exactly" [ ( "base", String.fromInt maxNameLength ) ]


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


encodePrivateKey : PrivateKey -> Value
encodePrivateKey (PrivateKey pk) =
    Encode.string pk


privateKeyToString : PrivateKey -> String
privateKeyToString (PrivateKey pk) =
    pk
