module Eos.Account exposing (Account, Name, Permission, PermissionName, PrivateKey, PublicKey, decoder, encodeName, encodePermissionName, encodePrivateKey, encodePublicKey, nameDecoder, nameMaxChars, nameMinChars, nameQueryUrlParser, nameSelectionSet, nameToString, nameValidationAttrs, permissionDecoder, permissionNameDecoder, privateKeyDecoder, privateKeyToString, publicKeyDecoder, publicKeyToString, samplePermission, stringToName, viewName)

import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html)
import Html.Attributes exposing (autocomplete, maxlength, minlength, pattern, spellcheck, title)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)



-- ACCOUNT


type alias Account =
    { name : Name
    , permissions : List Permission
    }


decoder : Decoder Account
decoder =
    Decode.map2 Account
        (Decode.field "account_name" nameDecoder)
        (Decode.field "permissions" (Decode.list permissionDecoder))



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


nameMinChars : Int
nameMinChars =
    3


nameMaxChars : Int
nameMaxChars =
    12


nameValidationAttrs : List (Html.Attribute msg)
nameValidationAttrs =
    [ title "Use only letters from a to z and numbers from 1 to 5."
    , pattern "[a-z1-5]+"
    , minlength nameMinChars
    , maxlength nameMaxChars
    , Html.Attributes.attribute "autocorrect" "off"
    , Html.Attributes.attribute "autocapitalize" "none"
    , autocomplete False
    , spellcheck False
    ]



-- PERMISSION


type alias Permission =
    { parent : PermissionName
    , name : PermissionName
    , publicKey : PublicKey
    }


permissionDecoder : Decoder Permission
permissionDecoder =
    Decode.map3 (\p n ks -> ( p, n, ks ))
        (Decode.field "parent" permissionNameDecoder)
        (Decode.field "perm_name" permissionNameDecoder)
        (Decode.at [ "required_auth", "keys" ]
            (Decode.list (Decode.field "key" publicKeyDecoder))
        )
        |> Decode.andThen
            (\( p, n, ks ) ->
                case List.head ks of
                    Nothing ->
                        Decode.fail "No public key"

                    Just k ->
                        Decode.succeed
                            { parent = p
                            , name = n
                            , publicKey = k
                            }
            )


type PermissionName
    = PermissionName String


encodePermissionName : PermissionName -> Value
encodePermissionName (PermissionName permissionName) =
    Encode.string permissionName


permissionNameDecoder : Decoder PermissionName
permissionNameDecoder =
    Decode.map PermissionName Decode.string


samplePermission : PermissionName
samplePermission =
    PermissionName "active"



-- PUBLIC KEY


type PublicKey
    = PublicKey String


encodePublicKey : PublicKey -> Value
encodePublicKey (PublicKey publicKey) =
    Encode.string publicKey


publicKeyDecoder : Decoder PublicKey
publicKeyDecoder =
    Decode.map PublicKey Decode.string


publicKeyToString : PublicKey -> String
publicKeyToString (PublicKey pk) =
    pk



-- PRIVATE KEY


type PrivateKey
    = PrivateKey String


encodePrivateKey : PrivateKey -> Value
encodePrivateKey (PrivateKey publicKey) =
    Encode.string publicKey


privateKeyDecoder : Decoder PrivateKey
privateKeyDecoder =
    Decode.map PrivateKey Decode.string


privateKeyToString : PrivateKey -> String
privateKeyToString (PrivateKey pk) =
    pk
