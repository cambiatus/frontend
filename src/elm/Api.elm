module Api exposing (UserId, backendUrl, blockchainUrl, communityInvite, editProfile, get, getBalances, getTableRows, signIn, signUp, signUpWithInvitation, uploadAvatar, uploadImage)

import Account
import Avatar exposing (Avatar)
import Community exposing (Balance)
import Eos exposing (Symbol)
import Eos.Account as Eos
import File exposing (File)
import Flags exposing (Endpoints)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Session.Shared exposing (Shared)
import Url.Builder exposing (QueryParameter)



-- BACKEND


backendUrl : Shared -> List String -> List QueryParameter -> String
backendUrl { endpoints } paths queryParams =
    Url.Builder.crossOrigin endpoints.api
        ("api" :: paths)
        queryParams


backendUrl_ : Endpoints -> List String -> List QueryParameter -> String
backendUrl_ endpoints paths queryParams =
    Url.Builder.crossOrigin endpoints.api
        ("api" :: paths)
        queryParams


blockchainUrl : Shared -> List String -> List QueryParameter -> String
blockchainUrl { endpoints } paths queryParams =
    Url.Builder.crossOrigin endpoints.eosio
        ("v1" :: paths)
        queryParams



-- METHODS


get : Endpoints -> List String -> List QueryParameter -> Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
get endpoints paths queryParams decoder toMsg =
    Http.get
        { url = backendUrl_ endpoints paths queryParams
        , expect = Http.expectJson toMsg decoder
        }



-- Requests for the blockchain


getTableRows : Shared -> Value -> String -> Int -> Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
getTableRows shared scope table limit dec toMsg =
    Http.request
        { method = "POST"
        , headers = []
        , url = blockchainUrl shared [ "chain", "get_table_rows" ] []
        , body =
            Encode.object
                [ ( "scope", scope )
                , ( "code", Encode.string "bespiral" )
                , ( "table", Encode.string table )
                , ( "json", Encode.bool True )
                , ( "limit", Encode.int limit )
                ]
                |> Encode.encode 0
                |> Http.stringBody "text/plain;charset=UTF-8"
        , expect = Http.expectJson toMsg dec
        , timeout = Nothing
        , tracker = Nothing
        }


type alias UserId =
    String


signIn : Shared -> Eos.Name -> (Result Http.Error Account.Profile -> msg) -> Cmd msg
signIn shared accountName toMsg =
    Http.post
        { url = backendUrl shared [ "auth", "sign_in" ] []
        , body =
            Account.encodeProfileLogin accountName
                |> Http.jsonBody
        , expect = Http.expectJson toMsg Account.decodeProfile
        }



-- Profile


editProfile : Shared -> Eos.Name -> Account.ProfileForm -> (Result Http.Error Account.Profile -> msg) -> Cmd msg
editProfile shared accountName form toMsg =
    Http.post
        { url = backendUrl shared [ "profile", Eos.nameToString accountName ] []
        , body =
            Account.encodeProfileForm accountName form
                |> Http.jsonBody
        , expect =
            Account.decodeProfile
                |> Http.expectJson toMsg
        }


signUp : Shared -> Account.ProfileCreate -> (Result Http.Error Account.Profile -> msg) -> Cmd msg
signUp shared form toMsg =
    Http.post
        { url = backendUrl shared [ "auth", "sign_up" ] []
        , body =
            Account.encodeProfileCreate form
                |> Http.jsonBody
        , expect = Http.expectJson toMsg Account.decodeProfile
        }


signUpWithInvitation : Shared -> Account.ProfileCreate -> (Result Http.Error Account.Profile -> msg) -> Cmd msg
signUpWithInvitation shared form toMsg =
    signUp shared form toMsg


uploadAvatar : Shared -> File -> (Result Http.Error Avatar -> msg) -> Cmd msg
uploadAvatar shared file toMsg =
    Http.post
        { url = backendUrl shared [ "ipfs" ] []
        , body =
            Http.multipartBody
                [ Http.filePart "file" file ]
        , expect =
            Decode.at [ "data", "hash" ] Avatar.decode
                |> Http.expectJson toMsg
        }



-- Community


getBalances : Shared -> Eos.Name -> (Result Http.Error (List Balance) -> msg) -> Cmd msg
getBalances shared accountName toMsg =
    let
        query =
            Eos.TableQuery "bes.token" (Eos.nameToString accountName) "accounts" 1000
    in
    Http.post
        { url = blockchainUrl shared [ "chain", "get_table_rows" ] []
        , body = Eos.encodeTableQuery query |> Http.jsonBody
        , expect = Decode.field "rows" (Decode.list Community.decodeBalance) |> Http.expectJson toMsg
        }


uploadImage : Shared -> File -> (Result Http.Error String -> msg) -> Cmd msg
uploadImage shared file toMsg =
    Http.post
        { url = backendUrl shared [ "ipfs" ] []
        , body =
            Http.multipartBody
                [ Http.filePart "file" file ]
        , expect =
            Decode.at [ "data", "hash" ] Decode.string
                |> Http.expectJson toMsg
        }


communityInvite : Shared -> Symbol -> Eos.Name -> String -> (Result Http.Error () -> msg) -> Cmd msg
communityInvite shared symbol inviter email toMsg =
    Http.post
        { url = backendUrl shared [ "communities", Eos.symbolToString symbol, "invite" ] []
        , body =
            Encode.object
                [ ( "inviter", Eos.encodeName inviter )
                , ( "invites", Encode.string email )
                ]
                |> Http.jsonBody
        , expect = Http.expectWhatever toMsg
        }
