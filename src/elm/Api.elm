module Api exposing
    ( UserId
    , backendUrl
    , blockchainUrl
    , communityInvite
    , editProfile
    , get
    , getBalances
    , getTableRows
    , signIn
    , signInInvitation
    , uploadAvatar
    , uploadImage
    )

import Avatar exposing (Avatar)
import Community exposing (Balance)
import Eos exposing (Symbol)
import Eos.Account as Eos
import File exposing (File)
import Flags exposing (Endpoints)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Profile exposing (Profile, ProfileForm)
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


signIn : Shared -> Eos.Name -> (Result Http.Error Profile -> msg) -> Cmd msg
signIn shared accountName toMsg =
    Http.post
        { url = backendUrl shared [ "auth", "sign_in" ] []
        , body =
            Profile.encodeProfileLogin accountName
                |> Http.jsonBody
        , expect = Http.expectJson toMsg Profile.decode
        }


signInInvitation : Shared -> Eos.Name -> String -> (Result Http.Error Profile -> msg) -> Cmd msg
signInInvitation shared accountName invitationId toMsg =
    Http.post
        { url = backendUrl shared [ "auth", "sign_in" ] []
        , body =
            Profile.encodeProfileLoginWithInvitation accountName invitationId
                |> Http.jsonBody
        , expect = Http.expectJson toMsg Profile.decode
        }



-- Profile


editProfile : Shared -> Eos.Name -> ProfileForm -> (Result Http.Error Profile -> msg) -> Cmd msg
editProfile shared accountName form toMsg =
    Http.post
        { url = backendUrl shared [ "profile", Eos.nameToString accountName ] []
        , body =
            Profile.encodeProfileForm accountName form
                |> Http.jsonBody
        , expect =
            Profile.decode
                |> Http.expectJson toMsg
        }


uploadAvatar : Shared -> File -> (Result Http.Error Avatar -> msg) -> Cmd msg
uploadAvatar shared file toMsg =
    Http.post
        { url = backendUrl shared [ "upload" ] []
        , body =
            Http.multipartBody
                [ Http.filePart "file" file ]
        , expect =
            Decode.at [ "data" ] Avatar.decode
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
        { url = backendUrl shared [ "upload" ] []
        , body =
            Http.multipartBody
                [ Http.filePart "file" file ]
        , expect =
            Decode.at [ "data" ] Decode.string
                |> Http.expectJson toMsg
        }


communityInvite : Shared -> Symbol -> Eos.Name -> (Result Http.Error String -> msg) -> Cmd msg
communityInvite shared symbol inviter toMsg =
    Http.post
        { url = backendUrl shared [ "invite" ] []
        , body =
            Encode.object
                [ ( "creator_id", Eos.encodeName inviter )
                , ( "community_id", Eos.symbolToString symbol |> Encode.string )
                ]
                |> Http.jsonBody
        , expect =
            Decode.at [ "data", "id" ] Decode.string
                |> Http.expectJson toMsg
        }
