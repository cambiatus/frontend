module Api exposing
    ( communityInvite
    , getBalances
    , getFromBlockchain
    , uploadImage
    )

import Community exposing (Balance)
import Eos
import Eos.Account as Eos
import File exposing (File)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Session.Shared exposing (Shared)
import Url.Builder exposing (QueryParameter)



-- BACKEND


backendUrl : Shared -> List String -> List QueryParameter -> String
backendUrl { endpoints } paths queryParams =
    Url.Builder.crossOrigin endpoints.api
        ("api" :: paths)
        queryParams



-- BLOCKCHAIN


blockchainUrl : Shared -> List String -> List QueryParameter -> String
blockchainUrl { endpoints } paths queryParams =
    Url.Builder.crossOrigin endpoints.eosio
        ("v1" :: paths)
        queryParams


getFromBlockchain : Shared -> Eos.TableQuery -> Decode.Decoder a -> (Result Http.Error a -> msg) -> Cmd msg
getFromBlockchain shared query decoder toMsg =
    Http.post
        { url = blockchainUrl shared [ "chain", "get_table_rows" ] []
        , body = Eos.encodeTableQuery query |> Http.jsonBody
        , expect = Http.expectJson toMsg decoder
        }



-- METHODS
-- Community


getBalances : Shared -> Eos.Name -> (Result Http.Error (List Balance) -> msg) -> Cmd msg
getBalances shared accountName toMsg =
    let
        query =
            Eos.TableQuery shared.contracts.token (Eos.nameToString accountName) "accounts" 1000
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


communityInvite : Shared -> Eos.Symbol -> Eos.Name -> (Result Http.Error String -> msg) -> Cmd msg
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
