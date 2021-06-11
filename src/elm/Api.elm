module Api exposing
    ( communityInvite
    , getBalances
    , getExpiryOpts
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
import Token
import Url.Builder exposing (QueryParameter)



-- BACKEND


backendUrl : Shared -> List String -> List QueryParameter -> String
backendUrl { endpoints } paths queryParams =
    Url.Builder.crossOrigin endpoints.api
        ("api" :: paths)
        queryParams


blockchainUrl : Shared -> List String -> List QueryParameter -> String
blockchainUrl { endpoints } paths queryParams =
    Url.Builder.crossOrigin endpoints.eosio
        ("v1" :: paths)
        queryParams



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



-- Token


getExpiryOpts : Shared -> Eos.Symbol -> (Result Http.Error (Maybe Token.ExpiryOptsData) -> msg) -> Cmd msg
getExpiryOpts shared symbol toMsg =
    let
        query =
            { code = shared.contracts.token
            , scope = shared.contracts.token
            , table = "expiryopts"
            , limit = 1000
            }
    in
    Http.post
        { url = blockchainUrl shared [ "chain", "get_table_rows" ] []
        , body = Eos.encodeTableQuery query |> Http.jsonBody
        , expect =
            Decode.field "rows"
                (Decode.list Token.expiryOptsDataDecoder
                    |> Decode.map (List.filter (.currency >> (==) symbol) >> List.head)
                )
                |> Http.expectJson toMsg
        }
