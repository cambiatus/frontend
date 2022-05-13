module Api exposing
    ( communityInvite
    , getBalances
    , uploadImage
    )

import Api.Eos
import Eos
import Eos.Account as Eos
import File exposing (File)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline
import Json.Encode as Encode
import Session.Shared exposing (Shared)
import Time
import Url.Builder exposing (QueryParameter)
import Utils


type alias Balance =
    { asset : Eos.Asset, lastActivity : Time.Posix }


decodeBalance : Decode.Decoder Balance
decodeBalance =
    Decode.succeed Balance
        |> Json.Decode.Pipeline.required "balance" Eos.decodeAsset
        |> Json.Decode.Pipeline.required "last_activity" Utils.decodeTimestamp



-- BLOCKCHAIN


getBalances : Shared -> Eos.Name -> (Result Http.Error (List Balance) -> msg) -> Cmd msg
getBalances shared accountName toMsg =
    Api.Eos.Token (Api.Eos.Accounts accountName)
        |> Api.Eos.queryWithList shared toMsg (Decode.list decodeBalance)



-- BACKEND


backendUrl : { shared | endpoints : { endpoints | api : String } } -> List String -> List QueryParameter -> String
backendUrl { endpoints } paths queryParams =
    Url.Builder.crossOrigin endpoints.api
        ("api" :: paths)
        queryParams


uploadImage : { shared | endpoints : { endpoints | api : String } } -> File -> (Result Http.Error String -> msg) -> Cmd msg
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
