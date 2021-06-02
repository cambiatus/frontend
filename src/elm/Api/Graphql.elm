module Api.Graphql exposing (mutation, query)

import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet exposing (SelectionSet)
import List.Extra as List
import RemoteData exposing (RemoteData)
import Session.Shared exposing (Shared)


withAuthToken : Maybe String -> Graphql.Http.Request decodesTo -> Graphql.Http.Request decodesTo
withAuthToken authToken =
    case authToken of
        Just t ->
            Graphql.Http.withHeader "authorization"
                ("Bearer " ++ t)

        Nothing ->
            identity


withCommunityDomain : Shared -> Graphql.Http.Request decodesTo -> Graphql.Http.Request decodesTo
withCommunityDomain shared =
    let
        communityDomain =
            if shared.useSubdomain then
                Session.Shared.communityDomain shared

            else if String.endsWith ".cambiatus.io" shared.url.path then
                let
                    sharedUrl =
                        shared.url
                in
                { sharedUrl
                    | path =
                        String.split "." sharedUrl.path
                            |> List.updateAt 0 (\_ -> "cambiatus")
                            |> String.join "."
                }
                    |> .path

            else
                "cambiatus.staging.cambiatus.io"
    in
    Graphql.Http.withHeader "Community-Domain" ("https://" ++ communityDomain)


query : Shared -> Maybe String -> SelectionSet a RootQuery -> (RemoteData (Graphql.Http.Error a) a -> msg) -> Cmd msg
query ({ endpoints } as shared) maybeAuthToken query_ toMsg =
    query_
        |> Graphql.Http.queryRequest endpoints.graphql
        |> withCommunityDomain shared
        |> withAuthToken maybeAuthToken
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)


mutation : Shared -> Maybe String -> SelectionSet a RootMutation -> (RemoteData (Graphql.Http.Error a) a -> msg) -> Cmd msg
mutation ({ endpoints } as shared) maybeAuthToken mutation_ toMsg =
    mutation_
        |> Graphql.Http.mutationRequest endpoints.graphql
        |> withCommunityDomain shared
        |> withAuthToken maybeAuthToken
        |> Graphql.Http.send (RemoteData.fromResult >> toMsg)
