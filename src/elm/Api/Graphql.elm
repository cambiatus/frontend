module Api.Graphql exposing (mutation, query)

import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Session.Shared exposing (Shared)


withAuthToken : Maybe String -> Graphql.Http.Request decodesTo -> Graphql.Http.Request decodesTo
withAuthToken authToken =
    case authToken of
        Just t ->
            Graphql.Http.withHeader "authorization"
                ("Bearer " ++ t)

        Nothing ->
            identity


query : Shared -> SelectionSet a RootQuery -> (Result (Graphql.Http.Error a) a -> msg) -> Cmd msg
query { endpoints, authToken } query_ toMsg =
    query_
        |> Graphql.Http.queryRequest endpoints.graphql
        |> withAuthToken authToken
        |> Graphql.Http.send toMsg


mutation : Shared -> SelectionSet a RootMutation -> (Result (Graphql.Http.Error a) a -> msg) -> Cmd msg
mutation { endpoints, authToken } mutation_ toMsg =
    mutation_
        |> Graphql.Http.mutationRequest endpoints.graphql
        |> withAuthToken authToken
        |> Graphql.Http.send toMsg
