module Api.Graphql exposing (mutation, query)

import Graphql.Http exposing (mutationRequest, queryRequest, send)
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet )
import Session.Shared exposing (Shared)


query : Shared -> SelectionSet a RootQuery -> (Result (Graphql.Http.Error a) a -> msg) -> Cmd msg
query { endpoints } query_ toMsg =
    query_
        |> Graphql.Http.queryRequest endpoints.graphql
        |> Graphql.Http.send toMsg


mutation : Shared -> SelectionSet a RootMutation -> (Result (Graphql.Http.Error a) a -> msg) -> Cmd msg
mutation { endpoints } mutation_ toMsg =
    mutation_
        |> Graphql.Http.mutationRequest endpoints.graphql
        |> Graphql.Http.send toMsg
