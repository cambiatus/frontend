module Api.GraphqlTests exposing (all)

import Api.Graphql
import Dict
import Expect
import Graphql.Http
import Graphql.Http.GraphqlError
import Json.Encode as Encode
import Test exposing (..)


all : Test
all =
    describe "Api.Graphql"
        [ isNonExistingCommunityError
        , isNewsNotFoundError
        , isAuthError
        ]



-- IS NON EXISTING COMMUNITY ERROR


isNonExistingCommunityError : Test
isNonExistingCommunityError =
    describe "isNonExistingCommunityError"
        [ test "reports error for community that doesn't exist" <|
            \() ->
                stringToGraphqlError "No community found using the domain thisisntarealcommunityanditneverwillbe.staging.cambiatus.io"
                    |> Api.Graphql.isNonExistingCommunityError
                    |> Expect.true "expected to detect as an error about a community that doesn't exist"
        , test "doesn't report error for community that exists" <|
            \() ->
                stringToGraphqlError "Some other error"
                    |> Api.Graphql.isNonExistingCommunityError
                    |> Expect.false "expected to know it's not an error about a community that doesn't exist"
        ]


isNewsNotFoundError : Test
isNewsNotFoundError =
    describe "isNewsNotFoundError"
        [ test "reports error when news were not found" <|
            \() ->
                stringToGraphqlError "News not found"
                    |> Api.Graphql.isNewsNotFoundError
                    |> Expect.true "expected to detect an error about news that weren't found"
        , test "doesn't report error for news that were found" <|
            \() ->
                stringToGraphqlError "Some other error"
                    |> Api.Graphql.isNewsNotFoundError
                    |> Expect.false "expected to know it's not an error about news that weren't found"
        ]


isAuthError : Test
isAuthError =
    describe "isAuthError"
        [ test "reports error when backend says the user isn't authenticated" <|
            \() ->
                stringToGraphqlError "Please sign in first!"
                    |> Api.Graphql.isAuthError
                    |> Expect.true "expected to detect an error about user not being authenticated"
        , test "doesn't report error for user who is authenticated" <|
            \() ->
                stringToGraphqlError "Some other error"
                    |> Api.Graphql.isAuthError
                    |> Expect.false "expected to know it's not an error about not being authenticated"
        ]


stringToGraphqlError : String -> Graphql.Http.Error a
stringToGraphqlError error =
    Graphql.Http.GraphqlError (Graphql.Http.GraphqlError.UnparsedData Encode.null)
        [ { message = error, locations = Nothing, details = Dict.empty } ]
