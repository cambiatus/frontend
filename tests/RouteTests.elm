module RouteTests exposing (all)

import Environment
import Expect
import Route
import Test exposing (..)
import TestHelpers.Fuzz


all : Test
all =
    describe "Route"
        [ addEnvironmentToUrl
        ]


addEnvironmentToUrl : Test
addEnvironmentToUrl =
    describe "addEnvironmentToUrl"
        [ fuzz (TestHelpers.Fuzz.cambiatusUrl (Just "")) "localhost" <|
            \fuzzUrl ->
                Route.addEnvironmentToUrl Environment.Development fuzzUrl
                    |> Expect.equal { fuzzUrl | host = fuzzUrl.host ++ ".staging.cambiatus.io" }
        , fuzz (TestHelpers.Fuzz.cambiatusUrl (Just "")) "staging" <|
            \fuzzUrl ->
                Route.addEnvironmentToUrl Environment.Development fuzzUrl
                    |> Expect.equal { fuzzUrl | host = fuzzUrl.host ++ ".staging.cambiatus.io" }
        , fuzz (TestHelpers.Fuzz.cambiatusUrl (Just "")) "demo" <|
            \fuzzUrl ->
                Route.addEnvironmentToUrl Environment.Demo fuzzUrl
                    |> Expect.equal { fuzzUrl | host = fuzzUrl.host ++ ".demo.cambiatus.io" }
        , fuzz (TestHelpers.Fuzz.cambiatusUrl (Just "")) "production" <|
            \fuzzUrl ->
                Route.addEnvironmentToUrl Environment.Production fuzzUrl
                    |> Expect.equal { fuzzUrl | host = fuzzUrl.host ++ ".cambiatus.io" }
        ]
