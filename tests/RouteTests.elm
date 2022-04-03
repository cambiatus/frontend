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
        , addRouteToUrl
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


addRouteToUrl : Test
addRouteToUrl =
    describe "addRouteToUrl"
        [ fuzz (TestHelpers.Fuzz.cambiatusUrl (Just "")) "/community/about" <|
            \fuzzUrl ->
                Route.addRouteToUrl { url = fuzzUrl } Route.CommunityAbout
                    |> Expect.equal
                        { fuzzUrl
                            | path = "/community/about"
                            , query = Nothing
                            , fragment = Nothing
                        }
        , fuzz (TestHelpers.Fuzz.cambiatusUrl (Just "")) "/dashboard" <|
            \fuzzUrl ->
                Route.addRouteToUrl { url = fuzzUrl } Route.Dashboard
                    |> Expect.equal
                        { fuzzUrl
                            | path = "/dashboard"
                            , query = Nothing
                            , fragment = Nothing
                        }
        , fuzz (TestHelpers.Fuzz.cambiatusUrl (Just "")) "/" <|
            \fuzzUrl ->
                Route.addRouteToUrl { url = fuzzUrl } Route.Root
                    |> Expect.equal
                        { fuzzUrl
                            | path = "/"
                            , query = Nothing
                            , fragment = Nothing
                        }
        , fuzz (TestHelpers.Fuzz.cambiatusUrl (Just "")) "/community/new" <|
            \fuzzUrl ->
                Route.addRouteToUrl { url = fuzzUrl } Route.NewCommunity
                    |> Expect.equal
                        { fuzzUrl
                            | path = "/community/new"
                            , query = Nothing
                            , fragment = Nothing
                        }
        , fuzz (TestHelpers.Fuzz.cambiatusUrl (Just "")) "/news/0" <|
            \fuzzUrl ->
                Route.addRouteToUrl { url = fuzzUrl } (Route.News { selectedNews = Just 0, showOthers = True })
                    |> Expect.equal
                        { fuzzUrl
                            | path = "/news/0"
                            , query = Nothing
                            , fragment = Nothing
                        }
        , fuzz (TestHelpers.Fuzz.cambiatusUrl (Just "")) "/news/0?showOthers=false" <|
            \fuzzUrl ->
                Route.addRouteToUrl { url = fuzzUrl } (Route.News { selectedNews = Just 0, showOthers = False })
                    |> Expect.equal
                        { fuzzUrl
                            | path = "/news/0"
                            , query = Just "showOthers=false"
                            , fragment = Nothing
                        }
        ]
