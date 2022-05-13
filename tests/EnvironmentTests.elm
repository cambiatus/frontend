module EnvironmentTests exposing (all)

import Environment
import Expect
import Test exposing (..)
import TestHelpers.Fuzz as Fuzz
import Url


all : Test
all =
    describe "Session.Shared"
        [ communityDomain
        , deploymentEnvironment
        ]



-- COMMUNITY DOMAIN


communityDomain : Test
communityDomain =
    describe "communityDomain"
        [ communityDomainOnProduction
        , communityDomainOnDevelopment
        ]


communityDomainOnProduction : Test
communityDomainOnProduction =
    let
        makeUrl : String -> Url.Url
        makeUrl host =
            { protocol = Url.Https
            , host = host
            , port_ = Nothing
            , path = "/"
            , query = Nothing
            , fragment = Nothing
            }
    in
    describe "when environment is production"
        [ describe "when on production"
            [ fuzz (Fuzz.cambiatusUrl (Just ".cambiatus.io"))
                "returns url.host"
              <|
                \urlFuzz ->
                    Environment.communityDomain urlFuzz
                        |> Expect.equal urlFuzz.host
            , test "returns correct community domain" <|
                \() ->
                    makeUrl "somecommunity.cambiatus.io"
                        |> Environment.communityDomain
                        |> Expect.equal "somecommunity.cambiatus.io"
            ]
        , describe "when on demo"
            [ fuzz (Fuzz.cambiatusUrl (Just ".demo.cambiatus.io"))
                "returns url.host"
              <|
                \urlFuzz ->
                    urlFuzz
                        |> Environment.communityDomain
                        |> Expect.equal urlFuzz.host
            , test "returns correct community domain" <|
                \() ->
                    makeUrl "somecommunity.demo.cambiatus.io"
                        |> Environment.communityDomain
                        |> Expect.equal "somecommunity.demo.cambiatus.io"
            ]
        , describe "when on staging"
            [ fuzz (Fuzz.cambiatusUrl (Just ".staging.cambiatus.io"))
                "returns url.host"
              <|
                \urlFuzz ->
                    urlFuzz
                        |> Environment.communityDomain
                        |> Expect.equal urlFuzz.host
            , test "returns correct community domain" <|
                \() ->
                    makeUrl "somecommunity.staging.cambiatus.io"
                        |> Environment.communityDomain
                        |> Expect.equal "somecommunity.staging.cambiatus.io"
            ]
        ]


communityDomainOnDevelopment : Test
communityDomainOnDevelopment =
    let
        makeUrl : String -> Url.Url
        makeUrl host =
            { protocol = Url.Https
            , host = host
            , port_ = Nothing
            , path = "/"
            , query = Nothing
            , fragment = Nothing
            }
    in
    describe "when environment is development"
        [ describe "when on production"
            [ fuzz (Fuzz.cambiatusUrl (Just ".cambiatus.io"))
                "returns url.host"
              <|
                \urlFuzz ->
                    urlFuzz
                        |> Environment.communityDomain
                        |> Expect.equal urlFuzz.host
            , test "returns correct community domain" <|
                \() ->
                    makeUrl "somecommunity.cambiatus.io"
                        |> Environment.communityDomain
                        |> Expect.equal "somecommunity.cambiatus.io"
            ]
        , describe "when on demo"
            [ fuzz (Fuzz.cambiatusUrl (Just ".demo.cambiatus.io"))
                "returns url.host"
              <|
                \urlFuzz ->
                    urlFuzz
                        |> Environment.communityDomain
                        |> Expect.equal urlFuzz.host
            , test "returns correct community domain" <|
                \() ->
                    makeUrl "somecommunity.demo.cambiatus.io"
                        |> Environment.communityDomain
                        |> Expect.equal "somecommunity.demo.cambiatus.io"
            ]
        , describe "when on staging"
            [ fuzz (Fuzz.cambiatusUrl (Just ".staging.cambiatus.io"))
                "returns url.host"
              <|
                \urlFuzz ->
                    urlFuzz
                        |> Environment.communityDomain
                        |> Expect.equal urlFuzz.host
            , test "returns correct community domain" <|
                \() ->
                    makeUrl "somecommunity.staging.cambiatus.io"
                        |> Environment.communityDomain
                        |> Expect.equal "somecommunity.staging.cambiatus.io"
            ]
        , describe "when on localhost"
            [ fuzz (Fuzz.cambiatusUrl (Just ".localhost"))
                "replaces localhost for staging"
              <|
                \urlFuzz ->
                    urlFuzz
                        |> Environment.communityDomain
                        |> Expect.equal (urlFuzz.host |> String.replace ".localhost" ".staging.cambiatus.io")
            , test "returns correct community domain" <|
                \() ->
                    makeUrl "somecommunity.localhost"
                        |> Environment.communityDomain
                        |> Expect.equal "somecommunity.staging.cambiatus.io"
            ]
        , describe "when on staging.localhost"
            [ fuzz (Fuzz.cambiatusUrl (Just ".staging.localhost"))
                "replaces staging.localhost for staging"
              <|
                \urlFuzz ->
                    urlFuzz
                        |> Environment.communityDomain
                        |> Expect.equal (urlFuzz.host |> String.replace ".localhost" ".cambiatus.io")
            , test "returns correct community domain" <|
                \() ->
                    makeUrl "somecommunity.staging.localhost"
                        |> Environment.communityDomain
                        |> Expect.equal "somecommunity.staging.cambiatus.io"
            ]
        ]



-- DEPLOYMENT ENVIRONMENT


deploymentEnvironment : Test
deploymentEnvironment =
    let
        makeUrl : String -> Url.Url
        makeUrl host =
            { protocol = Url.Https
            , host = host
            , port_ = Nothing
            , path = "/"
            , query = Nothing
            , fragment = Nothing
            }
    in
    describe "deploymentEnvironment"
        [ describe "when on production"
            [ test "correctly identifies muda" <|
                \() ->
                    makeUrl "muda.cambiatus.io"
                        |> Environment.fromUrl
                        |> Expect.equal Environment.Production
            , test "correctly identifies verdes" <|
                \() ->
                    makeUrl "verdes.cambiatus.io"
                        |> Environment.fromUrl
                        |> Expect.equal Environment.Production
            , fuzz (Fuzz.cambiatusUrl (Just ".cambiatus.io")) "correctly identifies prod communities" <|
                \urlFuzz ->
                    Environment.fromUrl urlFuzz
                        |> Expect.equal Environment.Production
            ]
        , describe "when on demo"
            [ test "correctly identifies cmbx" <|
                \() ->
                    makeUrl "cmbx.demo.cambiatus.io"
                        |> Environment.fromUrl
                        |> Expect.equal Environment.Demo
            , test "correctly identifies cmbgo" <|
                \() ->
                    makeUrl "cmbgo.demo.cambiatus.io"
                        |> Environment.fromUrl
                        |> Expect.equal Environment.Demo
            , fuzz (Fuzz.cambiatusUrl (Just ".demo.cambiatus.io")) "correctly identifies demo communities" <|
                \urlFuzz ->
                    Environment.fromUrl urlFuzz
                        |> Expect.equal Environment.Demo
            ]
        , describe "when on staging"
            [ test "correctly identifies buss" <|
                \() ->
                    makeUrl "buss.staging.cambiatus.io"
                        |> Environment.fromUrl
                        |> Expect.equal Environment.Staging
            , test "correctly identifies mizu" <|
                \() ->
                    makeUrl "mizu.staging.cambiatus.io"
                        |> Environment.fromUrl
                        |> Expect.equal Environment.Staging
            , fuzz (Fuzz.cambiatusUrl (Just ".staging.cambiatus.io")) "correctly identifies staging communities" <|
                \urlFuzz ->
                    Environment.fromUrl urlFuzz
                        |> Expect.equal Environment.Staging
            ]
        , fuzz (Fuzz.cambiatusUrl (Just ".netlify.app")) "correctly identifies netlify links as staging" <|
            \urlFuzz ->
                Environment.fromUrl urlFuzz
                    |> Expect.equal Environment.Staging
        , describe "when on localhost"
            [ test "correctly identifies buss" <|
                \() ->
                    makeUrl "buss.staging.localhost"
                        |> Environment.fromUrl
                        |> Expect.equal Environment.Development
            , fuzz (Fuzz.cambiatusUrl (Just ".staging.localhost")) "correctly identifies localhost communities" <|
                \urlFuzz ->
                    Environment.fromUrl urlFuzz
                        |> Expect.equal Environment.Development
            ]
        ]
