module Session.SharedTests exposing (all)

import Expect
import Session.Shared as Shared
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

        makeInput url =
            { url = url, environment = Shared.Production }
    in
    describe "when environment is production"
        [ describe "when on production"
            [ fuzz (Fuzz.cambiatusUrl (Just ".cambiatus.io"))
                "returns url.host"
              <|
                \urlFuzz ->
                    makeInput urlFuzz
                        |> Shared.communityDomain
                        |> Expect.equal urlFuzz.host
            , test "returns correct community domain" <|
                \() ->
                    makeUrl "somecommunity.cambiatus.io"
                        |> makeInput
                        |> Shared.communityDomain
                        |> Expect.equal "somecommunity.cambiatus.io"
            ]
        , describe "when on demo"
            [ fuzz (Fuzz.cambiatusUrl (Just ".demo.cambiatus.io"))
                "returns url.host"
              <|
                \urlFuzz ->
                    makeInput urlFuzz
                        |> Shared.communityDomain
                        |> Expect.equal urlFuzz.host
            , test "returns correct community domain" <|
                \() ->
                    makeUrl "somecommunity.demo.cambiatus.io"
                        |> makeInput
                        |> Shared.communityDomain
                        |> Expect.equal "somecommunity.demo.cambiatus.io"
            ]
        , describe "when on staging"
            [ fuzz (Fuzz.cambiatusUrl (Just ".staging.cambiatus.io"))
                "returns url.host"
              <|
                \urlFuzz ->
                    makeInput urlFuzz
                        |> Shared.communityDomain
                        |> Expect.equal urlFuzz.host
            , test "returns correct community domain" <|
                \() ->
                    makeUrl "somecommunity.staging.cambiatus.io"
                        |> makeInput
                        |> Shared.communityDomain
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

        makeInput url =
            { url = url, environment = Shared.Staging }
    in
    describe "when environment is development"
        [ describe "when on production"
            [ fuzz (Fuzz.cambiatusUrl (Just ".cambiatus.io"))
                "returns url.host"
              <|
                \urlFuzz ->
                    makeInput urlFuzz
                        |> Shared.communityDomain
                        |> Expect.equal urlFuzz.host
            , test "returns correct community domain" <|
                \() ->
                    makeUrl "somecommunity.cambiatus.io"
                        |> makeInput
                        |> Shared.communityDomain
                        |> Expect.equal "somecommunity.cambiatus.io"
            ]
        , describe "when on demo"
            [ fuzz (Fuzz.cambiatusUrl (Just ".demo.cambiatus.io"))
                "returns url.host"
              <|
                \urlFuzz ->
                    makeInput urlFuzz
                        |> Shared.communityDomain
                        |> Expect.equal urlFuzz.host
            , test "returns correct community domain" <|
                \() ->
                    makeUrl "somecommunity.demo.cambiatus.io"
                        |> makeInput
                        |> Shared.communityDomain
                        |> Expect.equal "somecommunity.demo.cambiatus.io"
            ]
        , describe "when on staging"
            [ fuzz (Fuzz.cambiatusUrl (Just ".staging.cambiatus.io"))
                "returns url.host"
              <|
                \urlFuzz ->
                    makeInput urlFuzz
                        |> Shared.communityDomain
                        |> Expect.equal urlFuzz.host
            , test "returns correct community domain" <|
                \() ->
                    makeUrl "somecommunity.staging.cambiatus.io"
                        |> makeInput
                        |> Shared.communityDomain
                        |> Expect.equal "somecommunity.staging.cambiatus.io"
            ]
        , describe "when on localhost"
            [ fuzz (Fuzz.cambiatusUrl (Just ".localhost"))
                "replaces localhost for staging"
              <|
                \urlFuzz ->
                    makeInput urlFuzz
                        |> Shared.communityDomain
                        |> Expect.equal (urlFuzz.host |> String.replace ".localhost" ".staging.cambiatus.io")
            , test "returns correct community domain" <|
                \() ->
                    makeUrl "somecommunity.localhost"
                        |> makeInput
                        |> Shared.communityDomain
                        |> Expect.equal "somecommunity.staging.cambiatus.io"
            ]
        , describe "when on staging.localhost"
            [ fuzz (Fuzz.cambiatusUrl (Just ".staging.localhost"))
                "replaces staging.localhost for staging"
              <|
                \urlFuzz ->
                    makeInput urlFuzz
                        |> Shared.communityDomain
                        |> Expect.equal (urlFuzz.host |> String.replace ".localhost" ".cambiatus.io")
            , test "returns correct community domain" <|
                \() ->
                    makeUrl "somecommunity.staging.localhost"
                        |> makeInput
                        |> Shared.communityDomain
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
                        |> Shared.environmentFromUrl
                        |> Expect.equal Shared.Production
            , test "correctly identifies verdes" <|
                \() ->
                    makeUrl "verdes.cambiatus.io"
                        |> Shared.environmentFromUrl
                        |> Expect.equal Shared.Production
            , fuzz (Fuzz.cambiatusUrl (Just ".cambiatus.io")) "correctly identifies prod communities" <|
                \urlFuzz ->
                    Shared.environmentFromUrl urlFuzz
                        |> Expect.equal Shared.Production
            ]
        , describe "when on demo"
            [ test "correctly identifies cmbx" <|
                \() ->
                    makeUrl "cmbx.demo.cambiatus.io"
                        |> Shared.environmentFromUrl
                        |> Expect.equal Shared.Demo
            , test "correctly identifies cmbgo" <|
                \() ->
                    makeUrl "cmbgo.demo.cambiatus.io"
                        |> Shared.environmentFromUrl
                        |> Expect.equal Shared.Demo
            , fuzz (Fuzz.cambiatusUrl (Just ".demo.cambiatus.io")) "correctly identifies demo communities" <|
                \urlFuzz ->
                    Shared.environmentFromUrl urlFuzz
                        |> Expect.equal Shared.Demo
            ]
        , describe "when on staging"
            [ test "correctly identifies buss" <|
                \() ->
                    makeUrl "buss.staging.cambiatus.io"
                        |> Shared.environmentFromUrl
                        |> Expect.equal Shared.Staging
            , test "correctly identifies mizu" <|
                \() ->
                    makeUrl "mizu.staging.cambiatus.io"
                        |> Shared.environmentFromUrl
                        |> Expect.equal Shared.Staging
            , fuzz (Fuzz.cambiatusUrl (Just ".staging.cambiatus.io")) "correctly identifies staging communities" <|
                \urlFuzz ->
                    Shared.environmentFromUrl urlFuzz
                        |> Expect.equal Shared.Staging
            ]
        , fuzz (Fuzz.cambiatusUrl (Just ".netlify.app")) "correctly identifies netlify links as staging" <|
            \urlFuzz ->
                Shared.environmentFromUrl urlFuzz
                    |> Expect.equal Shared.Staging
        , describe "when on localhost"
            [ test "correctly identifies buss" <|
                \() ->
                    makeUrl "buss.staging.localhost"
                        |> Shared.environmentFromUrl
                        |> Expect.equal Shared.Development
            , fuzz (Fuzz.cambiatusUrl (Just ".staging.localhost")) "correctly identifies localhost communities" <|
                \urlFuzz ->
                    Shared.environmentFromUrl urlFuzz
                        |> Expect.equal Shared.Development
            ]
        ]
