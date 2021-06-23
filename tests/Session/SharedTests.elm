module Session.SharedTests exposing (all)

import Expect
import Flags
import Session.Shared as Shared
import Test exposing (..)
import TestUtils
import Url


all : Test
all =
    describe "Session.Shared" [ communityDomain ]



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
            { url = url, environment = Flags.Production }
    in
    describe "when environment is production"
        [ describe "when on production"
            [ fuzz (TestUtils.cambiatusUrlFuzzer (Just ".cambiatus.io"))
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
            [ fuzz (TestUtils.cambiatusUrlFuzzer (Just ".demo.cambiatus.io"))
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
            [ fuzz (TestUtils.cambiatusUrlFuzzer (Just ".staging.cambiatus.io"))
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
            { url = url, environment = Flags.Development }
    in
    describe "when environment is development"
        [ describe "when on production"
            [ fuzz (TestUtils.cambiatusUrlFuzzer (Just ".cambiatus.io"))
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
            [ fuzz (TestUtils.cambiatusUrlFuzzer (Just ".demo.cambiatus.io"))
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
            [ fuzz (TestUtils.cambiatusUrlFuzzer (Just ".staging.cambiatus.io"))
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
            [ fuzz (TestUtils.cambiatusUrlFuzzer (Just ".localhost"))
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
            [ fuzz (TestUtils.cambiatusUrlFuzzer (Just ".staging.localhost"))
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
