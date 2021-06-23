module Session.GuestTests exposing (all)

import Expect
import Session.Guest as Guest
import Test exposing (..)
import TestUtils
import Url


all : Test
all =
    describe "Session.Guest"
        [ invalidCommunityRedirectUrl ]



-- INVALID COMMUNITY REDIRECT URL


invalidCommunityRedirectUrl : Test
invalidCommunityRedirectUrl =
    describe "invalidCommunityRedirectUrl"
        [ describe "when useSubdomain is `True`"
            [ describe "when on production"
                [ fuzz (TestUtils.cambiatusUrlFuzzer (Just ".cambiatus.io"))
                    "Returns welcome page"
                  <|
                    \urlFuzz ->
                        Guest.invalidCommunityRedirectUrl urlFuzz True
                            |> Expect.equal (Just "https://www.cambiatus.com/welcome")
                ]
            , describe "when on demo"
                [ fuzz (TestUtils.cambiatusUrlFuzzer (Just ".demo.cambiatus.io"))
                    "Returns demo welcome page"
                  <|
                    \urlFuzz ->
                        Guest.invalidCommunityRedirectUrl urlFuzz True
                            |> Expect.equal (Just "https://www.cambiatus.com/welcome-demo")
                ]
            , describe "when on staging"
                [ fuzz (TestUtils.cambiatusUrlFuzzer (Just ".staging.cambiatus.io"))
                    "Returns cambiatus community on staging"
                  <|
                    \urlFuzz ->
                        Guest.invalidCommunityRedirectUrl urlFuzz True
                            |> Expect.equal
                                ({ urlFuzz | host = "cambiatus.staging.cambiatus.io" }
                                    |> Url.toString
                                    |> Just
                                )
                ]
            , describe "when on localhost"
                [ fuzz (TestUtils.cambiatusUrlFuzzer (Just ".localhost"))
                    "Returns cambiatus community on staging"
                  <|
                    \urlFuzz ->
                        Guest.invalidCommunityRedirectUrl urlFuzz True
                            |> Expect.equal
                                ({ urlFuzz | host = "cambiatus.staging.localhost" }
                                    |> Url.toString
                                    |> Just
                                )
                ]
            ]
        , describe "when useSubdomain is `False`"
            [ describe "when on production"
                [ fuzz (TestUtils.cambiatusUrlFuzzer (Just ".cambiatus.io"))
                    "doesn't redirect"
                  <|
                    \urlFuzz ->
                        Guest.invalidCommunityRedirectUrl urlFuzz False
                            |> Expect.equal Nothing
                ]
            , describe "when on demo"
                [ fuzz (TestUtils.cambiatusUrlFuzzer (Just ".demo.cambiatus.io"))
                    "doesn't redirect"
                  <|
                    \urlFuzz ->
                        Guest.invalidCommunityRedirectUrl urlFuzz False
                            |> Expect.equal Nothing
                ]
            , describe "when on staging"
                [ fuzz (TestUtils.cambiatusUrlFuzzer (Just ".staging.cambiatus.io"))
                    "doesn't redirect"
                  <|
                    \urlFuzz ->
                        Guest.invalidCommunityRedirectUrl urlFuzz False
                            |> Expect.equal Nothing
                ]
            , describe "when on localhost"
                [ fuzz (TestUtils.cambiatusUrlFuzzer (Just ".localhost"))
                    "doesn't redirect"
                  <|
                    \urlFuzz ->
                        Guest.invalidCommunityRedirectUrl urlFuzz False
                            |> Expect.equal Nothing
                ]
            ]
        ]
