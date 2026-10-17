module AdminMovedTests exposing (all)

import Expect
import Html.Attributes
import Page.AdminMoved as AdminMoved
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


all : Test
all =
    describe "Page.AdminMoved"
        [ loginUrlTests
        , viewContentTests
        ]


loginUrlTests : Test
loginUrlTests =
    describe "loginUrl"
        [ test "appends /app/login to the API url" <|
            \_ ->
                AdminMoved.loginUrl "https://muda.cambiatus.io"
                    |> Expect.equal "https://muda.cambiatus.io/app/login"
        , test "drops a trailing slash before appending" <|
            \_ ->
                AdminMoved.loginUrl "http://localhost:4000/"
                    |> Expect.equal "http://localhost:4000/app/login"
        ]


viewContentTests : Test
viewContentTests =
    describe "viewContent"
        (let
            rendered =
                AdminMoved.viewContent { t = identity, tr = \key _ -> key } "https://muda.cambiatus.io/app/login"
                    |> Query.fromHtml
         in
         [ test "the login button goes to the /app login page and leaves the SPA" <|
            \_ ->
                rendered
                    |> Query.find [ Selector.id "admin-moved-login" ]
                    |> Query.has
                        [ Selector.attribute (Html.Attributes.href "https://muda.cambiatus.io/app/login")
                        , Selector.attribute (Html.Attributes.target "_self")
                        ]
         , test "shows the explainer texts" <|
            \_ ->
                rendered
                    |> Query.has
                        [ Selector.text "admin_moved.title"
                        , Selector.text "admin_moved.body"
                        , Selector.text "admin_moved.login_hint"
                        , Selector.text "admin_moved.go"
                        , Selector.text "admin_moved.back"
                        ]
         ]
        )
