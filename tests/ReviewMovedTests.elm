module ReviewMovedTests exposing (all)

import Expect
import Html.Attributes
import ReviewMoved
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


translators : { t : String -> String, tr : String -> List ( String, String ) -> String }
translators =
    { t = identity, tr = \key _ -> key }


all : Test
all =
    describe "ReviewMoved"
        [ describe "reviewUrl"
            [ test "points at /app/review" <|
                \_ ->
                    ReviewMoved.reviewUrl "https://muda.cambiatus.io" Nothing
                        |> Expect.equal "https://muda.cambiatus.io/app/review"
            , test "focuses one claim and drops a trailing slash" <|
                \_ ->
                    ReviewMoved.reviewUrl "http://localhost:4000/" (Just 42)
                        |> Expect.equal "http://localhost:4000/app/review?claim=42"
            ]
        , describe "viewClaimCta"
            [ test "links to the claim in the new app and leaves the SPA" <|
                \_ ->
                    ReviewMoved.viewClaimCta translators "https://muda.cambiatus.io" 7 []
                        |> Query.fromHtml
                        |> Query.find [ Selector.id "vote-in-new-app-7" ]
                        |> Query.has
                            [ Selector.attribute (Html.Attributes.href "https://muda.cambiatus.io/app/review?claim=7")
                            , Selector.attribute (Html.Attributes.target "_self")
                            ]
            ]
        , describe "viewMovedContent"
            [ test "shows the texts and a button to /app/review" <|
                \_ ->
                    let
                        rendered =
                            ReviewMoved.viewMovedContent translators "https://muda.cambiatus.io"
                                |> Query.fromHtml
                    in
                    Expect.all
                        [ Query.has [ Selector.text "review_moved.title", Selector.text "review_moved.body" ]
                        , Query.find [ Selector.id "review-moved-go" ]
                            >> Query.has
                                [ Selector.attribute (Html.Attributes.href "https://muda.cambiatus.io/app/review")
                                , Selector.attribute (Html.Attributes.target "_self")
                                ]
                        ]
                        rendered
            ]
        ]
