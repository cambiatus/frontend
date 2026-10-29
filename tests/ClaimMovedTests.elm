module ClaimMovedTests exposing (all)

import ClaimMoved
import Expect
import Html.Attributes
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


translators : { t : String -> String, tr : String -> List ( String, String ) -> String }
translators =
    { t = identity, tr = \key _ -> key }


all : Test
all =
    describe "ClaimMoved"
        [ test "focuses the action in the /app catalog" <|
            \_ ->
                ClaimMoved.claimUrl "https://muda.cambiatus.io/" 7
                    |> Expect.equal "https://muda.cambiatus.io/app/community/objectives?action=7"
        , test "the link leaves the SPA" <|
            \_ ->
                ClaimMoved.viewClaimLink translators "https://x" 7
                    |> Query.fromHtml
                    |> Query.has
                        [ Selector.attribute (Html.Attributes.target "_self")
                        , Selector.id "claim-in-new-app-7"
                        , Selector.attribute (Html.Attributes.href "https://x/app/community/objectives?action=7")
                        , Selector.text "claim_moved.go"
                        ]
        ]
