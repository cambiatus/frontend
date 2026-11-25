module NewAppCalloutTests exposing (all)

import Eos
import Expect
import Html.Attributes
import NewAppCallout
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


all : Test
all =
    describe "NewAppCallout"
        [ isVisibleTests
        , viewTests
        ]


symbol : String -> Eos.Symbol
symbol code =
    Eos.symbolFromString ("0," ++ code)
        |> Maybe.withDefault Eos.cambiatusSymbol


isVisibleTests : Test
isVisibleTests =
    describe "isVisible"
        [ test "always shows for MUDA members" <|
            \_ ->
                NewAppCallout.isVisible (symbol "MUDA")
                    |> Expect.equal True
        , test "stays away in other communities" <|
            \_ ->
                NewAppCallout.isVisible (symbol "CMB")
                    |> Expect.equal False
        ]


viewTests : Test
viewTests =
    describe "view"
        (let
            rendered =
                NewAppCallout.view { t = identity, tr = \key _ -> key }
                    { apiUrl = "https://muda.cambiatus.io/" }
                    |> Query.fromHtml
         in
         [ test "the button goes to the new app and leaves the SPA" <|
            \_ ->
                rendered
                    |> Query.find [ Selector.id "new-app-callout-go" ]
                    |> Query.has
                        [ Selector.attribute (Html.Attributes.href "https://muda.cambiatus.io/app")
                        , Selector.attribute (Html.Attributes.target "_self")
                        , Selector.text "dashboard.new_app.action"
                        ]
         , test "the 12 words link goes to the settings page" <|
            \_ ->
                rendered
                    |> Query.find [ Selector.id "new-app-callout-words" ]
                    |> Query.has
                        [ Selector.attribute (Html.Attributes.href "/settings")
                        , Selector.text "dashboard.new_app.words_link"
                        ]
         , test "shows the announcement texts" <|
            \_ ->
                rendered
                    |> Query.has
                        [ Selector.text "dashboard.new_app.eyebrow"
                        , Selector.text "dashboard.new_app.title"
                        , Selector.text "dashboard.new_app.body"
                        ]
         , test "has no close button: the invite is permanent" <|
            \_ ->
                rendered
                    |> Query.hasNot [ Selector.tag "button" ]
         , test "the words between ** are emphasised" <|
            \_ ->
                NewAppCallout.view { t = \_ -> "Check your **12 words** now", tr = \key _ -> key }
                    { apiUrl = "" }
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "strong" ]
                    |> Query.has [ Selector.text "12 words" ]
         ]
        )
