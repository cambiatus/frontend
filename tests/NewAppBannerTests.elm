module NewAppBannerTests exposing (all)

import Expect
import Html.Attributes
import Json.Encode as Encode
import NewAppBanner
import Session.Shared as Shared
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Time
import Url exposing (Url)


translators : { t : String -> String, tr : String -> List ( String, String ) -> String }
translators =
    { t = identity, tr = \key _ -> key }


url : String -> Maybe String -> Maybe String -> Url
url path query fragment =
    { protocol = Url.Https
    , host = "muda.cambiatus.io"
    , port_ = Nothing
    , path = path
    , query = query
    , fragment = fragment
    }


millis : Int -> Time.Posix
millis =
    Time.millisToPosix


cutoverMillis : Int
cutoverMillis =
    Time.posixToMillis NewAppBanner.cutover


all : Test
all =
    describe "NewAppBanner"
        [ describe "handoffUrl"
            [ test "encodes path and query, without host or fragment" <|
                \_ ->
                    NewAppBanner.handoffUrl (url "/shop/31" (Just "step=2") (Just "top"))
                        |> Expect.equal "/app/from-elm?path=%2Fshop%2F31%3Fstep%3D2"
            , test "works without a query" <|
                \_ ->
                    NewAppBanner.handoffUrl (url "/" Nothing Nothing)
                        |> Expect.equal "/app/from-elm?path=%2F"
            ]
        , describe "isVisible"
            [ test "shows before the cutover" <|
                \_ ->
                    NewAppBanner.isVisible { dismissed = False, now = millis (cutoverMillis - 1), translationsStatus = Shared.LoadedTranslation }
                        |> Expect.equal True
            , test "hides once dismissed" <|
                \_ ->
                    NewAppBanner.isVisible { dismissed = True, now = millis 0, translationsStatus = Shared.LoadedTranslation }
                        |> Expect.equal False
            , test "waits for the first translations" <|
                \_ ->
                    NewAppBanner.isVisible { dismissed = False, now = millis 0, translationsStatus = Shared.LoadingTranslation }
                        |> Expect.equal False
            , test "hides from the cutover on" <|
                \_ ->
                    NewAppBanner.isVisible { dismissed = False, now = millis cutoverMillis, translationsStatus = Shared.LoadedTranslation }
                        |> Expect.equal False
            ]
        , describe "dismissedFromFlags"
            [ test "reads the flag" <|
                \_ ->
                    Encode.object [ ( "newAppBannerDismissed", Encode.bool True ) ]
                        |> NewAppBanner.dismissedFromFlags
                        |> Expect.equal True
            , test "defaults to not dismissed" <|
                \_ ->
                    Encode.object []
                        |> NewAppBanner.dismissedFromFlags
                        |> Expect.equal False
            ]
        , describe "view"
            [ test "links outside the SPA to the handoff address" <|
                \_ ->
                    NewAppBanner.view translators { href = "https://x/app/from-elm?path=%2F", onDismiss = () }
                        |> Query.fromHtml
                        |> Query.find [ Selector.id "new-app-banner-go" ]
                        |> Query.has
                            [ Selector.attribute (Html.Attributes.href "https://x/app/from-elm?path=%2F")
                            , Selector.attribute (Html.Attributes.target "_self")
                            , Selector.text "new_app_banner.action"
                            ]
            ]
        ]
