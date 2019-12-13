module Route exposing (Route(..), fromUrl, href, pushUrl, replaceUrl)

import Browser.Navigation as Nav
import Eos exposing (Symbol)
import Html exposing (Attribute)
import Html.Attributes as Attr
import Shop
import Url exposing (Url)
import Url.Builder exposing (QueryParameter)
import Url.Parser as Url exposing ((</>), (<?>), Parser, int, oneOf, s, string, top)
import Url.Parser.Query as Query


type Route
    = Root
    | ComingSoon
    | Register (Maybe String) (Maybe Route)
    | Login (Maybe Route)
    | LoginWithPrivateKey (Maybe Route)
    | Logout
    | Notification
    | Profile
    | Dashboard
    | Community Symbol
    | Communities
    | NewCommunity
    | EditCommunity Symbol
    | NewObjective Symbol
    | EditObjective Symbol Int
    | NewAction Symbol String
    | VerifyClaim Symbol String String String
    | Shop (Maybe Shop.Filter)
    | NewSale
    | EditSale String
    | ViewSale String
    | Transfer Int


parser : Url -> Parser (Route -> a) a
parser url =
    oneOf
        [ Url.map Root top
        , Url.map ComingSoon (s "coming-soon")
        , Url.map (Just >> Register)
            (s "register"
                </> string
                <?> Query.map
                        (parseRedirect url)
                        (Query.string "redirect")
            )
        , Url.map (Register Nothing)
            (s "register"
                <?> Query.map
                        (parseRedirect url)
                        (Query.string "redirect")
            )
        , Url.map Login
            (s "login"
                <?> Query.map
                        (parseRedirect url)
                        (Query.string "redirect")
            )
        , Url.map LoginWithPrivateKey
            (s "login"
                </> s "mnemonic"
                <?> Query.map
                        (parseRedirect url)
                        (Query.string "redirect")
            )
        , Url.map Logout (s "logout")
        , Url.map Profile (s "profile")
        , Url.map Notification (s "notification")
        , Url.map Dashboard (s "dashboard")
        , Url.map Communities (s "community")
        , Url.map NewCommunity (s "community" </> s "new")
        , Url.map Community (s "community" </> Eos.symbolUrlParser)
        , Url.map EditCommunity (s "community" </> Eos.symbolUrlParser </> s "edit")
        , Url.map NewObjective (s "community" </> Eos.symbolUrlParser </> s "objective" </> s "new")
        , Url.map EditObjective (s "community" </> Eos.symbolUrlParser </> s "objective" </> int </> s "edit")
        , Url.map NewAction (s "community" </> Eos.symbolUrlParser </> s "objective" </> string </> s "action" </> s "new")
        , Url.map VerifyClaim (s "community" </> Eos.symbolUrlParser </> s "objective" </> string </> s "action" </> string </> s "claim" </> string </> s "verification")
        , Url.map Shop
            (s "shop"
                <?> Query.map
                        (\maybeF ->
                            case maybeF of
                                Nothing ->
                                    Nothing

                                Just query ->
                                    if String.startsWith "my-communities" query then
                                        Just Shop.MyCommunities

                                    else if String.startsWith "user" query then
                                        Just Shop.UserSales

                                    else
                                        Just Shop.All
                        )
                        (Query.string "filter")
            )
        , Url.map NewSale (s "shop" </> s "new" </> s "sell")
        , Url.map ViewSale (s "shop" </> string)
        , Url.map EditSale (s "shop" </> string </> s "edit")
        , Url.map Transfer (s "transfer" </> int)
        ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    Url.parse (parser url) url


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl key route =
    Nav.pushUrl key (routeToString route)



-- INTERNAL


parseRedirect : Url -> Maybe String -> Maybe Route
parseRedirect url maybeQuery =
    let
        protocol =
            case url.protocol of
                Url.Http ->
                    "http://"

                Url.Https ->
                    "https://"

        host =
            url.host

        port_ =
            case url.port_ of
                Nothing ->
                    ""

                Just p ->
                    ":" ++ String.fromInt p

        maybeUrl =
            case maybeQuery of
                Nothing ->
                    Nothing

                Just query ->
                    Url.fromString (protocol ++ host ++ port_ ++ query)
    in
    case maybeUrl of
        Nothing ->
            Nothing

        Just url_ ->
            Url.parse (parser url_) url_


shopFilterToString : Shop.Filter -> String
shopFilterToString filter =
    case filter of
        Shop.MyCommunities ->
            "my-communities"

        Shop.All ->
            "all"

        Shop.UserSales ->
            "user"


queryBuilder : (a -> String) -> Maybe a -> String -> List QueryParameter
queryBuilder fn maybeRedirect queryParam =
    Maybe.map fn maybeRedirect
        |> Maybe.map (Url.Builder.string queryParam)
        |> List.singleton
        |> List.filterMap identity


routeToString : Route -> String
routeToString route =
    let
        ( paths, queries ) =
            case route of
                Root ->
                    ( [], [] )

                ComingSoon ->
                    ( [ "coming-soon" ], [] )

                Register Nothing maybeRedirect ->
                    ( [ "register" ]
                    , queryBuilder routeToString maybeRedirect "redirect"
                    )

                Register (Just invitation) maybeRedirect ->
                    ( [ "register", invitation ]
                    , queryBuilder routeToString maybeRedirect "redirect"
                    )

                Login maybeRedirect ->
                    ( [ "login" ]
                    , queryBuilder routeToString maybeRedirect "redirect"
                    )

                LoginWithPrivateKey maybeRedirect ->
                    ( [ "login", "mnemonic" ]
                    , queryBuilder routeToString maybeRedirect "redirect"
                    )

                Logout ->
                    ( [ "logout" ], [] )

                Notification ->
                    ( [ "notification" ], [] )

                Profile ->
                    ( [ "profile" ], [] )

                Dashboard ->
                    ( [ "dashboard" ], [] )

                Community symbol ->
                    ( [ "community", Eos.symbolToString symbol ], [] )

                NewCommunity ->
                    ( [ "community", "new" ], [] )

                EditCommunity symbol ->
                    ( [ "community", Eos.symbolToString symbol, "edit" ], [] )

                NewObjective symbol ->
                    ( [ "community", Eos.symbolToString symbol, "objective", "new" ], [] )

                EditObjective symbol objectiveId ->
                    ( [ "community", Eos.symbolToString symbol, "objective", String.fromInt objectiveId, "edit" ], [] )

                NewAction symbol objectiveId ->
                    ( [ "community", Eos.symbolToString symbol, "objective", objectiveId, "action", "new" ]
                    , []
                    )

                VerifyClaim communityId objectiveId actionId claimId ->
                    ( [ "community", Eos.symbolToString communityId, "objective", objectiveId, "action", actionId, "claim", claimId, "verification" ]
                    , []
                    )

                Communities ->
                    ( [ "community" ], [] )

                Shop maybeFilter ->
                    ( [ "shop" ]
                    , queryBuilder shopFilterToString maybeFilter "filter"
                    )

                NewSale ->
                    ( [ "shop", "new", "sell" ], [] )

                EditSale saleId ->
                    ( [ "shop", saleId, "edit" ], [] )

                ViewSale saleId ->
                    ( [ "shop", saleId ], [] )

                Transfer transferId ->
                    ( [ "transfer", String.fromInt transferId ], [] )
    in
    Url.Builder.absolute paths queries
