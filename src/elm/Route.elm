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
    | Objectives Symbol
    | NewObjective Symbol
    | EditObjective Symbol Int
    | NewAction Symbol Int
    | EditAction Symbol Int Int
    | Claim Symbol Int Int Int
    | Shop Shop.Filter
    | NewSale
    | EditSale String
    | ViewSale String
    | ViewTransfer Int
    | Invite String
    | Transfer Symbol (Maybe String)


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
        , Url.map Objectives (s "community" </> Eos.symbolUrlParser </> s "objectives")
        , Url.map NewObjective (s "community" </> Eos.symbolUrlParser </> s "objectives" </> s "new")
        , Url.map EditObjective (s "community" </> Eos.symbolUrlParser </> s "objectives" </> int </> s "edit")
        , Url.map NewAction (s "community" </> Eos.symbolUrlParser </> s "objectives" </> int </> s "action" </> s "new")
        , Url.map EditAction (s "community" </> Eos.symbolUrlParser </> s "objectives" </> int </> s "action" </> int </> s "edit")
        , Url.map Claim (s "community" </> Eos.symbolUrlParser </> s "objectives" </> int </> s "action" </> int </> s "claim" </> int)
        , Url.map Shop
            (s "shop"
                <?> Query.map
                        (\filter ->
                            if String.startsWith "user" filter then
                                Shop.UserSales

                            else
                                Shop.All
                        )
                        (Query.map (Maybe.withDefault "") (Query.string "filter"))
            )
        , Url.map NewSale (s "shop" </> s "new" </> s "sell")
        , Url.map ViewSale (s "shop" </> string)
        , Url.map EditSale (s "shop" </> string </> s "edit")
        , Url.map ViewTransfer (s "transfer" </> int)
        , Url.map Invite (s "invite" </> string)
        , Url.map Transfer (s "community" </> Eos.symbolUrlParser </> s "transfer" <?> Query.string "to")
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

                Objectives symbol ->
                    ( [ "community", Eos.symbolToString symbol, "objectives" ], [] )

                NewObjective symbol ->
                    ( [ "community", Eos.symbolToString symbol, "objectives", "new" ], [] )

                EditObjective symbol objectiveId ->
                    ( [ "community", Eos.symbolToString symbol, "objectives", String.fromInt objectiveId, "edit" ], [] )

                NewAction symbol objectiveId ->
                    ( [ "community", Eos.symbolToString symbol, "objectives", String.fromInt objectiveId, "action", "new" ]
                    , []
                    )

                EditAction symbol objectiveId actionId ->
                    ( [ "community", Eos.symbolToString symbol, "objectives", String.fromInt objectiveId, "action", String.fromInt actionId, "edit" ]
                    , []
                    )

                Claim communityId objectiveId actionId claimId ->
                    ( [ "community"
                      , Eos.symbolToString communityId
                      , "objectives"
                      , String.fromInt objectiveId
                      , "action"
                      , String.fromInt actionId
                      , "claim"
                      , String.fromInt claimId
                      ]
                    , []
                    )

                Communities ->
                    ( [ "community" ], [] )

                Shop maybeFilter ->
                    ( [ "shop" ]
                    , queryBuilder shopFilterToString (Just maybeFilter) "filter"
                    )

                NewSale ->
                    ( [ "shop", "new", "sell" ], [] )

                EditSale saleId ->
                    ( [ "shop", saleId, "edit" ], [] )

                ViewSale saleId ->
                    ( [ "shop", saleId ], [] )

                ViewTransfer transferId ->
                    ( [ "transfer", String.fromInt transferId ], [] )

                Invite invitationId ->
                    ( [ "invite", invitationId ], [] )

                Transfer communityId maybeTo ->
                    ( [ "community"
                      , Eos.symbolToString communityId
                      , "transfer"
                      ]
                    , [ Url.Builder.string "to" (Maybe.withDefault "" maybeTo) ]
                    )
    in
    Url.Builder.absolute paths queries
