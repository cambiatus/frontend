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
    | ProfileEditor
    | ProfileAddKyc
    | ProfilePublic String
    | ProfileClaims String
    | PaymentHistory String
    | Profile
    | Dashboard
    | Community
    | NewCommunity
    | CommunitySettings
    | CommunitySettingsFeatures
    | EditCommunity
    | Objectives
    | NewObjective
    | EditObjective Int
    | NewAction Symbol Int
    | EditAction Symbol Int Int
    | Claim Int Int Int
    | Shop Shop.Filter
    | NewSale
    | EditSale String
    | ViewSale String
    | ViewTransfer Int
    | Invite String
    | Transfer (Maybe String)
    | Analysis


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
        , Url.map ProfileEditor (s "profile" </> s "edit")
        , Url.map ProfileAddKyc (s "profile" </> s "add-kyc")
        , Url.map ProfilePublic (s "profile" </> string)
        , Url.map ProfileClaims (s "profile" </> string </> s "claims")
        , Url.map PaymentHistory (s "payments" </> string)
        , Url.map Notification (s "notification")
        , Url.map Dashboard (s "dashboard")
        , Url.map NewCommunity (s "community" </> s "new")
        , Url.map Community (s "community")
        , Url.map EditCommunity (s "community" </> s "edit")
        , Url.map CommunitySettings (s "community" </> s "settings")
        , Url.map CommunitySettingsFeatures (s "community" </> s "settings" </> s "features")
        , Url.map Objectives (s "community" </> s "objectives")
        , Url.map NewObjective (s "community" </> s "objectives" </> s "new")
        , Url.map EditObjective (s "community" </> s "objectives" </> int </> s "edit")
        , Url.map NewAction (s "community" </> Eos.symbolUrlParser </> s "objectives" </> int </> s "action" </> s "new")
        , Url.map EditAction (s "community" </> Eos.symbolUrlParser </> s "objectives" </> int </> s "action" </> int </> s "edit")
        , Url.map Claim (s "objectives" </> int </> s "action" </> int </> s "claim" </> int)
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
        , Url.map Transfer (s "community" </> s "transfer" <?> Query.string "to")
        , Url.map Analysis (s "dashboard" </> s "analysis")
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
    in
    maybeQuery
        |> Maybe.andThen (\query -> Url.fromString (protocol ++ host ++ port_ ++ query))
        |> Maybe.andThen (\url_ -> Url.parse (parser url_) url_)


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

                ProfileEditor ->
                    ( [ "profile", "edit" ], [] )

                ProfileAddKyc ->
                    ( [ "profile", "add-kyc" ], [] )

                ProfilePublic accountName ->
                    ( [ "profile", accountName ], [] )

                ProfileClaims account ->
                    ( [ "profile", account, "claims" ], [] )

                PaymentHistory accountName ->
                    ( [ "payments", accountName ], [] )

                Profile ->
                    ( [ "profile" ], [] )

                Dashboard ->
                    ( [ "dashboard" ], [] )

                Community ->
                    ( [ "community" ], [] )

                CommunitySettings ->
                    ( [ "community", "settings" ], [] )

                CommunitySettingsFeatures ->
                    ( [ "community", "settings", "features" ], [] )

                NewCommunity ->
                    ( [ "community", "new" ], [] )

                EditCommunity ->
                    ( [ "community", "edit" ], [] )

                Objectives ->
                    ( [ "community", "objectives" ], [] )

                NewObjective ->
                    ( [ "community", "objectives", "new" ], [] )

                EditObjective objectiveId ->
                    ( [ "community", "objectives", String.fromInt objectiveId, "edit" ], [] )

                NewAction symbol objectiveId ->
                    ( [ "community", Eos.symbolToString symbol, "objectives", String.fromInt objectiveId, "action", "new" ]
                    , []
                    )

                EditAction symbol objectiveId actionId ->
                    ( [ "community", Eos.symbolToString symbol, "objectives", String.fromInt objectiveId, "action", String.fromInt actionId, "edit" ]
                    , []
                    )

                Claim objectiveId actionId claimId ->
                    ( [ "objectives"
                      , String.fromInt objectiveId
                      , "action"
                      , String.fromInt actionId
                      , "claim"
                      , String.fromInt claimId
                      ]
                    , []
                    )

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

                Transfer maybeTo ->
                    ( [ "community"
                      , "transfer"
                      ]
                    , [ Url.Builder.string "to" (Maybe.withDefault "" maybeTo) ]
                    )

                Analysis ->
                    ( [ "dashboard", "analysis" ], [] )
    in
    Url.Builder.absolute paths queries
