module Route exposing
    ( NewsEditorKind(..)
    , Route(..)
    , addEnvironmentToUrl
    , externalHref
    , fromUrl
    , href
    , loadExternalCommunity
    , pushUrl
    , replaceUrl
    )

import Browser.Navigation as Nav
import Environment exposing (Environment)
import Eos
import Eos.Account
import Html exposing (Attribute)
import Html.Attributes as Attr
import Session.Shared exposing (Shared)
import Shop
import Url exposing (Url)
import Url.Builder exposing (QueryParameter)
import Url.Parser as Url exposing ((</>), (<?>), Parser, int, oneOf, s, string, top)
import Url.Parser.Query as Query


type NewsEditorKind
    = CreateNews
    | EditNews Int
    | CopyNews Int


type Route
    = Root
    | ComingSoon
    | Register (Maybe String) (Maybe Route)
    | Login (Maybe String) (Maybe Route)
    | Logout
    | Notification
    | ProfileEditor
    | ProfileAddKyc
    | ProfileClaims String
    | ProfileAddContact
    | PaymentHistory Eos.Account.Name
    | Profile Eos.Account.Name
    | ProfileContributions Eos.Account.Name
    | Dashboard
    | Community
    | NewCommunity
    | News { selectedNews : Maybe Int, showOthers : Bool }
    | CommunitySettings
    | CommunitySettingsFeatures
    | CommunitySettingsInfo
    | CommunitySettingsNews
    | CommunitySettingsNewsEditor NewsEditorKind
    | CommunitySettingsCurrency
    | CommunitySettingsSponsorship
    | CommunitySettingsSponsorshipFiat
    | CommunitySettingsSponsorshipThankYouMessage
    | CommunitySelector (Maybe Route)
    | CommunityThankYou
    | CommunitySponsor
    | CommunitySupporters
    | Objectives
    | NewObjective
    | EditObjective Int
    | NewAction Int
    | EditAction Int Int
    | Claim Int Int Int
    | Shop Shop.Filter
    | NewSale
    | EditSale Int
    | ViewSale Int
    | ViewTransfer Int
    | Invite String
    | Join (Maybe Route)
    | Transfer (Maybe Eos.Account.Name)
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
        , Url.map (Just >> Login)
            (s "login"
                </> string
                <?> Query.map
                        (parseRedirect url)
                        (Query.string "redirect")
            )
        , Url.map (Login Nothing)
            (s "login"
                <?> Query.map
                        (parseRedirect url)
                        (Query.string "redirect")
            )
        , Url.map Logout (s "logout")
        , Url.map ProfileEditor (s "profile" </> s "edit")
        , Url.map ProfileAddKyc (s "profile" </> s "add-kyc")
        , Url.map ProfileAddContact (s "profile" </> s "add-contact")
        , Url.map Profile (s "profile" </> (string |> Url.map Eos.Account.stringToName))
        , Url.map ProfileContributions (s "profile" </> (string |> Url.map Eos.Account.stringToName) </> s "contributions")
        , Url.map ProfileClaims (s "profile" </> string </> s "claims")
        , Url.map PaymentHistory (s "payments" </> (string |> Url.map Eos.Account.stringToName))
        , Url.map Notification (s "notification")
        , Url.map Dashboard (s "dashboard")
        , Url.map NewCommunity (s "community" </> s "new")
        , Url.map (News { selectedNews = Nothing, showOthers = True }) (s "news")
        , Url.map (\newsId showOthers -> News { selectedNews = Just newsId, showOthers = showOthers })
            (s "news"
                </> int
                <?> Query.map (\showOthers -> showOthers /= Just "false")
                        (Query.string "showOthers")
            )
        , Url.map Community (s "community")
        , Url.map CommunitySettings (s "community" </> s "settings")
        , Url.map CommunitySettingsFeatures (s "community" </> s "settings" </> s "features")
        , Url.map CommunitySettingsInfo (s "community" </> s "settings" </> s "info")
        , Url.map CommunitySettingsNews (s "community" </> s "settings" </> s "news")
        , Url.map (CommunitySettingsNewsEditor CreateNews) (s "community" </> s "settings" </> s "news" </> s "new")
        , Url.map (EditNews >> CommunitySettingsNewsEditor) (s "community" </> s "settings" </> s "news" </> s "edit" </> int)
        , Url.map (CopyNews >> CommunitySettingsNewsEditor) (s "community" </> s "settings" </> s "news" </> s "copy" </> int)
        , Url.map CommunitySettingsCurrency (s "community" </> s "settings" </> s "currency")
        , Url.map CommunitySettingsSponsorship (s "community" </> s "settings" </> s "sponsorship")
        , Url.map CommunitySettingsSponsorshipFiat (s "community" </> s "settings" </> s "sponsorship" </> s "fiat")
        , Url.map CommunitySettingsSponsorshipThankYouMessage (s "community" </> s "settings" </> s "sponsorship" </> s "thank-you")
        , Url.map CommunitySelector
            (s "community"
                </> s "selector"
                <?> Query.map
                        (parseRedirect url)
                        (Query.string "redirect")
            )
        , Url.map CommunityThankYou (s "community" </> s "thank-you")
        , Url.map CommunitySponsor (s "community" </> s "sponsor")
        , Url.map CommunitySupporters (s "community" </> s "supporters")
        , Url.map Objectives (s "community" </> s "objectives")
        , Url.map NewObjective (s "community" </> s "objectives" </> s "new")
        , Url.map EditObjective (s "community" </> s "objectives" </> int </> s "edit")
        , Url.map NewAction (s "community" </> s "objectives" </> int </> s "action" </> s "new")
        , Url.map EditAction (s "community" </> s "objectives" </> int </> s "action" </> int </> s "edit")
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
        , Url.map ViewSale (s "shop" </> int)
        , Url.map EditSale (s "shop" </> int </> s "edit")
        , Url.map ViewTransfer (s "transfer" </> int)
        , Url.map Invite (s "invite" </> string)
        , Url.map Join
            (s "join"
                <?> Query.map
                        (parseRedirect url)
                        (Query.string "redirect")
            )
        , Url.map Transfer
            (s "community"
                </> s "transfer"
                <?> (Query.string "to"
                        |> Query.map (Maybe.map Eos.Account.stringToName)
                    )
            )
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


loadExternalCommunity : Shared -> { commuity | symbol : Eos.Symbol, subdomain : String } -> Route -> Cmd msg
loadExternalCommunity shared community route =
    if shared.useSubdomain then
        externalCommunityLink shared community.subdomain route
            |> Url.toString
            |> Nav.load

    else
        pushUrl shared.navKey route


externalHref : Shared -> { community | symbol : Eos.Symbol, subdomain : String } -> Route -> Attribute msg
externalHref shared community route =
    externalCommunityLink shared community.subdomain route
        |> Url.toString
        |> Attr.href


addEnvironmentToUrl : Environment -> Url.Url -> Url.Url
addEnvironmentToUrl environment url =
    let
        environmentString =
            case environment of
                Environment.Development ->
                    ".staging.cambiatus.io"

                Environment.Staging ->
                    ".staging.cambiatus.io"

                Environment.Demo ->
                    ".demo.cambiatus.io"

                Environment.Production ->
                    ".cambiatus.io"
    in
    { url | host = url.host ++ environmentString }



-- INTERNAL


{-| A link to a community. The link preserves the environment
(staging/demo/prod/localhost) based on the current url
-}
externalCommunityLink : Shared -> String -> Route -> Url
externalCommunityLink shared subdomain route =
    let
        currentUrl =
            shared.url

        communitySubdomain =
            subdomain
                |> String.split "."
                |> List.head
                |> Maybe.withDefault subdomain

        environments =
            [ "staging", "demo" ]

        defaultEnding =
            "cambiatus.io"

        possibleEndings =
            [ "localhost", defaultEnding ]

        ending =
            List.filter
                (\possibleEnding -> String.endsWith possibleEnding currentUrl.host)
                possibleEndings
                |> List.head
                |> Maybe.withDefault defaultEnding

        communityHost =
            case
                String.dropRight (String.length ending) currentUrl.host
                    |> String.split "."
                    |> List.filter (not << String.isEmpty)
            of
                [] ->
                    -- Something like `cambiatus.io`, `localhost`
                    String.join "." [ communitySubdomain, ending ]

                [ singlePart ] ->
                    if List.member singlePart environments then
                        -- Something like `staging.cambiatus.io`, `demo.cambiatus.io`
                        String.join "." [ communitySubdomain, singlePart, ending ]

                    else
                        -- Something like `community.cambiatus.io`
                        String.join "." [ communitySubdomain, ending ]

                _ :: env :: _ ->
                    -- Something like `community.staging.cambiatus.io`
                    String.join "." [ communitySubdomain, env, ending ]
    in
    { currentUrl
        | host = communityHost
        , path = routeToString route
    }


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


boolToString : Bool -> String
boolToString bool =
    if bool then
        "true"

    else
        "false"


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

                Login Nothing maybeRedirect ->
                    ( [ "login" ]
                    , queryBuilder routeToString maybeRedirect "redirect"
                    )

                Login (Just invitation) maybeRedirect ->
                    ( [ "login", invitation ]
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

                ProfileClaims account ->
                    ( [ "profile", account, "claims" ], [] )

                ProfileAddContact ->
                    ( [ "profile", "add-contact" ], [] )

                PaymentHistory accountName ->
                    ( [ "payments", Eos.Account.nameToString accountName ], [] )

                Profile accountName ->
                    ( [ "profile", Eos.Account.nameToString accountName ], [] )

                ProfileContributions accountName ->
                    ( [ "profile", Eos.Account.nameToString accountName, "contributions" ], [] )

                Dashboard ->
                    ( [ "dashboard" ], [] )

                Community ->
                    ( [ "community" ], [] )

                CommunitySettings ->
                    ( [ "community", "settings" ], [] )

                CommunitySettingsFeatures ->
                    ( [ "community", "settings", "features" ], [] )

                CommunitySettingsInfo ->
                    ( [ "community", "settings", "info" ], [] )

                CommunitySettingsNews ->
                    ( [ "community", "settings", "news" ], [] )

                CommunitySettingsNewsEditor CreateNews ->
                    ( [ "community", "settings", "news", "new" ], [] )

                CommunitySettingsNewsEditor (EditNews newsId) ->
                    ( [ "community", "settings", "news", "edit", String.fromInt newsId ], [] )

                CommunitySettingsNewsEditor (CopyNews newsId) ->
                    ( [ "community", "settings", "news", "copy", String.fromInt newsId ], [] )

                CommunitySettingsCurrency ->
                    ( [ "community", "settings", "currency" ], [] )

                CommunitySettingsSponsorship ->
                    ( [ "community", "settings", "sponsorship" ], [] )

                CommunitySettingsSponsorshipFiat ->
                    ( [ "community", "settings", "sponsorship", "fiat" ], [] )

                CommunitySettingsSponsorshipThankYouMessage ->
                    ( [ "community", "settings", "sponsorship", "thank-you" ], [] )

                CommunitySelector maybeRedirect ->
                    ( [ "community", "selector" ]
                    , queryBuilder routeToString maybeRedirect "redirect"
                    )

                CommunityThankYou ->
                    ( [ "community", "thank-you" ], [] )

                CommunitySponsor ->
                    ( [ "community", "sponsor" ], [] )

                CommunitySupporters ->
                    ( [ "community", "supporters" ], [] )

                NewCommunity ->
                    ( [ "community", "new" ], [] )

                News { selectedNews, showOthers } ->
                    case selectedNews of
                        Nothing ->
                            ( [ "news" ], [] )

                        Just newsId ->
                            ( [ "news", String.fromInt newsId ]
                            , if showOthers then
                                []

                              else
                                queryBuilder boolToString (Just showOthers) "showOthers"
                            )

                Objectives ->
                    ( [ "community", "objectives" ], [] )

                NewObjective ->
                    ( [ "community", "objectives", "new" ], [] )

                EditObjective objectiveId ->
                    ( [ "community", "objectives", String.fromInt objectiveId, "edit" ], [] )

                NewAction objectiveId ->
                    ( [ "community", "objectives", String.fromInt objectiveId, "action", "new" ]
                    , []
                    )

                EditAction objectiveId actionId ->
                    ( [ "community", "objectives", String.fromInt objectiveId, "action", String.fromInt actionId, "edit" ]
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
                    ( [ "shop", String.fromInt saleId, "edit" ], [] )

                ViewSale saleId ->
                    ( [ "shop", String.fromInt saleId ], [] )

                ViewTransfer transferId ->
                    ( [ "transfer", String.fromInt transferId ], [] )

                Invite invitationId ->
                    ( [ "invite", invitationId ], [] )

                Join maybeRedirect ->
                    ( [ "join" ]
                    , queryBuilder routeToString maybeRedirect "redirect"
                    )

                Transfer maybeTo ->
                    ( [ "community"
                      , "transfer"
                      ]
                    , [ Url.Builder.string "to"
                            (maybeTo
                                |> Maybe.map Eos.Account.nameToString
                                |> Maybe.withDefault ""
                            )
                      ]
                    )

                Analysis ->
                    ( [ "dashboard", "analysis" ], [] )
    in
    Url.Builder.absolute paths queries
