module Route exposing
    ( EditSaleStep(..)
    , NewsEditorKind(..)
    , Route(..)
    , SelectedObjective(..)
    , addEnvironmentToUrl
    , addRouteToUrl
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
import Shop.Category
import Url exposing (Url)
import Url.Builder exposing (QueryParameter)
import Url.Parser as Url exposing ((</>), (<?>), Parser, int, oneOf, s, string, top)
import Url.Parser.Query as Query


type NewsEditorKind
    = CreateNews
    | EditNews Int
    | CopyNews Int


type SelectedObjective
    = WithObjectiveSelected { id : Int, action : Maybe Int }
    | WithNoObjectiveSelected


type EditSaleStep
    = SaleMainInformation
    | SaleImages
    | SaleCategories
    | SalePriceAndInventory


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
    | CommunityAbout
    | CommunityObjectives SelectedObjective
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
    | CommunitySettingsObjectives
    | CommunitySettingsNewObjective
    | CommunitySettingsEditObjective Int
    | CommunitySettingsNewAction Int
    | CommunitySettingsEditAction Int Int
    | CommunitySettingsContacts
    | CommunitySettingsShopCategories
    | CommunitySelector (Maybe Route)
    | CommunityThankYou
    | CommunitySponsor
    | CommunitySupporters
    | Claim Int Int Int
    | Settings
    | Shop { owner : Maybe Eos.Account.Name, categories : List Shop.Category.Id }
    | NewSale EditSaleStep
    | EditSale Shop.Id EditSaleStep
    | ViewSale Shop.Id
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
        , Url.map CommunityAbout (s "community" </> s "about")
        , Url.map (CommunityObjectives WithNoObjectiveSelected)
            (s "community"
                </> s "objectives"
            )
        , Url.map (\objectiveId -> CommunityObjectives (WithObjectiveSelected { id = objectiveId, action = Nothing }))
            (s "community"
                </> s "objectives"
                </> int
            )
        , Url.map
            (\objectiveId actionId ->
                CommunityObjectives (WithObjectiveSelected { id = objectiveId, action = Just actionId })
            )
            (s "community"
                </> s "objectives"
                </> int
                </> s "action"
                </> int
            )
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
        , Url.map CommunitySettingsObjectives (s "community" </> s "settings" </> s "objectives")
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
        , Url.map CommunitySettingsNewObjective (s "community" </> s "settings" </> s "objectives" </> s "new")
        , Url.map CommunitySettingsEditObjective (s "community" </> s "settings" </> s "objectives" </> int </> s "edit")
        , Url.map CommunitySettingsNewAction (s "community" </> s "settings" </> s "objectives" </> int </> s "action" </> s "new")
        , Url.map CommunitySettingsEditAction (s "community" </> s "settings" </> s "objectives" </> int </> s "action" </> int </> s "edit")
        , Url.map CommunitySettingsContacts (s "community" </> s "settings" </> s "contacts")
        , Url.map CommunitySettingsShopCategories (s "community" </> s "settings" </> s "shop" </> s "categories")
        , Url.map Claim (s "objectives" </> int </> s "action" </> int </> s "claim" </> int)
        , Url.map Settings (s "settings")
        , Url.map (\maybeOwner categoriesIds -> Shop { owner = maybeOwner, categories = categoriesIds })
            (s "shop"
                <?> (Query.string "from"
                        |> Query.map (Maybe.map Eos.Account.stringToName)
                    )
                <?> (Query.string "categories"
                        |> Query.map
                            (Maybe.map
                                (String.split ","
                                    >> List.filterMap Shop.Category.idFromString
                                )
                                >> Maybe.withDefault []
                            )
                    )
            )
        , Url.map NewSale
            (s "shop"
                </> s "new"
                </> s "sell"
                <?> Query.map saleStepFromString (Query.string "step")
            )
        , Url.map ViewSale (s "shop" </> Shop.idUrlParser)
        , Url.map EditSale
            (s "shop"
                </> Shop.idUrlParser
                </> s "edit"
                <?> Query.map saleStepFromString (Query.string "step")
            )
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


addRouteToUrl :
    { shared | url : Url.Url }
    -> Route
    -> Url.Url
addRouteToUrl shared route =
    let
        protocol =
            case shared.url.protocol of
                Url.Http ->
                    "http://"

                Url.Https ->
                    "https://"

        port_ =
            case shared.url.port_ of
                Nothing ->
                    ""

                Just p ->
                    ":" ++ String.fromInt p
    in
    (protocol ++ shared.url.host ++ port_ ++ routeToString route)
        |> Url.fromString
        |> Maybe.withDefault shared.url



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


saleStepFromString : Maybe String -> EditSaleStep
saleStepFromString stepString =
    case stepString of
        Just "mainInformation" ->
            SaleMainInformation

        Just "images" ->
            SaleImages

        Just "categories" ->
            SaleCategories

        Just "priceAndInventory" ->
            SalePriceAndInventory

        _ ->
            SaleMainInformation


saleStepToString : EditSaleStep -> String
saleStepToString step =
    case step of
        SaleMainInformation ->
            "mainInformation"

        SaleImages ->
            "images"

        SaleCategories ->
            "categories"

        SalePriceAndInventory ->
            "priceAndInventory"


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

                CommunityAbout ->
                    ( [ "community", "about" ], [] )

                CommunityObjectives selectedObjective ->
                    let
                        params =
                            case selectedObjective of
                                WithNoObjectiveSelected ->
                                    []

                                WithObjectiveSelected { id, action } ->
                                    case action of
                                        Nothing ->
                                            [ String.fromInt id ]

                                        Just actionId ->
                                            [ String.fromInt id, "action", String.fromInt actionId ]
                    in
                    ( "community" :: "objectives" :: params, [] )

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

                CommunitySettingsObjectives ->
                    ( [ "community", "settings", "objectives" ], [] )

                CommunitySettingsNewObjective ->
                    ( [ "community", "settings", "objectives", "new" ], [] )

                CommunitySettingsEditObjective objectiveId ->
                    ( [ "community", "settings", "objectives", String.fromInt objectiveId, "edit" ], [] )

                CommunitySettingsNewAction objectiveId ->
                    ( [ "community", "settings", "objectives", String.fromInt objectiveId, "action", "new" ]
                    , []
                    )

                CommunitySettingsEditAction objectiveId actionId ->
                    ( [ "community", "settings", "objectives", String.fromInt objectiveId, "action", String.fromInt actionId, "edit" ]
                    , []
                    )

                CommunitySettingsContacts ->
                    ( [ "community", "settings", "contacts" ], [] )

                CommunitySettingsShopCategories ->
                    ( [ "community", "settings", "shop", "categories" ], [] )

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

                Settings ->
                    ( [ "settings" ], [] )

                Shop { owner, categories } ->
                    ( [ "shop" ]
                    , [ owner
                            |> Maybe.map
                                (Eos.Account.nameToString
                                    >> Url.Builder.string "from"
                                )
                      , if List.isEmpty categories then
                            Nothing

                        else
                            categories
                                |> List.map Shop.Category.idToString
                                |> String.join ","
                                |> Url.Builder.string "categories"
                                |> Just
                      ]
                        |> List.filterMap identity
                    )

                NewSale saleStep ->
                    ( [ "shop", "new", "sell" ]
                    , queryBuilder saleStepToString (Just saleStep) "step"
                    )

                EditSale saleId saleStep ->
                    ( [ "shop", Shop.idToString saleId, "edit" ]
                    , queryBuilder saleStepToString (Just saleStep) "step"
                    )

                ViewSale saleId ->
                    ( [ "shop", Shop.idToString saleId ], [] )

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
