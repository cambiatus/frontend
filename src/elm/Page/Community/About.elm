module Page.Community.About exposing (Model, Msg, init, jsAddressToMsg, msgToString, receiveBroadcast, update, view)

import Avatar
import Community
import Community.News
import Eos
import Html exposing (Html, a, button, div, h1, h2, hr, img, li, p, span, text, ul)
import Html.Attributes exposing (alt, class, classList, href, media, src, style)
import Html.Events exposing (onClick)
import Http
import Icons
import Json.Encode as Encode
import List.Extra
import Log
import Markdown
import Maybe.Extra
import Page
import Profile.Contact as Contact
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import Token
import Translation
import UpdateResult as UR
import Url
import Utils
import View.Components



-- MODEL


type alias Model =
    { tokenInfo : RemoteData Http.Error Token.Model }


init : LoggedIn.Model -> UpdateResult
init loggedIn =
    { tokenInfo = RemoteData.Loading }
        |> UR.init
        |> UR.addCmd (LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn)
        |> UR.addExt (LoggedIn.RequestedCommunityField Community.UploadsField)
        |> UR.addExt (LoggedIn.RequestedCommunityField Community.ContributionsField)
        |> UR.addExt (LoggedIn.RequestedCommunityField Community.NewsField)



-- TYPES


type Msg
    = NoOp
    | CompletedLoadCommunity Community.Model
    | GotTokenInfo (Result Http.Error Token.Model)
    | ClickedShareCommunity


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        NoOp ->
            UR.init model

        CompletedLoadCommunity community ->
            UR.init model
                |> UR.addCmd (Token.getToken loggedIn.shared community.symbol GotTokenInfo)

        GotTokenInfo (Ok tokenInfo) ->
            { model | tokenInfo = RemoteData.Success tokenInfo }
                |> UR.init

        GotTokenInfo (Err err) ->
            { model | tokenInfo = RemoteData.Failure err }
                |> UR.init
                |> UR.logHttpError msg
                    (Just loggedIn.accountName)
                    "Got an error when retrieving token info"
                    { moduleName = "Page.Community.About", function = "update" }
                    [ Log.contextFromCommunity loggedIn.selectedCommunity ]
                    err

        ClickedShareCommunity ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    UR.init model
                        |> UR.addPort
                            { responseAddress = msg
                            , responseData = Encode.null
                            , data =
                                Encode.object
                                    [ ( "name", Encode.string "share" )
                                    , ( "title", Encode.string community.name )
                                    , ( "url"
                                      , loggedIn.shared.url
                                            |> Url.toString
                                            |> Encode.string
                                      )
                                    ]
                            }

                _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Tried sharing community, but community wasn't loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.About", function = "update" }
                            []



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        defaultCoverPhoto =
            Html.node "picture"
                []
                [ Html.source
                    [ Html.Attributes.attribute "srcset" "/images/community-bg-mobile.svg"

                    -- md breakpoint
                    , media "(min-width:768px)"
                    ]
                    []
                , img
                    [ src "/images/community-bg-desktop.svg"
                    , class "mb-6 object-cover min-w-full max-h-43 sm:max-h-60 lg:max-h-80"
                    , style "object-position" "50% 62%"
                    , alt ""
                    ]
                    []
                ]

        { t, tr } =
            loggedIn.shared.translators
    in
    { title = t "community.index.about_title"
    , content =
        case loggedIn.selectedCommunity of
            RemoteData.Success community ->
                div [ class "mb-20" ]
                    [ case RemoteData.map List.head community.uploads of
                        RemoteData.Success (Just upload) ->
                            img
                                [ src upload
                                , class "mb-6 object-cover min-w-full max-h-43 sm:max-h-60 lg:max-h-80"
                                , alt ""
                                ]
                                []

                        RemoteData.Loading ->
                            div [ class "w-full h-43 sm:h-60 lg:h-80 animate-skeleton-loading" ] []

                        _ ->
                            defaultCoverPhoto
                    , div [ class "container mx-auto px-4 py-10" ]
                        [ div [ class "grid lg:grid-cols-3 gap-6" ]
                            [ viewCommunityCard loggedIn.shared community
                            , if showSupportersCard community then
                                viewSupportersCard loggedIn.shared.translators community

                              else
                                text ""
                            , if showNewsCard community then
                                viewNewsCard loggedIn.shared community

                              else
                                text ""
                            ]
                        , h2 [ class "mt-6" ]
                            [ span [] [ text <| t "community.index.our_numbers" ]
                            , text " "
                            , span [ class "font-bold" ] [ text <| t "community.index.numbers" ]
                            ]
                        , div [ class "mt-4 grid sm:grid-cols-2 md:grid-cols-3 gap-x-6 gap-y-4" ]
                            [ case model.tokenInfo of
                                RemoteData.Success tokenInfo ->
                                    viewStatsCard loggedIn.shared.translators
                                        { number = round tokenInfo.supply.amount
                                        , description =
                                            tr "community.index.amount_in_circulation"
                                                [ ( "currency_name", Eos.symbolToSymbolCodeString community.symbol ) ]
                                        , imgSrc = "/images/three_coins.svg"
                                        , imgClass = "place-self-center"
                                        }

                                RemoteData.Loading ->
                                    div [ class "rounded animate-skeleton-loading" ] []

                                _ ->
                                    text ""
                            , viewStatsCard loggedIn.shared.translators
                                { number = community.memberCount
                                , description = t "community.index.members"
                                , imgSrc = "/images/man_sipping_coconut_water.svg"
                                , imgClass = "mt-auto mx-auto"
                                }
                            , viewStatsCard loggedIn.shared.translators
                                { number = community.claimCount
                                , description = t "community.index.claims"
                                , imgSrc = "/images/woman_driving_truck.svg"
                                , imgClass = "self-center"
                                }
                            , viewStatsCard loggedIn.shared.translators
                                { number = community.transferCount
                                , description = t "community.index.transfers"
                                , imgSrc = "/images/people_trading_bread.svg"
                                , imgClass = "mt-auto mx-auto"
                                }
                            , viewStatsCard loggedIn.shared.translators
                                { number = community.productCount
                                , description = t "community.index.products"
                                , imgSrc = "/images/man_in_shop.svg"
                                , imgClass = "mt-auto"
                                }
                            , viewStatsCard loggedIn.shared.translators
                                { number = community.orderCount
                                , description = t "community.index.orders"
                                , imgSrc = "/images/dog_walking_with_purse.svg"
                                , imgClass = "place-self-center"
                                }
                            ]
                        ]
                    ]

            RemoteData.Loading ->
                Page.fullPageLoading loggedIn.shared

            RemoteData.NotAsked ->
                Page.fullPageLoading loggedIn.shared

            RemoteData.Failure e ->
                Page.fullPageGraphQLError (t "community.index.about_title") e
    }


viewCommunityCard : Shared -> Community.Model -> Html Msg
viewCommunityCard ({ translators } as shared) community =
    div
        [ classList
            [ ( "lg:col-start-2", not (showSupportersCard community) && not (showNewsCard community) )
            , ( "lg:col-span-2"
              , xor (not <| showSupportersCard community)
                    (not <| showNewsCard community)
              )
            ]
        ]
        -- We need some hidden text so the cards are aligned on desktop
        [ p [ class "mb-4 opacity-0 pointer-events-none hidden lg:block" ] [ text "hidden text" ]
        , div [ class "flex flex-col bg-white rounded relative px-4 pt-4 pb-6" ]
            -- TW doesn't support translating on the x and y axies simultaneously,
            -- so we need a parent element to center the icon horizontally
            [ div [ class "absolute w-full top-0 left-0 flex justify-center -translate-y-1/2 pointer-events-none" ]
                [ div
                    [ class "h-21 w-21 bg-white p-4 flex items-center justify-center rounded-full shadow-sm"
                    ]
                    [ img
                        [ src community.logo
                        , class "max-w-full max-h-full"
                        , alt <| translators.tr "community.index.logo" [ ( "community", community.name ) ]
                        ]
                        []
                    ]
                ]
            , if shared.canShare then
                button
                    [ class "bg-gray-100 p-2 rounded-full ml-auto focus-ring hover:opacity-80"
                    , onClick ClickedShareCommunity
                    ]
                    [ Icons.share "" ]

              else
                text ""
            , h1
                [ class "text-lg font-bold text-center"
                , classList [ ( "mt-10", not shared.canShare ) ]
                ]
                [ text community.name ]
            , case community.website of
                Nothing ->
                    text ""

                Just website ->
                    let
                        removeProtocol site =
                            if String.startsWith "http://" site then
                                -- Remove http://
                                String.dropLeft 7 site

                            else if String.startsWith "https://" website then
                                -- Remove https://
                                String.dropLeft 8 site

                            else
                                site

                        removeTrailingSlash site =
                            if String.endsWith "/" site then
                                String.dropRight 1 site

                            else
                                site
                    in
                    a
                        [ href website
                        , class "mx-auto text-sm font-bold text-gray-900 hover:underline focus-ring"
                        ]
                        [ website
                            |> removeProtocol
                            |> removeTrailingSlash
                            |> text
                        ]
            , Markdown.view [ class "mt-6" ] community.description
            , ul [ class "flex flex-wrap mt-8 gap-x-6 gap-y-4 items-center justify-center" ]
                (community.contacts
                    |> List.map
                        (\contact ->
                            li [ class "w-10 h-10" ]
                                [ Contact.circularIconWithGrayBg translators "" contact
                                ]
                        )
                )
            ]
        ]


viewSupportersCard : Translation.Translators -> Community.Model -> Html Msg
viewSupportersCard { t } community =
    let
        compareAvatars first second =
            case ( Avatar.toMaybeString first, Avatar.toMaybeString second ) of
                ( Just _, Just _ ) ->
                    EQ

                ( Just _, Nothing ) ->
                    LT

                ( Nothing, Just _ ) ->
                    GT

                ( Nothing, Nothing ) ->
                    EQ

        viewLoading =
            List.range 0 4
                |> List.map
                    (\index ->
                        div
                            [ class "w-14 h-14 rounded-full -mr-2 border border-white bg-gray-300 animate-skeleton-loading last:mr-0"
                            , style "animation-delay" (String.fromInt (index * 100) ++ "ms")
                            ]
                            []
                    )
    in
    div []
        [ h2 []
            [ span [] [ text <| t "community.index.our_supporters" ]
            , text " "
            , span [ class "font-bold" ] [ text <| t "community.index.supporters" ]
            ]
        , div [ class "bg-white rounded px-4 pt-4 pb-6 mt-4" ]
            [ p [] [ text <| t "community.index.you_can_support" ]
            , a
                [ class "button button-primary w-full mt-6"
                , Route.href Route.CommunitySponsor
                ]
                [ text <| t "community.index.support_us" ]
            , p [ class "mt-8 text-center" ] [ text <| t "community.index.see_supporters" ]
            , div [ class "flex flex-wrap mt-4 justify-center" ]
                (case community.contributions of
                    RemoteData.Success contributions ->
                        contributions
                            |> List.map (.user >> .avatar)
                            |> List.Extra.unique
                            |> List.sortWith compareAvatars
                            |> List.take 5
                            |> List.map (\avatar -> Avatar.view avatar "w-14 h-14 object-cover rounded-full -mr-2 border border-white last:mr-0")

                    RemoteData.Loading ->
                        viewLoading

                    RemoteData.NotAsked ->
                        viewLoading

                    RemoteData.Failure _ ->
                        []
                )
            , a
                [ class "button button-secondary w-full mt-4"
                , Route.href Route.CommunitySupporters
                ]
                [ text <| t "community.index.see_all_supporters" ]
            ]
        ]


viewNewsCard : Shared -> Community.Model -> Html Msg
viewNewsCard ({ translators } as shared) community =
    let
        { t } =
            translators
    in
    div [ class "mt-4 lg:mt-0" ]
        [ h2 []
            [ span [] [ text <| t "community.index.our_messages" ]
            , text " "
            , span [ class "font-bold" ] [ text <| t "community.index.messages" ]
            ]
        , div [ class "bg-white rounded px-4 pt-4 pb-6 mt-4 relative" ]
            (img
                [ src "/images/doggo-announcing.svg"
                , class "absolute top-0 right-0 -translate-y-full"
                , alt ""
                ]
                []
                :: (case community.news of
                        RemoteData.Success news ->
                            if List.isEmpty news then
                                [ p [ class "text-center text-gray-900 pt-10 pb-8" ]
                                    [ text <| t "menu.coming_soon" ]
                                ]

                            else
                                [ news
                                    |> List.filter (Community.News.isPublished shared.now)
                                    |> List.take 3
                                    |> Community.News.viewList shared []
                                , hr [ class "mb-4 border-gray-100" ] []
                                , a
                                    [ class "button button-secondary w-full"
                                    , Route.href (Route.News { selectedNews = Nothing, showOthers = True })
                                    ]
                                    [ text <| t "news.view_more" ]
                                ]

                        RemoteData.Loading ->
                            [ View.Components.loadingLogoAnimated translators "mb-10" ]

                        RemoteData.NotAsked ->
                            [ View.Components.loadingLogoAnimated translators "mb-10" ]

                        RemoteData.Failure _ ->
                            [ p [ class "text-center text-gray-900 pt-10 pb-8" ]
                                [ text <| t "news.error_fetching" ]
                            ]
                   )
            )
        ]


viewStatsCard :
    Translation.Translators
    ->
        { number : Int
        , description : String
        , imgSrc : String
        , imgClass : String
        }
    -> Html Msg
viewStatsCard translators { number, description, imgSrc, imgClass } =
    div [ class "grid grid-cols-3 bg-white rounded" ]
        [ img
            [ src imgSrc
            , class imgClass
            ]
            []
        , div [ class "col-span-2 py-6 text-center" ]
            [ p [ class "text-xl font-bold text-green" ]
                [ number
                    |> Utils.formatInt (Just translators)
                    |> text
                ]
            , p [ class "uppercase text-sm font-bold text-gray-900" ]
                [ text description ]
            ]
        ]


showSupportersCard : Community.Model -> Bool
showSupportersCard community =
    community.contributionConfiguration
        |> Maybe.andThen .paypalAccount
        |> Maybe.Extra.isJust


showNewsCard : Community.Model -> Bool
showNewsCard community =
    community.hasNews



-- UTILS


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityLoaded community ->
            Just (CompletedLoadCommunity community)

        _ ->
            Nothing


jsAddressToMsg : List String -> Encode.Value -> Maybe Msg
jsAddressToMsg addr _ =
    case addr of
        "ClickedShareCommunity" :: _ ->
            Just NoOp

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        GotTokenInfo r ->
            [ "GotTokenInfo", UR.resultToString r ]

        ClickedShareCommunity ->
            [ "ClickedShareCommunity" ]
