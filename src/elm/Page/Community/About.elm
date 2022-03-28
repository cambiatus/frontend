module Page.Community.About exposing (Model, Msg, init, jsAddressToMsg, msgToString, update, view)

import Avatar
import Community
import Community.News
import Html exposing (Html, a, button, div, h1, h2, hr, img, li, p, span, text, ul)
import Html.Attributes exposing (alt, class, href, media, src, style)
import Html.Events exposing (onClick)
import Icons
import Json.Encode as Encode
import List.Extra
import Markdown
import Profile.Contact as Contact
import RemoteData
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import Translation
import UpdateResult as UR
import Url
import View.Components



-- MODEL


type alias Model =
    {}


init : LoggedIn.Model -> UpdateResult
init _ =
    UR.init {}
        |> UR.addExt (LoggedIn.RequestedCommunityField Community.UploadsField)
        |> UR.addExt (LoggedIn.RequestedCommunityField Community.ContributionsField)
        |> UR.addExt (LoggedIn.RequestedCommunityField Community.NewsField)



-- TYPES


type Msg
    = NoOp
    | ClickedShareCommunity


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        NoOp ->
            UR.init model

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

                                    -- TODO - Figure out a nice description
                                    -- TODO - Detect if can share. If can't, do what? Just hide the button?
                                    -- , ( "text", Markdown.encode community.description )
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
                    , class "mb-6 object-cover min-w-full max-h-[10.75rem] sm:max-h-60 lg:max-h-80"
                    , style "object-position" "50% 62%"
                    , alt ""
                    ]
                    []
                ]
    in
    { title = "TODO"
    , content =
        case loggedIn.selectedCommunity of
            RemoteData.Success community ->
                div []
                    [ case RemoteData.map List.head community.uploads of
                        RemoteData.Success (Just upload) ->
                            img
                                [ src upload
                                , class "mb-6 object-cover min-w-full max-h-43 sm:max-h-60 lg:max-h-80"
                                , alt ""
                                ]
                                []

                        _ ->
                            defaultCoverPhoto
                    , div [ class "container mx-auto px-4 py-10 grid md:grid-cols-3 gap-6" ]
                        [ viewCommunityCard loggedIn.shared.translators community
                        , viewSupportersCard loggedIn.shared.translators community
                        , viewNewsCard loggedIn.shared community
                        ]
                    ]

            _ ->
                text "TODO"
    }


viewCommunityCard : Translation.Translators -> Community.Model -> Html Msg
viewCommunityCard translators community =
    div []
        -- We need some hidden text so the cards are aligned on desktop
        [ p [ class "mb-4 opacity-0 pointer-events-none hidden md:block" ] [ text "hidden text" ]
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

                        -- TODO - Add alt explaining it's the community's logo
                        , alt ""
                        ]
                        []
                    ]
                ]
            , button
                [ class "bg-gray-100 p-2 rounded-full ml-auto focus-ring"
                , onClick ClickedShareCommunity
                ]
                [ Icons.share "" ]
            , h1 [ class "text-lg font-bold text-center" ] [ text community.name ]
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
    div []
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
                                    -- TODO - Change text to `See all news`
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



-- UTILS


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

        ClickedShareCommunity ->
            [ "ClickedShareCommunity" ]
