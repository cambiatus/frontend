module Page.News exposing (Model, Msg, init, msgToString, update, view)

import Community
import Community.News
import Html exposing (Html, a, div, h1, h2, img, p, span, text)
import Html.Attributes exposing (class, classList, src, style)
import Icons
import List.Extra
import Maybe.Extra
import Page
import RemoteData
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import UpdateResult as UR
import Utils
import View.Components
import View.MarkdownEditor



-- TODO - If there are no news, this page "shouldn't exist"
-- TODO - Scroll to top on init
-- MODEL


type alias Model =
    { newsId : Maybe Int }


init : Maybe Int -> LoggedIn.Model -> UpdateResult
init maybeNewsId loggedIn =
    { newsId = maybeNewsId }
        |> UR.init
        |> UR.addExt (LoggedIn.RequestedReloadCommunityField Community.NewsField)



-- TYPES


type Msg
    = NoOp


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update _ model _ =
    UR.init model



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        title =
            -- TODO - I18N
            "News"

        content =
            case Community.getField loggedIn.selectedCommunity .news of
                RemoteData.Success ( community, news ) ->
                    div []
                        [ Page.viewHeader loggedIn title
                        , view_ loggedIn.shared model community news
                        ]

                RemoteData.Failure failure ->
                    case failure of
                        Community.FieldError err ->
                            -- TODO - I18N
                            Page.fullPageGraphQLError "Error fetching news" err

                        Community.CommunityError err ->
                            -- TODO - I18N
                            Page.fullPageGraphQLError "Error fetching community" err

                RemoteData.Loading ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.NotAsked ->
                    Page.fullPageLoading loggedIn.shared
    in
    { title = title, content = content }


view_ : Shared -> Model -> Community.Model -> List Community.News.Model -> Html Msg
view_ shared model community news =
    div []
        [ model.newsId
            |> Maybe.andThen (\newsId -> List.Extra.find (\{ id } -> id == newsId) news)
            |> Maybe.Extra.orElse community.highlightedNews
            |> Maybe.Extra.orElse (List.head news)
            |> Maybe.map viewMainNews
            -- TODO - Maybe show something when there are no news
            |> Maybe.withDefault (text "")
        , h1 [ class "container mx-auto px-4 mt-8 mb-4 text-lg font-bold text-gray-900" ]
            [ text "Read "
            , span [ class "text-purple-500" ] [ text "other news" ]
            ]
        , div [ class "bg-white" ]
            [ div [ class "container mx-auto px-4 pt-6 divide-y divide-gray-500 space-y-4" ]
                (news
                    |> List.Extra.groupWhile
                        (\n1 n2 ->
                            Utils.areSameDay
                                shared.timezone
                                n1.insertedAt
                                n2.insertedAt
                        )
                    |> List.map
                        (\( firstNews, otherNews ) ->
                            div [ class "pt-4 first:pt-0" ]
                                [ View.Components.dateViewer
                                    [ class "text-black text-sm font-bold uppercase" ]
                                    identity
                                    shared
                                    firstNews.insertedAt
                                , div [ class "divide-y divide-gray-500 space-y-4 mt-4" ]
                                    (List.map
                                        (\theseNews ->
                                            viewNewsSummary
                                                -- TODO - Use real data for hasRead
                                                (modBy 2 theseNews.id == 0)
                                                theseNews
                                        )
                                        (firstNews :: otherNews)
                                    )
                                ]
                        )
                )
            , div [ class "container mx-auto px-4 mt-16" ]
                [ img
                    [ class "mx-auto md:mr-0"
                    , src "/images/woman_announcer.svg"
                    ]
                    []
                ]
            ]
        ]


viewMainNews : Community.News.Model -> Html Msg
viewMainNews news =
    div [ class "bg-white" ]
        [ div [ class "container mx-auto px-4 py-10" ]
            [ h1 [ class "text-lg text-black font-bold" ] [ text news.title ]
            , View.MarkdownEditor.viewReadOnly [ class "mt-6 text-black colored-links" ]
                news.description
            ]

        -- TODO - Add Reactions
        ]


viewNewsSummary : Bool -> Community.News.Model -> Html Msg
viewNewsSummary hasRead news =
    let
        speechBubbleColor =
            if hasRead then
                "text-gray-900"

            else
                "text-purple-500"
    in
    div
        [ class "grid items-center pt-4 first:pt-0"
        , style "grid-template-columns" "28px 1fr 80px"
        ]
        [ Icons.speechBubble ("flex-shrink-0 stroke-current " ++ speechBubbleColor)
        , div
            [ class "truncate ml-4 mr-16"
            , classList
                [ ( "text-gray-900", hasRead )
                , ( "text-purple-500", not hasRead )
                ]
            ]
            [ h2 [ class "font-bold truncate" ] [ text news.title ]
            , p [ class "truncate" ]
                [ text <| View.MarkdownEditor.removeFormatting news.description ]
            ]
        , if not hasRead then
            a
                [ class "button button-primary w-auto px-4"
                , Route.href (Route.News (Just news.id))
                ]
                -- TODO - I18N
                [ text "Read" ]

          else
            text ""
        ]



-- UTILS


msgToString : Msg -> List String
msgToString _ =
    -- TODO
    []
