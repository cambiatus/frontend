module Page.News exposing (Model, Msg, init, msgToString, update, view)

import Browser.Dom
import Community
import Community.News
import Html exposing (Html, div, h1, span, text)
import Html.Attributes exposing (class)
import List.Extra
import Maybe.Extra
import Page
import RemoteData
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import Task
import UpdateResult as UR
import View.MarkdownEditor



-- TODO - Mark news as read
-- MODEL


type alias Model =
    { newsId : Maybe Int }


init : Maybe Int -> LoggedIn.Model -> UpdateResult
init maybeNewsId _ =
    { newsId = maybeNewsId }
        |> UR.init
        |> UR.addExt (LoggedIn.RequestedReloadCommunityField Community.NewsField)
        |> UR.addCmd
            (Browser.Dom.setViewport 0 0
                |> Task.perform (\_ -> NoOp)
            )



-- TYPES


type Msg
    = NoOp


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model _ =
    case msg of
        NoOp ->
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
                    let
                        maybeSelectedNews : Maybe Community.News.Model
                        maybeSelectedNews =
                            if not community.hasNews then
                                Nothing

                            else
                                model.newsId
                                    |> Maybe.andThen (\newsId -> List.Extra.find (\{ id } -> id == newsId) news)
                                    |> Maybe.Extra.orElse community.highlightedNews
                                    |> Maybe.Extra.orElse (List.head news)
                    in
                    div []
                        [ Page.viewHeader loggedIn title
                        , case maybeSelectedNews of
                            Nothing ->
                                Page.fullPageNotFound
                                    -- TODO - I18N
                                    "There's nothing to see here"
                                    "Your community hasn't posted any communications"

                            Just selectedNews ->
                                view_ loggedIn.shared
                                    selectedNews
                                    (List.filter ((/=) selectedNews) news)
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


view_ : Shared -> Community.News.Model -> List Community.News.Model -> Html Msg
view_ shared selectedNews news =
    div []
        [ viewMainNews selectedNews
        , h1 [ class "container mx-auto px-4 mt-8 mb-4 text-lg font-bold text-gray-900" ]
            -- TODO - I18N
            [ text "Read "
            , span [ class "text-purple-500" ] [ text "other news" ]
            ]
        , div [ class "bg-white" ]
            [ Community.News.viewList shared
                [ class "container mx-auto px-4 pt-6" ]
                news
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



-- UTILS


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]
