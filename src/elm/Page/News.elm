module Page.News exposing (Model, Msg, init, msgToString, update, view)

import Api.Graphql
import Browser.Dom
import Cambiatus.Mutation
import Community
import Community.News
import Dict
import Graphql.Http
import Html exposing (Html, button, details, div, h1, li, span, summary, text, ul)
import Html.Attributes exposing (class, classList, id, style, tabindex)
import Html.Attributes.Aria exposing (ariaHasPopup, ariaHidden, ariaLabel, role)
import Html.Events exposing (onClick)
import Icons
import Json.Encode
import List.Extra
import Log
import Maybe.Extra
import Page
import RemoteData exposing (RemoteData)
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared, Translators)
import Task
import UpdateResult as UR
import Utils exposing (onClickPreventAll)
import View.Components
import View.MarkdownEditor



-- MODEL


type alias Model =
    { newsId : Maybe Int
    , showReactionPicker : Bool
    , reactionCounts : List { reaction : Community.News.Reaction, count : Int }
    , selectedReactions : List Community.News.Reaction
    }


init : Maybe Int -> LoggedIn.Model -> UpdateResult
init maybeNewsId loggedIn =
    let
        markNewsAsRead =
            case maybeNewsId of
                Nothing ->
                    identity

                Just newsId ->
                    UR.addCmd
                        (Api.Graphql.mutation loggedIn.shared
                            (Just loggedIn.authToken)
                            (Cambiatus.Mutation.read
                                { newsId = newsId }
                                Community.News.receiptSelectionSet
                            )
                            CompletedMarkingNewsAsRead
                        )
    in
    { newsId = maybeNewsId
    , showReactionPicker = False

    -- TODO - Use real data
    , reactionCounts = Community.News.mockSelectedReactions
    , selectedReactions =
        Community.News.mockSelectedReactions
            |> List.head
            |> Maybe.map (.reaction >> List.singleton)
            |> Maybe.withDefault []
    }
        |> UR.init
        |> UR.addExt (LoggedIn.RequestedReloadCommunityField Community.NewsField)
        |> markNewsAsRead
        |> UR.addCmd
            (Browser.Dom.setViewport 0 0
                |> Task.perform (\_ -> NoOp)
            )



-- TYPES


type Msg
    = NoOp
    | CompletedMarkingNewsAsRead (RemoteData (Graphql.Http.Error (Maybe Community.News.Receipt)) (Maybe Community.News.Receipt))
    | ClickedToggleReactions
    | ToggledReaction Community.News.Reaction
    | CompletedTogglingReaction Community.News.Reaction (RemoteData (Graphql.Http.Error (Maybe Community.News.Receipt)) (Maybe Community.News.Receipt))


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        NoOp ->
            UR.init model

        CompletedMarkingNewsAsRead (RemoteData.Success _) ->
            UR.init model
                |> UR.addBreadcrumb
                    { type_ = Log.InfoBreadcrumb
                    , category = msg
                    , message = "Completed marking news as read"
                    , data =
                        case model.newsId of
                            Nothing ->
                                Dict.empty

                            Just newsId ->
                                Dict.fromList
                                    [ ( "News id", Json.Encode.int newsId )
                                    ]
                    , level = Log.Info
                    }

        CompletedMarkingNewsAsRead (RemoteData.Failure err) ->
            UR.init model
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when marking news as read"
                    { moduleName = "Page.News", function = "update" }
                    []
                    err

        CompletedMarkingNewsAsRead RemoteData.NotAsked ->
            UR.init model

        CompletedMarkingNewsAsRead RemoteData.Loading ->
            UR.init model

        ClickedToggleReactions ->
            { model | showReactionPicker = not model.showReactionPicker }
                |> UR.init
                |> UR.addCmd
                    (Browser.Dom.focus "reaction-0"
                        |> Task.attempt (\_ -> NoOp)
                    )

        ToggledReaction reaction ->
            case model.newsId of
                Just id ->
                    model
                        |> UR.init
                        |> UR.addCmd
                            (Api.Graphql.mutation loggedIn.shared
                                (Just loggedIn.authToken)
                                (Community.News.reactToNews
                                    { newsId = id
                                    , reactions =
                                        if List.member reaction model.selectedReactions then
                                            List.filter ((/=) reaction) model.selectedReactions

                                        else
                                            reaction :: model.selectedReactions
                                    }
                                )
                                (CompletedTogglingReaction reaction)
                            )

                Nothing ->
                    model
                        |> UR.init

        CompletedTogglingReaction reaction (RemoteData.Success receipt) ->
            -- TODO
            model
                |> UR.init

        CompletedTogglingReaction _ (RemoteData.Failure err) ->
            model
                |> UR.init
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when reacting to news"
                    { moduleName = "Page.News", function = "update" }
                    []
                    err

        CompletedTogglingReaction _ RemoteData.Loading ->
            UR.init model

        CompletedTogglingReaction _ RemoteData.NotAsked ->
            UR.init model



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        { t } =
            loggedIn.shared.translators

        title =
            t "news.title"

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
                                    (t "news.no_news.title")
                                    (t "news.no_news.description")

                            Just selectedNews ->
                                news
                                    |> List.filter
                                        (\n ->
                                            Community.News.isPublished
                                                loggedIn.shared.now
                                                n
                                                && (n /= selectedNews)
                                        )
                                    |> view_ loggedIn.shared
                                        model
                                        selectedNews
                        ]

                RemoteData.Failure failure ->
                    case failure of
                        Community.FieldError err ->
                            Page.fullPageGraphQLError title err

                        Community.CommunityError err ->
                            Page.fullPageGraphQLError title err

                RemoteData.Loading ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.NotAsked ->
                    Page.fullPageLoading loggedIn.shared
    in
    { title = title, content = content }


view_ : Shared -> Model -> Community.News.Model -> List Community.News.Model -> Html Msg
view_ shared model selectedNews news =
    let
        { t } =
            shared.translators
    in
    div []
        [ viewMainNews shared.translators model selectedNews
        , h1 [ class "container mx-auto px-4 mt-8 mb-4 text-lg font-bold text-gray-900" ]
            [ span [] [ text <| t "news.read" ]
            , text " "
            , span [ class "text-purple-500" ] [ text <| t "news.other_news" ]
            ]
        , div [ class "bg-white" ]
            [ Community.News.viewList shared
                [ class "container mx-auto px-4 pt-6" ]
                news
            ]
        ]


viewMainNews : Translators -> Model -> Community.News.Model -> Html Msg
viewMainNews translators model news =
    div [ class "bg-white" ]
        [ div [ class "container mx-auto px-4 pt-10 pb-4" ]
            [ h1 [ class "text-lg text-black font-bold" ] [ text news.title ]
            , View.MarkdownEditor.viewReadOnly [ class "mt-6 text-black colored-links" ]
                news.description
            , div [ class "flex items-center mt-8" ]
                (viewReactionPicker translators model
                    ++ viewReactions translators model
                )

            -- TODO - Show last edit
            ]
        ]


viewReactionPicker : Translators -> Model -> List (Html Msg)
viewReactionPicker { t } model =
    details
        [ class "inline-block relative z-10"
        , onClickPreventAll ClickedToggleReactions
        , if model.showReactionPicker then
            Html.Attributes.attribute "open" ""

          else
            class ""
        ]
        [ summary
            [ role "button"
            , ariaHasPopup "true"
            , ariaLabel (t "news.reaction.choose")
            , class "list-none bg-gray-200 rounded-full p-0.5 focus-ring transition-colors hover:bg-gray-500 active:bg-gray-300"
            ]
            [ Icons.smilingFace "fill-current text-gray-900 w-6 h-6"
            ]
        , View.Components.focusTrap
            { firstFocusContainer = Nothing }
            []
            [ ul
                [ class "absolute shadow grid gap-1 p-2 rounded-sm bg-white"
                , classList [ ( "animate-bounce-in", model.showReactionPicker ) ]
                , style "grid-template-columns" "repeat(5, 1fr)"
                ]
                (List.indexedMap
                    (\index reaction ->
                        li []
                            [ button
                                [ class "w-8 h-8 flex items-center justify-center rounded-sm focus-ring transition-colors"
                                , classList
                                    [ ( "bg-green bg-opacity-80 hover:bg-opacity-60", List.member reaction model.selectedReactions )
                                    , ( "bg-white hover:bg-gray-200 focus-visible:bg-gray-200", not (List.member reaction model.selectedReactions) )
                                    ]

                                -- TODO - Handle click
                                , onClick NoOp
                                , id ("reaction-" ++ String.fromInt index)
                                ]
                                [ text (Community.News.reactionToString reaction) ]
                            ]
                    )
                    Community.News.listReactions
                )
            ]
        ]
        :: (if model.showReactionPicker then
                [ button
                    [ class "fixed top-0 right-0 w-screen h-screen cursor-auto"
                    , onClick ClickedToggleReactions
                    , ariaLabel "close reaction picker"
                    , tabindex -1
                    ]
                    []
                , View.Components.keyListener
                    { onKeyDown =
                        { acceptedKeys = [ View.Components.Escape ]
                        , toMsg = \_ -> ClickedToggleReactions
                        , stopPropagation = True
                        }
                    }
                ]

            else
                []
           )


viewReactions : Translators -> Model -> List (Html Msg)
viewReactions { tr } model =
    let
        isSelected reaction =
            List.member reaction model.selectedReactions
    in
    model.reactionCounts
        |> List.filter (\{ count } -> count > 0)
        |> List.map
            (\{ reaction, count } ->
                button
                    [ class "ml-4 rounded-full py-0.5 px-2 flex gap-1 focus-ring transition-colors"
                    , classList
                        [ ( "bg-green bg-opacity-50 border border-green hover:bg-opacity-40 active:bg-opacity-60", isSelected reaction )
                        , ( "bg-gray-200 hover:bg-gray-500 active:bg-gray-300", not (isSelected reaction) )
                        ]
                    , ariaLabel <|
                        tr "news.reaction.reactions"
                            [ ( "count", String.fromInt count )
                            , ( "reaction", Community.News.reactionName reaction )
                            ]
                    , onClick (ToggledReaction reaction)
                    ]
                    [ span [ ariaHidden True ] [ text (Community.News.reactionToString reaction) ]
                    , span [ ariaHidden True ] [ text (String.fromInt count) ]
                    ]
            )



-- UTILS


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        CompletedMarkingNewsAsRead r ->
            [ "CompletedMarkingNewsAsRead", UR.remoteDataToString r ]

        ClickedToggleReactions ->
            [ "ClickedToggleReactions" ]

        ToggledReaction _ ->
            [ "ToggledReaction" ]

        CompletedTogglingReaction _ _ ->
            [ "CompletedTogglingReaction" ]
