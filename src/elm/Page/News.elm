module Page.News exposing (Model, Msg, init, msgToString, receiveBroadcast, update, view)

import Browser.Dom
import Cambiatus.Mutation
import Community
import Community.News
import Dict
import Eos.Account
import Graphql.Http
import Html exposing (Html, a, button, details, div, h2, li, p, span, summary, text, ul)
import Html.Attributes exposing (class, classList, id, style, tabindex, type_)
import Html.Attributes.Aria exposing (ariaHasPopup, ariaHidden, ariaLabel, role)
import Html.Events exposing (onClick)
import Html.Keyed
import Icons
import Json.Encode
import List.Extra
import Log
import Markdown
import Page
import Profile.Summary
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Translators)
import Task
import UpdateResult as UR
import Utils exposing (onClickPreventAll)
import View.Components



-- MODEL


type alias Model =
    { newsId : Maybe Int
    , showReactionPicker : Bool
    , showOtherNews : Bool
    , lastEditorSummary : Profile.Summary.Model
    }


init : { selectedNews : Maybe Int, showOthers : Bool } -> LoggedIn.Model -> UpdateResult
init { selectedNews, showOthers } _ =
    { newsId = selectedNews
    , showReactionPicker = False
    , showOtherNews = showOthers
    , lastEditorSummary = Profile.Summary.init False
    }
        |> UR.init
        |> UR.addExt (LoggedIn.RequestedReloadCommunityField Community.NewsField)
        |> UR.addCmd
            (Browser.Dom.setViewport 0 0
                |> Task.perform (\_ -> NoOp)
            )



-- TYPES


type Msg
    = NoOp
    | CompletedLoadNews (List Community.News.Model)
    | CompletedMarkingNewsAsRead (RemoteData (Graphql.Http.Error (Maybe Community.News.Receipt)) (Maybe Community.News.Receipt))
    | ClickedToggleReactions
    | ToggledReaction Community.News.Reaction
    | CompletedTogglingReaction Community.News.Reaction Community.News.Model (RemoteData (Graphql.Http.Error (Maybe Community.News.Receipt)) (Maybe Community.News.Receipt))
    | GotLastEditorSummaryMsg Profile.Summary.Msg


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        NoOp ->
            UR.init model

        CompletedLoadNews news ->
            let
                markNewsAsRead =
                    case
                        model.newsId
                            |> Maybe.andThen
                                (\newsId ->
                                    List.Extra.find (\{ id } -> id == newsId) news
                                )
                    of
                        Nothing ->
                            identity

                        Just selectedNews ->
                            case selectedNews.receipt of
                                Just _ ->
                                    identity

                                Nothing ->
                                    -- Only mark as read if the user hasn't read it yet
                                    UR.addExt
                                        (LoggedIn.mutation loggedIn
                                            (Cambiatus.Mutation.read
                                                { newsId = selectedNews.id }
                                                Community.News.receiptSelectionSet
                                            )
                                            CompletedMarkingNewsAsRead
                                        )
            in
            model
                |> UR.init
                |> markNewsAsRead

        CompletedMarkingNewsAsRead (RemoteData.Success maybeReceipt) ->
            UR.init model
                |> UR.addExt
                    (LoggedIn.UpdatedLoggedIn
                        { loggedIn
                            | maybeHighlightedNews =
                                Maybe.map (\highlightedNews -> { highlightedNews | receipt = maybeReceipt })
                                    loggedIn.maybeHighlightedNews
                        }
                    )
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
                    (if model.showReactionPicker then
                        Browser.Dom.focus "reaction-picker"
                            |> Task.attempt (\_ -> NoOp)

                     else
                        Browser.Dom.focus "reaction-0"
                            |> Task.attempt (\_ -> NoOp)
                    )

        ToggledReaction reaction ->
            let
                maybeSelectedNews =
                    Community.getField loggedIn.selectedCommunity .news
                        |> RemoteData.toMaybe
                        |> Maybe.map Tuple.second
                        |> Maybe.andThen
                            (\news ->
                                model.newsId
                                    |> Maybe.andThen
                                        (\currentId ->
                                            List.Extra.find (\{ id } -> id == currentId)
                                                news
                                        )
                            )
            in
            case maybeSelectedNews of
                Just selectedNews ->
                    model
                        |> UR.init
                        |> UR.addExt
                            (LoggedIn.mutation loggedIn
                                (Community.News.reactToNews selectedNews
                                    { newsId = selectedNews.id
                                    , reaction = reaction
                                    }
                                )
                                (CompletedTogglingReaction reaction selectedNews)
                            )

                Nothing ->
                    model
                        |> UR.init

        CompletedTogglingReaction reaction selectedNews (RemoteData.Success receipt) ->
            case Community.getField loggedIn.selectedCommunity .news of
                RemoteData.Success ( _, news ) ->
                    let
                        hasAdded =
                            receipt
                                |> Maybe.map .reactions
                                |> Maybe.withDefault []
                                |> List.member reaction

                        reactionAlreadyExists =
                            selectedNews.reactions
                                |> List.any
                                    (\reactionWithCount ->
                                        reactionWithCount.reaction == reaction
                                    )

                        newReactions =
                            if reactionAlreadyExists then
                                List.Extra.updateIf
                                    (\reactionWithCount ->
                                        reactionWithCount.reaction == reaction
                                    )
                                    (\prevReaction ->
                                        { count =
                                            if hasAdded then
                                                prevReaction.count + 1

                                            else
                                                prevReaction.count - 1
                                        , reaction = reaction
                                        }
                                    )
                                    selectedNews.reactions

                            else
                                { count = 1, reaction = reaction }
                                    :: selectedNews.reactions

                        newNews =
                            news
                                |> List.Extra.updateIf ((==) selectedNews)
                                    (\_ ->
                                        { selectedNews
                                            | receipt = receipt
                                            , reactions = newReactions
                                        }
                                    )
                    in
                    model
                        |> UR.init
                        |> UR.addExt
                            (LoggedIn.SetCommunityField (Community.NewsValue newNews))

                _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Completed toggling reaction, but community wasn't loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.News", function = "update" }
                            [ Log.contextFromCommunity loggedIn.selectedCommunity ]

        CompletedTogglingReaction _ _ (RemoteData.Failure err) ->
            model
                |> UR.init
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when reacting to news"
                    { moduleName = "Page.News", function = "update" }
                    []
                    err

        CompletedTogglingReaction _ _ RemoteData.Loading ->
            UR.init model

        CompletedTogglingReaction _ _ RemoteData.NotAsked ->
            UR.init model

        GotLastEditorSummaryMsg subMsg ->
            { model | lastEditorSummary = Profile.Summary.update subMsg model.lastEditorSummary }
                |> UR.init



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
                    div []
                        [ Page.viewHeader loggedIn title
                        , if not community.hasNews || List.isEmpty news then
                            Page.fullPageNotFound
                                (t "news.no_news.title")
                                (t "news.no_news.description")

                          else
                            news
                                |> List.filter
                                    (\n ->
                                        Community.News.isPublished loggedIn.shared.now n
                                            && (Just n.id /= model.newsId)
                                    )
                                |> view_ loggedIn
                                    model
                                    (model.newsId
                                        |> Maybe.andThen
                                            (\newsId ->
                                                List.Extra.find (\{ id } -> id == newsId)
                                                    news
                                            )
                                    )
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


view_ : LoggedIn.Model -> Model -> Maybe Community.News.Model -> List Community.News.Model -> Html Msg
view_ ({ shared } as loggedIn) model maybeSelectedNews news =
    let
        { t } =
            shared.translators
    in
    div []
        (List.concat
            [ case maybeSelectedNews of
                Just selectedNews ->
                    [ viewMainNews loggedIn model selectedNews
                    , if List.isEmpty news || not model.showOtherNews then
                        text ""

                      else
                        h2 [ class "container mx-auto px-4 mt-16 mb-4 text-lg font-bold text-gray-900" ]
                            [ span [] [ text <| t "news.read" ]
                            , text " "
                            , span [ class "text-purple-500" ] [ text <| t "news.other_news" ]
                            ]
                    ]

                Nothing ->
                    []
            , [ if model.showOtherNews then
                    div [ class "bg-white pb-4" ]
                        [ Community.News.viewList shared
                            [ class "container mx-auto px-4 pt-6" ]
                            news
                        ]

                else
                    text ""
              ]
            ]
        )


viewMainNews : LoggedIn.Model -> Model -> Community.News.Model -> Html Msg
viewMainNews ({ shared } as loggedIn) model news =
    let
        { translators } =
            shared
    in
    div [ class "bg-white" ]
        [ div [ class "container mx-auto px-4 pt-10 pb-4" ]
            [ h2 [ class "text-lg text-black font-bold" ] [ text news.title ]
            , Markdown.view [ class "mt-6 text-black colored-links" ]
                news.description
            , div [ class "flex items-center mt-8" ]
                (viewReactionPicker translators model news
                    ++ [ viewReactions translators news ]
                )
            , if news.insertedAt /= news.updatedAt then
                div [ class "flex items-center mt-6" ]
                    [ model.lastEditorSummary
                        |> Profile.Summary.withoutName
                        |> Profile.Summary.withImageSize "h-8 w-8"
                        |> Profile.Summary.view shared.translators
                            loggedIn.accountName
                            news.creator
                        |> Html.map GotLastEditorSummaryMsg
                    , p [ class "text-gray-900 ml-2" ]
                        [ text <| translators.t "news.edited_by"
                        , a
                            [ class "font-bold hover:underline"
                            , Route.href (Route.Profile news.creator.account)
                            ]
                            [ news.creator.name
                                |> Maybe.withDefault (Eos.Account.nameToString news.creator.account)
                                |> text
                            ]
                        , View.Components.dateViewer []
                            (\translations ->
                                { translations
                                    | today = Nothing
                                    , yesterday = Nothing
                                    , other = translators.t "news.edited_date"
                                }
                            )
                            shared
                            news.updatedAt
                        ]
                    ]

              else
                text ""
            ]
        ]


viewReactionPicker : Translators -> Model -> Community.News.Model -> List (Html Msg)
viewReactionPicker { t } model news =
    let
        hasReaction reaction =
            case news.receipt of
                Nothing ->
                    False

                Just receipt ->
                    List.member reaction receipt.reactions
    in
    details
        [ class "inline-block relative z-10"
        , id "reaction-picker"
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
            , class "marker-hidden bg-gray-200 rounded-full p-0.5 focus-ring transition-colors hover:bg-gray-500 active:bg-gray-300"
            ]
            [ Icons.smilingFace "fill-current text-gray-900 w-6 h-6"
            ]
        , View.Components.focusTrap
            { initialFocusId = Nothing }
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
                                    [ ( "bg-green bg-opacity-80 hover:bg-opacity-60", hasReaction reaction )
                                    , ( "bg-white hover:bg-gray-200 focus-visible:bg-gray-200", not (hasReaction reaction) )
                                    ]
                                , onClick (ToggledReaction reaction)
                                , id ("reaction-" ++ String.fromInt index)
                                ]
                                [ span [ class "sr-only" ]
                                    [ if hasReaction reaction then
                                        text <| t "news.reaction.remove"

                                      else
                                        text <| t "news.reaction.add"
                                    ]
                                , text (Community.News.reactionToString reaction)
                                ]
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
                    , ariaLabel <| t "news.reaction.close"
                    , tabindex -1
                    ]
                    []
                , View.Components.keyListener
                    { acceptedKeys = [ View.Components.Escape ]
                    , toMsg = \_ -> ClickedToggleReactions
                    , stopPropagation = True
                    , preventDefault = False
                    }
                ]

            else
                []
           )


viewReactions : Translators -> Community.News.Model -> Html Msg
viewReactions { tr } news =
    let
        hasReaction reaction =
            case news.receipt of
                Nothing ->
                    False

                Just receipt ->
                    List.member reaction receipt.reactions
    in
    Html.Keyed.ul [ class "flex items-center" ]
        (news.reactions
            |> List.filter (\{ count } -> count > 0)
            |> List.map
                (\{ reaction, count } ->
                    ( Community.News.reactionName reaction
                    , li []
                        [ button
                            [ class "ml-4 rounded-full py-0.5 px-2 flex gap-1 focus-ring transition-colors"
                            , classList
                                [ ( "bg-green bg-opacity-50 border border-green hover:bg-opacity-40 active:bg-opacity-60", hasReaction reaction )
                                , ( "bg-gray-200 hover:bg-gray-500 active:bg-gray-300", not (hasReaction reaction) )
                                ]
                            , type_ "button"
                            , onClick (ToggledReaction reaction)
                            ]
                            [ span [ class "sr-only" ]
                                [ text <|
                                    tr "news.reaction.reactions"
                                        [ ( "count", String.fromInt count ) ]
                                ]
                            , span [] [ text (Community.News.reactionToString reaction) ]
                            , span [ ariaHidden True ] [ text (String.fromInt count) ]
                            ]
                        ]
                    )
                )
        )



-- UTILS


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityFieldLoaded _ (Community.NewsValue news) ->
            Just (CompletedLoadNews news)

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        CompletedLoadNews _ ->
            [ "CompletedLoadNews" ]

        CompletedMarkingNewsAsRead r ->
            [ "CompletedMarkingNewsAsRead", UR.remoteDataToString r ]

        ClickedToggleReactions ->
            [ "ClickedToggleReactions" ]

        ToggledReaction _ ->
            [ "ToggledReaction" ]

        CompletedTogglingReaction _ _ _ ->
            [ "CompletedTogglingReaction" ]

        GotLastEditorSummaryMsg subMsg ->
            "GotLastEditorSummaryMsg" :: Profile.Summary.msgToString subMsg
