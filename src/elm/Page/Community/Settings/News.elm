module Page.Community.Settings.News exposing (Model, Msg, init, msgToString, receiveBroadcast, update, view)

import Cambiatus.Mutation
import Cambiatus.Object.Community
import Community
import Community.News
import Form.Toggle
import Graphql.Http
import Graphql.OptionalArgument
import Graphql.SelectionSet
import Html exposing (Html, a, button, div, h1, p, small, text)
import Html.Attributes exposing (class, classList)
import Html.Attributes.Aria exposing (ariaLabel)
import Html.Events exposing (onClick)
import Log
import Markdown
import Page
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn
import Time
import UpdateResult as UR
import View.Components
import View.Feedback as Feedback
import View.Modal as Modal



-- MODEL


type alias Model =
    { highlightNewsConfirmationModal : HighlightNewsConfirmationModal }


init : LoggedIn.Model -> UpdateResult
init loggedIn =
    { highlightNewsConfirmationModal = NotVisible }
        |> UR.init
        |> UR.addCmd (LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn)
        |> UR.addExt (LoggedIn.RequestedReloadCommunityField Community.NewsField)



-- TYPES


type HighlightNewsConfirmationModal
    = NotVisible
    | Visible { newsId : Int, isHighlighted : Bool }


type Msg
    = NoOp
    | CompletedLoadCommunity Community.Model
    | ToggledHighlightNews Int Bool
    | CompletedSettingHighlightedNews Bool (RemoteData (Graphql.Http.Error (Maybe Community.News.Model)) (Maybe Community.News.Model))
    | ClosedHighlightNewsConfirmationModal
    | ConfirmedHighlightNews Int


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        NoOp ->
            UR.init model

        CompletedLoadCommunity community ->
            if community.creator == loggedIn.accountName then
                UR.init model

            else
                UR.init model
                    |> UR.addCmd (Route.pushUrl loggedIn.shared.navKey Route.Dashboard)

        ToggledHighlightNews newsId isHighlighted ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    case community.highlightedNews of
                        Nothing ->
                            UR.init model
                                |> setHighlightNews loggedIn newsId isHighlighted

                        Just highlightedNews ->
                            if highlightedNews.id == newsId then
                                UR.init model
                                    |> setHighlightNews loggedIn newsId isHighlighted

                            else
                                { model
                                    | highlightNewsConfirmationModal =
                                        Visible { newsId = newsId, isHighlighted = isHighlighted }
                                }
                                    |> UR.init

                _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Tried toggling highlighted news, but community wasn't loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.News", function = "update" }
                            [ Log.contextFromCommunity loggedIn.selectedCommunity ]

        CompletedSettingHighlightedNews isHighlighted (RemoteData.Success maybeNews) ->
            { model | highlightNewsConfirmationModal = NotVisible }
                |> UR.init
                |> UR.addExt
                    (LoggedIn.UpdatedLoggedIn
                        { loggedIn
                            | selectedCommunity =
                                RemoteData.map
                                    (\community ->
                                        { community
                                            | highlightedNews =
                                                if isHighlighted then
                                                    maybeNews

                                                else
                                                    Nothing
                                        }
                                    )
                                    loggedIn.selectedCommunity
                        }
                    )

        CompletedSettingHighlightedNews _ (RemoteData.Failure err) ->
            { model | highlightNewsConfirmationModal = NotVisible }
                |> UR.init
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when highlighting news"
                    { moduleName = "Page.Community.Settings.News", function = "update" }
                    [ Log.contextFromCommunity loggedIn.selectedCommunity ]
                    err
                |> UR.addExt
                    (LoggedIn.ShowFeedback Feedback.Failure
                        (loggedIn.shared.translators.t "news.error_highlighting")
                    )

        CompletedSettingHighlightedNews _ RemoteData.NotAsked ->
            UR.init model

        CompletedSettingHighlightedNews _ RemoteData.Loading ->
            UR.init model

        ClosedHighlightNewsConfirmationModal ->
            { model | highlightNewsConfirmationModal = NotVisible }
                |> UR.init

        ConfirmedHighlightNews newsId ->
            model
                |> UR.init
                |> setHighlightNews loggedIn newsId True


setHighlightNews : LoggedIn.Model -> Int -> Bool -> (UpdateResult -> UpdateResult)
setHighlightNews loggedIn newsId isHighlighted =
    LoggedIn.mutation loggedIn
        (Cambiatus.Mutation.highlightedNews
            (\optionals ->
                { optionals
                    | newsId =
                        if isHighlighted then
                            Graphql.OptionalArgument.Present newsId

                        else
                            Graphql.OptionalArgument.Null
                }
            )
            (Cambiatus.Object.Community.highlightedNews Community.News.selectionSet)
            |> Graphql.SelectionSet.map (Maybe.andThen identity)
        )
        (CompletedSettingHighlightedNews isHighlighted)
        |> UR.addExt



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        { t } =
            loggedIn.shared.translators

        title =
            t "news.title"

        content =
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    div []
                        [ Page.viewHeader loggedIn title
                        , view_ loggedIn community
                        , case model.highlightNewsConfirmationModal of
                            NotVisible ->
                                text ""

                            Visible { newsId } ->
                                Modal.initWith
                                    { closeMsg = ClosedHighlightNewsConfirmationModal
                                    , isVisible = True
                                    }
                                    |> Modal.withHeader (t "news.highlight")
                                    |> Modal.withBody
                                        [ p []
                                            [ text <| t "news.already_highlighted" ]
                                        , p [ class "my-2" ]
                                            [ text <| t "news.replace_notice" ]
                                        , p [ class "mb-6" ]
                                            [ text <| t "news.replace_confirmation" ]
                                        ]
                                    |> Modal.withFooter
                                        [ div [ class "w-full flex flex-col md:flex-row md:space-x-4" ]
                                            [ button
                                                [ class "button button-secondary w-full mb-4 md:mb-0"
                                                , onClick ClosedHighlightNewsConfirmationModal
                                                ]
                                                [ text <| t "community.actions.form.no" ]
                                            , button
                                                [ class "button button-primary w-full"
                                                , onClick (ConfirmedHighlightNews newsId)
                                                ]
                                                [ text <| t "community.actions.form.yes" ]
                                            ]
                                        ]
                                    |> Modal.toHtml
                        ]

                RemoteData.Loading ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.NotAsked ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.Failure err ->
                    Page.fullPageGraphQLError title err
    in
    { title = title, content = content }


view_ : LoggedIn.Model -> Community.Model -> Html Msg
view_ ({ shared } as loggedIn) community =
    div []
        [ div [ class "bg-white py-4" ]
            [ div [ class "container mx-auto px-4" ]
                [ View.Components.disablableLink
                    { isDisabled = not loggedIn.hasAcceptedCodeOfConduct }
                    [ class "button button-primary w-full"
                    , classList [ ( "button-disabled", not loggedIn.hasAcceptedCodeOfConduct ) ]
                    , Route.href (Route.CommunitySettingsNewsEditor Route.CreateNews)
                    ]
                    [ text (shared.translators.t "news.create") ]
                ]
            ]
        , div [ class "container mx-auto px-4 my-4" ]
            [ case community.news of
                RemoteData.Success news ->
                    div [ class "grid gap-4 md:grid-cols-2" ]
                        (List.map
                            (\newsForCard ->
                                viewNewsCard loggedIn
                                    (Just newsForCard.id == Maybe.map .id community.highlightedNews)
                                    newsForCard
                            )
                            news
                        )

                RemoteData.Failure err ->
                    Page.fullPageGraphQLError
                        (shared.translators.t "news.error_fetching")
                        err

                RemoteData.NotAsked ->
                    Page.fullPageLoading shared

                RemoteData.Loading ->
                    Page.fullPageLoading shared
            ]
        ]


viewNewsCard : LoggedIn.Model -> Bool -> Community.News.Model -> Html Msg
viewNewsCard loggedIn isHighlighted news =
    let
        ({ translators } as shared) =
            loggedIn.shared
    in
    div [ class "bg-white rounded p-4 pb-6 flex flex-col" ]
        [ h1 [ class "font-bold" ]
            [ text news.title ]
        , small [ class "uppercase text-gray-900 text-sm font-bold block mt-4" ]
            [ text <| translators.t "news.created_at"
            , View.Components.dateViewer [] identity shared news.insertedAt
            ]
        , case news.scheduling of
            Nothing ->
                text ""

            Just scheduling ->
                let
                    padTime time =
                        String.repeat (2 - String.length time) "0" ++ time
                in
                small [ class "uppercase text-gray-900 text-sm font-bold block mt-2" ]
                    [ text <| translators.t "news.scheduled_at"
                    , View.Components.dateViewer [] identity shared scheduling
                    , text <|
                        translators.tr "news.scheduled_at_time"
                            [ ( "hour"
                              , Time.toHour shared.timezone scheduling
                                    |> String.fromInt
                                    |> padTime
                              )
                            , ( "minute"
                              , Time.toMinute shared.timezone scheduling
                                    |> String.fromInt
                                    |> padTime
                              )
                            ]
                    ]
        , div [ class "mb-10 relative mt-4" ]
            [ p [ class "text-gray-900 max-h-11 overflow-hidden" ]
                [ text (Markdown.toUnformattedString news.description)
                ]
            , a
                [ class "absolute right-0 bottom-0 bg-white pl-2 text-orange-300 hover:underline focus:underline outline-none"
                , Route.href (Route.News { selectedNews = Just news.id, showOthers = False })
                , ariaLabel <| translators.t "news.view_more"
                ]
                [ text <| translators.t "news.view_more_with_ellipsis" ]
            ]
        , Form.Toggle.init { label = text <| translators.t "news.highlight", id = "highlight-news-toggle-" ++ String.fromInt news.id }
            |> Form.Toggle.withContainerAttrs [ class "mt-auto" ]
            |> Form.Toggle.withStatusText Form.Toggle.YesNo
            |> Form.Toggle.withDisabled (not loggedIn.hasAcceptedCodeOfConduct)
            |> (\options ->
                    Form.Toggle.view options
                        { onToggle = ToggledHighlightNews news.id
                        , onBlur = NoOp
                        , value = isHighlighted
                        , error = text ""
                        , hasError = False
                        , isRequired = False
                        , translators = translators
                        }
               )
        , View.Components.disablableLink { isDisabled = not loggedIn.hasAcceptedCodeOfConduct }
            [ class "button button-primary w-full mt-10 mb-4"
            , classList [ ( "button-disabled", not loggedIn.hasAcceptedCodeOfConduct ) ]
            , Route.href (Route.CommunitySettingsNewsEditor (Route.EditNews news.id))
            ]
            [ text <| translators.t "news.edit" ]
        , View.Components.disablableLink { isDisabled = not loggedIn.hasAcceptedCodeOfConduct }
            [ class "button button-secondary w-full"
            , classList [ ( "button-disabled", not loggedIn.hasAcceptedCodeOfConduct ) ]
            , Route.href (Route.CommunitySettingsNewsEditor (Route.CopyNews news.id))
            ]
            [ text <| translators.t "news.copy" ]
        ]



-- UTILS


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityLoaded community ->
            Just (CompletedLoadCommunity community)

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        ToggledHighlightNews _ _ ->
            [ "ToggledHighlightNews" ]

        CompletedSettingHighlightedNews _ r ->
            [ "CompletedSettingHighlightedNews", UR.remoteDataToString r ]

        ClosedHighlightNewsConfirmationModal ->
            [ "ClosedHighlightNewsConfirmationModal" ]

        ConfirmedHighlightNews _ ->
            [ "ConfirmedHighlightNews" ]
