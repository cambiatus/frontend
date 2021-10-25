module Page.Community.Settings.News exposing (Model, Msg, init, msgToString, receiveBroadcast, update, view)

import Api.Graphql
import Cambiatus.Mutation
import Cambiatus.Object.Community
import Community
import Community.News
import Eos
import Graphql.Http
import Graphql.SelectionSet
import Html exposing (Html, a, button, div, h1, p, small, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Log
import Page
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared, Translators)
import UpdateResult as UR
import View.Feedback as Feedback
import View.Form.Toggle
import View.MarkdownEditor as MarkdownEditor
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
    | CompletedSettingHighlightedNews (RemoteData (Graphql.Http.Error (Maybe Community.News.Model)) (Maybe Community.News.Model))
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
                                |> UR.addCmd (setHighlightNews loggedIn community newsId isHighlighted)

                        Just _ ->
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

        CompletedSettingHighlightedNews (RemoteData.Success maybeNews) ->
            { model | highlightNewsConfirmationModal = NotVisible }
                |> UR.init
                |> UR.addExt
                    (LoggedIn.UpdatedLoggedIn
                        { loggedIn
                            | selectedCommunity =
                                RemoteData.map
                                    (\community ->
                                        { community | highlightedNews = maybeNews }
                                    )
                                    loggedIn.selectedCommunity
                        }
                    )

        CompletedSettingHighlightedNews (RemoteData.Failure err) ->
            { model | highlightNewsConfirmationModal = NotVisible }
                |> UR.init
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when highlighting news"
                    { moduleName = "Page.Community.Settings.News", function = "update" }
                    [ Log.contextFromCommunity loggedIn.selectedCommunity ]
                    err
                -- TODO - I18N
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure "Something wrong happened when highlighting news")

        CompletedSettingHighlightedNews RemoteData.NotAsked ->
            UR.init model

        CompletedSettingHighlightedNews RemoteData.Loading ->
            UR.init model

        ClosedHighlightNewsConfirmationModal ->
            { model | highlightNewsConfirmationModal = NotVisible }
                |> UR.init

        ConfirmedHighlightNews newsId ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    model
                        |> UR.init
                        |> UR.addCmd (setHighlightNews loggedIn community newsId True)

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Confirmed setting highlighted news, but community wasn't loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.News", function = "update" }
                            [ Log.contextFromCommunity loggedIn.selectedCommunity ]


setHighlightNews : LoggedIn.Model -> Community.Model -> Int -> Bool -> Cmd Msg
setHighlightNews loggedIn community newsId isHighlighted =
    -- TODO - Check if we can have no highlighted news
    Api.Graphql.mutation loggedIn.shared
        (Just loggedIn.authToken)
        (Cambiatus.Mutation.highlightedNews
            { communityId = Eos.symbolToString community.symbol
            , newsId = newsId
            }
            (Cambiatus.Object.Community.highlightedNews Community.News.selectionSet)
            -- We need to remove nested Maybes (one from the highlightedNews
            -- mutation, and another one from the highlightedNews selectionSet)
            |> Graphql.SelectionSet.map (Maybe.andThen identity)
        )
        CompletedSettingHighlightedNews



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
                        , view_ loggedIn.shared community
                        , case model.highlightNewsConfirmationModal of
                            NotVisible ->
                                text ""

                            Visible { newsId } ->
                                Modal.initWith
                                    { closeMsg = ClosedHighlightNewsConfirmationModal
                                    , isVisible = True
                                    }
                                    -- TODO - I18N
                                    |> Modal.withHeader "Destacar o comunicado"
                                    |> Modal.withBody
                                        [ p []
                                            -- TODO - I18N
                                            [ text "Você já tem um comunicado ativo" ]
                                        , p [ class "my-2" ]
                                            [ text "Ao destacar este comunicado ele substituirá o comunicado existente" ]
                                        , p [ class "mb-6" ]
                                            [ text "Tem certeza que deseja destacá-lo?" ]
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


view_ : Shared -> Community.Model -> Html Msg
view_ shared community =
    div []
        [ div [ class "bg-white py-4" ]
            [ div [ class "container mx-auto px-4" ]
                [ a
                    [ class "button button-primary w-full"
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
                                viewNewsCard shared.translators
                                    (Just newsForCard == community.highlightedNews)
                                    newsForCard
                            )
                            news
                        )

                RemoteData.Failure err ->
                    Page.fullPageGraphQLError
                        -- TODO - I18N
                        "Something went wrong while fetching communications"
                        err

                RemoteData.NotAsked ->
                    Page.fullPageLoading shared

                RemoteData.Loading ->
                    Page.fullPageLoading shared
            ]
        ]


viewNewsCard : Translators -> Bool -> Community.News.Model -> Html Msg
viewNewsCard translators isHighlighted news =
    div [ class "bg-white rounded p-4 pb-6 flex flex-col" ]
        [ h1 [ class "font-bold" ]
            [ text news.title ]
        , small [ class "uppercase text-gray-900 text-[12px] font-bold block mt-4" ]
            -- TODO - Use new typography classes (#622)
            -- TODO - I18N
            [ text "Criado: OUT/01/2019" ]
        , small [ class "uppercase text-gray-900 text-[12px] font-bold block mt-2 mb-6" ]
            -- TODO - I18N
            -- TODO - Use new typography classes (#622)
            [ text "AGENDADO: 09/06/2021 AS 8:00" ]
        , div [ class "mb-10 relative" ]
            [ p [ class "text-gray-900 max-h-[44px] overflow-hidden" ]
                -- TODO - Use new typography classes (#622) and check spacing with "See more" link
                [ text (MarkdownEditor.removeFormatting news.description)
                ]
            , a
                [ class "absolute right-0 bottom-0 bg-white pl-2 text-orange-300 hover:underline focus:underline outline-none"

                -- TODO - Use correct route
                , Route.href Route.Dashboard
                ]
                -- TODO - I18N
                [ text "... Ver mais" ]
            ]
        , View.Form.Toggle.init
            { -- TODO - I18N
              label = text "Destacar esse comunicado"
            , id = "highlight-news-toggle-" ++ String.fromInt news.id
            , onToggle = ToggledHighlightNews news.id

            -- TODO - Check if we can have no highlighted news
            , disabled = False
            , value = isHighlighted
            }
            |> View.Form.Toggle.withStatusText View.Form.Toggle.YesNo
            |> View.Form.Toggle.withAttrs [ class "mt-auto" ]
            |> View.Form.Toggle.toHtml translators
        , a
            [ class "button button-primary w-full mt-10 mb-4"
            , Route.href (Route.CommunitySettingsNewsEditor (Route.EditNews news.id))
            ]
            -- TODO - I18N
            [ text "Edit" ]
        , a
            [ class "button button-secondary w-full"
            , Route.href (Route.CommunitySettingsNewsEditor (Route.CopyNews news.id))
            ]
            -- TODO - I18N
            [ text "Create a copy" ]
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

        CompletedSettingHighlightedNews r ->
            [ "CompletedSettingHighlightedNews", UR.remoteDataToString r ]

        ClosedHighlightNewsConfirmationModal ->
            [ "ClosedHighlightNewsConfirmationModal" ]

        ConfirmedHighlightNews _ ->
            [ "ConfirmedHighlightNews" ]
