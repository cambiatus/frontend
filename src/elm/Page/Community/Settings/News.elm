module Page.Community.Settings.News exposing (Model, Msg, init, msgToString, receiveBroadcast, update, view)

import Community
import Community.News
import Html exposing (Html, a, div, h1, p, small, text)
import Html.Attributes exposing (class)
import Page
import RemoteData
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared, Translators)
import UpdateResult as UR
import View.Form.Toggle
import View.MarkdownEditor as MarkdownEditor



-- MODEL


type alias Model =
    {}


init : LoggedIn.Model -> UpdateResult
init loggedIn =
    UR.init {}
        |> UR.addCmd (LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn)
        |> UR.addExt (LoggedIn.RequestedReloadCommunityField Community.NewsField)



-- TYPES


type Msg
    = NoOp
    | CompletedLoadCommunity Community.Model


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



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn _ =
    let
        title =
            loggedIn.shared.translators.t "news.title"

        content =
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    div []
                        [ Page.viewHeader loggedIn title
                        , view_ loggedIn.shared community
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
        , div [ class "container mx-auto px-4" ]
            [ case community.news of
                RemoteData.Success news ->
                    div [ class "grid gap-4 md:grid-cols-2" ]
                        (List.map (viewNewsCard shared.translators) news)

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


viewNewsCard : Translators -> Community.News.Model -> Html Msg
viewNewsCard translators news =
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
            , onToggle = \_ -> NoOp
            , disabled = False
            , value = False
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
