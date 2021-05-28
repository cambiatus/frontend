module Page.Community.Settings.Settings exposing (Model, Msg, init, msgToString, receiveBroadcast, update, view)

import Community
import Eos
import Html exposing (Html, a, button, div, span, text)
import Html.Attributes exposing (class, style)
import Page
import RemoteData
import Route exposing (Route)
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import Task
import UpdateResult as UR


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( initModel
    , Cmd.batch
        [ Task.succeed RequestedReloadCommunity |> Task.perform identity
        , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
        ]
    )


type Model
    = Loading
    | Authorized
    | Unauthorized


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = CompletedLoadCommunity Community.Model
    | RequestedReloadCommunity


initModel : Model
initModel =
    Loading


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view ({ shared } as loggedIn) model =
    let
        title =
            shared.translators.t "community.edit.title"

        content =
            let
                headerText =
                    title

                header =
                    Page.viewHeader loggedIn headerText
            in
            case ( loggedIn.selectedCommunity, model ) of
                ( _, Loading ) ->
                    Page.fullPageLoading shared

                ( RemoteData.Loading, _ ) ->
                    Page.fullPageLoading shared

                ( RemoteData.NotAsked, _ ) ->
                    Page.fullPageLoading shared

                ( RemoteData.Failure e, _ ) ->
                    Page.fullPageGraphQLError headerText e

                ( RemoteData.Success community, Authorized ) ->
                    div []
                        [ header
                        , viewSettingsList shared community
                        ]

                ( RemoteData.Success _, Unauthorized ) ->
                    div []
                        [ Page.viewHeader loggedIn title
                        , div [ class "card" ]
                            [ text (shared.translators.t "community.edit.unauthorized") ]
                        ]
    in
    { title = title
    , content = content
    }


viewSettingsList : Shared -> Community.Model -> Html Msg
viewSettingsList shared community =
    let
        translate =
            shared.translators.t

        featuresDescription =
            translate "community.objectives.title_plural" ++ ", " ++ translate "menu.shop"
    in
    div
        [ class "grid my-4"
        , class "flex container mx-auto"
        , style "grid-template-columns" "0 1fr 0"
        , style "grid-template-rows" "auto"
        , style "grid-gap" "16px"
        ]
        [ settingCard (translate "settings.community_info.title") (translate "menu.edit") (translate "settings.community_info.description") Route.CommunitySettingsInfo
        , settingCard (translate "settings.community_currency.title") (translate "menu.edit") (Eos.symbolToSymbolCodeString community.symbol) Route.CommunitySettingsCurrency
        , if community.hasObjectives then
            settingCard (translate "settings.actions.title") (translate "menu.edit") "" Route.Objectives

          else
            text ""
        , settingCard (translate "settings.features.title") (translate "menu.edit") featuresDescription Route.CommunitySettingsFeatures
        ]


settingCard : String -> String -> String -> Route -> Html Msg
settingCard title action description route =
    div
        [ class "flex flex-col justify-around bg-white w-full h-32 rounded px-4 pt-3 pb-4"
        , style "grid-column" "1 / 4"
        ]
        [ span [ class "text-sm font-medium" ] [ text title ]
        , span [ class "text-xs text-gray-900 uppercase" ] [ text description ]
        , a [ Route.href route ]
            [ button
                [ class "w-full bg-orange-300 rounded-lg text-sm uppercase text-white font-medium h-8"
                ]
                [ text action ]
            ]
        ]



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        CompletedLoadCommunity community ->
            if community.creator == loggedIn.accountName then
                UR.init Authorized

            else
                UR.init Unauthorized

        RequestedReloadCommunity ->
            UR.init model
                |> UR.addExt (LoggedIn.ReloadResource LoggedIn.CommunityResource)


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
        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        RequestedReloadCommunity ->
            [ "RequestedReloadCommunity" ]
