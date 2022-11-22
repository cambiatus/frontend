module Page.Community.Settings.Settings exposing (Model, Msg, init, msgToString, receiveBroadcast, update, view)

import Community
import Eos
import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (class)
import Maybe.Extra
import Page
import RemoteData
import Route exposing (Route)
import Session.LoggedIn as LoggedIn exposing (External)
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
        { t } =
            shared.translators
    in
    div [ class "flex flex-col container my-4 mx-auto gap-4 px-4" ]
        [ settingCard (t "settings.community_info.title") (t "menu.edit") (t "settings.community_info.description") Route.CommunitySettingsInfo
        , settingCard (t "settings.community_currency.title") (t "menu.edit") (Eos.symbolToSymbolCodeString community.symbol) Route.CommunitySettingsCurrency
        , if community.hasObjectives then
            settingCard (t "settings.actions.title") (t "menu.edit") "" Route.CommunitySettingsObjectives

          else
            text ""
        , settingCard (t "settings.shop.categories.title") (t "menu.edit") (t "settings.shop.categories.description") Route.CommunitySettingsShopCategories
        , settingCard (t "settings.contacts.title") (t "menu.edit") (t "settings.contacts.description") Route.CommunitySettingsContacts
        , settingCard (t "settings.features.title") (t "menu.edit") (t "settings.features.description") Route.CommunitySettingsFeatures
        , if Maybe.Extra.isJust community.contributionConfiguration then
            settingCard (t "sponsorship.title") (t "menu.edit") (t "sponsorship.description") Route.CommunitySettingsSponsorship

          else
            text ""
        , if community.hasNews then
            settingCard (t "news.title") (t "menu.edit") (t "news.description") Route.CommunitySettingsNews

          else
            text ""
        ]


settingCard : String -> String -> String -> Route -> Html Msg
settingCard title action description route =
    div
        [ class "flex flex-col justify-around bg-white w-full h-32 rounded px-4 pt-3 pb-4"
        ]
        [ span [ class "font-semibold" ] [ text title ]
        , span [ class "text-sm text-gray-900 uppercase" ] [ text description ]
        , a
            [ Route.href route
            , class "button button-primary w-full h-8"
            ]
            [ text action ]
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
