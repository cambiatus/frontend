module Page.Community.Settings.Settings exposing (Model, Msg, init, msgToString, update, view)

import Api.Graphql
import Community
import Eos exposing (Symbol)
import Graphql.Http
import Html exposing (Html, a, button, div, span, text)
import Html.Attributes exposing (class, style)
import I18Next exposing (t)
import Page exposing (viewHeader)
import Route exposing (Route)
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import UpdateResult as UR


init : LoggedIn.Model -> Symbol -> ( Model, Cmd Msg )
init { shared } symbol =
    ( initModel symbol
    , Api.Graphql.query shared (Community.communityQuery symbol) CompletedLoad
    )


type alias Model =
    { currency : String
    , status : Status
    }


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Status
    = Loading
    | LoadingFailed (Graphql.Http.Error (Maybe Community.Model))
    | Loaded Community.Model
    | Unauthorized


type Msg
    = CompletedLoad (Result (Graphql.Http.Error (Maybe Community.Model)) (Maybe Community.Model))


initModel : Symbol -> Model
initModel symbol =
    { currency = Eos.symbolToString symbol
    , status = Loading
    }


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        translate =
            t loggedIn.shared.translations

        title =
            translate "community.edit.title"

        content =
            let
                headerText =
                    title

                header =
                    Page.viewHeader loggedIn headerText Route.Dashboard
            in
            case model.status of
                Loading ->
                    Page.fullPageLoading

                Loaded community ->
                    div []
                        [ header
                        , viewSettingsList loggedIn.shared community
                        ]

                LoadingFailed e ->
                    Page.fullPageGraphQLError headerText e

                Unauthorized ->
                    div []
                        [ Page.viewHeader loggedIn title Route.Dashboard
                        , div [ class "card" ]
                            [ text (translate "community.edit.unauthorized") ]
                        ]
    in
    { title = title
    , content = content
    }


viewSettingsList : Shared -> Community.Model -> Html Msg
viewSettingsList shared community =
    let
        translate =
            t shared.translations
    in
    div
        [ class "grid my-4"
        , style "grid-template-columns" "0 1fr 0"
        , style "grid-template-rows" "auto"
        , style "grid-gap" "16px"
        ]
        [ settingCard (translate "settings.features.title") (translate "settings.features.description") (Route.CommunitySettingsFeatures community.symbol)
        , if community.hasObjectives then
            settingCard (translate "settings.actions.title") "" (Route.Objectives community.symbol)

          else
            text ""
        ]


settingCard : String -> String -> Route -> Html Msg
settingCard title description route =
    div
        [ class "flex flex-col justify-around bg-white w-full h-32 rounded px-4 pt-3 pb-4"
        , style "grid-column" "2 / 3"
        ]
        [ span [ class "text-sm font-medium" ] [ text title ]
        , span [ class "text-xs text-gray-900 uppercase" ] [ text description ]
        , a [ Route.href route ]
            [ button
                [ class "w-full bg-orange-300 rounded-lg text-sm uppercase text-white font-medium h-8"
                ]
                [ text "Edit" ]
            ]
        ]



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        CompletedLoad (Ok (Just community)) ->
            let
                newStatus =
                    if community.creator == loggedIn.accountName then
                        Loaded community

                    else
                        Unauthorized
            in
            UR.init { model | status = newStatus }

        CompletedLoad (Ok Nothing) ->
            UR.init model

        CompletedLoad (Err err) ->
            UR.init { model | status = LoadingFailed err }
                |> UR.logGraphqlError msg err


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedLoad r ->
            [ "CompletedLoad", UR.resultToString r ]
