module Page.Community.Objectives exposing (Model, Msg, init, msgToString, update, view)

import Action exposing (Action)
import Community
import Html exposing (Html, a, b, details, div, h1, h2, img, li, p, span, summary, text, ul)
import Html.Attributes exposing (alt, class, src)
import Icons
import Markdown
import RemoteData
import Route
import Session.LoggedIn as LoggedIn
import UpdateResult as UR



-- MODEL


type alias Model =
    {}


init : LoggedIn.Model -> UpdateResult
init _ =
    UR.init {}
        |> UR.addExt (LoggedIn.RequestedReloadCommunityField Community.ObjectivesField)



-- TYPES


type Msg
    = NoOp


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        NoOp ->
            UR.init model



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    { title = "TODO"
    , content =
        case loggedIn.selectedCommunity of
            RemoteData.Success community ->
                div [ class "container mx-auto px-4 pt-8 mb-20" ]
                    [ h1 []
                        [ -- TODO - I18N
                          span [] [ text "Ganhe" ]
                        , text " "
                        , span [ class "font-bold" ] [ text "Buss" ]
                        ]
                    , div [ class "mt-4 bg-white rounded relative" ]
                        [ p [ class "p-4" ]
                            -- TODO - I18N
                            [ text "Complete ações que essa comunidade traçou e ganhe "
                            , b [] [ text "Buss" ]
                            ]
                        , img
                            [ src "/images/doggo_holding_coins.svg"
                            , alt ""
                            , class "absolute right-1 top-0 -translate-y-2/3"
                            ]
                            []
                        ]
                    , h2 [ class "mt-6" ]
                        [ -- TODO - I18N
                          span [] [ text "Objetivos e" ]
                        , text " "
                        , span [ class "font-bold" ] [ text "Ações" ]
                        ]
                    , case community.objectives of
                        RemoteData.Success objectives ->
                            -- TODO - Filter objectives that should be shown
                            ul [ class "space-y-4 mt-4" ] (List.map viewObjective objectives)

                        RemoteData.Loading ->
                            -- TODO - Show loading state
                            div [] []

                        RemoteData.NotAsked ->
                            -- TODO - Show loading state
                            div [] []

                        RemoteData.Failure _ ->
                            -- TODO - Show error state
                            div [] []
                    , div [ class "bg-white rounded p-4 pb-6 relative mt-18" ]
                        -- TODO - I18N
                        [ p [] [ text "Visite a página da comunidade para saber mais sobre." ]
                        , a
                            [ Route.href Route.CommunityAbout
                            , class "button button-secondary w-full mt-4"
                            ]
                            -- TODO - I18N
                            [ text "Ir para a página da comunidade" ]
                        , div [ class "absolute top-0 left-0 w-full flex justify-center" ]
                            [ img
                                [ src "/images/success-doggo.svg"
                                , alt ""
                                , class "-translate-y-3/4"
                                ]
                                []
                            ]
                        ]
                    ]

            _ ->
                text "TODO"
    }


viewObjective : Community.Objective -> Html Msg
viewObjective objective =
    li [ class "bg-white rounded px-4 py-6" ]
        [ details []
            [ summary [ class "flex marker-hidden items-center" ]
                [ Icons.cambiatusCoin "text-blue fill-current flex-shrink-0 self-start mt-1"
                , Markdown.view [ class "font-bold px-4 line-clamp-4 self-start mt-1" ] objective.description
                , Icons.arrowDown "ml-auto text-gray-900 fill-current flex-shrink-0"
                ]
            ]
        ]



-- UTILS


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]
