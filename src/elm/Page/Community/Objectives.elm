module Page.Community.Objectives exposing (Model, Msg, init, msgToString, update, view)

import Action exposing (Action)
import Community
import Eos
import Html exposing (Html, a, b, button, details, div, h1, h2, h3, h4, img, li, p, span, summary, text, ul)
import Html.Attributes exposing (alt, class, classList, id, src, title)
import Html.Events exposing (onClick)
import Icons
import Json.Encode as Encode
import Markdown
import RemoteData
import Route
import Session.LoggedIn as LoggedIn
import Translation
import UpdateResult as UR
import View.Components exposing (intersectionObserver)



-- MODEL


type alias Model =
    { shownAction : Maybe Int
    }


init : LoggedIn.Model -> UpdateResult
init _ =
    UR.init { shownAction = Just 175 }
        |> UR.addExt (LoggedIn.RequestedReloadCommunityField Community.ObjectivesField)



-- TYPES


type Msg
    = NoOp
    | ClickedScrollToAction Action
    | StartedIntersecting String


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        NoOp ->
            UR.init model

        ClickedScrollToAction action ->
            model
                |> UR.init
                |> UR.addPort
                    { responseAddress = NoOp
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "smoothHorizontalScroll" )
                            , ( "containerId", Encode.string (objectiveContainerId action.objective) )
                            , ( "targetId", Encode.string (actionCardId action) )
                            ]
                    }

        StartedIntersecting actionId ->
            { model | shownAction = idFromActionCardId actionId }
                |> UR.init



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    { title = "TODO"
    , content =
        case loggedIn.selectedCommunity of
            RemoteData.Success community ->
                div [ class "container mx-auto px-4 pt-8 mb-20" ]
                    [ h1 [ class "lg:w-2/3 lg:mx-auto" ]
                        [ -- TODO - I18N
                          span [] [ text "Ganhe" ]
                        , text " "
                        , span [ class "font-bold" ] [ text "Buss" ]
                        ]
                    , div [ class "mt-4 bg-white rounded relative lg:w-2/3 lg:mx-auto" ]
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
                    , h2 [ class "mt-6 lg:w-2/3 lg:mx-auto" ]
                        [ -- TODO - I18N
                          span [] [ text "Objetivos e" ]
                        , text " "
                        , span [ class "font-bold" ] [ text "Ações" ]
                        ]
                    , case community.objectives of
                        RemoteData.Success objectives ->
                            let
                                filteredObjectives =
                                    -- TODO - Test this
                                    List.filter (\objective -> not objective.isCompleted) objectives
                            in
                            div []
                                [ ul [ class "space-y-4 mt-4" ]
                                    (List.map
                                        (viewObjective loggedIn.shared.translators model)
                                        filteredObjectives
                                    )
                                , intersectionObserver
                                    { targetSelectors =
                                        filteredObjectives
                                            |> List.concatMap .actions
                                            -- TODO - Test this
                                            |> List.filterMap
                                                (\action ->
                                                    if action.isCompleted then
                                                        Nothing

                                                    else
                                                        Just ("#" ++ actionCardId action)
                                                )
                                    , threshold = 0.9
                                    , onStartedIntersecting = StartedIntersecting
                                    }
                                ]

                        RemoteData.Loading ->
                            -- TODO - Show loading state
                            div [] []

                        RemoteData.NotAsked ->
                            -- TODO - Show loading state
                            div [] []

                        RemoteData.Failure _ ->
                            -- TODO - Show error state
                            div [] []
                    , div [ class "bg-white rounded p-4 pb-6 relative mt-18 lg:w-2/3 lg:mx-auto" ]
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


viewObjective : Translation.Translators -> Model -> Community.Objective -> Html Msg
viewObjective translators model objective =
    let
        filteredActions =
            -- TODO - Test this
            List.filter (\action -> not action.isCompleted) objective.actions
    in
    li []
        [ details []
            [ summary [ class "marker-hidden" ]
                [ div
                    [ class "flex marker-hidden items-center bg-white rounded px-4 py-6 cursor-pointer lg:w-2/3 lg:mx-auto"
                    ]
                    [ Icons.cambiatusCoin "text-blue fill-current flex-shrink-0 self-start mt-1"
                    , h3 [ title (Markdown.toRawString objective.description) ]
                        [ Markdown.view [ class "font-bold px-4 line-clamp-4 self-start mt-1" ] objective.description ]
                    , Icons.arrowDown "ml-auto text-gray-900 fill-current flex-shrink-0"
                    ]
                ]
            , ul
                [ class "mt-4 mb-2 flex overflow-scroll snap-x snap-proximity scrollbar-hidden gap-4 lg:gap-6 lg:grid lg:grid-cols-2 xl:grid-cols-3"
                , id (objectiveContainerId objective)
                ]
                (List.indexedMap
                    (viewAction translators)
                    filteredActions
                )

            -- TODO - Adjust case where some cards are taller than others
            , div [ class "flex justify-center gap-2 lg:hidden" ]
                (objective.actions
                    |> List.map
                        (\action ->
                            button
                                [ class "border border-gray-900 rounded-full w-3 h-3 transition-colors"
                                , classList [ ( "border-orange-300 bg-orange-300", Just action.id == model.shownAction ) ]
                                , id ("go-to-action-" ++ String.fromInt action.id)
                                , onClick (ClickedScrollToAction action)
                                ]
                                []
                        )
                )
            ]
        ]


viewAction : Translation.Translators -> Int -> Action -> Html Msg
viewAction translators index action =
    li
        [ class "bg-white rounded px-4 pt-4 pb-6 self-start"
        , class "w-full flex-shrink-0 snap-center snap-always"
        , id (actionCardId action)
        ]
        [ div [ class "flex" ]
            [ span [ class "text-lg text-gray-500 font-bold" ] [ text (String.fromInt (index + 1)), text "." ]
            , div [ class "ml-5 mt-1 min-w-0" ]
                [ h4
                    [ class "line-clamp-3"
                    , title (Markdown.toRawString action.description)
                    ]
                    [ Markdown.view [] action.description ]
                , span [ class "font-bold text-sm text-gray-900 uppercase block mt-6" ]
                    -- TODO - I18N
                    [ text "Recompensa" ]
                , div [ class "mt-1 text-green font-bold" ]
                    [ span [ class "text-2xl mr-1" ]
                        [ text
                            (Eos.formatSymbolAmount
                                translators
                                action.objective.community.symbol
                                action.reward
                            )
                        ]
                    , text (Eos.symbolToSymbolCodeString action.objective.community.symbol)
                    ]
                ]
            ]
        , div [ class "grid grid-cols-1 sm:grid-cols-2 gap-x-4 gap-y-2 mt-6" ]
            [ button [ class "button button-secondary w-full" ]
                [ Icons.share "mr-2 flex-shrink-0"

                -- TODO - I18N
                , text "Compartilhar"
                ]
            , button [ class "button button-primary w-full sm:col-span-1" ]
                [ Icons.camera "w-4 mr-2 flex-shrink-0"

                -- TODO - I18N
                , text "Reivindicar"
                ]
            ]
        ]


objectiveContainerId : { objective | id : Int } -> String
objectiveContainerId objective =
    "objective-container-" ++ String.fromInt objective.id


actionCardId : Action -> String
actionCardId action =
    "action-card-" ++ String.fromInt action.id


idFromActionCardId : String -> Maybe Int
idFromActionCardId elementId =
    -- Remove the leading "action-card-"
    String.dropLeft 12 elementId
        |> String.toInt



-- UTILS


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        ClickedScrollToAction _ ->
            [ "ClickedScrollToAction" ]

        StartedIntersecting _ ->
            [ "StartedIntersecting" ]
