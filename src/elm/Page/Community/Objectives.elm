module Page.Community.Objectives exposing (Model, Msg, init, msgToString, receiveBroadcast, update, view)

import Action exposing (Action)
import AssocList exposing (Dict)
import Browser.Dom
import Community
import Dict
import Eos
import Html exposing (Html, a, b, br, button, details, div, h1, h2, h3, img, li, p, span, summary, text, ul)
import Html.Attributes exposing (alt, class, classList, id, src, style, tabindex, title)
import Html.Attributes.Aria exposing (ariaHasPopup, ariaHidden, ariaLabel, role)
import Html.Events exposing (onClick)
import Icons
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Log
import Markdown
import Maybe.Extra
import Page
import RemoteData
import Route
import Session.LoggedIn as LoggedIn
import Task
import UpdateResult as UR
import View.Components exposing (intersectionObserver)



-- MODEL


type alias Model =
    { shownObjectives :
        Dict
            Action.ObjectiveId
            { visibleAction : Maybe Action.Id
            , visibleActionHeight : Maybe Float
            , previousVisibleAction : Maybe Action.Id
            , previousVisibleActionHeight : Maybe Float
            , openHeight : Maybe Float
            , closedHeight : Maybe Float
            , isClosing : Bool
            }
    , highlightedAction : Maybe { objectiveId : Action.ObjectiveId, actionId : Maybe Action.Id }
    }


init : Route.SelectedObjective -> LoggedIn.Model -> UpdateResult
init selectedObjective _ =
    UR.init
        { highlightedAction =
            case selectedObjective of
                Route.WithNoObjectiveSelected ->
                    Nothing

                Route.WithObjectiveSelected { id, action } ->
                    Just
                        { objectiveId = Action.objectiveIdFromInt id
                        , actionId = Maybe.map Action.idFromInt action
                        }
        , shownObjectives =
            case selectedObjective of
                Route.WithNoObjectiveSelected ->
                    AssocList.empty

                Route.WithObjectiveSelected { id } ->
                    AssocList.fromList
                        [ ( Action.objectiveIdFromInt id
                          , { visibleAction = Nothing
                            , visibleActionHeight = Nothing
                            , previousVisibleAction = Nothing
                            , previousVisibleActionHeight = Nothing
                            , openHeight = Nothing
                            , closedHeight = Nothing
                            , isClosing = False
                            }
                          )
                        ]
        }
        |> UR.addExt (LoggedIn.RequestedReloadCommunityField Community.ObjectivesField)
        |> UR.addCmd (Browser.Dom.setViewport 0 0 |> Task.attempt (\_ -> NoOp))



-- TYPES


type Msg
    = NoOp
    | CompletedLoadObjectives (List Community.Objective)
    | ClickedToggleObjectiveVisibility Community.Objective
    | FinishedOpeningActions Community.Objective
    | FinishedClosingObjective Community.Objective
    | GotObjectiveDetailsHeight Community.Objective (Result Browser.Dom.Error Browser.Dom.Element)
    | GotObjectiveSummaryHeight Community.Objective (Result Browser.Dom.Error Browser.Dom.Element)
    | GotVisibleActionViewport { objectiveId : Action.ObjectiveId, actionId : Action.Id } (Result Browser.Dom.Error Browser.Dom.Viewport)
    | ClickedScrollToAction Action
    | GotActionMsg Action.Msg
    | StartedIntersecting String
    | StoppedIntersecting String


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        NoOp ->
            UR.init model

        CompletedLoadObjectives objectives ->
            let
                scrollActionIntoView =
                    case model.highlightedAction of
                        Nothing ->
                            identity

                        Just { objectiveId, actionId } ->
                            let
                                maybeAction =
                                    List.Extra.find (\objective -> objective.id == objectiveId) objectives
                                        |> Maybe.andThen
                                            (\foundObjective ->
                                                List.Extra.find (\action -> Just action.id == actionId) foundObjective.actions
                                            )

                                getHighlightedObjectiveSummaryHeight =
                                    case List.Extra.find (\objective -> objective.id == objectiveId) objectives of
                                        Just objective ->
                                            UR.addCmd
                                                (Browser.Dom.getElement (objectiveSummaryId objective)
                                                    |> Task.attempt (GotObjectiveSummaryHeight objective)
                                                )

                                        Nothing ->
                                            identity
                            in
                            case maybeAction of
                                Nothing ->
                                    let
                                        hasObjective =
                                            List.any (\objective -> objective.id == objectiveId) objectives
                                    in
                                    if hasObjective then
                                        UR.addPort
                                            { responseAddress = NoOp
                                            , responseData = Encode.null
                                            , data =
                                                Encode.object
                                                    [ ( "name", Encode.string "scrollIntoView" )
                                                    , ( "id", Encode.string (objectiveDetailsId { id = objectiveId }) )
                                                    ]
                                            }
                                            >> getHighlightedObjectiveSummaryHeight

                                    else
                                        identity

                                Just highlightedAction ->
                                    UR.addPort
                                        { responseAddress = NoOp
                                        , responseData = Encode.null
                                        , data =
                                            Encode.object
                                                [ ( "name", Encode.string "scrollIntoView" )
                                                , ( "id", Encode.string (actionCardId highlightedAction.id) )
                                                ]
                                        }
                                        >> getHighlightedObjectiveSummaryHeight

                startClaimingAction =
                    case model.highlightedAction of
                        Nothing ->
                            identity

                        Just { objectiveId, actionId } ->
                            let
                                maybeActionAndPosition : Maybe ( Action, Int )
                                maybeActionAndPosition =
                                    List.Extra.find (\objective -> objective.id == objectiveId) objectives
                                        |> Maybe.andThen
                                            (\foundObjective ->
                                                Maybe.map2 Tuple.pair
                                                    (List.Extra.find (\action -> Just action.id == actionId) foundObjective.actions)
                                                    (List.Extra.findIndex (\action -> Just action.id == actionId) foundObjective.actions
                                                        |> Maybe.map ((+) 1)
                                                    )
                                            )
                            in
                            case maybeActionAndPosition of
                                Nothing ->
                                    identity

                                Just ( action, position ) ->
                                    Action.startClaiming { position = Just position } action
                                        |> LoggedIn.ExternalActionMsg
                                        |> UR.addExt
            in
            model
                |> UR.init
                |> scrollActionIntoView
                |> startClaimingAction

        ClickedToggleObjectiveVisibility objective ->
            { model
                | shownObjectives =
                    AssocList.update objective.id
                        (\currentValue ->
                            case currentValue of
                                Nothing ->
                                    Just
                                        { visibleAction = Nothing
                                        , visibleActionHeight = Nothing
                                        , previousVisibleAction = Nothing
                                        , previousVisibleActionHeight = Nothing
                                        , openHeight = Nothing
                                        , closedHeight = Nothing
                                        , isClosing = False
                                        }

                                Just value ->
                                    Just { value | isClosing = True }
                        )
                        model.shownObjectives
                , highlightedAction = Nothing
            }
                |> UR.init
                |> UR.addCmd
                    (Browser.Dom.getElement (objectiveSummaryId objective)
                        |> Task.attempt (GotObjectiveSummaryHeight objective)
                    )

        FinishedOpeningActions objective ->
            model
                |> UR.init
                |> UR.addCmd
                    (Browser.Dom.getElement (objectiveDetailsId objective)
                        |> Task.attempt (GotObjectiveDetailsHeight objective)
                    )

        FinishedClosingObjective objective ->
            { model | shownObjectives = AssocList.remove objective.id model.shownObjectives }
                |> UR.init

        GotObjectiveDetailsHeight objective (Ok { element }) ->
            { model
                | shownObjectives =
                    AssocList.update objective.id
                        (Maybe.map (\value -> { value | openHeight = Just element.height }))
                        model.shownObjectives
            }
                |> UR.init

        GotObjectiveDetailsHeight _ (Err (Browser.Dom.NotFound id)) ->
            model
                |> UR.init
                |> UR.logImpossible msg
                    "Couldn't get objective details height"
                    (Just loggedIn.accountName)
                    { moduleName = "Page.Community.Objectives", function = "update" }
                    [ { name = "Error"
                      , extras =
                            Dict.fromList
                                [ ( "type", Encode.string "Browser.Dom.NotFound" )
                                , ( "id", Encode.string id )
                                ]
                      }
                    ]

        GotObjectiveSummaryHeight objective (Ok { element }) ->
            { model
                | shownObjectives =
                    AssocList.update objective.id
                        (Maybe.map (\value -> { value | closedHeight = Just element.height }))
                        model.shownObjectives
            }
                |> UR.init

        GotObjectiveSummaryHeight _ (Err (Browser.Dom.NotFound id)) ->
            model
                |> UR.init
                |> UR.logImpossible msg
                    "Couldn't get objective summary height"
                    (Just loggedIn.accountName)
                    { moduleName = "Page.Community.Objectives", function = "update" }
                    [ { name = "Error"
                      , extras =
                            Dict.fromList
                                [ ( "type", Encode.string "Browser.Dom.NotFound" )
                                , ( "id", Encode.string id )
                                ]
                      }
                    ]

        GotVisibleActionViewport { objectiveId, actionId } (Ok { viewport }) ->
            { model
                | shownObjectives =
                    model.shownObjectives
                        |> AssocList.update objectiveId
                            (\maybeValue ->
                                case maybeValue of
                                    Nothing ->
                                        Just
                                            { visibleAction = Just actionId
                                            , visibleActionHeight = Just viewport.height
                                            , previousVisibleAction = Nothing
                                            , previousVisibleActionHeight = Nothing
                                            , openHeight = Nothing
                                            , closedHeight = Nothing
                                            , isClosing = False
                                            }

                                    Just value ->
                                        Just { value | visibleActionHeight = Just viewport.height }
                            )
            }
                |> UR.init

        GotVisibleActionViewport _ (Err _) ->
            model
                |> UR.init

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
                            , ( "targetId", Encode.string (actionCardId action.id) )
                            ]
                    }

        GotActionMsg subMsg ->
            UR.init model
                |> UR.addExt (LoggedIn.ExternalActionMsg subMsg)

        StartedIntersecting actionCard ->
            case Community.getField loggedIn.selectedCommunity .objectives of
                RemoteData.Success ( _, objectives ) ->
                    let
                        maybeActionIdAndParentObjective =
                            idFromActionCardId actionCard
                                |> Maybe.andThen
                                    (\actionId ->
                                        objectives
                                            |> List.Extra.find
                                                (\objective ->
                                                    objective.actions
                                                        |> List.map .id
                                                        |> List.member actionId
                                                )
                                            |> Maybe.map (Tuple.pair actionId)
                                    )
                    in
                    case maybeActionIdAndParentObjective of
                        Nothing ->
                            model
                                |> UR.init

                        Just ( actionId, parentObjective ) ->
                            { model
                                | shownObjectives =
                                    AssocList.update parentObjective.id
                                        (\maybeValue ->
                                            case maybeValue of
                                                Nothing ->
                                                    Just
                                                        { visibleAction = Just actionId
                                                        , visibleActionHeight = Nothing
                                                        , previousVisibleAction = Nothing
                                                        , previousVisibleActionHeight = Nothing
                                                        , openHeight = Nothing
                                                        , closedHeight = Nothing
                                                        , isClosing = False
                                                        }

                                                Just value ->
                                                    Just
                                                        { visibleAction = Just actionId
                                                        , visibleActionHeight = Nothing
                                                        , previousVisibleAction = value.visibleAction
                                                        , previousVisibleActionHeight = value.visibleActionHeight
                                                        , openHeight = value.openHeight
                                                        , closedHeight = value.closedHeight
                                                        , isClosing = value.isClosing
                                                        }
                                        )
                                        model.shownObjectives
                            }
                                |> UR.init
                                |> UR.addCmd
                                    (Browser.Dom.getViewportOf actionCard
                                        |> Task.attempt
                                            (GotVisibleActionViewport { objectiveId = parentObjective.id, actionId = actionId })
                                    )

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Action started showing up, but objectives weren't loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Objectives", function = "update" }
                            [ Log.contextFromCommunity loggedIn.selectedCommunity ]

        StoppedIntersecting targetId ->
            let
                newShownObjectives =
                    AssocList.foldl
                        (\objectiveId value currDict ->
                            if value.visibleAction == idFromActionCardId targetId then
                                AssocList.insert objectiveId
                                    { visibleAction = value.previousVisibleAction
                                    , visibleActionHeight = value.previousVisibleActionHeight
                                    , previousVisibleAction = Nothing
                                    , previousVisibleActionHeight = Nothing
                                    , openHeight = value.openHeight
                                    , closedHeight = value.closedHeight
                                    , isClosing = value.isClosing
                                    }
                                    currDict

                            else if value.previousVisibleAction == idFromActionCardId targetId then
                                AssocList.insert objectiveId
                                    { value
                                        | previousVisibleAction = Nothing
                                        , previousVisibleActionHeight = Nothing
                                    }
                                    currDict

                            else
                                AssocList.insert objectiveId value currDict
                        )
                        AssocList.empty
                        model.shownObjectives
            in
            { model | shownObjectives = newShownObjectives }
                |> UR.init



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        title =
            loggedIn.shared.translators.t "community.objectives.title"
    in
    { title = title
    , content =
        case loggedIn.selectedCommunity of
            RemoteData.Success community ->
                if community.hasObjectives then
                    viewPage loggedIn community model

                else
                    Page.fullPageNotFound title (loggedIn.shared.translators.t "community.objectives.disabled_objectives_description")

            RemoteData.Loading ->
                Page.fullPageLoading loggedIn.shared

            RemoteData.NotAsked ->
                Page.fullPageLoading loggedIn.shared

            RemoteData.Failure err ->
                Page.fullPageGraphQLError title err
    }


viewPage : LoggedIn.Model -> Community.Model -> Model -> Html Msg
viewPage loggedIn community model =
    let
        { t, tr } =
            loggedIn.shared.translators
    in
    div [ class "container mx-auto px-4 pt-8 mb-20" ]
        [ h1
            [ class "lg:w-2/3 lg:mx-auto"
            , ariaLabel (t "community.objectives.earn" ++ " " ++ Eos.symbolToSymbolCodeString community.symbol)
            ]
            [ span [ ariaHidden True ] [ text <| t "community.objectives.earn" ]
            , text " "
            , span [ class "font-bold", ariaHidden True ]
                [ text (Eos.symbolToSymbolCodeString community.symbol) ]
            ]
        , div [ class "mt-4 bg-white rounded relative lg:w-2/3 lg:mx-auto" ]
            [ p
                [ class "p-4"
                ]
                [ span [ class "sr-only" ] [ text <| t "community.objectives.complete_actions" ++ " " ++ Eos.symbolToSymbolCodeString community.symbol ]
                , span [ ariaHidden True ] [ text <| t "community.objectives.complete_actions" ]
                , text " "
                , b [ ariaHidden True ] [ text (Eos.symbolToSymbolCodeString community.symbol) ]
                ]
            , img
                [ src "/images/doggo_holding_coins.svg"
                , alt ""
                , class "absolute right-1 top-0 -translate-y-2/3"
                ]
                []
            ]
        , h2
            [ class "mt-6 lg:w-2/3 lg:mx-auto"
            , ariaLabel (t "community.objectives.objectives_and" ++ " " ++ t "community.objectives.actions")
            ]
            [ span [ ariaHidden True ] [ text <| t "community.objectives.objectives_and" ]
            , text " "
            , span [ class "font-bold", ariaHidden True ] [ text <| t "community.objectives.actions" ]
            ]
        , case community.objectives of
            RemoteData.Success objectives ->
                let
                    filteredObjectives =
                        List.filter (\objective -> not objective.isCompleted)
                            objectives
                in
                div []
                    [ if List.isEmpty filteredObjectives then
                        div [ class "lg:w-1/2 xl:w-1/3 lg:mx-auto flex flex-col items-center pt-4 pb-6" ]
                            [ img [ src "/images/doggo-laying-down.svg", alt (t "community.objectives.empty_dog_alt") ] []
                            , p [ class "mt-4 text-black font-bold" ]
                                [ text <| t "community.objectives.empty_title"
                                ]
                            , p [ class "text-center mt-4" ]
                                [ text <| t "community.objectives.empty_objectives_line_1"
                                , br [] []
                                , br [] []
                                , text <| t "community.objectives.empty_objectives_line_2"
                                ]
                            ]

                      else
                        ul [ class "space-y-4 mt-4" ]
                            (List.map
                                (viewObjective loggedIn model)
                                filteredObjectives
                            )
                    , intersectionObserver
                        { targetSelectors =
                            filteredObjectives
                                |> List.filter (\objective -> List.member objective.id (AssocList.keys model.shownObjectives))
                                |> List.concatMap .actions
                                |> List.filterMap
                                    (\action ->
                                        if action.isCompleted then
                                            Nothing

                                        else
                                            Just ("#" ++ actionCardId action.id)
                                    )
                        , threshold = 0.01
                        , breakpointToExclude = Just View.Components.Lg
                        , onStartedIntersecting = Just StartedIntersecting
                        , onStoppedIntersecting = Just StoppedIntersecting
                        }
                    ]

            RemoteData.Loading ->
                ul [ class "space-y-4 mt-4" ]
                    (List.range 0 4
                        |> List.map (\_ -> li [ class "bg-white py-10 rounded animate-skeleton-loading lg:w-2/3 lg:mx-auto" ] [])
                    )

            RemoteData.NotAsked ->
                ul [ class "space-y-4 mt-4" ]
                    (List.range 0 4
                        |> List.map (\_ -> li [ class "bg-white py-10 rounded animate-skeleton-loading lg:w-2/3 lg:mx-auto" ] [])
                    )

            RemoteData.Failure _ ->
                div [ class "mt-4 bg-white rounded py-6 px-4 flex flex-col items-center lg:w-2/3 lg:mx-auto" ]
                    [ img
                        [ alt ""
                        , src "/images/not_found.svg"
                        , class "max-h-40"
                        ]
                        []
                    , p [ class "text-center mt-4" ]
                        [ text <| t "community.objectives.error_loading" ]
                    ]
        , div [ class "bg-white rounded p-4 pb-6 relative mt-18 lg:w-2/3 lg:mx-auto" ]
            [ p [ class "text-center mt-2" ] [ text <| t "community.objectives.visit_community_page" ]
            , a
                [ Route.href Route.CommunityAbout
                , class "button button-secondary w-full mt-4"
                ]
                [ text <| t "community.objectives.go_to_community_page" ]
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


viewObjective : LoggedIn.Model -> Model -> Community.Objective -> Html Msg
viewObjective loggedIn model objective =
    let
        { translators } =
            loggedIn.shared

        filteredActions =
            List.filter (\action -> not action.isCompleted)
                objective.actions

        isOpen =
            AssocList.get objective.id model.shownObjectives
                |> Maybe.map (\{ isClosing } -> not isClosing)
                |> Maybe.withDefault False

        isObjectiveHighlighted =
            case model.highlightedAction of
                Nothing ->
                    False

                Just { objectiveId, actionId } ->
                    objectiveId == objective.id && Maybe.Extra.isNothing actionId

        isActionHighlighted action =
            case model.highlightedAction of
                Nothing ->
                    False

                Just { objectiveId, actionId } ->
                    objectiveId == objective.id && actionId == Just action.id

        maybeShownObjectivesInfo =
            AssocList.get objective.id model.shownObjectives

        visibleActionId =
            maybeShownObjectivesInfo
                |> Maybe.andThen .visibleAction

        visibleActionHeight =
            maybeShownObjectivesInfo
                |> Maybe.andThen .visibleActionHeight

        previousVisibleActionHeight =
            maybeShownObjectivesInfo
                |> Maybe.andThen .previousVisibleActionHeight

        openHeight =
            maybeShownObjectivesInfo
                |> Maybe.andThen .openHeight

        closedHeight =
            maybeShownObjectivesInfo
                |> Maybe.andThen .closedHeight
    in
    li
        []
        [ case ( openHeight, Maybe.Extra.or closedHeight visibleActionHeight ) of
            ( Just open, Just closed ) ->
                Html.node "style"
                    []
                    [ """
                    @keyframes shrink-details-{{id}} {
                        0% { height: calc({{open-height}}px - 16px); }
                        100% { height: {{closed-height}}px; }
                    }
                    """
                        |> String.replace "{{id}}" (String.fromInt (Action.objectiveIdToInt objective.id))
                        |> String.replace "{{open-height}}" (String.fromFloat open)
                        |> String.replace "{{closed-height}}" (String.fromFloat closed)
                        |> text
                    ]

            _ ->
                text ""
        , details
            [ id (objectiveDetailsId objective)
            , if isOpen then
                Html.Attributes.attribute "open" "true"

              else
                class ""
            , style "animation-duration" "300ms"
            , style "animation-timing-function" "ease-in-out"
            , if isOpen then
                class ""

              else
                style "animation-name" ("shrink-details-" ++ String.fromInt (Action.objectiveIdToInt objective.id))
            , Html.Events.on "animationend"
                (Decode.field "animationName" Decode.string
                    |> Decode.andThen
                        (\animationName ->
                            if animationName == "shrink-details-" ++ String.fromInt (Action.objectiveIdToInt objective.id) then
                                Decode.succeed (FinishedClosingObjective objective)

                            else
                                Decode.fail "animationName did not match"
                        )
                )
            ]
            [ summary
                [ id (objectiveSummaryId objective)
                , class "marker-hidden lg:w-2/3 lg:mx-auto focus-ring rounded"
                , classList [ ( "border border-green ring ring-green ring-opacity-30", isObjectiveHighlighted ) ]
                , role "button"
                , ariaHasPopup "true"
                , onClick (ClickedToggleObjectiveVisibility objective)
                ]
                [ div
                    [ class "flex marker-hidden items-center bg-white rounded px-4 py-6 cursor-pointer"
                    ]
                    [ Icons.cambiatusCoin "text-blue fill-current flex-shrink-0 self-start mt-1"
                    , h3 [ title (Markdown.toRawString objective.description), class "max-w-[calc(100%-64px)]" ]
                        [ Markdown.view
                            [ class "font-bold px-4 self-start mt-1"
                            , classList [ ( "line-clamp-4 hide-children-from-2", not isOpen ) ]
                            ]
                            objective.description
                        ]
                    , span
                        [ class "ml-auto flex-shrink-0 transition-transform duration-150 motion-reduce:transition-none"
                        , classList
                            [ ( "rotate-180", isOpen )
                            , ( "rotate-0", not isOpen )
                            ]
                        ]
                        [ Icons.arrowDown "text-gray-900 fill-current"
                        ]
                    ]
                ]
            , div
                [ case visibleActionHeight of
                    Nothing ->
                        case previousVisibleActionHeight of
                            Nothing ->
                                class ""

                            Just height ->
                                style "height"
                                    ("calc(" ++ String.fromInt (ceiling height) ++ "px + 32px)")

                    Just height ->
                        style "height"
                            (max (ceiling height)
                                (ceiling <| Maybe.withDefault 0 previousVisibleActionHeight)
                                |> String.fromInt
                                |> (\heightString -> "calc(" ++ heightString ++ "px + 32px")
                            )
                , class "overflow-y-hidden duration-300 ease-in-out origin-top motion-reduce:transition-none"
                , classList [ ( "transition-all", Maybe.Extra.isJust visibleActionHeight ) ]
                , tabindex -1
                ]
                [ div
                    [ class "duration-300 ease-in-out origin-top lg:!h-full motion-reduce:transition-none"
                    , classList
                        [ ( "lg:scale-0", not isOpen )
                        , ( "lg:scale-1", isOpen )
                        , ( "transition-transform", Maybe.Extra.isNothing visibleActionHeight )
                        ]
                    ]
                    [ if not isOpen then
                        text ""

                      else if List.isEmpty filteredActions then
                        div
                            [ class "lg:w-1/2 xl:w-1/3 lg:mx-auto flex flex-col items-center pt-4 pb-6 animate-fade-in-from-above"
                            , Html.Events.on "animationend" (Decode.succeed (FinishedOpeningActions objective))
                            , Html.Events.on "animationcancel" (Decode.succeed (FinishedOpeningActions objective))
                            ]
                            [ img
                                [ src "/images/doggo-laying-down.svg"
                                , alt (translators.t "community.objectives.empty_dog_alt")
                                ]
                                []
                            , p [ class "mt-4 text-black font-bold text-center" ]
                                [ text <| translators.t "community.objectives.empty_title" ]
                            , p [ class "text-center mt-4" ]
                                [ text <| translators.t "community.objectives.empty_line_1"
                                , br [] []
                                , br [] []
                                , text <| translators.tr "community.objectives.empty_line_2" [ ( "symbol", Eos.symbolToSymbolCodeString objective.community.symbol ) ]
                                ]
                            ]

                      else
                        View.Components.masonryLayout
                            [ View.Components.Lg, View.Components.Xl ]
                            { transitionWithParent =
                                case model.highlightedAction of
                                    Nothing ->
                                        True

                                    Just { objectiveId } ->
                                        objectiveId /= objective.id
                            }
                            [ class "mt-4 mb-2 flex h-full overflow-y-hidden overflow-x-scroll snap-x scrollbar-hidden gap-4 transition-all lg:gap-x-6 lg:overflow-visible lg:-mb-4"
                            , classList
                                [ ( "lg:grid-cols-1 lg:w-1/2 lg:mx-auto xl:w-1/3", List.length filteredActions == 1 )
                                , ( "lg:grid-cols-2 xl:grid-cols-2 xl:w-2/3 xl:mx-auto", List.length filteredActions == 2 )
                                , ( "lg:grid-cols-2 xl:grid-cols-3", List.length filteredActions > 2 )
                                ]
                            , id (objectiveContainerId objective)
                            , role "list"
                            ]
                            (List.indexedMap
                                (\index action ->
                                    Action.viewCard loggedIn
                                        { containerAttrs =
                                            [ class "mb-6 snap-center snap-always animate-fade-in-from-above motion-reduce:animate-none"
                                            , classList [ ( "border border-green ring ring-green ring-opacity-30", isActionHighlighted action ) ]
                                            , style "animation-delay" ("calc(75ms * " ++ String.fromInt index ++ ")")
                                            , id (actionCardId action.id)
                                            , Html.Events.on "animationend" (Decode.succeed (FinishedOpeningActions objective))
                                            ]
                                        , position = Just (index + 1)
                                        , toMsg = GotActionMsg
                                        }
                                        action
                                )
                                filteredActions
                            )
                    ]
                ]
            , div [ class "flex justify-center gap-2 lg:hidden" ]
                (filteredActions
                    |> List.indexedMap
                        (\index action ->
                            button
                                [ class "border border-gray-900 rounded-full w-3 h-3 transition-colors focus-ring"
                                , classList
                                    [ ( "border-orange-300 bg-orange-300", Just action.id == visibleActionId )
                                    , ( "hover:bg-orange-300/50 hover:border-orange-300/50", Just action.id /= visibleActionId )
                                    ]
                                , id ("go-to-action-" ++ Action.idToString action.id)
                                , onClick (ClickedScrollToAction action)
                                , ariaLabel <|
                                    translators.tr "community.objectives.go_to_action"
                                        [ ( "index"
                                          , String.fromInt (index + 1)
                                          )
                                        ]
                                , role "link"
                                ]
                                []
                        )
                )
            ]
        ]



-- UTILS


objectiveDetailsId : { objective | id : Action.ObjectiveId } -> String
objectiveDetailsId objective =
    "objective-details-" ++ String.fromInt (Action.objectiveIdToInt objective.id)


objectiveSummaryId : { objective | id : Action.ObjectiveId } -> String
objectiveSummaryId objective =
    "objective-summary-" ++ String.fromInt (Action.objectiveIdToInt objective.id)


objectiveContainerId : { objective | id : Action.ObjectiveId } -> String
objectiveContainerId objective =
    "objective-container-" ++ String.fromInt (Action.objectiveIdToInt objective.id)


actionCardId : Action.Id -> String
actionCardId actionId =
    "action-card-" ++ Action.idToString actionId


idFromActionCardId : String -> Maybe Action.Id
idFromActionCardId elementId =
    -- Remove the leading "action-card-"
    String.dropLeft 12 elementId
        |> Action.idFromString


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityFieldLoaded _ (Community.ObjectivesValue objectives) ->
            Just (CompletedLoadObjectives objectives)

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        CompletedLoadObjectives _ ->
            [ "CompletedLoadObjectives" ]

        ClickedToggleObjectiveVisibility _ ->
            [ "ClickedToggleObjectiveVisibility" ]

        FinishedOpeningActions _ ->
            [ "FinishedOpeningActions" ]

        FinishedClosingObjective _ ->
            [ "FinishedClosingObjective" ]

        GotObjectiveDetailsHeight _ _ ->
            [ "GotObjectiveDetailsHeight" ]

        GotObjectiveSummaryHeight _ _ ->
            [ "GotObjectiveSummaryHeight" ]

        GotVisibleActionViewport _ _ ->
            [ "GotVisibleActionViewport" ]

        ClickedScrollToAction _ ->
            [ "ClickedScrollToAction" ]

        GotActionMsg subMsg ->
            "GotActionMsg" :: Action.msgToString subMsg

        StartedIntersecting _ ->
            [ "StartedIntersecting" ]

        StoppedIntersecting _ ->
            [ "StoppedIntersecting" ]
