module Page.Community.Settings.Objectives exposing (Model, Msg, init, msgToString, receiveBroadcast, update, view)

import Action exposing (Action)
import AssocList
import Cambiatus.Enum.VerificationType as VerificationType
import Community
import Dict
import Eos
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (class, classList, id)
import Html.Events exposing (onClick)
import Icons
import List.Extra as List
import Log
import Markdown
import Page
import Profile.Summary
import RemoteData
import Route
import Session.LoggedIn as LoggedIn exposing (External)
import Time exposing (Posix)
import UpdateResult as UR
import Utils
import View.Components


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( initModel
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )



-- MODEL


type alias Model =
    { status : Status
    , openObjective : Maybe Action.ObjectiveId
    , profileSummaries : AssocList.Dict Action.Id (List Profile.Summary.Model)
    }


initModel : Model
initModel =
    { status = Loading
    , openObjective = Nothing
    , profileSummaries = AssocList.empty
    }


type Status
    = Loading
    | Loaded
    | Unauthorized



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view ({ shared } as loggedIn) model =
    let
        t =
            shared.translators.t

        title =
            t "community.objectives.title_plural"

        content =
            case
                ( Community.getField loggedIn.selectedCommunity .objectives
                , model.status
                )
            of
                ( RemoteData.Success ( community, objectives ), Loaded ) ->
                    div []
                        [ Page.viewHeader loggedIn (t "community.objectives.title_plural")
                        , div [ class "container mx-auto px-4 my-10" ]
                            [ div [ class "flex justify-end mb-10" ] [ viewNewObjectiveButton loggedIn community ]
                            , div []
                                (objectives
                                    |> List.sortBy (.id >> Action.objectiveIdToInt)
                                    |> List.reverse
                                    |> List.map (viewObjective loggedIn model)
                                )
                            ]
                        ]

                ( RemoteData.Success _, Unauthorized ) ->
                    div []
                        [ Page.viewHeader loggedIn title
                        , div [ class "card" ]
                            [ text (shared.translators.t "community.edit.unauthorized") ]
                        ]

                ( RemoteData.Failure (Community.CommunityError e), _ ) ->
                    Page.fullPageGraphQLError (t "community.objectives.title_plural") e

                ( RemoteData.Failure (Community.FieldError e), _ ) ->
                    Page.fullPageGraphQLError (t "community.objectives.title_plural") e

                ( RemoteData.Loading, _ ) ->
                    Page.fullPageLoading shared

                ( RemoteData.NotAsked, _ ) ->
                    Page.fullPageLoading shared

                ( _, Loading ) ->
                    Page.fullPageLoading shared
    in
    { title = title
    , content = content
    }


viewNewObjectiveButton : LoggedIn.Model -> Community.Model -> Html msg
viewNewObjectiveButton ({ shared } as loggedIn) community =
    if LoggedIn.isAccount community.creator loggedIn then
        View.Components.disablableLink { isDisabled = not loggedIn.hasAcceptedCodeOfConduct }
            [ class "button button-primary button-sm w-full md:w-64"
            , classList [ ( "button-disabled", not loggedIn.hasAcceptedCodeOfConduct ) ]
            , Route.href Route.CommunitySettingsNewObjective
            ]
            [ text (shared.translators.t "community.objectives.new") ]

    else
        text ""


viewObjective : LoggedIn.Model -> Model -> Community.Objective -> Html Msg
viewObjective ({ shared } as loggedIn) model objective =
    let
        isOpen : Bool
        isOpen =
            case model.openObjective of
                Just obj ->
                    obj == objective.id

                Nothing ->
                    False

        text_ s =
            text (shared.translators.t s)
    in
    div
        [ class "bg-white rounded mt-4 hover:shadow" ]
        [ div [ class "" ]
            [ -- Clickable header
              div
                [ class "p-4 sm:px-6 cursor-pointer rounded flex justify-between"
                , classList [ ( "pb-0", isOpen ) ]
                , onClick (OpenObjective objective.id)
                ]
                [ div [ class "overflow-hidden" ]
                    [ Markdown.view
                        [ class "text-sm"
                        , classList [ ( "truncate-children", not isOpen ) ]
                        ]
                        objective.description
                    , p [ class "text-gray-900 text-sm uppercase mt-2" ]
                        [ text
                            (shared.translators.tr
                                "community.objectives.action_count"
                                [ ( "actions", objective.actions |> List.length |> String.fromInt ) ]
                            )
                        ]
                    , if objective.isCompleted then
                        p [ class "bg-green rounded-sm text-center inline-block px-7 py-1 text-sm text-black uppercase mt-4 tracking-wide" ]
                            [ text_ "community.objectives.complete" ]

                      else
                        text ""
                    ]
                , button
                    [ class "h-8" ]
                    [ if isOpen then
                        Icons.arrowDown "rotate-180"

                      else
                        Icons.arrowDown ""
                    ]
                ]
            ]
        , if isOpen then
            div [ class "p-4 sm:px-6 pt-0" ]
                [ div [ class "flex flex-wrap mt-2" ]
                    [ View.Components.disablableLink
                        { isDisabled = objective.isCompleted || not loggedIn.hasAcceptedCodeOfConduct }
                        [ class "button button-secondary button-sm w-full sm:w-48 mt-2 px-1 sm:mr-4"
                        , Route.href (Route.CommunitySettingsEditObjective (Action.objectiveIdToInt objective.id))
                        , classList [ ( "button-disabled", objective.isCompleted || not loggedIn.hasAcceptedCodeOfConduct ) ]
                        ]
                        [ text_ "community.objectives.edit" ]
                    , View.Components.disablableLink
                        { isDisabled = objective.isCompleted || not loggedIn.hasAcceptedCodeOfConduct }
                        [ class "button button-secondary button-sm w-full sm:w-48 mt-4 sm:mt-2 px-1 mb-4"
                        , Route.href (Route.CommunitySettingsNewAction (Action.objectiveIdToInt objective.id))
                        , classList [ ( "button-disabled", objective.isCompleted || not loggedIn.hasAcceptedCodeOfConduct ) ]
                        ]
                        [ text_ "community.actions.new" ]
                    ]
                , div []
                    (objective.actions
                        |> List.map (viewAction loggedIn model objective.id)
                    )
                ]

          else
            text ""
        ]


viewAction : LoggedIn.Model -> Model -> Action.ObjectiveId -> Action -> Html Msg
viewAction ({ shared } as loggedIn) model objectiveId action =
    let
        symbol =
            action.objective.community.symbol

        posixDeadline : Posix
        posixDeadline =
            action.deadline
                |> Utils.fromMaybeDateTime

        ( usages, usagesLeft ) =
            ( String.fromInt action.usages, String.fromInt action.usagesLeft )

        isClosed =
            Action.isPastDeadline action shared.now

        validationType =
            action.verificationType
                |> VerificationType.toString

        text_ s =
            text (shared.translators.t s)

        tr rId replaces =
            loggedIn.shared.translators.tr rId replaces
    in
    div [ class "flex flex-wrap sm:flex-nowrap mt-8 mb-4 relative bg-purple-500 rounded-lg px-4 py-5" ]
        [ div [ class "absolute top-0 left-0 right-0 -mt-6" ] [ Icons.flag "w-full fill-current text-green" ]
        , div [ class "w-full" ]
            [ Markdown.view [ class "text-white" ] action.description
            , div [ class "flex flex-wrap gap-y-4 gap-x-6 my-6 items-center" ]
                [ div [ class "text-white" ]
                    [ p [ class "label text-green" ]
                        [ text_ "community.actions.reward" ]
                    , p [ class "uppercase" ]
                        [ Eos.assetToString shared.translators
                            { symbol = symbol
                            , amount = action.reward
                            }
                            |> text
                        ]
                    ]
                , if validationType == "CLAIMABLE" then
                    div []
                        [ p [ class "label text-green" ]
                            [ text_ "community.actions.validation_reward" ]
                        , p [ class "uppercase text-white" ]
                            [ Eos.assetToString shared.translators
                                { symbol = symbol
                                , amount = action.verifierReward
                                }
                                |> text
                            ]
                        ]

                  else
                    text ""
                , if action.deadline == Nothing && action.usages == 0 then
                    text ""

                  else
                    div []
                        [ p [ class "label text-green" ]
                            [ text_ "community.actions.available_until" ]
                        , p []
                            [ if action.usages > 0 then
                                p [ class "text-white" ]
                                    [ text (tr "community.actions.usages" [ ( "usages", usages ), ( "usagesLeft", usagesLeft ) ]) ]

                              else
                                text ""
                            , case action.deadline of
                                Just _ ->
                                    View.Components.dateViewer [ class "capitalize text-white" ]
                                        identity
                                        shared
                                        posixDeadline

                                Nothing ->
                                    text ""
                            ]
                        ]
                , div [ class "mt-auto" ]
                    [ if action.isCompleted then
                        div [ class "tag bg-green" ]
                            [ text_ "community.actions.completed" ]

                      else if isClosed then
                        div [ class "tag bg-gray-500 text-red" ] [ text_ "community.actions.closed" ]

                      else
                        text ""
                    ]
                ]
            , div [ class "flex flex-wrap justify-between items-end" ]
                [ div [ class "w-full sm:w-4/5" ]
                    [ p [ class "label text-green" ] [ text_ "community.actions.verifiers" ]
                    , if validationType == "AUTOMATIC" then
                        div [ class "flex items-center" ]
                            [ p [ class "text-white" ] [ text_ "community.actions.automatic_analyzers" ]
                            , Icons.exclamation "ml-2 text-white fill-current"
                            ]

                      else
                        div [ class "flex gap-4 flex-wrap" ]
                            (List.indexedMap
                                (\validatorIndex u ->
                                    case
                                        AssocList.get action.id model.profileSummaries
                                            |> Maybe.andThen (List.getAt validatorIndex)
                                    of
                                        Nothing ->
                                            text ""

                                        Just validatorSummary ->
                                            let
                                                validatorId =
                                                    [ Action.objectiveIdToInt objectiveId, Action.idToInt action.id, validatorIndex ]
                                                        |> List.map String.fromInt
                                                        |> (::) "validator"
                                                        |> String.join "-"
                                            in
                                            div
                                                [ class "action-verifier relative"
                                                , id validatorId
                                                ]
                                                [ validatorSummary
                                                    |> Profile.Summary.withRelativeSelector ("#" ++ validatorId)
                                                    |> Profile.Summary.view shared.translators loggedIn.accountName u
                                                    |> Html.map (GotProfileSummaryMsg action.id validatorIndex)
                                                ]
                                )
                                action.validators
                            )
                    ]
                , View.Components.disablableLink
                    { isDisabled = action.objective.isCompleted || not loggedIn.hasAcceptedCodeOfConduct }
                    [ class "button button-primary button-sm w-full sm:w-40 mt-8 focus:ring-offset-indigo-500"
                    , Route.href
                        (Route.CommunitySettingsEditAction
                            (Action.objectiveIdToInt objectiveId)
                            (Action.idToInt action.id)
                        )
                    , classList [ ( "button-disabled", action.objective.isCompleted || not loggedIn.hasAcceptedCodeOfConduct ) ]
                    ]
                    [ text_ "community.actions.edit" ]
                ]
            ]
        ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = CompletedLoadCommunity Community.Model
    | OpenObjective Action.ObjectiveId
    | GotProfileSummaryMsg Action.Id Int Profile.Summary.Msg


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        CompletedLoadCommunity community ->
            let
                ( status, maybeRequestObjectives ) =
                    if community.hasObjectives && community.creator == loggedIn.accountName then
                        ( Loaded, UR.addExt (LoggedIn.RequestedReloadCommunityField Community.ObjectivesField) )

                    else
                        ( Unauthorized, identity )
            in
            { model | status = status }
                |> UR.init
                |> maybeRequestObjectives

        OpenObjective index ->
            if model.openObjective == Just index then
                { model | openObjective = Nothing, profileSummaries = AssocList.empty }
                    |> UR.init
                    |> UR.addBreadcrumb
                        { type_ = Log.DebugBreadcrumb
                        , category = msg
                        , message = "Closed objective"
                        , data = Dict.fromList [ ( "objectiveId", Action.encodeObjectiveId index ) ]
                        , level = Log.DebugLevel
                        }

            else
                { model
                    | openObjective = Just index
                    , profileSummaries =
                        loggedIn.selectedCommunity
                            |> RemoteData.toMaybe
                            |> Maybe.andThen (.objectives >> RemoteData.toMaybe)
                            |> Maybe.andThen (List.find (\objective -> objective.id == index))
                            |> Maybe.map .actions
                            |> Maybe.withDefault []
                            |> List.map
                                (\action ->
                                    ( action.id
                                    , List.length action.validators
                                        |> Profile.Summary.initMany False
                                    )
                                )
                            |> AssocList.fromList
                }
                    |> UR.init
                    |> UR.addBreadcrumb
                        { type_ = Log.DebugBreadcrumb
                        , category = msg
                        , message = "Closed objective"
                        , data = Dict.fromList [ ( "objectiveId", Action.encodeObjectiveId index ) ]
                        , level = Log.DebugLevel
                        }

        GotProfileSummaryMsg actionIndex validatorIndex subMsg ->
            { model
                | profileSummaries =
                    AssocList.update actionIndex
                        (Maybe.withDefault []
                            >> List.updateAt validatorIndex (Profile.Summary.update subMsg)
                            >> Just
                        )
                        model.profileSummaries
            }
                |> UR.init


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

        OpenObjective _ ->
            [ "OpenObjective" ]

        GotProfileSummaryMsg _ _ subMsg ->
            "GotProfileSummaryMsg" :: Profile.Summary.msgToString subMsg
