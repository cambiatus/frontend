module Page.Community.Objectives exposing (Model, Msg, init, msgToString, receiveBroadcast, update, view)

import Action exposing (Action)
import Cambiatus.Enum.VerificationType as VerificationType
import Community exposing (Model)
import Eos
import Html exposing (Html, a, button, div, p, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Icons
import Page
import Profile
import RemoteData
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Strftime
import Task
import Time exposing (Posix, posixToMillis)
import UpdateResult as UR
import Utils


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( initModel
    , Cmd.batch
        [ LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
        , Task.perform GotTime Time.now
        ]
    )



-- MODEL


type alias Model =
    { status : Status
    , openObjective : Maybe Int
    , date : Maybe Posix
    }


initModel : Model
initModel =
    { status = Loading
    , openObjective = Nothing
    , date = Nothing
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
            case ( loggedIn.selectedCommunity, model.status ) of
                ( RemoteData.Success community, Loaded ) ->
                    div []
                        [ Page.viewHeader loggedIn (t "community.objectives.title_plural") Route.Community
                        , div [ class "container mx-auto px-4 my-10" ]
                            [ div [ class "flex justify-end mb-10" ] [ viewNewObjectiveButton loggedIn community ]
                            , div []
                                (community.objectives
                                    |> List.sortBy .id
                                    |> List.reverse
                                    |> List.indexedMap (viewObjective loggedIn model community)
                                )
                            ]
                        ]

                ( RemoteData.Success _, Unauthorized ) ->
                    div []
                        [ Page.viewHeader loggedIn title Route.Dashboard
                        , div [ class "card" ]
                            [ text (shared.translators.t "community.edit.unauthorized") ]
                        ]

                ( RemoteData.Failure e, _ ) ->
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
        a
            [ class "button button-primary button-sm w-full md:w-64"
            , Route.href Route.NewObjective
            ]
            [ text (shared.translators.t "community.objectives.new") ]

    else
        text ""


viewObjective : LoggedIn.Model -> Model -> Community.Model -> Int -> Community.Objective -> Html Msg
viewObjective ({ shared } as loggedIn) model community index objective =
    let
        isOpen : Bool
        isOpen =
            case model.openObjective of
                Just obj ->
                    obj == index

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
                , onClick (OpenObjective index)
                ]
                [ div []
                    [ p [ class "text-sm" ] [ text objective.description ]
                    , p [ class "text-gray-900 text-caption uppercase mt-2" ]
                        [ text
                            (shared.translators.tr
                                "community.objectives.action_count"
                                [ ( "actions", objective.actions |> List.length |> String.fromInt ) ]
                            )
                        ]
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
                    [ a
                        [ class "button button-secondary button-sm w-full sm:w-48 mt-2 px-1 sm:mr-4"
                        , Route.href (Route.EditObjective objective.id)
                        ]
                        [ text_ "community.objectives.edit" ]
                    , a
                        [ class "button button-secondary button-sm w-full sm:w-48 mt-2 px-1 mb-4"
                        , Route.href (Route.NewAction objective.id)
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


viewAction : LoggedIn.Model -> Model -> Int -> Action -> Html Msg
viewAction ({ shared } as loggedIn) model objectiveId action =
    let
        symbol =
            action.objective.community.symbol

        posixDeadline : Posix
        posixDeadline =
            action.deadline
                |> Utils.posixDateTime

        deadlineStr : String
        deadlineStr =
            posixDeadline
                |> Strftime.format "%d %B %Y" Time.utc

        pastDeadline : Bool
        pastDeadline =
            case action.deadline of
                Just _ ->
                    case model.date of
                        Just today ->
                            posixToMillis today > posixToMillis posixDeadline

                        Nothing ->
                            False

                Nothing ->
                    False

        ( usages, usagesLeft ) =
            ( String.fromInt action.usages, String.fromInt action.usagesLeft )

        isClosed =
            pastDeadline || (action.usages > 0 && action.usagesLeft == 0)

        validationType =
            action.verificationType
                |> VerificationType.toString

        text_ s =
            text (shared.translators.t s)

        tr r_id replaces =
            loggedIn.shared.translators.tr r_id replaces
    in
    div [ class "flex flex-wrap sm:flex-no-wrap mt-8 mb-4 relative bg-purple-500 rounded-lg px-4 py-5" ]
        [ div [ class "absolute top-0 left-0 right-0 -mt-6" ] [ Icons.flag "w-full fill-current text-green" ]
        , div [ class "w-full" ]
            [ p [ class "text-white" ] [ text action.description ]
            , div [ class "flex flex-wrap my-6 -mx-2 items-center" ]
                [ div [ class "mx-2 mb-2 text-white" ]
                    [ p [ class "input-label" ]
                        [ text_ "community.actions.reward" ]
                    , p [ class "uppercase text-body" ]
                        [ String.fromFloat action.reward
                            ++ " "
                            ++ Eos.symbolToString symbol
                            |> text
                        ]
                    ]
                , if validationType == "CLAIMABLE" then
                    div [ class "mx-2 mb-2" ]
                        [ p [ class "input-label" ]
                            [ text_ "community.actions.validation_reward" ]
                        , p [ class "uppercase text-body text-white" ]
                            [ String.fromFloat action.verifierReward
                                ++ " "
                                ++ Eos.symbolToString symbol
                                |> text
                            ]
                        ]

                  else
                    text ""
                , if action.deadline == Nothing && action.usages == 0 then
                    text ""

                  else
                    div [ class "mx-2 mb-2" ]
                        [ p [ class "input-label" ]
                            [ text_ "community.actions.available_until" ]
                        , p [ class "text-body" ]
                            [ if action.usages > 0 then
                                p [ classList [ ( "text-red", action.usagesLeft == 0 ), ( "text-white", action.usagesLeft /= 1 ) ] ]
                                    [ text (tr "community.actions.usages" [ ( "usages", usages ), ( "usagesLeft", usagesLeft ) ]) ]

                              else
                                text ""
                            , case action.deadline of
                                Just _ ->
                                    p [ classList [ ( "text-red", pastDeadline ), ( "text-white", not pastDeadline ) ] ] [ text deadlineStr ]

                                Nothing ->
                                    text ""
                            ]
                        ]
                , div [ class "mx-2 mb-2" ]
                    [ if action.isCompleted then
                        div [ class "tag bg-green" ] [ text_ "community.actions.completed" ]

                      else if isClosed then
                        div [ class "tag bg-gray-500 text-red" ] [ text_ "community.actions.closed" ]

                      else
                        text ""
                    ]
                ]
            , div [ class "flex flex-wrap justify-between items-end" ]
                [ div [ class "w-full sm:w-4/5" ]
                    [ p [ class "input-label mb-4" ] [ text_ "community.actions.verifiers" ]
                    , if validationType == "AUTOMATIC" then
                        div [ class "flex items-center" ]
                            [ p [ class "text-body text-white" ] [ text_ "community.actions.automatic_analyzers" ]
                            , Icons.exclamation "ml-2 text-white fill-current"
                            ]

                      else
                        div [ class "flex mr-2 overflow-x-auto" ]
                            (List.map
                                (\u ->
                                    div [ class "mr-4 action-verifier" ]
                                        [ Profile.view shared loggedIn.accountName u ]
                                )
                                action.validators
                            )
                    ]
                , a
                    [ class "button button-primary button-sm w-full sm:w-40 mt-8"
                    , Route.href (Route.EditAction objectiveId action.id)
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
    | GotTime Posix
    | OpenObjective Int


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        CompletedLoadCommunity community ->
            UR.init
                { model
                    | status =
                        if not community.hasObjectives then
                            Unauthorized

                        else if community.creator == loggedIn.accountName then
                            Loaded

                        else
                            Unauthorized
                }

        GotTime date ->
            UR.init { model | date = Just date }

        OpenObjective index ->
            if model.openObjective == Just index then
                { model | openObjective = Nothing }
                    |> UR.init

            else
                { model | openObjective = Just index }
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

        GotTime _ ->
            [ "GotTime" ]

        OpenObjective _ ->
            [ "OpenObjective" ]
