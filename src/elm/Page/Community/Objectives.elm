module Page.Community.Objectives exposing (Model, Msg, init, msgToString, update, view)

import Api.Graphql
import Bespiral.Enum.VerificationType as VerificationType exposing (VerificationType)
import Community exposing (Community, communityQuery)
import Eos exposing (Symbol)
import Graphql.Http
import Html exposing (..)
import Html.Attributes exposing (class, classList, disabled)
import Html.Events exposing (onClick)
import I18Next exposing (Delims(..), Translations, t)
import Icons
import Page
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Strftime
import Task
import Time exposing (Posix, posixToMillis)
import UpdateResult as UR
import User
import Utils


init : LoggedIn.Model -> Symbol -> ( Model, Cmd Msg )
init ({ shared } as loggedIn) symbol =
    ( initModel loggedIn symbol
    , Cmd.batch
        [ Api.Graphql.query shared (Community.communityQuery symbol) CompletedLoad
        , Task.perform GotTime Time.now
        ]
    )



-- MODEL


type alias Model =
    { communityId : Symbol
    , status : Status
    , openObjective : Maybe Int
    , date : Maybe Posix
    }


type Status
    = Loading
    | Loaded Community
    | NotFound
    | Failed (Graphql.Http.Error (Maybe Community))


initModel : LoggedIn.Model -> Symbol -> Model
initModel loggedIn symbol =
    { communityId = symbol
    , status = Loading
    , openObjective = Nothing
    , date = Nothing
    }



-- VIEW


view : LoggedIn.Model -> Model -> Html Msg
view ({ shared } as loggedIn) model =
    case model.status of
        Loading ->
            Page.fullPageLoading

        NotFound ->
            Page.viewCardEmpty [ text "Community not found" ]

        Failed e ->
            Page.fullPageGraphQLError (t shared.translations "community.objectives.title_plural") e

        Loaded community ->
            div []
                [ Page.viewHeader loggedIn (t shared.translations "community.objectives.title_plural") (Route.Community model.communityId)
                , div [ class "container mx-auto px-4 my-10" ]
                    [ div [ class "flex justify-end" ] [ viewNewObjectiveButton loggedIn community ]
                    , div []
                        (community.objectives
                            |> List.indexedMap (viewObjective loggedIn model community)
                        )
                    ]
                ]


viewNewObjectiveButton : LoggedIn.Model -> Community -> Html msg
viewNewObjectiveButton ({ shared } as loggedIn) community =
    if LoggedIn.isAccount community.creator loggedIn then
        a
            [ class "button button-primary button-sm w-full"
            , Route.href (Route.NewObjective community.symbol)
            ]
            [ text (t shared.translations "community.objectives.new") ]

    else
        text ""


viewObjective : LoggedIn.Model -> Model -> Community -> Int -> Community.Objective -> Html Msg
viewObjective ({ shared } as loggedIn) model community index objective =
    let
        canEdit : Bool
        canEdit =
            LoggedIn.isAccount community.creator loggedIn

        isOpen : Bool
        isOpen =
            case model.openObjective of
                Just obj ->
                    obj == index

                Nothing ->
                    False

        text_ s =
            text (t shared.translations s)
    in
    div [ class "p-4 sm:px-6 bg-white rounded mt-4" ]
        [ div [ class "flex justify-between items-start" ]
            [ div []
                [ p [ class "text-sm" ] [ text objective.description ]
                , p [ class "text-gray-900 text-caption uppercase mt-2" ]
                    [ text
                        (I18Next.tr shared.translations
                            Curly
                            "community.objectives.action_count"
                            [ ( "actions", objective.actions |> List.length |> String.fromInt ) ]
                        )
                    ]
                ]
            , div [ class "flex" ]
                [ if canEdit then
                    -- a [ class "button button-secondary" ] [ text_ "menu.edit" ]
                    div [] []

                  else
                    text ""
                , button [ onClick (OpenObjective index) ] [ Icons.arrowDown "" ]
                ]
            ]
        , if isOpen then
            div []
                [ a
                    [ class "w-full button button-primary button-sm mt-6 mb-8"
                    , Route.href
                        (Route.NewAction community.symbol (Community.unwrapObjectiveId objective.id))
                    ]
                    [ text_ "community.actions.new" ]
                , div []
                    (objective.actions
                        |> List.map (viewAction loggedIn model)
                    )
                , a [ class "w-full button button-secondary button-sm" ] [ text_ "menu.edit" ]
                ]

          else
            text ""
        , if not isOpen then
            div [ class "flex items-center justify-end" ]
                [ a [ class "button button-secondary button-sm" ] [ text_ "menu.edit" ]
                ]

          else
            text ""
        ]


viewAction : LoggedIn.Model -> Model -> Community.Action -> Html Msg
viewAction ({ shared } as loggedIn) model action =
    let
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
                Just deadline ->
                    case model.date of
                        Just today ->
                            posixToMillis today > posixToMillis posixDeadline

                        Nothing ->
                            False

                Nothing ->
                    False

        ( usages, usagesLeft ) =
            ( String.fromInt action.usages, String.fromInt action.usagesLeft )

        validationType =
            action.verificationType
                |> VerificationType.toString

        text_ s =
            text (t shared.translations s)

        tr r_id replaces =
            I18Next.tr loggedIn.shared.translations I18Next.Curly r_id replaces
    in
    div [ class "bg-gray-100 my-8 p-4" ]
        [ Icons.flag "mx-auto mb-4"
        , p [ class "text-body" ] [ text action.description ]
        , div [ class "flex flex-wrap my-6 -mx-2" ]
            [ div [ class "mx-2 mb-2" ]
                [ p [ class "input-label" ]
                    [ text_ "community.actions.reward" ]
                , p [ class "uppercase text-body" ]
                    [ String.fromFloat action.reward
                        ++ " "
                        ++ Eos.symbolToString model.communityId
                        |> text
                    ]
                ]
            , div [ class "mx-2 mb-2" ]
                [ p [ class "input-label" ]
                    [ text_ "community.actions.validation_reward" ]
                , p [ class "uppercase text-body" ]
                    [ String.fromFloat action.verificationReward
                        ++ " "
                        ++ Eos.symbolToString model.communityId
                        |> text
                    ]
                ]
            , div [ class "mx-2 mb-2" ]
                [ p [ class "input-label" ]
                    [ text_ "community.actions.available_until" ]
                , p [ class "text-body" ]
                    [ p [ classList [ ( "text-red", action.usagesLeft == 0 ) ] ]
                        [ text (tr "community.actions.usages" [ ( "usages", usages ), ( "usagesLeft", usagesLeft ) ]) ]
                    , p [ classList [ ( "text-red", pastDeadline ) ] ] [ text deadlineStr ]
                    ]
                ]
            , div [ class "mx-2 mb-2" ]
                [ if action.isCompleted then
                    div [ class "tag bg-green" ] [ text "completed" ]

                  else if pastDeadline || action.usagesLeft == 0 then
                    div [ class "tag bg-gray-500 text-red" ] [ text "closed" ]

                  else
                    text ""
                ]
            ]
        , div [ class "py-8" ]
            [ p [ class "input-label mb-4" ] [ text_ "community.actions.verifiers" ]
            , div [ class "flex overflow-scrol -mx-2" ]
                (List.map
                    (\u ->
                        div [ class "mx-2" ]
                            [ User.view shared.endpoints.ipfs loggedIn.accountName shared.translations u.validator ]
                    )
                    action.validators
                )
            ]
        , if validationType == "CLAIMABLE" then
            let
                isDisabled =
                    not action.isCompleted || not (pastDeadline || action.usagesLeft == 0)
            in
            div [ class "mb-4" ]
                [ button
                    [ class "w-full button button-sm"
                    , classList [ ( "button-disabled", isDisabled ), ( "button-primary", not isDisabled ) ]
                    , disabled isDisabled
                    ]
                    [ text "claim" ]
                ]

          else
            text ""
        ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = CompletedLoad (Result (Graphql.Http.Error (Maybe Community)) (Maybe Community))
    | GotTime Posix
    | OpenObjective Int


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        CompletedLoad (Ok community) ->
            case community of
                Just cmm ->
                    UR.init { model | status = Loaded cmm }

                Nothing ->
                    UR.init { model | status = NotFound }

        CompletedLoad (Err error) ->
            { model | status = Failed error }
                |> UR.init
                |> UR.logGraphqlError msg error

        GotTime date ->
            UR.init { model | date = Just date }

        OpenObjective index ->
            { model | openObjective = Just index }
                |> UR.init


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedLoad r ->
            [ "CompletedLoad", UR.resultToString r ]

        GotTime _ ->
            [ "GotTime" ]

        OpenObjective _ ->
            [ "OpenObjective" ]
