module Page.Community.Objectives exposing (Model, Msg, init, jsAddressToMsg, msgToString, receiveBroadcast, subscriptions, update, view)

import Action exposing (Action, Msg(..))
import Browser.Dom
import Cambiatus.Enum.Permission as Permission
import Cambiatus.Enum.VerificationType as VerificationType
import Community
import Dict exposing (Dict)
import Eos
import Eos.Account
import Form
import Form.File
import Form.Text
import Html exposing (Html, a, b, button, details, div, h1, h2, h3, h4, img, li, p, span, summary, text, ul)
import Html.Attributes exposing (alt, class, classList, id, src, style, tabindex, title)
import Html.Attributes.Aria exposing (role)
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
import Session.Shared exposing (Shared)
import Sha256
import Task
import Time
import Time.Extra
import Translation
import UpdateResult as UR
import Url
import Utils
import View.Components exposing (intersectionObserver)
import View.Feedback
import View.Modal



-- MODEL


type alias Model =
    { shownObjectives :
        Dict
            ObjectiveId
            { visibleAction : Maybe Int
            , visibleActionHeight : Maybe Float
            , previousVisibleAction : Maybe Int
            , previousVisibleActionHeight : Maybe Float
            }
    , highlightedAction : Maybe { objectiveId : Int, actionId : Int }
    , sharingAction : Maybe Action
    , claimingStatus : ClaimingStatus
    }


type alias ObjectiveId =
    Int


type ClaimingStatus
    = NotClaiming
    | Claiming { position : Int, action : Action, proof : Proof }


type Proof
    = NoProofNecessary
    | WithProof (Form.Model Form.File.Model) ProofCode


type ProofCode
    = GeneratingCode
    | WithCode
        { code : String
        , expiration : Time.Posix
        , generation : Time.Posix
        }


init : Route.SelectedObjective -> LoggedIn.Model -> UpdateResult
init selectedObjective _ =
    UR.init
        { highlightedAction =
            case selectedObjective of
                Route.WithNoObjectiveSelected ->
                    Nothing

                Route.WithObjectiveSelected { id, action } ->
                    Maybe.map (\actionId -> { objectiveId = id, actionId = actionId }) action
        , shownObjectives =
            case selectedObjective of
                Route.WithNoObjectiveSelected ->
                    Dict.empty

                Route.WithObjectiveSelected { id } ->
                    Dict.fromList
                        [ ( id
                          , { visibleAction = Nothing
                            , visibleActionHeight = Nothing
                            , previousVisibleAction = Nothing
                            , previousVisibleActionHeight = Nothing
                            }
                          )
                        ]
        , sharingAction = Nothing

        -- TODO - Open modal when action is selected on url?
        , claimingStatus = NotClaiming
        }
        |> UR.addExt (LoggedIn.RequestedReloadCommunityField Community.ObjectivesField)



-- TYPES


type Msg
    = NoOp
    | CompletedLoadObjectives (List Community.Objective)
    | ClickedToggleObjectiveVisibility Community.Objective
    | GotVisibleActionViewport { objectiveId : ObjectiveId, actionId : Int } (Result Browser.Dom.Error Browser.Dom.Viewport)
    | ClickedScrollToAction Action
    | ClickedShareAction Action
    | ClickedClaimAction { position : Int, action : Action }
    | ClickedCloseClaimModal
    | StartedIntersecting String
    | StoppedIntersecting String
    | ConfirmedClaimAction
    | ConfirmedClaimActionWithPhotoProof String
    | GotPhotoProofFormMsg (Form.Msg Form.File.Model)
    | GotUint64Name String
    | GotTime
    | CompletedClaimingAction (Result Encode.Value ())


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
                                                List.Extra.find (\action -> action.id == actionId) foundObjective.actions
                                            )
                            in
                            case maybeAction of
                                Nothing ->
                                    identity

                                Just highlightedAction ->
                                    UR.addPort
                                        { responseAddress = NoOp
                                        , responseData = Encode.null
                                        , data =
                                            Encode.object
                                                [ ( "name", Encode.string "scrollIntoView" )
                                                , ( "id", Encode.string (actionCardId highlightedAction) )
                                                ]
                                        }
            in
            UR.init model
                |> scrollActionIntoView

        ClickedToggleObjectiveVisibility objective ->
            { model
                | shownObjectives =
                    Dict.update objective.id
                        (\currentValue ->
                            case currentValue of
                                Nothing ->
                                    Just
                                        { visibleAction = Nothing
                                        , visibleActionHeight = Nothing
                                        , previousVisibleAction = Nothing
                                        , previousVisibleActionHeight = Nothing
                                        }

                                Just _ ->
                                    Nothing
                        )
                        model.shownObjectives
                , highlightedAction = Nothing
            }
                |> UR.init

        GotVisibleActionViewport { objectiveId, actionId } (Ok { viewport }) ->
            { model
                | shownObjectives =
                    model.shownObjectives
                        |> Dict.update objectiveId
                            (\maybeValue ->
                                case maybeValue of
                                    Nothing ->
                                        Just
                                            { visibleAction = Just actionId
                                            , visibleActionHeight = Just viewport.height
                                            , previousVisibleAction = Nothing
                                            , previousVisibleActionHeight = Nothing
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
                            , ( "targetId", Encode.string (actionCardId action) )
                            ]
                    }

        ClickedShareAction action ->
            let
                sharePort =
                    if loggedIn.shared.canShare then
                        { responseAddress = msg
                        , responseData = Encode.null
                        , data =
                            Encode.object
                                [ ( "name", Encode.string "share" )

                                -- TODO - Maybe we should add some extra text?
                                , ( "title", Markdown.encode action.description )
                                , ( "url"
                                  , Route.CommunityObjectives
                                        (Route.WithObjectiveSelected
                                            { id = action.objective.id
                                            , action = Just action.id
                                            }
                                        )
                                        |> Route.addRouteToUrl loggedIn.shared
                                        |> Url.toString
                                        |> Encode.string
                                  )
                                ]
                        }

                    else
                        { responseAddress = msg
                        , responseData = Encode.null
                        , data =
                            Encode.object
                                [ ( "name", Encode.string "copyToClipboard" )
                                , ( "id", Encode.string "share-fallback-input" )
                                ]
                        }
            in
            { model | sharingAction = Just action }
                |> UR.init
                |> UR.addPort sharePort

        ClickedClaimAction { position, action } ->
            let
                proof =
                    if action.hasProofPhoto then
                        WithProof (Form.init (Form.File.initModel Nothing)) GeneratingCode

                    else
                        NoProofNecessary

                generateProofCodePort =
                    if action.hasProofPhoto then
                        UR.addPort
                            { responseAddress = msg
                            , responseData = Encode.null
                            , data =
                                Encode.object
                                    [ ( "name", Encode.string "accountNameToUint64" )
                                    , ( "accountName", Eos.Account.encodeName loggedIn.accountName )
                                    ]
                            }

                    else
                        identity
            in
            { model
                | claimingStatus =
                    Claiming
                        { position = position
                        , action = action
                        , proof = proof
                        }
            }
                |> UR.init
                |> generateProofCodePort

        ClickedCloseClaimModal ->
            { model | claimingStatus = NotClaiming }
                |> UR.init

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
                                    Dict.update parentObjective.id
                                        (\maybeValue ->
                                            case maybeValue of
                                                Nothing ->
                                                    Just
                                                        { visibleAction = Just actionId
                                                        , visibleActionHeight = Nothing
                                                        , previousVisibleAction = Nothing
                                                        , previousVisibleActionHeight = Nothing
                                                        }

                                                Just value ->
                                                    Just
                                                        { visibleAction = Just actionId
                                                        , visibleActionHeight = Nothing
                                                        , previousVisibleAction = value.visibleAction
                                                        , previousVisibleActionHeight = value.visibleActionHeight
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
                            { moduleName = "Page.Community.Objectives.elm", function = "update" }
                            [ Log.contextFromCommunity loggedIn.selectedCommunity ]

        StoppedIntersecting targetId ->
            let
                newShownObjectives =
                    Dict.foldl
                        (\objectiveId value currDict ->
                            if value.visibleAction == idFromActionCardId targetId then
                                Dict.insert objectiveId
                                    { visibleAction = value.previousVisibleAction
                                    , visibleActionHeight = value.previousVisibleActionHeight
                                    , previousVisibleAction = Nothing
                                    , previousVisibleActionHeight = Nothing
                                    }
                                    currDict

                            else if value.previousVisibleAction == idFromActionCardId targetId then
                                Dict.insert objectiveId
                                    { value
                                        | previousVisibleAction = Nothing
                                        , previousVisibleActionHeight = Nothing
                                    }
                                    currDict

                            else
                                Dict.insert objectiveId value currDict
                        )
                        Dict.empty
                        model.shownObjectives
            in
            { model | shownObjectives = newShownObjectives }
                |> UR.init

        ConfirmedClaimAction ->
            case model.claimingStatus of
                Claiming { action } ->
                    UR.init model
                        |> UR.addPort
                            { responseAddress = msg
                            , responseData = Encode.null
                            , data =
                                Eos.encodeTransaction
                                    [ { accountName = loggedIn.shared.contracts.community
                                      , name = "claimaction"
                                      , authorization =
                                            { actor = loggedIn.accountName
                                            , permissionName = Eos.Account.samplePermission
                                            }
                                      , data =
                                            Action.encodeClaimAction
                                                { communityId = action.objective.community.symbol
                                                , actionId = action.id
                                                , claimer = loggedIn.accountName
                                                , proof = Nothing
                                                }
                                      }
                                    ]
                            }
                        |> LoggedIn.withPrivateKey loggedIn
                            [ Permission.Claim ]
                            model
                            { successMsg = msg, errorMsg = NoOp }

                _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Confirmed claim action, but wasn't claiming"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Objectives", function = "update" }
                            []

        ConfirmedClaimActionWithPhotoProof proofUrl ->
            case model.claimingStatus of
                Claiming { action, proof } ->
                    case proof of
                        WithProof _ (WithCode { code, generation }) ->
                            UR.init model
                                |> UR.addPort
                                    { responseAddress = msg
                                    , responseData = Encode.null
                                    , data =
                                        Eos.encodeTransaction
                                            [ { accountName = loggedIn.shared.contracts.community
                                              , name = "claimaction"
                                              , authorization =
                                                    { actor = loggedIn.accountName
                                                    , permissionName = Eos.Account.samplePermission
                                                    }
                                              , data =
                                                    Action.encodeClaimAction
                                                        { communityId = action.objective.community.symbol
                                                        , actionId = action.id
                                                        , claimer = loggedIn.accountName
                                                        , proof =
                                                            Just
                                                                { photo = proofUrl
                                                                , code = code
                                                                , time = generation
                                                                }
                                                        }
                                              }
                                            ]
                                    }
                                |> LoggedIn.withPrivateKey loggedIn
                                    [ Permission.Claim ]
                                    model
                                    { successMsg = msg, errorMsg = NoOp }

                        _ ->
                            UR.init model

                _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Confirmed claim action with proof, but wasn't claiming"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Objectives", function = "update" }
                            []

        GotPhotoProofFormMsg subMsg ->
            case model.claimingStatus of
                Claiming { position, action, proof } ->
                    case proof of
                        WithProof formModel proofCode ->
                            Form.update loggedIn.shared subMsg formModel
                                |> UR.fromChild
                                    (\newFormModel ->
                                        { model
                                            | claimingStatus =
                                                Claiming
                                                    { position = position
                                                    , action = action
                                                    , proof = WithProof newFormModel proofCode
                                                    }
                                        }
                                    )
                                    GotPhotoProofFormMsg
                                    LoggedIn.addFeedback
                                    model

                        NoProofNecessary ->
                            UR.init model

                NotClaiming ->
                    UR.init model

        GotUint64Name uint64Name ->
            case model.claimingStatus of
                Claiming { position, action, proof } ->
                    case proof of
                        WithProof formModel _ ->
                            let
                                proofCode =
                                    generateProofCode action
                                        uint64Name
                                        loggedIn.shared.now

                                expiration =
                                    Time.Extra.add Time.Extra.Minute
                                        30
                                        loggedIn.shared.timezone
                                        loggedIn.shared.now
                            in
                            { model
                                | claimingStatus =
                                    Claiming
                                        { position = position
                                        , action = action
                                        , proof =
                                            WithProof formModel
                                                (WithCode
                                                    { code = proofCode
                                                    , expiration = expiration
                                                    , generation = loggedIn.shared.now
                                                    }
                                                )
                                        }
                            }
                                |> UR.init

                        NoProofNecessary ->
                            UR.init model

                _ ->
                    UR.init model

        GotTime ->
            UR.init model
                |> UR.addExt (LoggedIn.ReloadResource LoggedIn.TimeResource)

        CompletedClaimingAction (Ok ()) ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    { model | claimingStatus = NotClaiming }
                        |> UR.init
                        |> UR.addExt (LoggedIn.ShowFeedback View.Feedback.Success (loggedIn.shared.translators.tr "dashboard.check_claim.success" [ ( "symbolCode", Eos.symbolToSymbolCodeString community.symbol ) ]))
                        |> UR.addCmd
                            (Eos.Account.nameToString loggedIn.accountName
                                |> Route.ProfileClaims
                                |> Route.pushUrl loggedIn.shared.navKey
                            )

                _ ->
                    { model | claimingStatus = NotClaiming }
                        |> UR.init
                        |> UR.logImpossible msg
                            "Completed claiming action, but community wasn't loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Objectives", function = "update" }
                            []
                        |> UR.addCmd
                            (Eos.Account.nameToString loggedIn.accountName
                                |> Route.ProfileClaims
                                |> Route.pushUrl loggedIn.shared.navKey
                            )

        CompletedClaimingAction (Err val) ->
            { model | claimingStatus = NotClaiming }
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback View.Feedback.Failure (loggedIn.shared.translators.t "dashboard.check_claim.failure"))
                |> UR.logJsonValue msg
                    (Just loggedIn.accountName)
                    "Got an error when claiming an action"
                    { moduleName = "Page.Community.Objectives", function = "update" }
                    []
                    val



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        title =
            "TODO"
    in
    { title = title
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
                                    List.filter (\objective -> not objective.isCompleted)
                                        objectives
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
                                            |> List.filter (\objective -> List.member objective.id (Dict.keys model.shownObjectives))
                                            |> List.concatMap .actions
                                            |> List.filterMap
                                                (\action ->
                                                    if action.isCompleted then
                                                        Nothing

                                                    else
                                                        Just ("#" ++ actionCardId action)
                                                )
                                    , threshold = 0.01
                                    , breakpointToExclude = View.Components.Lg
                                    , onStartedIntersecting = Just StartedIntersecting
                                    , onStoppedIntersecting = Just StoppedIntersecting
                                    }
                                , if not loggedIn.shared.canShare then
                                    Form.Text.view
                                        (Form.Text.init
                                            { label = ""
                                            , id = "share-fallback-input"
                                            }
                                            |> Form.Text.withExtraAttrs
                                                [ class "absolute opacity-0 left-[-9999em]"
                                                , tabindex -1
                                                ]
                                            |> Form.Text.withContainerAttrs [ class "mb-0 overflow-hidden" ]
                                        )
                                        { onChange = \_ -> NoOp
                                        , onBlur = NoOp
                                        , value =
                                            -- TODO - Add some better text
                                            case model.sharingAction of
                                                Nothing ->
                                                    Url.toString loggedIn.shared.url

                                                Just sharingAction ->
                                                    Route.WithObjectiveSelected
                                                        { id = sharingAction.objective.id
                                                        , action = Just sharingAction.id
                                                        }
                                                        |> Route.CommunityObjectives
                                                        |> Route.addRouteToUrl loggedIn.shared
                                                        |> Url.toString
                                        , error = text ""
                                        , hasError = False
                                        , translators = loggedIn.shared.translators
                                        , isRequired = False
                                        }

                                  else
                                    text ""
                                , viewClaimModal loggedIn.shared model
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

                                -- TODO - I18N
                                , p [ class "text-center mt-4" ] [ text "Algo de errado aconteceu ao buscar os objetivos da comunidade" ]
                                ]
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

            RemoteData.Loading ->
                Page.fullPageLoading loggedIn.shared

            RemoteData.NotAsked ->
                Page.fullPageLoading loggedIn.shared

            RemoteData.Failure err ->
                Page.fullPageGraphQLError title err
    }


viewObjective : Translation.Translators -> Model -> Community.Objective -> Html Msg
viewObjective translators model objective =
    let
        filteredActions =
            List.filter (\action -> not action.isCompleted)
                objective.actions

        isOpen =
            List.member objective.id (Dict.keys model.shownObjectives)

        maybeShownObjectivesInfo =
            Dict.get objective.id model.shownObjectives

        visibleActionId =
            maybeShownObjectivesInfo
                |> Maybe.andThen .visibleAction

        visibleActionHeight =
            maybeShownObjectivesInfo
                |> Maybe.andThen .visibleActionHeight

        previousVisibleActionHeight =
            maybeShownObjectivesInfo
                |> Maybe.andThen .previousVisibleActionHeight
    in
    li []
        [ details
            [ if isOpen then
                Html.Attributes.attribute "open" "true"

              else
                class ""
            ]
            [ summary
                [ class "marker-hidden lg:w-2/3 lg:mx-auto"
                , onClick (ClickedToggleObjectiveVisibility objective)
                ]
                [ div
                    [ class "flex marker-hidden items-center bg-white rounded px-4 py-6 cursor-pointer"
                    ]
                    [ Icons.cambiatusCoin "text-blue fill-current flex-shrink-0 self-start mt-1"
                    , h3 [ title (Markdown.toRawString objective.description) ]
                        [ Markdown.view [ class "font-bold px-4 line-clamp-4 self-start mt-1" ] objective.description ]
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
                [ class "duration-300 ease-in-out origin-top lg:!h-full motion-reduce:transition-none"
                , classList
                    [ ( "lg:scale-0", not isOpen )
                    , ( "lg:scale-1", isOpen )
                    , ( "transition-all", Maybe.Extra.isJust visibleActionHeight )
                    , ( "transition-transform", Maybe.Extra.isNothing visibleActionHeight )
                    ]
                , case visibleActionHeight of
                    Nothing ->
                        case previousVisibleActionHeight of
                            Nothing ->
                                class ""

                            Just height ->
                                style "height"
                                    ("calc(" ++ String.fromInt (ceiling height) ++ "px + 24px)")

                    Just height ->
                        style "height"
                            (max (ceiling height)
                                (ceiling <| Maybe.withDefault 0 <| previousVisibleActionHeight)
                                |> String.fromInt
                                |> (\heightString -> "calc(" ++ heightString ++ "px + 24px")
                            )
                ]
                [ if not isOpen then
                    text ""

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
                        [ class "mt-4 mb-2 flex h-full overflow-y-hidden overflow-x-scroll snap-x snap-proximity scrollbar-hidden gap-4 transition-all lg:gap-x-6 lg:overflow-visible lg:-mb-4"
                        , classList
                            [ ( "lg:grid-cols-1 lg:w-1/2 lg:mx-auto xl:w-1/3", List.length filteredActions == 1 )
                            , ( "lg:grid-cols-2 xl:grid-cols-2 xl:w-2/3 xl:mx-auto", List.length filteredActions == 2 )
                            , ( "lg:grid-cols-2 xl:grid-cols-3", List.length filteredActions > 2 )
                            ]
                        , id (objectiveContainerId objective)
                        , role "list"
                        ]
                        (List.indexedMap
                            (viewAction translators model)
                            filteredActions
                        )
                ]
            , div [ class "flex justify-center gap-2 lg:hidden" ]
                (filteredActions
                    |> List.map
                        (\action ->
                            button
                                [ class "border border-gray-900 rounded-full w-3 h-3 transition-colors"
                                , classList [ ( "border-orange-300 bg-orange-300", Just action.id == visibleActionId ) ]
                                , id ("go-to-action-" ++ String.fromInt action.id)
                                , onClick (ClickedScrollToAction action)
                                ]
                                []
                        )
                )
            ]
        ]


viewAction : Translation.Translators -> Model -> Int -> Action -> Html Msg
viewAction translators model index action =
    let
        isHighlighted =
            case model.highlightedAction of
                Nothing ->
                    False

                Just { actionId } ->
                    actionId == action.id

        isClaimable =
            case action.verificationType of
                VerificationType.Claimable ->
                    True

                VerificationType.Automatic ->
                    False
    in
    li
        [ class "bg-white rounded px-4 pt-4 pb-6 self-start w-full flex-shrink-0 snap-center snap-always mb-6 animate-fade-in-from-above motion-reduce:animate-none"
        , classList [ ( "border border-green ring ring-green ring-opacity-30", isHighlighted ) ]
        , style "animation-delay" ("calc(75ms * " ++ String.fromInt index ++ ")")
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
        , div
            [ class "grid grid-cols-1 sm:grid-cols-2 gap-x-4 gap-y-2 mt-6"
            , classList [ ( "sm:grid-cols-1", not isClaimable ) ]
            ]
            [ button
                [ class "button button-secondary w-full"
                , onClick (ClickedShareAction action)
                ]
                [ Icons.share "mr-2 flex-shrink-0"

                -- TODO - I18N
                , text "Compartilhar"
                ]
            , if isClaimable then
                button
                    [ class "button button-primary w-full sm:col-span-1"
                    , onClick (ClickedClaimAction { position = index + 1, action = action })
                    ]
                    [ if action.hasProofPhoto then
                        Icons.camera "w-4 mr-2 flex-shrink-0"

                      else
                        text ""

                    -- TODO - I18N
                    , text "Reivindicar"
                    ]

              else
                text ""
            ]
        ]


viewClaimModal : Shared -> Model -> Html Msg
viewClaimModal ({ translators } as shared) model =
    case model.claimingStatus of
        NotClaiming ->
            text ""

        Claiming { position, action, proof } ->
            let
                viewClaimCount attrs =
                    div
                        (class "mt-4 p-2 bg-gray-100 flex items-center justify-center text-gray-900 font-semibold text-sm rounded-sm"
                            :: attrs
                        )
                        [ img
                            [ src "/images/doggo_holding_coins.svg"
                            , alt ""
                            , class "w-8 mr-2"
                            ]
                            []

                        -- TODO - I18N
                        , text "Membros reivindicaram esta ação"

                        -- TODO - Use real data
                        , span [ class "text-base ml-1 font-bold" ] [ text " 340 vezes" ]
                        ]
            in
            View.Modal.initWith
                { closeMsg = ClickedCloseClaimModal
                , isVisible = True
                }
                |> View.Modal.withBody
                    [ div [ class "flex" ]
                        [ span [ class "text-lg text-gray-500 font-bold" ] [ text (String.fromInt position ++ ".") ]
                        , div [ class "ml-5 mt-1 min-w-0 w-full" ]
                            [ Markdown.view [ class "truncate" ] action.description
                            , div [ class "md:flex md:justify-between md:w-full" ]
                                [ div []
                                    [ span [ class "font-bold text-sm text-gray-900 uppercase block mt-6" ]
                                        -- TODO - I18N
                                        [ text "Recompensa" ]
                                    , div [ class "text-green font-bold" ]
                                        [ span [ class "text-2xl mr-1" ]
                                            [ text
                                                (Eos.formatSymbolAmount translators
                                                    action.objective.community.symbol
                                                    action.reward
                                                )
                                            ]
                                        , text (Eos.symbolToSymbolCodeString action.objective.community.symbol)
                                        ]
                                    ]
                                , viewClaimCount [ class "hidden md:flex md:self-end md:mr-8" ]
                                ]
                            ]
                        ]
                    , viewClaimCount [ class "md:hidden" ]
                    , case proof of
                        WithProof formModel proofCode ->
                            let
                                timeLeft =
                                    case proofCode of
                                        GeneratingCode ->
                                            Nothing

                                        WithCode { expiration } ->
                                            let
                                                minutes =
                                                    Time.Extra.diff Time.Extra.Minute
                                                        shared.timezone
                                                        shared.now
                                                        expiration

                                                seconds =
                                                    Time.Extra.diff Time.Extra.Second
                                                        shared.timezone
                                                        shared.now
                                                        expiration
                                                        |> modBy 60
                                            in
                                            Just { minutes = minutes, seconds = seconds }

                                isTimeOver =
                                    case timeLeft of
                                        Nothing ->
                                            False

                                        Just { minutes } ->
                                            minutes < 0
                            in
                            div []
                                [ -- TODO - I18N
                                  p [ class "text-lg font-bold text-gray-333 mt-6 mb-4 md:text-center" ] [ text "Esta ação requer uma prova fotográfica" ]
                                , case action.photoProofInstructions of
                                    Just instructions ->
                                        Markdown.view [ class "text-center" ] instructions

                                    Nothing ->
                                        -- TODO - I18N
                                        p [] [ text "Favor enviar uma foto com a prova de que você reivindicou esta ação." ]
                                , div [ class "p-4 mt-4 bg-gray-100 rounded-sm flex flex-col items-center justify-center md:w-1/2 md:mx-auto" ]
                                    [ -- TODO - I18N
                                      span [ class "uppercase text-gray-333 font-bold text-sm" ] [ text "Código de verificação" ]
                                    , case proofCode of
                                        GeneratingCode ->
                                            span [ class "bg-gray-333 animate-skeleton-loading h-10 w-44 mt-2" ] []

                                        WithCode { code } ->
                                            span [ class "font-bold text-xl text-gray-333" ] [ text code ]
                                    ]
                                , p
                                    [ class "text-purple-500 text-center mt-4"
                                    , classList [ ( "text-red", isTimeOver ) ]
                                    ]
                                    -- TODO - I18N
                                    [ text "o código é válido por "

                                    -- TODO - I18N
                                    , span [ class "font-bold" ]
                                        [ case timeLeft of
                                            Nothing ->
                                                text "30:00"

                                            Just { minutes, seconds } ->
                                                (Utils.padInt 2 minutes ++ ":" ++ Utils.padInt 2 seconds)
                                                    |> text
                                        ]
                                    , text " minutos"
                                    ]
                                , Form.view []
                                    translators
                                    (\submitButton ->
                                        [ div [ class "grid md:grid-cols-2 gap-4 my-6" ]
                                            [ button
                                                [ class "button button-secondary w-full"
                                                , onClick ClickedCloseClaimModal
                                                ]
                                                -- TODO - I18N
                                                [ text "Cancelar" ]
                                            , submitButton [ class "button button-primary w-full" ]
                                                -- TODO - I18N
                                                [ text "Reivindicar" ]
                                            ]
                                        ]
                                    )
                                    (claimWithPhotoForm translators)
                                    formModel
                                    { toMsg = GotPhotoProofFormMsg
                                    , onSubmit = ConfirmedClaimActionWithPhotoProof
                                    }
                                ]

                        NoProofNecessary ->
                            div [ class "grid md:grid-cols-2 gap-4 my-6" ]
                                [ button
                                    [ class "button button-secondary w-full"
                                    , onClick ClickedCloseClaimModal
                                    ]
                                    -- TODO - I18N
                                    [ text "Cancelar" ]

                                -- TODO - I18N
                                , button
                                    [ class "button button-primary w-full"
                                    , onClick ConfirmedClaimAction
                                    ]
                                    [ text "Reivindicar" ]
                                ]
                    ]
                |> View.Modal.withSize View.Modal.Large
                |> View.Modal.toHtml


claimWithPhotoForm : Translation.Translators -> Form.Form msg Form.File.Model String
claimWithPhotoForm translators =
    Form.succeed identity
        |> Form.with
            (Form.File.init
                { label = ""
                , id = "photo-proof-input"
                }
                |> Form.File.withVariant (Form.File.LargeRectangle Form.File.Gray)
                |> Form.File.withFileTypes [ Form.File.Image, Form.File.PDF ]
                |> Form.file
                    { translators = translators
                    , value = identity
                    , update = \newModel _ -> newModel
                    , externalError = always Nothing
                    }
            )



-- UTILS


generateProofCode : Action -> String -> Time.Posix -> String
generateProofCode action claimerAccountUint64 time =
    (String.fromInt action.id
        ++ claimerAccountUint64
        ++ String.fromInt (Time.posixToMillis time // 1000)
    )
        |> Sha256.sha256
        |> String.slice 0 8


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


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityFieldLoaded _ (Community.ObjectivesValue objectives) ->
            Just (CompletedLoadObjectives objectives)

        _ ->
            Nothing


subscriptions : Model -> Sub Msg
subscriptions _ =
    -- We just tell logged in to get the time. This way, we have a single source of truth for time
    Time.every 1000 (\_ -> GotTime)


jsAddressToMsg : List String -> Encode.Value -> Maybe Msg
jsAddressToMsg addr val =
    let
        decodeConfirmedClaimAction =
            Decode.decodeValue (Decode.field "transactionId" Decode.string) val
                |> Result.map (\_ -> ())
                |> Result.mapError (\_ -> val)
                |> CompletedClaimingAction
                |> Just
    in
    case addr of
        "ClickedShareAction" :: _ ->
            Just NoOp

        "ClickedClaimAction" :: _ ->
            Decode.decodeValue (Decode.field "uint64name" Decode.string) val
                |> Result.map GotUint64Name
                |> Result.toMaybe

        "ConfirmedClaimAction" :: _ ->
            decodeConfirmedClaimAction

        "ConfirmedClaimActionWithPhotoProof" :: _ ->
            decodeConfirmedClaimAction

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

        GotVisibleActionViewport _ _ ->
            [ "GotVisibleActionViewport" ]

        ClickedScrollToAction _ ->
            [ "ClickedScrollToAction" ]

        ClickedShareAction _ ->
            [ "ClickedShareAction" ]

        ClickedClaimAction _ ->
            [ "ClickedClaimAction" ]

        ClickedCloseClaimModal ->
            [ "ClickedCloseClaimModal" ]

        StartedIntersecting _ ->
            [ "StartedIntersecting" ]

        StoppedIntersecting _ ->
            [ "StoppedIntersecting" ]

        ConfirmedClaimAction ->
            [ "ConfirmedClaimAction" ]

        ConfirmedClaimActionWithPhotoProof _ ->
            [ "ConfirmedClaimActionWithPhotoProof" ]

        GotPhotoProofFormMsg _ ->
            [ "GotPhotoProofFormMsg" ]

        GotUint64Name _ ->
            [ "GotUint64Name" ]

        GotTime ->
            [ "GotTime" ]

        CompletedClaimingAction r ->
            [ "CompletedClaimingAction", UR.resultToString r ]
