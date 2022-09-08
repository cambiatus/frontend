module Page.Community.Settings.ObjectiveEditor exposing (Model, Msg, initEdit, initNew, jsAddressToMsg, msgToString, receiveBroadcast, update, view)

import Action
import Cambiatus.Object
import Cambiatus.Object.Objective as Objective
import Community
import Dict
import Eos
import Eos.Account as Eos
import Form exposing (Form)
import Form.RichText
import Graphql.Http
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, a, br, button, div, h2, img, p, span, text)
import Html.Attributes exposing (alt, class, disabled, src, style, type_)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import Log
import Markdown exposing (Markdown)
import Page
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared as Shared exposing (Shared)
import UpdateResult as UR
import View.Components
import View.Feedback as Feedback
import View.Modal as Modal



-- INIT


initNew : LoggedIn.Model -> ( Model, Cmd Msg )
initNew loggedIn =
    ( { status = Loading
      , objectiveId = Nothing
      }
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )


initEdit : LoggedIn.Model -> Action.ObjectiveId -> ( Model, Cmd Msg )
initEdit loggedIn objectiveId =
    ( { status = Loading
      , objectiveId = Just objectiveId
      }
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )



-- MODEL


type alias Model =
    { status : Status
    , objectiveId : Maybe Action.ObjectiveId
    }


type Status
    = Loading
    | Authorized (Form.Model Form.RichText.Model) FormStatus
    | Unauthorized
    | NotFound


type FormStatus
    = CreatingObjective CreatingStatus
    | EditingObjective Community.Objective EditingStatus


type CreatingStatus
    = Creating
    | SavingCreation


type EditingStatus
    = Editing
    | SavingEdit
    | RequestingConfirmation
    | CompletingActions CompletionStatus
    | CompletedObjective


type alias CompletionStatus =
    { completed : List Action.Action
    , left : List { tries : Int, action : Action.Action }
    }


type alias Objective =
    { id : Int
    , description : String
    , isCompleted : Bool
    }


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = CompletedLoadCommunity Community.Model
    | CompletedLoadObjectives (List Community.Objective)
    | ClosedAuthModal
    | GotFormMsg (Form.Msg Form.RichText.Model)
    | ClickedSaveObjective Markdown
    | ClickedCompleteObjective
    | DeniedCompleteObjective
    | AcceptedCompleteObjective
    | GotCompleteActionResponse (Result Int String)
    | GotCompleteObjectiveResponse (RemoteData (Graphql.Http.Error (Maybe Objective)) (Maybe Objective))
    | GotSaveObjectiveResponse (Result Value String)
    | ClosedCelebrateObjectiveModal


initForm : Maybe Markdown -> Form.Model Form.RichText.Model
initForm maybeMarkdown =
    Form.RichText.initModel "description-input" maybeMarkdown
        |> Form.init



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view ({ shared } as loggedIn) model =
    let
        { t } =
            shared.translators

        title =
            case model.status of
                Authorized _ editStatus ->
                    let
                        action =
                            case editStatus of
                                CreatingObjective _ ->
                                    t "menu.create"

                                EditingObjective _ _ ->
                                    t "menu.edit"
                    in
                    action
                        ++ " "
                        ++ t "community.objectives.title"

                _ ->
                    ""

        content =
            case ( loggedIn.selectedCommunity, model.status ) of
                ( _, Loading ) ->
                    Page.fullPageLoading shared

                ( _, NotFound ) ->
                    Page.fullPageNotFound (t "community.objectives.editor.not_found") ""

                ( RemoteData.Loading, _ ) ->
                    Page.fullPageLoading shared

                ( RemoteData.NotAsked, _ ) ->
                    Page.fullPageLoading shared

                ( RemoteData.Failure e, _ ) ->
                    Page.fullPageGraphQLError (t "community.objectives.editor.error") e

                ( RemoteData.Success _, Unauthorized ) ->
                    Page.fullPageNotFound (t "community.edit.unauthorized") ""

                ( RemoteData.Success _, Authorized formModel formStatus ) ->
                    div []
                        [ Page.viewHeader loggedIn (t "community.objectives.title")
                        , div
                            [ class "w-full bg-white py-10"
                            ]
                            [ viewForm loggedIn formModel formStatus ]
                        , viewMarkAsCompletedConfirmationModal shared.translators model
                        , case formStatus of
                            EditingObjective _ (CompletingActions completionStatus) ->
                                viewCompletion shared completionStatus

                            _ ->
                                text ""
                        , viewCompletionCelebrationModal shared.translators model
                        ]
    in
    { title = title
    , content =
        case RemoteData.map .hasObjectives loggedIn.selectedCommunity of
            RemoteData.Success True ->
                content

            RemoteData.Success False ->
                Page.fullPageNotFound
                    (t "error.pageNotFound")
                    (t "community.objectives.disabled.description")

            RemoteData.Loading ->
                Page.fullPageLoading shared

            RemoteData.NotAsked ->
                Page.fullPageLoading shared

            RemoteData.Failure e ->
                Page.fullPageGraphQLError (t "community.error_loading") e
    }


viewCompletionCelebrationModal : Shared.Translators -> Model -> Html Msg
viewCompletionCelebrationModal { t } model =
    Modal.initWith
        { closeMsg = ClosedCelebrateObjectiveModal
        , isVisible = hasCompletedObjective model
        }
        |> Modal.withBody
            [ div [ class "min-h-full flex flex-col text-white text-center mx-auto" ]
                [ img
                    [ src "/images/animated-sparkling-doggo.svg"
                    , alt ""
                    , class "px-4 max-w-sm mx-auto"
                    ]
                    []
                , h2 [ class "font-bold text-xl mt-7" ]
                    [ text <| t "community.objectives.editor.celebration.title"
                    ]
                , p [ class "mt-3 flex-grow" ]
                    [ text <| t "community.objectives.editor.celebration.completed"
                    , br [] []
                    , br [] []
                    , text <| t "community.objectives.editor.celebration.special_moment"
                    , br [] []
                    , text <| t "community.objectives.editor.celebration.cheers"
                    ]
                , a
                    [ class "button button-primary w-full mt-8 flex-shrink-0"
                    , Route.href Route.CommunitySettingsObjectives
                    ]
                    [ text <| t "community.objectives.editor.celebration.return" ]
                ]
            ]
        |> Modal.withAttrs [ class "bg-purple-500" ]
        |> Modal.withSize Modal.FullScreen
        |> Modal.toHtml


createForm : Shared.Translators -> Form msg Form.RichText.Model Markdown
createForm { t } =
    Form.RichText.init { label = t "community.objectives.editor.description_label" }
        |> Form.RichText.withPlaceholder (t "community.objectives.editor.description_placeholder")
        |> Form.richText
            { parser = Ok
            , value = identity
            , update = \newModel _ -> newModel
            , externalError = always Nothing
            }


viewForm : LoggedIn.Model -> Form.Model Form.RichText.Model -> FormStatus -> Html Msg
viewForm ({ shared } as loggedIn) formModel formStatus =
    let
        ( isDisabled, isEdit, isCompleted ) =
            case formStatus of
                CreatingObjective creatingStatus ->
                    ( creatingStatus == SavingCreation
                    , False
                    , False
                    )

                EditingObjective objective editingStatus ->
                    ( editingStatus /= Editing
                    , True
                    , objective.isCompleted
                    )
    in
    Form.view [ class "container mx-auto px-4" ]
        shared.translators
        (\submitButton ->
            [ div [ class "mt-10 flex flex-col w-full gap-4 md:flex-row md:justify-between" ]
                [ submitButton
                    [ class "button button-primary w-full md:w-48"
                    , disabled (isDisabled || not loggedIn.hasAcceptedCodeOfConduct)
                    ]
                    [ text <| shared.translators.t "community.objectives.editor.submit" ]
                , if isEdit && not isCompleted then
                    button
                        [ class "button button-secondary w-full md:w-auto md:px-6 md:ml-auto"
                        , type_ "button"
                        , onClick ClickedCompleteObjective
                        , disabled (isDisabled || not loggedIn.hasAcceptedCodeOfConduct)
                        ]
                        [ text <| shared.translators.t "community.objectives.editor.mark_as_complete" ]

                  else
                    text ""
                ]
            ]
        )
        (createForm shared.translators)
        formModel
        { toMsg = GotFormMsg
        , onSubmit = ClickedSaveObjective
        }


viewMarkAsCompletedConfirmationModal : Shared.Translators -> Model -> Html Msg
viewMarkAsCompletedConfirmationModal { t } model =
    let
        isVisible =
            case model.status of
                Authorized _ (EditingObjective _ RequestingConfirmation) ->
                    True

                _ ->
                    False
    in
    Modal.initWith
        { closeMsg = DeniedCompleteObjective
        , isVisible = isVisible
        }
        |> Modal.withHeader (t "community.objectives.editor.modal.title")
        |> Modal.withBody
            [ text (t "community.objectives.editor.modal.body")
            ]
        |> Modal.withFooter
            [ button
                [ class "modal-cancel"
                , onClick DeniedCompleteObjective
                ]
                [ text (t "community.objectives.editor.modal.cancel") ]
            , button
                [ class "modal-accept"
                , onClick AcceptedCompleteObjective
                ]
                [ text (t "community.objectives.editor.modal.confirm") ]
            ]
        |> Modal.toHtml


viewCompletion : Shared -> CompletionStatus -> Html Msg
viewCompletion shared completionStatus =
    let
        totalNumber =
            List.length completionStatus.completed
                + List.length completionStatus.left

        progressWidth =
            String.fromFloat
                (toFloat (List.length completionStatus.completed)
                    / toFloat totalNumber
                )
    in
    viewModal
        [ View.Components.loadingLogoWithCustomText shared.translators
            "community.objectives.editor.completion_text"
            ""
        , div [ class "mb-10 mt-6" ]
            [ span [ class "text-black uppercase font-light text-sm" ]
                [ text
                    (shared.translators.tr
                        "community.objectives.editor.completed_progress"
                        [ ( "progress", List.length completionStatus.completed |> String.fromInt )
                        , ( "total", String.fromInt totalNumber )
                        ]
                    )
                ]
            , div [ class "h-2 relative flex mt-2 bg-gray-900 rounded-full overflow-hidden" ]
                [ div
                    [ class "bg-green w-full transition-transform origin-left"
                    , if List.length completionStatus.completed /= totalNumber then
                        style "transform" ("scaleX(" ++ progressWidth ++ ")")

                      else
                        class ""
                    ]
                    []
                ]
            ]
        ]


viewModal : List (Html Msg) -> Html Msg
viewModal body =
    div [ class "fixed inset-0 z-50" ]
        [ View.Components.bgNoScroll [ class "fixed inset-0 bg-black opacity-50" ]
            View.Components.PreventScrollAlways
        , div [ class "fixed top-modal inset-x-4 mx-auto max-w-sm px-8 pt-2 text-center bg-white rounded-lg" ]
            body
        ]



-- UPDATE


objectiveSelectionSet : SelectionSet Objective Cambiatus.Object.Objective
objectiveSelectionSet =
    SelectionSet.succeed Objective
        |> with Objective.id
        |> with Objective.description
        |> with Objective.isCompleted


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    let
        t =
            loggedIn.shared.translators.t
    in
    case msg of
        CompletedLoadCommunity community ->
            if community.creator == loggedIn.accountName then
                if model.status == Loading then
                    case model.objectiveId of
                        Just _ ->
                            model
                                |> UR.init
                                |> UR.addExt (LoggedIn.RequestedCommunityField Community.ObjectivesField)

                        Nothing ->
                            { model
                                | status =
                                    Creating
                                        |> CreatingObjective
                                        |> Authorized (initForm Nothing)
                            }
                                |> UR.init

                else
                    model |> UR.init

            else
                { model | status = Unauthorized }
                    |> UR.init

        CompletedLoadObjectives objectives ->
            case model.objectiveId of
                Nothing ->
                    { model
                        | status =
                            Creating
                                |> CreatingObjective
                                |> Authorized (initForm Nothing)
                    }
                        |> UR.init

                Just objectiveId ->
                    case List.find (.id >> (==) objectiveId) objectives of
                        Nothing ->
                            { model | status = NotFound }
                                |> UR.init

                        Just objective ->
                            { model
                                | status =
                                    Editing
                                        |> EditingObjective objective
                                        |> Authorized (initForm (Just objective.description))
                            }
                                |> UR.init

        ClosedAuthModal ->
            case model.status of
                Authorized form (CreatingObjective creatingStatus) ->
                    { model | status = Authorized (Form.withDisabled False form) (CreatingObjective creatingStatus) }
                        |> UR.init

                Authorized form (EditingObjective objective SavingEdit) ->
                    { model | status = Authorized (Form.withDisabled False form) (EditingObjective objective Editing) }
                        |> UR.init

                Authorized form status ->
                    { model | status = Authorized (Form.withDisabled False form) status }
                        |> UR.init

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Closed auth modal, but isn't authorized"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.ObjectiveEditor", function = "update" }
                            []

        ClickedCompleteObjective ->
            case model.status of
                Authorized form (EditingObjective objective Editing) ->
                    { model
                        | status =
                            RequestingConfirmation
                                |> EditingObjective objective
                                |> Authorized (Form.withDisabled True form)
                    }
                        |> UR.init

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Requested to complete objective, but isn't authorized"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.ObjectiveEditor", function = "update" }
                            []

        DeniedCompleteObjective ->
            case model.status of
                Authorized form (EditingObjective objective RequestingConfirmation) ->
                    { model | status = Authorized form (EditingObjective objective Editing) }
                        |> UR.init

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Denied to complete objective, but isn't authorized"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.ObjectiveEditor", function = "update" }
                            []

        AcceptedCompleteObjective ->
            case model.status of
                Authorized form (EditingObjective objective RequestingConfirmation) ->
                    let
                        completionStatus =
                            { completed = List.filter .isCompleted objective.actions
                            , left =
                                List.filterMap
                                    (\action ->
                                        if action.isCompleted then
                                            Nothing

                                        else
                                            Just { tries = 0, action = action }
                                    )
                                    objective.actions
                            }
                    in
                    { model
                        | status =
                            completionStatus
                                |> CompletingActions
                                |> EditingObjective objective
                                |> Authorized form
                    }
                        |> UR.init
                        |> completeActionOrObjective loggedIn
                            model
                            msg
                            completionStatus
                            objective

                _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Accepted to complete objective, but isn't authorized"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.ObjectiveEditor", function = "update" }
                            []

        GotCompleteActionResponse (Ok _) ->
            case model.status of
                Authorized form (EditingObjective objective (CompletingActions completionStatus)) ->
                    case completionStatus.left of
                        [] ->
                            model
                                |> UR.init
                                |> UR.logImpossible msg
                                    "Finished completing an action successfully, but there were none left"
                                    (Just loggedIn.accountName)
                                    { moduleName = "Page.Community.Settings.ObjectiveEditor", function = "update" }
                                    []

                        { action } :: left ->
                            let
                                newCompletionStatus =
                                    { completionStatus
                                        | completed = action :: completionStatus.completed
                                        , left = left
                                    }
                            in
                            { model
                                | status =
                                    newCompletionStatus
                                        |> CompletingActions
                                        |> EditingObjective objective
                                        |> Authorized form
                            }
                                |> UR.init
                                |> completeActionOrObjective loggedIn
                                    model
                                    msg
                                    newCompletionStatus
                                    objective

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Completed an action, but isn't authorized"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.ObjectiveEditor", function = "update" }
                            []

        GotCompleteActionResponse (Err _) ->
            case model.status of
                Authorized form (EditingObjective objective (CompletingActions completionStatus)) ->
                    case completionStatus.left of
                        [] ->
                            model
                                |> UR.init
                                |> UR.logImpossible msg
                                    "Finished completing an action with an error, but there were none left"
                                    (Just loggedIn.accountName)
                                    { moduleName = "Page.Community.Settings.ObjectiveEditor", function = "update" }
                                    []

                        { tries, action } :: left ->
                            let
                                maxRetries =
                                    2
                            in
                            if tries >= maxRetries then
                                let
                                    newCompletionStatus =
                                        { completionStatus
                                            | completed = action :: completionStatus.completed
                                            , left = left
                                        }
                                in
                                -- If we can't do it in `maxRetries` tries,
                                -- consider it completed and log it
                                { model
                                    | status =
                                        newCompletionStatus
                                            |> CompletingActions
                                            |> EditingObjective objective
                                            |> Authorized form
                                }
                                    |> UR.init
                                    |> completeActionOrObjective loggedIn
                                        model
                                        msg
                                        newCompletionStatus
                                        objective
                                    |> UR.logEvent
                                        { username = Just loggedIn.accountName
                                        , message = "Error when trying to complete action with objective"
                                        , tags = []
                                        , location = { moduleName = "Page.Community.Settings.ObjectiveEditor", function = "update" }
                                        , contexts =
                                            [ { name = "Details"
                                              , extras =
                                                    Dict.fromList
                                                        [ ( "actionId", Action.encodeId action.id )
                                                        , ( "objectiveId", Action.encodeObjectiveId objective.id )
                                                        , ( "tries", Encode.int tries )
                                                        , ( "maximumRetries", Encode.int maxRetries )
                                                        ]
                                              }
                                            ]
                                        , transaction = msg
                                        , level = Log.Warning
                                        }

                            else
                                let
                                    newCompletionStatus =
                                        { completionStatus
                                            | left =
                                                { tries = tries + 1, action = action }
                                                    :: left
                                        }
                                in
                                { model
                                    | status =
                                        newCompletionStatus
                                            |> CompletingActions
                                            |> EditingObjective objective
                                            |> Authorized form
                                }
                                    |> UR.init
                                    |> completeActionOrObjective loggedIn
                                        model
                                        msg
                                        newCompletionStatus
                                        objective
                                    |> UR.addBreadcrumb
                                        { type_ = Log.ErrorBreadcrumb
                                        , category = msg
                                        , message = "Failed to complete action"
                                        , data =
                                            Dict.fromList
                                                [ ( "tries", Encode.int tries )
                                                , ( "actionId", Action.encodeId action.id )
                                                ]
                                        , level = Log.Warning
                                        }

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Completed an action without being authorized"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.ObjectiveEditor", function = "update" }
                            []

        GotCompleteObjectiveResponse (RemoteData.Success _) ->
            case model.status of
                Authorized formModel formStatus ->
                    case formStatus of
                        EditingObjective objective _ ->
                            { model
                                | status =
                                    CompletedObjective
                                        |> EditingObjective objective
                                        |> Authorized formModel
                            }
                                |> UR.init

                        CreatingObjective _ ->
                            model
                                |> UR.init
                                |> UR.logImpossible msg
                                    "Completed objective, but was just creating an objective"
                                    (Just loggedIn.accountName)
                                    { moduleName = "Page.Community.Settings.ObjectiveEditor", function = "update" }
                                    []

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Completed objective, but was not authorized"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.ObjectiveEditor", function = "update" }
                            []

        GotCompleteObjectiveResponse (RemoteData.Failure err) ->
            { model
                | status =
                    case model.status of
                        Authorized form (EditingObjective objective _) ->
                            Editing
                                |> EditingObjective objective
                                |> Authorized (Form.withDisabled False form)

                        _ ->
                            model.status
            }
                |> UR.init
                |> UR.addExt (ShowFeedback Feedback.Failure (t "community.objectives.editor.error_marking_as_complete"))
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when completing objective"
                    { moduleName = "Page.Community.Settings.ObjectiveEditor", function = "update" }
                    []
                    err

        GotCompleteObjectiveResponse _ ->
            UR.init model

        ClickedSaveObjective description ->
            case ( loggedIn.selectedCommunity, model.status ) of
                ( RemoteData.Success community, Authorized form (CreatingObjective Creating) ) ->
                    { model
                        | status =
                            SavingCreation
                                |> CreatingObjective
                                |> Authorized (Form.withDisabled True form)
                    }
                        |> UR.init
                        |> UR.addPort
                            { responseAddress = ClickedSaveObjective description
                            , responseData = Encode.null
                            , data =
                                Eos.encodeTransaction
                                    [ { accountName = loggedIn.shared.contracts.community
                                      , name = "upsertobjctv"
                                      , authorization =
                                            { actor = loggedIn.accountName
                                            , permissionName = Eos.samplePermission
                                            }
                                      , data =
                                            { communityId = community.symbol
                                            , description = description
                                            , creator = loggedIn.accountName
                                            }
                                                |> Community.encodeCreateObjectiveAction
                                      }
                                    ]
                            }
                        |> LoggedIn.withPrivateKey loggedIn
                            []
                            model
                            { successMsg = msg, errorMsg = ClosedAuthModal }

                ( RemoteData.Success _, Authorized form (EditingObjective objective _) ) ->
                    { model
                        | status =
                            SavingEdit
                                |> EditingObjective objective
                                |> Authorized (Form.withDisabled True form)
                    }
                        |> UR.init
                        |> UR.addPort
                            { responseAddress = ClickedSaveObjective description
                            , responseData = Encode.null
                            , data =
                                Eos.encodeTransaction
                                    [ { accountName = loggedIn.shared.contracts.community
                                      , name = "upsertobjctv"
                                      , authorization =
                                            { actor = loggedIn.accountName
                                            , permissionName = Eos.samplePermission
                                            }
                                      , data =
                                            { communityId = objective.community.symbol
                                            , objectiveId = objective.id
                                            , description = description
                                            , editor = loggedIn.accountName
                                            }
                                                |> Community.encodeUpdateObjectiveAction
                                      }
                                    ]
                            }
                        |> LoggedIn.withPrivateKey loggedIn
                            []
                            model
                            { successMsg = msg, errorMsg = ClosedAuthModal }

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Tried saving objective without having the community loaded or without being authorized"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.ObjectiveEditor", function = "update" }
                            []

        GotSaveObjectiveResponse (Ok _) ->
            UR.init model
                |> UR.addExt (ShowFeedback Feedback.Success (t "community.objectives.create_success"))
                -- TODO - This only works sometimes
                |> UR.addExt (LoggedIn.RequestedReloadCommunityField Community.ObjectivesField)
                |> UR.addCmd (Route.replaceUrl loggedIn.shared.navKey (Route.CommunityObjectives Route.WithNoObjectiveSelected))

        GotSaveObjectiveResponse (Err v) ->
            let
                newModel =
                    case model.status of
                        Authorized form (CreatingObjective SavingCreation) ->
                            { model
                                | status =
                                    Creating
                                        |> CreatingObjective
                                        |> Authorized (Form.withDisabled False form)
                            }
                                |> UR.init
                                |> UR.logJsonValue msg
                                    (Just loggedIn.accountName)
                                    "Got an error when creating an objective"
                                    { moduleName = "Page.Community.Settings.ObjectiveEditor", function = "update" }
                                    []
                                    v

                        Authorized form (EditingObjective objective SavingEdit) ->
                            { model
                                | status =
                                    Editing
                                        |> EditingObjective objective
                                        |> Authorized (Form.withDisabled False form)
                            }
                                |> UR.init
                                |> UR.logJsonValue msg
                                    (Just loggedIn.accountName)
                                    "Got an error when updating an objective"
                                    { moduleName = "Page.Community.Settings.ObjectiveEditor", function = "update" }
                                    [ { name = "Objective"
                                      , extras = Dict.fromList [ ( "ID", Action.encodeObjectiveId objective.id ) ]
                                      }
                                    ]
                                    v

                        _ ->
                            model
                                |> UR.init
                                |> UR.logImpossible msg
                                    "Saved objective without being authorized"
                                    (Just loggedIn.accountName)
                                    { moduleName = "Page.Community.Settings.ObjectiveEditor", function = "update" }
                                    []
            in
            newModel
                |> UR.addExt (ShowFeedback Feedback.Failure (t "error.unknown"))

        GotFormMsg subMsg ->
            case model.status of
                Authorized form status ->
                    Form.update loggedIn.shared subMsg form
                        |> UR.fromChild (\newForm -> { model | status = Authorized newForm status })
                            GotFormMsg
                            LoggedIn.addFeedback
                            model

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Tried updating objective editor form, but wasn't authorized"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.ObjectiveEditor", function = "update" }
                            []

        ClosedCelebrateObjectiveModal ->
            model
                |> UR.init
                |> UR.addCmd (Route.pushUrl loggedIn.shared.navKey Route.CommunitySettingsObjectives)



-- UTILS


hasCompletedObjective : Model -> Bool
hasCompletedObjective model =
    case model.status of
        Authorized _ formStatus ->
            case formStatus of
                CreatingObjective _ ->
                    False

                EditingObjective _ status ->
                    case status of
                        CompletedObjective ->
                            True

                        _ ->
                            False

        _ ->
            False


completeActionOrObjective :
    LoggedIn.Model
    -> Model
    -> Msg
    -> CompletionStatus
    -> Community.Objective
    -> (UpdateResult -> UpdateResult)
completeActionOrObjective loggedIn model msg completionStatus objective =
    case List.head completionStatus.left of
        Nothing ->
            LoggedIn.mutation loggedIn
                (Action.completeObjectiveSelectionSet objective.id objectiveSelectionSet)
                GotCompleteObjectiveResponse
                |> UR.addExt

        Just { action } ->
            UR.addPort
                ({ action | isCompleted = True }
                    |> Action.updateAction loggedIn.accountName loggedIn.shared
                    |> (\completedAction ->
                            { responseAddress = AcceptedCompleteObjective
                            , responseData = Action.encodeId action.id
                            , data = Eos.encodeTransaction [ completedAction ]
                            }
                       )
                )
                >> LoggedIn.withPrivateKey loggedIn
                    []
                    model
                    { successMsg = msg, errorMsg = ClosedAuthModal }


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityLoaded community ->
            Just (CompletedLoadCommunity community)

        LoggedIn.CommunityFieldLoaded _ (Community.ObjectivesValue objectives) ->
            Just (CompletedLoadObjectives objectives)

        _ ->
            Nothing


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "ClickedSaveObjective" :: [] ->
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.field "transactionId" Decode.string
                        |> Decode.map Ok
                    , Decode.succeed (Err val)
                    ]
                )
                val
                |> Result.map (Just << GotSaveObjectiveResponse)
                |> Result.withDefault Nothing

        "AcceptedCompleteObjective" :: [] ->
            val
                |> Decode.decodeValue
                    (Decode.oneOf
                        [ Decode.field "transactionId" Decode.string
                            |> Decode.map Ok
                        , Decode.field "addressData" Decode.int
                            |> Decode.map Err
                        ]
                    )
                |> Result.map GotCompleteActionResponse
                |> Result.toMaybe

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        CompletedLoadObjectives _ ->
            [ "CompletedLoadObjectives" ]

        ClosedAuthModal ->
            [ "ClosedAuthModal" ]

        ClickedSaveObjective _ ->
            [ "ClickedSaveObjective" ]

        ClickedCompleteObjective ->
            [ "ClickedCompleteObjective" ]

        DeniedCompleteObjective ->
            [ "DeniedCompleteObjective" ]

        AcceptedCompleteObjective ->
            [ "AcceptedCompleteObjective" ]

        GotCompleteActionResponse r ->
            [ "GotCompleteActionResponse", UR.resultToString r ]

        GotCompleteObjectiveResponse r ->
            [ "GotCompleteObjectiveResponse", UR.remoteDataToString r ]

        GotSaveObjectiveResponse r ->
            [ "GotSaveObjectiveResponse", UR.resultToString r ]

        GotFormMsg subMsg ->
            "GotFormMsg" :: Form.msgToString subMsg

        ClosedCelebrateObjectiveModal ->
            [ "ClosedCelebrateObjectiveModal" ]
