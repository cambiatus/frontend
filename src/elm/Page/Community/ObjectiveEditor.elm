module Page.Community.ObjectiveEditor exposing (Model, Msg, initEdit, initNew, jsAddressToMsg, msgToString, receiveBroadcast, subscriptions, update, view)

import Action
import Api.Graphql
import Cambiatus.Mutation as Mutation
import Cambiatus.Object
import Cambiatus.Object.Objective as Objective
import Community
import Dict
import Eos
import Eos.Account as Eos
import Graphql.Http
import Graphql.Operation exposing (RootMutation)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, disabled, style, type_)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import Log
import Page
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared, Translators)
import UpdateResult as UR
import View.Components
import View.Feedback as Feedback
import View.MarkdownEditor as MarkdownEditor
import View.Modal as Modal



-- INIT


initNew : LoggedIn.Model -> ( Model, Cmd Msg )
initNew loggedIn =
    ( { status = Loading
      , objectiveId = Nothing
      }
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )


initEdit : LoggedIn.Model -> Int -> ( Model, Cmd Msg )
initEdit loggedIn objectiveId =
    ( { status = Loading
      , objectiveId = Just objectiveId
      }
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )



-- MODEL


type alias Model =
    { status : Status
    , objectiveId : Maybe Int
    }


type Status
    = Loading
    | Authorized FormStatus
    | Unauthorized
    | NotFound


type FormStatus
    = CreatingObjective ObjectiveForm CreatingStatus
    | EditingObjective Community.Objective ObjectiveForm EditingStatus


type CreatingStatus
    = Creating
    | SavingCreation


type EditingStatus
    = Editing
    | SavingEdit
    | RequestingConfirmation
    | CompletingActions CompletionStatus


type alias CompletionStatus =
    { completed : List Action.Action
    , left : List { tries : Int, action : Action.Action }
    }


type alias ObjectiveForm =
    { description : MarkdownEditor.Model
    , isCompleted : Bool
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
    | ClosedAuthModal
    | GotDescriptionEditorMsg MarkdownEditor.Msg
    | ClickedSaveObjective
    | ClickedCompleteObjective
    | DeniedCompleteObjective
    | AcceptedCompleteObjective
    | GotCompleteActionResponse (Result Int String)
    | GotCompleteObjectiveResponse (RemoteData (Graphql.Http.Error (Maybe Objective)) (Maybe Objective))
    | GotSaveObjectiveResponse (Result Value String)


initObjectiveForm : ObjectiveForm
initObjectiveForm =
    { description = MarkdownEditor.init "objective-description"
    , isCompleted = False
    }



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view ({ shared } as loggedIn) model =
    let
        { t } =
            shared.translators

        title =
            case model.status of
                Authorized editStatus ->
                    let
                        action =
                            case editStatus of
                                CreatingObjective _ _ ->
                                    t "menu.create"

                                EditingObjective _ _ _ ->
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
                    text "not allowed to edit"

                ( RemoteData.Success _, Authorized editStatus ) ->
                    div []
                        [ Page.viewHeader loggedIn (t "community.objectives.title")
                        , viewForm loggedIn editStatus
                        , viewMarkAsCompletedConfirmationModal shared.translators model
                        , case editStatus of
                            EditingObjective _ _ (CompletingActions completionStatus) ->
                                viewCompletion shared completionStatus

                            _ ->
                                text ""
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


viewForm : LoggedIn.Model -> FormStatus -> Html Msg
viewForm { shared } formStatus =
    let
        t =
            shared.translators.t

        ( isDisabled, objForm, isEdit ) =
            case formStatus of
                CreatingObjective form status ->
                    case status of
                        Creating ->
                            ( False, form, False )

                        SavingCreation ->
                            ( True, form, False )

                EditingObjective _ form status ->
                    case status of
                        Editing ->
                            ( False, form, True )

                        _ ->
                            ( True, form, True )
    in
    div [ class "bg-white w-full p-10" ]
        [ div [ class "container mx-auto" ]
            [ Html.form
                [ class "mb-10"
                ]
                [ MarkdownEditor.view
                    { translators = shared.translators
                    , placeholder = Just (t "community.objectives.editor.description_placeholder")
                    , label = t "community.objectives.editor.description_label"
                    , problem = Nothing
                    , disabled = False
                    }
                    []
                    objForm.description
                    |> Html.map GotDescriptionEditorMsg
                ]
            , div [ class "flex flex-col w-full space-y-4 md:space-y-0 md:flex-row md:justify-between" ]
                [ button
                    [ class "button button-primary w-full md:w-48"
                    , type_ "button"
                    , onClick ClickedSaveObjective
                    , disabled isDisabled
                    ]
                    [ text (t "community.objectives.editor.submit") ]
                , if isEdit && not objForm.isCompleted then
                    button
                        [ class "button button-secondary w-full md:w-48"
                        , type_ "button"
                        , onClick ClickedCompleteObjective
                        , disabled isDisabled
                        ]
                        [ text (t "community.objectives.editor.mark_as_complete") ]

                  else
                    text ""
                ]
            ]
        ]


viewMarkAsCompletedConfirmationModal : Translators -> Model -> Html Msg
viewMarkAsCompletedConfirmationModal { t } model =
    let
        isVisible =
            case model.status of
                Authorized (EditingObjective _ _ RequestingConfirmation) ->
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
            [ span [ class "text-black uppercase font-light text-xs" ]
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


completeObjectiveSelectionSet : Int -> SelectionSet (Maybe Objective) RootMutation
completeObjectiveSelectionSet objectiveId =
    Mutation.completeObjective { input = { objectiveId = objectiveId } }
        objectiveSelectionSet


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
                        Just objectiveId ->
                            case List.find (\o -> o.id == objectiveId) community.objectives of
                                Just objective ->
                                    { model
                                        | status =
                                            Editing
                                                |> EditingObjective objective
                                                    { description =
                                                        MarkdownEditor.init "objective-description"
                                                            |> MarkdownEditor.setContents objective.description
                                                    , isCompleted = objective.isCompleted
                                                    }
                                                |> Authorized
                                    }
                                        |> UR.init

                                Nothing ->
                                    { model | status = NotFound }
                                        |> UR.init

                        Nothing ->
                            { model
                                | status =
                                    Creating
                                        |> CreatingObjective initObjectiveForm
                                        |> Authorized
                            }
                                |> UR.init

                else
                    model |> UR.init

            else
                { model | status = Unauthorized }
                    |> UR.init

        ClosedAuthModal ->
            case model.status of
                Authorized (CreatingObjective form SavingCreation) ->
                    { model | status = Authorized (CreatingObjective form Creating) }
                        |> UR.init

                Authorized (EditingObjective objective form SavingEdit) ->
                    { model | status = Authorized (EditingObjective objective form Editing) }
                        |> UR.init

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Closed auth modal, but isn't authorized"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.ObjectiveEditor", function = "update" }
                            []

        GotDescriptionEditorMsg subMsg ->
            case model.status of
                Authorized (CreatingObjective objForm Creating) ->
                    let
                        ( newDescription, descriptionCmd ) =
                            MarkdownEditor.update subMsg objForm.description
                    in
                    UR.init
                        { model
                            | status =
                                CreatingObjective
                                    { objForm | description = newDescription }
                                    Creating
                                    |> Authorized
                        }
                        |> UR.addCmd (Cmd.map GotDescriptionEditorMsg descriptionCmd)

                Authorized (EditingObjective objective objForm Editing) ->
                    let
                        ( newDescription, descriptionCmd ) =
                            MarkdownEditor.update subMsg objForm.description
                    in
                    UR.init
                        { model
                            | status =
                                EditingObjective objective { objForm | description = newDescription } Editing
                                    |> Authorized
                        }
                        |> UR.addCmd (Cmd.map GotDescriptionEditorMsg descriptionCmd)

                _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Tried updating the objective form, but isn't authorized"
                            Nothing
                            { moduleName = "Page.Community.ObjectiveEditor", function = "update" }
                            []

        ClickedCompleteObjective ->
            case model.status of
                Authorized (EditingObjective objective form Editing) ->
                    { model
                        | status =
                            Authorized
                                (EditingObjective objective form RequestingConfirmation)
                    }
                        |> UR.init

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Requested to complete objective, but isn't authorized"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.ObjectiveEditor", function = "update" }
                            []

        DeniedCompleteObjective ->
            case model.status of
                Authorized (EditingObjective objective form RequestingConfirmation) ->
                    { model | status = Authorized (EditingObjective objective form Editing) }
                        |> UR.init

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Denied to complete objective, but isn't authorized"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.ObjectiveEditor", function = "update" }
                            []

        AcceptedCompleteObjective ->
            case model.status of
                Authorized (EditingObjective objective form RequestingConfirmation) ->
                    let
                        completionStatus =
                            { completed = List.filter .isCompleted objective.actions
                            , left =
                                List.filter (not << .isCompleted) objective.actions
                                    |> List.map (\action -> { tries = 0, action = action })
                            }
                    in
                    { model
                        | status =
                            completionStatus
                                |> CompletingActions
                                |> EditingObjective objective form
                                |> Authorized
                    }
                        |> UR.init
                        |> completeActionOrObjective loggedIn model msg completionStatus objective

                _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Accepted to complete objective, but isn't authorized"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.ObjectiveEditor", function = "update" }
                            []

        GotCompleteActionResponse (Ok _) ->
            case model.status of
                Authorized (EditingObjective objective form (CompletingActions completionStatus)) ->
                    case completionStatus.left of
                        [] ->
                            model
                                |> UR.init
                                |> UR.logImpossible msg
                                    "Finished completing an action successfully, but there were none left"
                                    (Just loggedIn.accountName)
                                    { moduleName = "Page.Community.ObjectiveEditor", function = "update" }
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
                                        |> EditingObjective objective form
                                        |> Authorized
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
                            { moduleName = "Page.Community.ObjectiveEditor", function = "update" }
                            []

        GotCompleteActionResponse (Err _) ->
            case model.status of
                Authorized (EditingObjective objective form (CompletingActions completionStatus)) ->
                    case completionStatus.left of
                        [] ->
                            model
                                |> UR.init
                                |> UR.logImpossible msg
                                    "Finished completing an action with an error, but there were none left"
                                    (Just loggedIn.accountName)
                                    { moduleName = "Page.Community.ObjectiveEditor", function = "update" }
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
                                            |> EditingObjective objective form
                                            |> Authorized
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
                                        , location = { moduleName = "Page.Community.ObjectiveEditor", function = "update" }
                                        , contexts =
                                            [ { name = "Details"
                                              , extras =
                                                    Dict.fromList
                                                        [ ( "actionId", Encode.int action.id )
                                                        , ( "objectiveId", Encode.int objective.id )
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
                                            |> EditingObjective objective form
                                            |> Authorized
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
                                                , ( "actionId", Encode.int action.id )
                                                ]
                                        , level = Log.Warning
                                        }

                -- =======
                -- >>>>>>> master
                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Completed an action without being authorized"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.ObjectiveEditor", function = "update" }
                            []

        GotCompleteObjectiveResponse (RemoteData.Success _) ->
            UR.init model
                |> UR.addCmd (Route.pushUrl loggedIn.shared.navKey Route.Objectives)
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Success (t "community.objectives.editor.completed_success"))

        GotCompleteObjectiveResponse (RemoteData.Failure err) ->
            UR.init model
                |> UR.addExt (ShowFeedback Feedback.Failure (t "community.objectives.editor.error_marking_as_complete"))
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when completing objective"
                    { moduleName = "Page.Community.ObjectiveEditor", function = "update" }
                    []
                    err

        GotCompleteObjectiveResponse _ ->
            UR.init model

        ClickedSaveObjective ->
            case ( loggedIn.selectedCommunity, model.status ) of
                ( RemoteData.Success community, Authorized (CreatingObjective objForm _) ) ->
                    { model | status = Authorized (CreatingObjective objForm SavingCreation) }
                        |> UR.init
                        |> UR.addPort
                            { responseAddress = ClickedSaveObjective
                            , responseData = Encode.null
                            , data =
                                Eos.encodeTransaction
                                    [ { accountName = loggedIn.shared.contracts.community
                                      , name = "newobjective"
                                      , authorization =
                                            { actor = loggedIn.accountName
                                            , permissionName = Eos.samplePermission
                                            }
                                      , data =
                                            { asset = Eos.Asset 0 community.symbol
                                            , description = objForm.description.contents
                                            , creator = loggedIn.accountName
                                            }
                                                |> Community.encodeCreateObjectiveAction
                                      }
                                    ]
                            }
                        |> LoggedIn.withAuthentication loggedIn
                            model
                            { successMsg = msg, errorMsg = ClosedAuthModal }

                ( RemoteData.Success _, Authorized (EditingObjective objective objForm _) ) ->
                    { model | status = Authorized (EditingObjective objective objForm SavingEdit) }
                        |> UR.init
                        |> UR.addPort
                            { responseAddress = ClickedSaveObjective
                            , responseData = Encode.null
                            , data =
                                Eos.encodeTransaction
                                    [ { accountName = loggedIn.shared.contracts.community
                                      , name = "updobjective"
                                      , authorization =
                                            { actor = loggedIn.accountName
                                            , permissionName = Eos.samplePermission
                                            }
                                      , data =
                                            { objectiveId = objective.id
                                            , description = objForm.description.contents
                                            , editor = loggedIn.accountName
                                            }
                                                |> Community.encodeUpdateObjectiveAction
                                      }
                                    ]
                            }
                        |> LoggedIn.withAuthentication loggedIn
                            model
                            { successMsg = msg, errorMsg = ClosedAuthModal }

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Tried saving objective without having the community loaded or without being authorized"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.ObjectiveEditor", function = "update" }
                            []

        GotSaveObjectiveResponse (Ok _) ->
            UR.init model
                |> UR.addExt (ShowFeedback Feedback.Success (t "community.objectives.create_success"))
                -- TODO - This only works sometimes
                |> UR.addExt (LoggedIn.ReloadResource LoggedIn.CommunityResource)
                |> UR.addCmd (Route.replaceUrl loggedIn.shared.navKey Route.Community)

        GotSaveObjectiveResponse (Err v) ->
            let
                newModel =
                    case model.status of
                        Authorized (CreatingObjective form SavingCreation) ->
                            { model
                                | status =
                                    Creating
                                        |> CreatingObjective form
                                        |> Authorized
                            }
                                |> UR.init
                                |> UR.logJsonValue msg
                                    (Just loggedIn.accountName)
                                    "Got an error when creating an objective"
                                    { moduleName = "Page.Community.ObjectiveEditor", function = "update" }
                                    []
                                    v

                        Authorized (EditingObjective objective form SavingEdit) ->
                            { model
                                | status =
                                    Editing
                                        |> EditingObjective objective form
                                        |> Authorized
                            }
                                |> UR.init
                                |> UR.logJsonValue msg
                                    (Just loggedIn.accountName)
                                    "Got an error when updating an objective"
                                    { moduleName = "Page.Community.ObjectiveEditor", function = "update" }
                                    [ { name = "Objective"
                                      , extras = Dict.fromList [ ( "ID", Encode.int objective.id ) ]
                                      }
                                    ]
                                    v

                        _ ->
                            model
                                |> UR.init
                                |> UR.logImpossible msg
                                    "Saved objective without being authorized"
                                    (Just loggedIn.accountName)
                                    { moduleName = "Page.Community.ObjectiveEditor", function = "update" }
                                    []
            in
            newModel
                |> UR.addExt (ShowFeedback Feedback.Failure (t "errors.unknown"))



-- UTILS


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
            Api.Graphql.mutation
                loggedIn.shared
                (Just loggedIn.authToken)
                (completeObjectiveSelectionSet objective.id)
                GotCompleteObjectiveResponse
                |> UR.addCmd

        Just { action } ->
            UR.addPort
                ({ action | isCompleted = True }
                    |> Action.updateAction loggedIn.accountName loggedIn.shared
                    |> (\completedAction ->
                            { responseAddress = AcceptedCompleteObjective
                            , responseData = Encode.int action.id
                            , data = Eos.encodeTransaction [ completedAction ]
                            }
                       )
                )
                >> LoggedIn.withAuthentication loggedIn
                    model
                    { successMsg = msg, errorMsg = ClosedAuthModal }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.status of
        Authorized (CreatingObjective form _) ->
            MarkdownEditor.subscriptions form.description
                |> Sub.map GotDescriptionEditorMsg

        Authorized (EditingObjective _ form _) ->
            MarkdownEditor.subscriptions form.description
                |> Sub.map GotDescriptionEditorMsg

        _ ->
            Sub.none


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityLoaded community ->
            Just (CompletedLoadCommunity community)

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

        ClosedAuthModal ->
            [ "ClosedAuthModal" ]

        GotDescriptionEditorMsg subMsg ->
            "GotDescriptionEditorMsg" :: MarkdownEditor.msgToString subMsg

        ClickedSaveObjective ->
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
