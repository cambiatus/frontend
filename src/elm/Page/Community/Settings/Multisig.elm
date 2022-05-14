module Page.Community.Settings.Multisig exposing (Model, Msg, init, jsAddressToMsg, msgToString, receiveBroadcast, update, view)

import Api.Eos
import Community
import Date
import Eos.Permission
import Form
import Form.DatePicker
import Form.Radio
import Form.Text
import Form.UserPicker
import Form.Validate
import Html exposing (Html, button, div, h1, h2, p, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Http
import Iso8601
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode as Encode
import List.Extra
import Log
import Page
import Profile
import RemoteData exposing (RemoteData)
import Session.LoggedIn as LoggedIn
import Time
import UpdateResult as UR
import Utils
import View.Components
import View.Feedback as Feedback



-- MODEL


type alias Model =
    { proposals : RemoteData ProposalsError (List Proposal)
    , approvals : RemoteData Http.Error (List Approval)
    , changePermissionsInput : Form.Model ChangePermissionsInput
    , proposeObjectiveInput : Form.Model ProposeObjectiveInput
    , searchProposalInput : Form.Model SearchProposalInput
    , proposalPermission : Eos.Permission.PermissionType
    , proposalVoters :
        List
            { profile : Profile.Minimal
            , permission : Eos.Permission.PermissionType
            }
    , selectedSearch : Maybe Profile.Minimal
    }


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( { proposals = RemoteData.NotAsked
      , approvals = RemoteData.NotAsked
      , changePermissionsInput =
            Form.init
                { threshold = "1"
                , targetPermission = Eos.Permission.default
                , voters =
                    Form.UserPicker.initMultiple
                        { id = "permissions-voters-picker"
                        , selectedProfiles = []
                        }
                }
      , proposeObjectiveInput = initProposeObjectiveInput loggedIn
      , searchProposalInput = Form.init { search = Form.UserPicker.initSingle { id = "search-proposal-picker" } }
      , proposalPermission = Eos.Permission.default
      , proposalVoters = []
      , selectedSearch = Nothing
      }
    , LoggedIn.maybeInitWith CompletedLoadCommunityCreatorPermissions .communityCreatorPermissions loggedIn
    )


initProposeObjectiveInput : LoggedIn.Model -> Form.Model ProposeObjectiveInput
initProposeObjectiveInput loggedIn =
    let
        nextWeek =
            Date.fromPosix loggedIn.shared.timezone loggedIn.shared.now
                |> Date.add Date.Days 7
    in
    Form.init
        { name = ""
        , permissionToUse = Eos.Permission.default
        , voters =
            Form.UserPicker.initMultiple
                { id = "propose-objective-voters-picker"
                , selectedProfiles = []
                }
        , expiration = Form.DatePicker.initModel nextWeek
        }



-- TYPES


type Msg
    = NoOp
    | GotPermissionsFormMsg (Form.Msg ChangePermissionsInput)
    | SubmittedPermissionsForm ChangePermissionsOutput
    | CompletedChangeAccountPermissions (Result Json.Decode.Error ())
    | GotProposeObjectiveFormMsg (Form.Msg ProposeObjectiveInput)
    | SubmittedProposeObjectiveForm ProposeObjectiveOutput
    | GotSearchProposalFormMsg (Form.Msg SearchProposalInput)
    | CompletedLoadProposals (Result Http.Error (List ProposalRow))
    | CompletedLoadApprovals (Result Http.Error (List Approval))
    | DeserializedProposals (Result Json.Decode.Error (List Proposal))
    | ClickedApproveProposal Proposal
    | ClickedUnapproveProposal Proposal
    | ClickedExecuteProposal Proposal
    | ClickedCancelProposal Proposal
    | CompletedApprovingProposal (Result Json.Decode.Error ())
    | CompletedUnapprovingProposal (Result Json.Decode.Error ())
    | CompletedExecutingProposal (Result Json.Decode.Error ())
    | CompletedCancellingProposal (Result Json.Decode.Error ())
    | CompletedProposingNewObjective (Result Json.Decode.Error ())
    | CompletedLoadCommunityCreatorPermissions Eos.Permission.Permissions


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


type ProposalsError
    = HttpError
    | DecodeError


type alias Proposal =
    { name : String
    , expiration : Time.Posix
    , actions : List Action
    }


type alias Action =
    { account : String
    , name : String
    , description : String
    }



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        NoOp ->
            UR.init model

        GotPermissionsFormMsg subMsg ->
            Form.update loggedIn.shared subMsg model.changePermissionsInput
                |> UR.fromChild (\input -> { model | changePermissionsInput = input })
                    GotPermissionsFormMsg
                    (LoggedIn.executeFeedback >> UR.addExt)
                    model

        SubmittedPermissionsForm output ->
            model
                |> UR.init
                |> UR.addPort
                    (Api.Eos.UpdateAuth
                        { targetAccount = loggedIn.accountName
                        , targetPermission = output.targetPermission
                        , threshold = output.threshold
                        , accounts =
                            List.map
                                (\voter ->
                                    { account = voter.account

                                    -- TODO - Choose permission and weight
                                    , permission = Eos.Permission.Active
                                    , weight = 1
                                    }
                                )
                                output.voters
                        }
                        |> Api.Eos.EosAction
                        |> Api.Eos.transact loggedIn.shared
                            { actor = loggedIn.accountName, permission = Eos.Permission.Owner }
                            msg
                    )
                |> LoggedIn.withPrivateKey loggedIn
                    []
                    model
                    { successMsg = msg
                    , errorMsg = NoOp
                    }

        CompletedChangeAccountPermissions (Ok ()) ->
            model
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Success "Account permissions changed")

        CompletedChangeAccountPermissions (Err err) ->
            model
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure "Something went wrong when changing account permissions")
                |> UR.logDecodingError msg
                    (Just loggedIn.accountName)
                    "Got an error when changing account permissions"
                    { moduleName = "Page.Community.Settings.Multisig", function = "update" }
                    [ Log.contextFromCommunity loggedIn.selectedCommunity ]
                    err

        GotProposeObjectiveFormMsg subMsg ->
            Form.update loggedIn.shared subMsg model.proposeObjectiveInput
                |> UR.fromChild (\input -> { model | proposeObjectiveInput = input })
                    GotProposeObjectiveFormMsg
                    (LoggedIn.executeFeedback >> UR.addExt)
                    model

        SubmittedProposeObjectiveForm output ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    let
                        proposedActions =
                            [ ( Api.Eos.CreateObjective
                                    { communitySymbol = community.symbol
                                    , objectiveDescription = output.name
                                    , communityAdmin = community.creator
                                    }
                                    |> Api.Eos.CommunityAction
                              , { actor = community.creator, permission = Eos.Permission.Active }
                              )
                            ]

                        voterAccounts =
                            List.map .account output.voters
                    in
                    model
                        |> UR.init
                        |> UR.addPort
                            (Api.Eos.Propose
                                { proposer = loggedIn.accountName
                                , proposalName = output.name
                                , requestedVotes =
                                    model.proposalVoters
                                        |> List.filterMap
                                            (\{ profile, permission } ->
                                                if List.member profile.account voterAccounts then
                                                    Just
                                                        { actor = profile.account
                                                        , permission = permission
                                                        }

                                                else
                                                    Nothing
                                            )
                                , expiration =
                                    Utils.posixFromDate loggedIn.shared.timezone
                                        output.expiration
                                , actions = proposedActions
                                }
                                |> Api.Eos.MultiSigAction
                                |> Api.Eos.transact loggedIn.shared
                                    { actor = loggedIn.accountName
                                    , permission = Eos.Permission.Active
                                    }
                                    msg
                            )
                        |> LoggedIn.withPrivateKey loggedIn
                            []
                            model
                            { successMsg = msg
                            , errorMsg = NoOp
                            }

                _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Tried proposing new objective, but community wasn't loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.Multisig", function = "update" }
                            [ Log.contextFromCommunity loggedIn.selectedCommunity ]

        GotSearchProposalFormMsg subMsg ->
            let
                updatedForm =
                    Form.update loggedIn.shared subMsg model.searchProposalInput

                maybeNewSelectedProfile =
                    Form.getValue .search updatedForm.model
                        |> Form.UserPicker.getSingleProfile

                oldSelectedProfile =
                    Form.getValue .search model.searchProposalInput
                        |> Form.UserPicker.getSingleProfile

                ( newModel, queryCmd ) =
                    case maybeNewSelectedProfile of
                        Nothing ->
                            ( { model | selectedSearch = Nothing }, Cmd.none )

                        Just selectedProfile ->
                            if Just selectedProfile == oldSelectedProfile then
                                ( model, Cmd.none )

                            else
                                ( { model
                                    | selectedSearch = Just selectedProfile
                                    , proposals = RemoteData.Loading
                                    , approvals = RemoteData.Loading
                                  }
                                , Cmd.batch
                                    [ Api.Eos.Proposal selectedProfile.account
                                        |> Api.Eos.MultiSig
                                        |> Api.Eos.query loggedIn.shared
                                            CompletedLoadProposals
                                            proposalRowDecoder
                                    , Api.Eos.Approvals2 selectedProfile.account
                                        |> Api.Eos.MultiSig
                                        |> Api.Eos.query loggedIn.shared
                                            CompletedLoadApprovals
                                            approvalDecoder
                                    ]
                                )
            in
            updatedForm
                |> UR.fromChild (\input -> { model | searchProposalInput = input })
                    GotSearchProposalFormMsg
                    (LoggedIn.executeFeedback >> UR.addExt)
                    newModel
                |> UR.addCmd queryCmd

        CompletedLoadProposals (Ok proposals) ->
            model
                |> UR.init
                |> UR.addPort
                    { responseAddress = msg
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "deserializeProposals" )
                            , ( "proposals", Encode.list proposalRowEncoder proposals )
                            ]
                    }

        CompletedLoadProposals (Err err) ->
            { model | proposals = RemoteData.Failure HttpError }
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure "Got an error when loading proposals")
                |> UR.logHttpError msg
                    (Just loggedIn.accountName)
                    "Got an error when loading proposals"
                    { moduleName = "Page.Community.Settings.Multisig", function = "update" }
                    []
                    err

        DeserializedProposals (Ok proposals) ->
            { model | proposals = RemoteData.Success proposals }
                |> UR.init

        DeserializedProposals (Err err) ->
            { model | proposals = RemoteData.Failure DecodeError }
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure "Got an error when deserializing proposals")
                |> UR.logDecodingError msg
                    (Just loggedIn.accountName)
                    "Got an error when deserializing proposals"
                    { moduleName = "Page.Community.Settings.Multisig", function = "update" }
                    []
                    err

        CompletedLoadApprovals (Ok approvals) ->
            { model | approvals = RemoteData.Success approvals }
                |> UR.init

        CompletedLoadApprovals (Err err) ->
            { model | approvals = RemoteData.Failure err }
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure "Got an error when loading approvals2 data")
                |> UR.logHttpError msg
                    (Just loggedIn.accountName)
                    "Got an error when loading approvals2 data"
                    { moduleName = "Page.Community.Settings.Multisig", function = "update" }
                    []
                    err

        ClickedApproveProposal proposal ->
            case model.selectedSearch of
                Just proposer ->
                    UR.init model
                        |> UR.addPort
                            (Api.Eos.Approve
                                { proposer = proposer.account
                                , proposalName = proposal.name
                                }
                                |> Api.Eos.MultiSigAction
                                |> Api.Eos.transact loggedIn.shared
                                    { actor = loggedIn.accountName, permission = Eos.Permission.Active }
                                    msg
                            )
                        |> LoggedIn.withPrivateKey loggedIn
                            []
                            model
                            { successMsg = msg, errorMsg = NoOp }

                Nothing ->
                    UR.init model

        ClickedUnapproveProposal proposal ->
            case model.selectedSearch of
                Just proposer ->
                    UR.init model
                        |> UR.addPort
                            (Api.Eos.Unapprove
                                { proposer = proposer.account
                                , proposalName = proposal.name
                                }
                                |> Api.Eos.MultiSigAction
                                |> Api.Eos.transact loggedIn.shared
                                    { actor = loggedIn.accountName, permission = Eos.Permission.Active }
                                    msg
                            )
                        |> LoggedIn.withPrivateKey loggedIn
                            []
                            model
                            { successMsg = msg, errorMsg = NoOp }

                Nothing ->
                    UR.init model

        ClickedExecuteProposal proposal ->
            case model.selectedSearch of
                Just proposer ->
                    UR.init model
                        |> UR.addPort
                            (Api.Eos.Execute
                                { proposer = proposer.account
                                , proposalName = proposal.name
                                }
                                |> Api.Eos.MultiSigAction
                                |> Api.Eos.transact loggedIn.shared
                                    { actor = loggedIn.accountName, permission = Eos.Permission.Active }
                                    msg
                            )
                        |> LoggedIn.withPrivateKey loggedIn
                            []
                            model
                            { successMsg = msg, errorMsg = NoOp }

                Nothing ->
                    UR.init model

        ClickedCancelProposal proposal ->
            case model.selectedSearch of
                Just proposer ->
                    UR.init model
                        |> UR.addPort
                            (Api.Eos.Cancel
                                { proposer = proposer.account
                                , proposalName = proposal.name
                                }
                                |> Api.Eos.MultiSigAction
                                |> Api.Eos.transact loggedIn.shared
                                    { actor = loggedIn.accountName, permission = Eos.Permission.Active }
                                    msg
                            )
                        |> LoggedIn.withPrivateKey loggedIn
                            []
                            model
                            { successMsg = msg, errorMsg = NoOp }

                Nothing ->
                    UR.init model

        CompletedApprovingProposal (Ok ()) ->
            case model.selectedSearch of
                Just profile ->
                    { model | approvals = RemoteData.Loading }
                        |> UR.init
                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Success "Marked as approved")
                        |> UR.addCmd
                            (Api.Eos.Approvals2 profile.account
                                |> Api.Eos.MultiSig
                                |> Api.Eos.query loggedIn.shared
                                    CompletedLoadApprovals
                                    approvalDecoder
                            )

                Nothing ->
                    model |> UR.init

        CompletedApprovingProposal (Err err) ->
            model
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure "Error when approving proposal")
                |> UR.logDecodingError msg
                    (Just loggedIn.accountName)
                    "Got an error when approving proposal"
                    { moduleName = "Page.Community.Settings.Multisig", function = "update" }
                    []
                    err

        CompletedUnapprovingProposal (Ok ()) ->
            case model.selectedSearch of
                Just profile ->
                    { model | approvals = RemoteData.Loading }
                        |> UR.init
                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Success "Removed approval vote")
                        |> UR.addCmd
                            (Api.Eos.Approvals2 profile.account
                                |> Api.Eos.MultiSig
                                |> Api.Eos.query loggedIn.shared
                                    CompletedLoadApprovals
                                    approvalDecoder
                            )

                Nothing ->
                    UR.init model

        CompletedUnapprovingProposal (Err err) ->
            model
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure "Error when unapproving proposal")
                |> UR.logDecodingError msg
                    (Just loggedIn.accountName)
                    "Got an error when unapproving proposal"
                    { moduleName = "Page.Community.Settings.Multisig", function = "update" }
                    []
                    err

        CompletedExecutingProposal (Ok ()) ->
            case model.selectedSearch of
                Just profile ->
                    model
                        |> UR.init
                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Success "Executed transaction")
                        |> UR.addCmd
                            (Api.Eos.Approvals2 profile.account
                                |> Api.Eos.MultiSig
                                |> Api.Eos.query loggedIn.shared
                                    CompletedLoadApprovals
                                    approvalDecoder
                            )

                _ ->
                    UR.init model

        CompletedExecutingProposal (Err err) ->
            model
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure "Error when executing proposal")
                |> UR.logDecodingError msg
                    (Just loggedIn.accountName)
                    "Got an error when executing proposal"
                    { moduleName = "Page.Community.Settings.Multisig", function = "update" }
                    []
                    err

        CompletedCancellingProposal (Ok ()) ->
            case model.selectedSearch of
                Just profile ->
                    model
                        |> UR.init
                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Success "Cancelled proposal")
                        |> UR.addCmd
                            (Api.Eos.Approvals2 profile.account
                                |> Api.Eos.MultiSig
                                |> Api.Eos.query loggedIn.shared
                                    CompletedLoadApprovals
                                    approvalDecoder
                            )

                _ ->
                    UR.init model

        CompletedCancellingProposal (Err err) ->
            model
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure "Error when cancelling proposal")
                |> UR.logDecodingError msg
                    (Just loggedIn.accountName)
                    "Got an error when cancelling proposal"
                    { moduleName = "Page.Community.Settings.Multisig", function = "update" }
                    []
                    err

        CompletedProposingNewObjective (Ok ()) ->
            { model | proposeObjectiveInput = initProposeObjectiveInput loggedIn }
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Success "Objective created")

        CompletedProposingNewObjective (Err err) ->
            model
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure "Something went wrong when proposing new objective")
                |> UR.logDecodingError msg
                    (Just loggedIn.accountName)
                    "Got an error when proposing a new objective"
                    { moduleName = "Page.Community.Settings.Multisig", function = "update" }
                    [ Log.contextFromCommunity loggedIn.selectedCommunity ]
                    err

        CompletedLoadCommunityCreatorPermissions permissions ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    let
                        permission =
                            case model.proposalPermission of
                                Eos.Permission.Active ->
                                    permissions.active

                                Eos.Permission.Owner ->
                                    permissions.owner

                                Eos.Permission.RootPermission ->
                                    permissions.owner
                    in
                    { model | proposalVoters = List.filterMap (makeVoter permission) community.members }
                        |> UR.init

                _ ->
                    UR.init model



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        title =
            "Multisig"

        content =
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    div []
                        [ Page.viewHeader loggedIn title
                        , view_ loggedIn community model
                        ]

                RemoteData.Loading ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.NotAsked ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.Failure err ->
                    Page.fullPageGraphQLError title err
    in
    { title = title, content = content }


view_ : LoggedIn.Model -> Community.Model -> Model -> Html Msg
view_ loggedIn community model =
    div [ class "container mx-auto" ]
        [ div [ class "px-4" ]
            [ viewChangePermissions loggedIn community model
            , viewProposeObjective loggedIn model
            , viewProposalCard loggedIn community model
            ]
        ]


type alias ChangePermissionsInput =
    { threshold : String
    , targetPermission : Eos.Permission.PermissionType
    , voters : Form.UserPicker.MultiplePickerModel
    }


type alias ChangePermissionsOutput =
    { threshold : Int
    , targetPermission : Eos.Permission.PermissionType
    , voters : List Profile.Minimal
    }


changePermissionsForm : LoggedIn.Model -> Community.Model -> Form.Form msg ChangePermissionsInput ChangePermissionsOutput
changePermissionsForm loggedIn community =
    let
        translators =
            loggedIn.shared.translators
    in
    Form.succeed ChangePermissionsOutput
        |> Form.with
            (Form.Text.init
                { label = "Threshold"
                , id = "threshold-input"
                }
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.int
                            >> Form.Validate.intGreaterThan 0
                            >> Form.Validate.validate translators
                    , value = .threshold
                    , update = \threshold values -> { values | threshold = threshold }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Radio.init
                { label = "Target permission"
                , id = "permission-radio"
                , optionToString = Eos.Permission.toString
                }
                |> Form.Radio.withOption Eos.Permission.Owner (text "Owner")
                |> Form.Radio.withOption Eos.Permission.Active (text "Active")
                |> Form.radio Eos.Permission.fromString
                    { parser = Ok
                    , value = .targetPermission
                    , update = \targetPermission values -> { values | targetPermission = targetPermission }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.UserPicker.init
                { label = "Voters"
                , currentUser = loggedIn.accountName
                , profiles = community.members
                }
                |> Form.UserPicker.withContainerAttrs [ class "mt-10" ]
                |> Form.userPickerMultiple
                    { parser = Ok
                    , value = .voters
                    , update = \voters values -> { values | voters = voters }
                    , externalError = always Nothing
                    }
            )


viewChangePermissions : LoggedIn.Model -> Community.Model -> Model -> Html Msg
viewChangePermissions ({ shared } as loggedIn) community model =
    div [ class "bg-white rounded-sm shadow p-4 my-10" ]
        [ p [ class "font-bold mb-2" ]
            [ text "This will change the permission on the account you're currently logged in to. Since this is still a WIP, make sure you're using a test account, otherwise you may need to manually submit a transaction to the blockchain to revert your actions." ]
        , p [ class "mb-4" ] [ text "Here you can set the threshold of a specific permission. You also choose the accounts that will be associated with that permission" ]
        , Form.view []
            shared.translators
            (\submitButton ->
                [ submitButton [ class "button button-primary w-full" ]
                    [ text "Change account permissions" ]
                ]
            )
            (changePermissionsForm loggedIn community)
            model.changePermissionsInput
            { toMsg = GotPermissionsFormMsg
            , onSubmit = SubmittedPermissionsForm
            }
        ]


type alias ProposeObjectiveInput =
    { name : String
    , permissionToUse : Eos.Permission.PermissionType
    , voters : Form.UserPicker.MultiplePickerModel
    , expiration : Form.DatePicker.Model
    }


type alias ProposeObjectiveOutput =
    { name : String
    , permissionToUse : Eos.Permission.PermissionType
    , voters : List Profile.Minimal
    , expiration : Date.Date
    }


proposeObjectiveForm : LoggedIn.Model -> List Profile.Minimal -> Form.Form msg ProposeObjectiveInput ProposeObjectiveOutput
proposeObjectiveForm loggedIn possibleVoters =
    let
        translators =
            loggedIn.shared.translators
    in
    Form.succeed ProposeObjectiveOutput
        |> Form.with
            (Form.Text.init
                { label = "New objective name"
                , id = "new-objective-name-input"
                }
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.stringLengthExactly 12
                            >> Form.Validate.validate translators
                    , value = .name
                    , update = \name values -> { values | name = name }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Radio.init
                { label = "Permission to use"
                , id = "new-objective-permission-radio"
                , optionToString = Eos.Permission.toString
                }
                |> Form.Radio.withOption Eos.Permission.Owner (text "Owner")
                |> Form.Radio.withOption Eos.Permission.Active (text "Active")
                |> Form.radio Eos.Permission.fromString
                    { parser = Ok
                    , value = .permissionToUse
                    , update = \permissionToUse values -> { values | permissionToUse = permissionToUse }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.UserPicker.init
                { label = "People who can vote"
                , currentUser = loggedIn.accountName
                , profiles = possibleVoters
                }
                |> Form.userPickerMultiple
                    { parser = Ok
                    , value = .voters
                    , update = \voters values -> { values | voters = voters }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.DatePicker.init
                { label = "Expiration date"
                , id = "new-objective-expiration-input"
                }
                |> Form.DatePicker.withContainerAttrs [ class "mt-10" ]
                |> Form.datePicker
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.required
                            >> Form.Validate.validate translators
                    , value = .expiration
                    , update = \expiration values -> { values | expiration = expiration }
                    , externalError = always Nothing
                    }
            )


viewProposeObjective : LoggedIn.Model -> Model -> Html Msg
viewProposeObjective loggedIn model =
    let
        canPropose =
            List.any (\{ profile } -> profile.account == loggedIn.accountName)
                model.proposalVoters
    in
    div [ class "bg-white rounded-sm shadow p-4 mb-10" ]
        [ p [ class "text-sm mb-4" ]
            [ text "This will create an objective with the name given. We automatically show everyone who can vote (those who are in the owner or active permission of the community admin). "
            , text "We use the same name for the objective and for the proposal, so it needs to be <= 12 characters long. "
            , text "The `Permission to Use` field indicates if the new objective will be created using the active or owner permission of the community creator, and we use it to determine who can vote (and with which permission) on the proposal. "
            , text "You can select and deselect the accounts that may vote on the proposal. "
            , text "You can set the date in which the proposal will expire. It will expire at 23:59:59 of that date."
            ]
        , Form.view []
            loggedIn.shared.translators
            (\submitButton ->
                [ submitButton
                    [ class "button button-primary w-full mt-10"
                    , disabled (not canPropose)
                    ]
                    [ text "Propose new objective" ]
                ]
            )
            (proposeObjectiveForm loggedIn (List.map .profile model.proposalVoters))
            (model.proposeObjectiveInput
                |> Form.withDisabled (not canPropose)
            )
            { toMsg = GotProposeObjectiveFormMsg
            , onSubmit = SubmittedProposeObjectiveForm
            }
        ]


viewProposalVoter :
    LoggedIn.Model
    -> { profile : Profile.Minimal, isChecked : Bool, permission : Eos.Permission.PermissionType }
    -> Html Msg
viewProposalVoter loggedIn { profile, isChecked } =
    -- Checkbox.init
    --     { description =
    --         div [ class "flex flex-col items-center p-2" ]
    --             [ Avatar.view profile.avatar "w-10 h-10 mb-2"
    --             , Profile.viewProfileNameTag loggedIn.shared loggedIn.accountName profile
    --             ]
    --     , id = "proposal-voter-" ++ Eos.Account.nameToString profile.account
    --     , value = isChecked
    --     , disabled = False
    --     , onCheck = CheckedProposalVoter profile.account
    --     }
    --     |> Checkbox.withContainerAttrs [ class "flex flex-col-reverse items-center gap-2 border p-4 rounded-sm" ]
    --     |> Checkbox.toHtml
    text "View proposal voter"


viewProposalCard : LoggedIn.Model -> Community.Model -> Model -> Html Msg
viewProposalCard loggedIn community model =
    div [ class "bg-white rounded-sm shadow p-4 my-10" ]
        [ p [ class "text-sm mb-4" ]
            [ text "Here you can use this select element to search for proposals proposed by some user. "
            , text "Available actions are `Approve`, `Unapprove`, `Execute` and `Cancel`. "
            , text "All of them are straight forward, except for `Unapprove`. It doesn't mean \"Vote No\", it means \"Remove my Yes vote (if there is one)\""
            ]
        , Form.viewWithoutSubmit []
            loggedIn.shared.translators
            (\_ -> [])
            (searchProposalForm loggedIn community)
            model.searchProposalInput
            { toMsg = GotSearchProposalFormMsg
            }
        , div [ class "grid gap-4 grid-cols-2 mt-4" ]
            (case ( model.proposals, model.approvals ) of
                ( RemoteData.Success proposals, RemoteData.Success approvals ) ->
                    List.map2 (viewProposal loggedIn)
                        (List.sortBy .name proposals)
                        (List.sortBy .proposalName approvals)

                ( RemoteData.Failure _, _ ) ->
                    [ text "Error when fetching proposals" ]

                ( _, RemoteData.Failure _ ) ->
                    [ text "Error when fetching approvals" ]

                ( RemoteData.Loading, RemoteData.Loading ) ->
                    [ text "Loading" ]

                ( RemoteData.Loading, _ ) ->
                    [ text "Loading proposals" ]

                ( _, RemoteData.Loading ) ->
                    [ text "Loading approvals" ]

                _ ->
                    []
            )
        ]


type alias SearchProposalInput =
    { search : Form.UserPicker.SinglePickerModel
    }


type alias SearchProposalOutput =
    { result : Profile.Minimal
    }


searchProposalForm : LoggedIn.Model -> Community.Model -> Form.Form msg SearchProposalInput SearchProposalOutput
searchProposalForm loggedIn community =
    Form.succeed SearchProposalOutput
        |> Form.with
            (Form.UserPicker.init
                { label = "Search proposals by user"
                , currentUser = loggedIn.accountName
                , profiles = community.members
                }
                |> Form.userPicker
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.required
                            >> Form.Validate.validate loggedIn.shared.translators
                    , value = .search
                    , update = \search values -> { values | search = search }
                    , externalError = always Nothing
                    }
            )


viewProposal : LoggedIn.Model -> Proposal -> Approval -> Html Msg
viewProposal loggedIn proposal approval =
    let
        communityCreatorInfo =
            case loggedIn.communityCreatorPermissions of
                RemoteData.Success permissions ->
                    Just
                        -- TODO - Use proposal's authorization's permission
                        ( permissions.active.accounts
                            |> List.filterMap
                                (\account ->
                                    if
                                        List.member account.authorization
                                            approval.providedApprovals
                                    then
                                        Just account.weight

                                    else
                                        Nothing
                                )
                            |> List.sum
                        , permissions.active.threshold
                        )

                _ ->
                    Nothing

        isExpired =
            Time.posixToMillis loggedIn.shared.now > Time.posixToMillis proposal.expiration
    in
    div [ class "bg-white rounded border p-4" ]
        [ h1 [ class "font-bold" ] [ text proposal.name ]
        , View.Components.dateViewer [ class "text-sm text-gray-900" ]
            identity
            loggedIn.shared
            proposal.expiration
        , div [ class "my-4" ] (List.map viewAction proposal.actions)
        , p [ class "mb-4" ]
            [ text "Approvals: "
            , case communityCreatorInfo of
                Just ( votes, _ ) ->
                    text (String.fromInt votes)

                Nothing ->
                    text "Loading"
            ]
        , div [ class "flex justify-between gap-4" ]
            [ if isExpired then
                button
                    [ class "w-full button-danger"
                    , onClick (ClickedCancelProposal proposal)
                    ]
                    [ text "Cancel" ]

              else if
                List.any (\requested -> requested.actor == loggedIn.accountName)
                    approval.requestedApprovals
              then
                button
                    [ class "button w-full button-primary"
                    , onClick (ClickedApproveProposal proposal)
                    ]
                    [ text "Approve" ]

              else if
                List.any (\provided -> provided.actor == loggedIn.accountName)
                    approval.providedApprovals
              then
                button
                    [ class "button w-full button-danger"
                    , onClick (ClickedUnapproveProposal proposal)
                    ]
                    [ text "Unapprove" ]

              else
                text ""
            , case communityCreatorInfo of
                Just ( votes, threshold ) ->
                    if votes >= threshold && not isExpired then
                        button
                            [ class "button w-full button-secondary"
                            , onClick (ClickedExecuteProposal proposal)
                            ]
                            [ text "Execute" ]

                    else
                        text ""

                Nothing ->
                    text ""
            ]
        ]


viewAction : Action -> Html Msg
viewAction action =
    div []
        [ div [ class "flex justify-between" ]
            [ h2 [ class "font-bold" ] [ text "Account" ]
            , p [] [ text action.account ]
            ]
        , div [ class "flex justify-between" ]
            [ h2 [ class "font-bold" ] [ text "Action" ]
            , p [] [ text action.name ]
            ]
        , div [ class "flex justify-between" ]
            [ h2 [ class "font-bold" ] [ text "Proposed description" ]
            , p [] [ text action.description ]
            ]
        ]



-- JSON


type alias ProposalRow =
    { proposalName : String, serializedTransaction : String }


type alias Approval =
    { proposalName : String
    , requestedApprovals : List Eos.Permission.Authorization
    , providedApprovals : List Eos.Permission.Authorization
    }


approvalDecoder : Json.Decode.Decoder Approval
approvalDecoder =
    Json.Decode.succeed Approval
        |> Json.Decode.Pipeline.required "proposal_name" Json.Decode.string
        |> Json.Decode.Pipeline.required "requested_approvals"
            (Json.Decode.list
                (Json.Decode.field "level" Eos.Permission.authorizationDecoder)
            )
        |> Json.Decode.Pipeline.required "provided_approvals"
            (Json.Decode.list
                (Json.Decode.field "level" Eos.Permission.authorizationDecoder)
            )


proposalRowDecoder : Json.Decode.Decoder ProposalRow
proposalRowDecoder =
    Json.Decode.succeed ProposalRow
        |> Json.Decode.Pipeline.required "proposal_name" Json.Decode.string
        |> Json.Decode.Pipeline.required "packed_transaction" Json.Decode.string


proposalRowEncoder : ProposalRow -> Encode.Value
proposalRowEncoder proposalRow =
    Encode.object
        [ ( "proposalName", Encode.string proposalRow.proposalName )
        , ( "serializedTransaction", Encode.string proposalRow.serializedTransaction )
        ]


proposalDecoder : Json.Decode.Decoder Proposal
proposalDecoder =
    Json.Decode.succeed Proposal
        |> Json.Decode.Pipeline.required "proposalName" Json.Decode.string
        |> Json.Decode.Pipeline.requiredAt [ "actions", "expiration" ] Iso8601.decoder
        |> Json.Decode.Pipeline.requiredAt [ "actions", "actions" ] (Json.Decode.list actionDecoder)


actionDecoder : Json.Decode.Decoder Action
actionDecoder =
    Json.Decode.succeed Action
        |> Json.Decode.Pipeline.required "account" Json.Decode.string
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
        |> Json.Decode.Pipeline.requiredAt [ "data", "description" ] Json.Decode.string



-- UTILS


makeVoter :
    Eos.Permission.Permission
    -> Profile.Minimal
    -> Maybe { profile : Profile.Minimal, permission : Eos.Permission.PermissionType }
makeVoter permission member =
    List.Extra.find
        (\{ authorization } -> authorization.actor == member.account)
        permission.accounts
        |> Maybe.map
            (\{ authorization } ->
                { profile = member
                , permission = authorization.permission
                }
            )


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityCreatorPermissionsLoaded permissions ->
            Just (CompletedLoadCommunityCreatorPermissions permissions)

        _ ->
            Nothing


jsAddressToMsg : List String -> Encode.Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "SubmittedPermissionsForm" :: _ ->
            val
                |> Json.Decode.decodeValue (Json.Decode.field "transactionId" Json.Decode.string)
                |> Result.map (\_ -> ())
                |> CompletedChangeAccountPermissions
                |> Just

        "CompletedLoadProposals" :: _ ->
            Json.Decode.decodeValue
                (Json.Decode.field "deserializedProposals"
                    (Json.Decode.list proposalDecoder)
                )
                val
                |> DeserializedProposals
                |> Just

        [ "SubmittedProposeObjectiveForm" ] ->
            val
                |> Json.Decode.decodeValue (Json.Decode.field "transactionId" Json.Decode.string)
                |> Result.map (\_ -> ())
                |> CompletedProposingNewObjective
                |> Just

        [ "ClickedApproveProposal" ] ->
            val
                |> Json.Decode.decodeValue (Json.Decode.field "transactionId" Json.Decode.string)
                |> Result.map (\_ -> ())
                |> CompletedApprovingProposal
                |> Just

        [ "ClickedUnapproveProposal" ] ->
            val
                |> Json.Decode.decodeValue (Json.Decode.field "transactionId" Json.Decode.string)
                |> Result.map (\_ -> ())
                |> CompletedUnapprovingProposal
                |> Just

        [ "ClickedExecuteProposal" ] ->
            val
                |> Json.Decode.decodeValue (Json.Decode.field "transactionId" Json.Decode.string)
                |> Result.map (\_ -> ())
                |> CompletedExecutingProposal
                |> Just

        [ "ClickedCancelProposal" ] ->
            val
                |> Json.Decode.decodeValue (Json.Decode.field "transactionId" Json.Decode.string)
                |> Result.map (\_ -> ())
                |> CompletedCancellingProposal
                |> Just

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        GotPermissionsFormMsg subMsg ->
            "GotPermissionsFormMsg" :: Form.msgToString subMsg

        SubmittedPermissionsForm _ ->
            [ "SubmittedPermissionsForm" ]

        CompletedChangeAccountPermissions r ->
            [ "CompletedChangeAccountPermissions", UR.resultToString r ]

        GotProposeObjectiveFormMsg subMsg ->
            "GotProposeObjectiveFormMsg" :: Form.msgToString subMsg

        SubmittedProposeObjectiveForm _ ->
            [ "SubmittedProposeObjectiveForm" ]

        GotSearchProposalFormMsg subMsg ->
            "GotSearchProposalFormMsg" :: Form.msgToString subMsg

        CompletedLoadProposals r ->
            [ "CompletedLoadProposals", UR.resultToString r ]

        CompletedLoadApprovals r ->
            [ "CompletedLoadApprovals", UR.resultToString r ]

        DeserializedProposals _ ->
            [ "DeserializedProposals" ]

        ClickedApproveProposal _ ->
            [ "ClickedApproveProposal" ]

        ClickedUnapproveProposal _ ->
            [ "ClickedUnapproveProposal" ]

        ClickedExecuteProposal _ ->
            [ "ClickedExecuteProposal" ]

        ClickedCancelProposal _ ->
            [ "ClickedCancelProposal" ]

        CompletedApprovingProposal r ->
            [ "CompletedApprovingProposal", UR.resultToString r ]

        CompletedUnapprovingProposal r ->
            [ "CompletedUnapprovingProposal", UR.resultToString r ]

        CompletedExecutingProposal r ->
            [ "CompletedExecutingProposal", UR.resultToString r ]

        CompletedCancellingProposal r ->
            [ "CompletedCancellingProposal", UR.resultToString r ]

        CompletedProposingNewObjective r ->
            [ "CompletedProposingNewObjective", UR.resultToString r ]

        CompletedLoadCommunityCreatorPermissions _ ->
            [ "CompletedLoadCommunityCreatorPermissions" ]
