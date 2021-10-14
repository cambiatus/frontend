module Page.Community.Settings.Multisig exposing (Model, Msg, init, jsAddressToMsg, msgToString, receiveBroadcast, update, view)

import Api.Eos
import Avatar
import Community
import Date
import DatePicker
import Eos.Account
import Eos.Permission
import Html exposing (Html, button, div, h1, h2, img, p, text)
import Html.Attributes exposing (class, disabled, src, tabindex)
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
import Select
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import Time
import UpdateResult as UR
import Utils
import View.Components
import View.Feedback as Feedback
import View.Form
import View.Form.Checkbox as Checkbox
import View.Form.Input as Input
import View.Form.Radio as Radio



-- MODEL


type alias Model =
    { proposals : RemoteData Http.Error (List Proposal)
    , threshold : Int
    , voterState : Select.State
    , selectedVoters :
        List
            { profile : Profile.Minimal
            , permission : Eos.Permission.PermissionType
            , weight : Int
            }
    , targetPermission : Eos.Permission.PermissionType
    , newObjectiveName : String
    , proposalPermission : Eos.Permission.PermissionType
    , proposalVoters :
        List
            { profile : Profile.Minimal
            , isChecked : Bool
            , permission : Eos.Permission.PermissionType
            }
    , newObjectiveExpirationDate : Date.Date
    , newObjectiveExpirationDatePicker : DatePicker.DatePicker
    , searchState : Select.State
    , selectedSearch : Maybe Profile.Minimal
    }


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    let
        nextWeek =
            Date.fromPosix loggedIn.shared.timezone loggedIn.shared.now
                |> Date.add Date.Days 7
    in
    ( { proposals = RemoteData.NotAsked
      , threshold = 1
      , voterState = Select.newState "voter-select"
      , selectedVoters = []
      , targetPermission = Eos.Permission.default
      , newObjectiveName = ""
      , proposalPermission = Eos.Permission.default
      , proposalVoters = []
      , newObjectiveExpirationDate = nextWeek
      , newObjectiveExpirationDatePicker = DatePicker.initFromDate nextWeek
      , searchState = Select.newState "search-select"
      , selectedSearch = Nothing
      }
    , LoggedIn.maybeInitWith CompletedLoadCommunityCreatorPermissions .communityCreatorPermissions loggedIn
    )



-- TYPES


type Msg
    = NoOp
    | EnteredThreshold String
    | SelectedVoter (Maybe Profile.Minimal)
    | GotVoterSelectMsg (Select.Msg Profile.Minimal)
    | EnteredVoterWeight Eos.Account.Name String
    | SelectedVoterPermission Eos.Account.Name Eos.Permission.PermissionType
    | RemovedVoter Eos.Account.Name
    | SelectedTargetPermission Eos.Permission.PermissionType
    | ClickedChangeAccountPermissions
    | CompletedChangeAccountPermissions (Result Json.Decode.Error ())
    | ClickedProposeNewObjective
    | CompletedLoadProposals (Result Http.Error (List ProposalRow))
    | DeserializedProposals (Result Json.Decode.Error (List Proposal))
    | ClickedApproveProposal Proposal
    | ClickedUnapproveProposal Proposal
    | ClickedExecuteProposal Proposal
    | EnteredNewObjectiveName String
    | CompletedProposingNewObjective (Result Json.Decode.Error ())
    | SelectedProposalPermission Eos.Permission.PermissionType
    | CheckedProposalVoter Eos.Account.Name Bool
    | CompletedLoadCommunityCreatorPermissions Eos.Permission.Permissions
    | GotNewObjectiveDatePickerMsg DatePicker.Msg
    | GotSearchSelectMsg (Select.Msg Profile.Minimal)
    | SelectedProposalSearch (Maybe Profile.Minimal)


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


type alias Proposal =
    { name : String
    , expiration : Time.Posix
    , proposer : Eos.Account.Name
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

        EnteredThreshold stringThreshold ->
            case String.toInt stringThreshold of
                Nothing ->
                    UR.init model

                Just threshold ->
                    if threshold > 0 then
                        { model | threshold = threshold }
                            |> UR.init

                    else
                        UR.init model

        SelectedVoter Nothing ->
            -- TODO - Add log
            model
                |> UR.init

        SelectedVoter (Just voter) ->
            let
                newVoters =
                    if
                        List.any (\{ profile } -> profile == voter)
                            model.selectedVoters
                    then
                        model.selectedVoters

                    else
                        { profile = voter
                        , permission = Eos.Permission.default
                        , weight = 1
                        }
                            :: model.selectedVoters
            in
            { model | selectedVoters = newVoters }
                |> UR.init

        GotVoterSelectMsg subMsg ->
            let
                ( updatedVoters, cmd ) =
                    Select.update (voterSelectConfiguration loggedIn.shared)
                        subMsg
                        model.voterState
            in
            UR.init { model | voterState = updatedVoters }
                |> UR.addCmd cmd

        EnteredVoterWeight voterName stringWeight ->
            case String.toInt stringWeight of
                Nothing ->
                    UR.init model

                Just weight ->
                    if weight > 0 then
                        { model
                            | selectedVoters =
                                List.Extra.updateIf
                                    (\{ profile } -> profile.account == voterName)
                                    (\voter -> { voter | weight = weight })
                                    model.selectedVoters
                        }
                            |> UR.init

                    else
                        UR.init model

        SelectedVoterPermission voterName permission ->
            { model
                | selectedVoters =
                    List.Extra.updateIf
                        (\{ profile } -> profile.account == voterName)
                        (\voter -> { voter | permission = permission })
                        model.selectedVoters
            }
                |> UR.init

        RemovedVoter voterName ->
            { model
                | selectedVoters =
                    List.filter
                        (\{ profile } -> profile.account /= voterName)
                        model.selectedVoters
            }
                |> UR.init

        SelectedTargetPermission permission ->
            { model | targetPermission = permission }
                |> UR.init

        ClickedChangeAccountPermissions ->
            model
                |> UR.init
                |> UR.addPort
                    (Api.Eos.UpdateAuth
                        { targetAccount = loggedIn.accountName
                        , targetPermission = model.targetPermission
                        , threshold = model.threshold
                        , accounts =
                            List.map
                                (\{ profile, permission, weight } ->
                                    { account = profile.account
                                    , permission = permission
                                    , weight = weight
                                    }
                                )
                                model.selectedVoters
                        }
                        |> Api.Eos.EosAction
                        |> Api.Eos.transact loggedIn.shared
                            { actor = loggedIn.accountName, permission = Eos.Permission.Owner }
                            msg
                    )
                |> LoggedIn.withAuthentication loggedIn
                    model
                    { successMsg = ClickedChangeAccountPermissions
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

        ClickedProposeNewObjective ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    let
                        proposedActions =
                            [ ( Api.Eos.CreateObjective
                                    { communitySymbol = community.symbol
                                    , objectiveDescription = model.newObjectiveName
                                    , communityAdmin = community.creator
                                    }
                                    |> Api.Eos.CommunityAction
                              , { actor = community.creator, permission = Eos.Permission.Active }
                              )
                            ]
                    in
                    model
                        |> UR.init
                        |> UR.addPort
                            (Api.Eos.Propose
                                { proposer = loggedIn.accountName
                                , proposalName = model.newObjectiveName
                                , requestedVotes =
                                    model.proposalVoters
                                        |> List.filterMap
                                            (\{ profile, isChecked, permission } ->
                                                if isChecked then
                                                    Just
                                                        { actor = profile.account
                                                        , permission = permission
                                                        }

                                                else
                                                    Nothing
                                            )
                                , expiration =
                                    Utils.posixFromDate loggedIn.shared.timezone
                                        model.newObjectiveExpirationDate
                                , actions = proposedActions
                                }
                                |> Api.Eos.MultiSigAction
                                |> Api.Eos.transact loggedIn.shared
                                    { actor = loggedIn.accountName, permission = Eos.Permission.Active }
                                    ClickedProposeNewObjective
                            )
                        |> LoggedIn.withAuthentication loggedIn
                            model
                            { successMsg = ClickedProposeNewObjective
                            , errorMsg = NoOp
                            }

                _ ->
                    UR.init model

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

        CompletedLoadProposals (Err _) ->
            -- TODO
            UR.init model

        DeserializedProposals (Ok proposals) ->
            { model | proposals = RemoteData.Success proposals }
                |> UR.init

        DeserializedProposals (Err err) ->
            -- TODO - Use error in model
            -- UR.init model
            model
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure "Got an error when deserializing proposals")
                |> UR.logDecodingError msg
                    (Just loggedIn.accountName)
                    "Got an error when deserializing proposals"
                    { moduleName = "Page.Community.Settings.Multising", function = "update" }
                    []
                    err

        ClickedApproveProposal proposal ->
            UR.init model
                |> UR.addPort
                    (Api.Eos.Approve
                        { proposer = proposal.proposer
                        , proposalName = proposal.name
                        }
                        |> Api.Eos.MultiSigAction
                        |> Api.Eos.transact loggedIn.shared
                            { actor = loggedIn.accountName, permission = Eos.Permission.Active }
                            msg
                    )
                |> LoggedIn.withAuthentication loggedIn
                    model
                    { successMsg = msg, errorMsg = NoOp }

        ClickedUnapproveProposal proposal ->
            UR.init model
                |> UR.addPort
                    (Api.Eos.Unapprove
                        { proposer = proposal.proposer
                        , proposalName = proposal.name
                        }
                        |> Api.Eos.MultiSigAction
                        |> Api.Eos.transact loggedIn.shared
                            { actor = loggedIn.accountName, permission = Eos.Permission.Active }
                            msg
                    )
                |> LoggedIn.withAuthentication loggedIn
                    model
                    { successMsg = msg, errorMsg = NoOp }

        ClickedExecuteProposal proposal ->
            UR.init model
                |> UR.addPort
                    (Api.Eos.Execute
                        { proposer = proposal.proposer
                        , proposalName = proposal.name
                        }
                        |> Api.Eos.MultiSigAction
                        |> Api.Eos.transact loggedIn.shared
                            { actor = loggedIn.accountName, permission = Eos.Permission.Active }
                            msg
                    )
                |> LoggedIn.withAuthentication loggedIn
                    model
                    { successMsg = msg, errorMsg = NoOp }

        EnteredNewObjectiveName newObjectiveName ->
            { model | newObjectiveName = newObjectiveName }
                |> UR.init

        CompletedProposingNewObjective (Ok ()) ->
            { model | newObjectiveName = "" }
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Success "Objective created")
                |> UR.addCmd
                    (Api.Eos.query loggedIn.shared
                        CompletedLoadProposals
                        proposalRowDecoder
                        (Api.Eos.MultiSig (Api.Eos.Proposal (Eos.Account.stringToName "henriquebuss")))
                    )

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

        SelectedProposalPermission proposalPermission ->
            case ( loggedIn.selectedCommunity, loggedIn.communityCreatorPermissions ) of
                ( RemoteData.Success community, RemoteData.Success communityCreatorPermissions ) ->
                    let
                        permission =
                            case proposalPermission of
                                Eos.Permission.Active ->
                                    communityCreatorPermissions.active

                                Eos.Permission.Owner ->
                                    communityCreatorPermissions.owner

                                Eos.Permission.RootPermission ->
                                    communityCreatorPermissions.owner
                    in
                    { model
                        | proposalPermission = proposalPermission
                        , proposalVoters = List.filterMap (makeVoter permission) community.members
                    }
                        |> UR.init

                _ ->
                    UR.init model

        CheckedProposalVoter proposalVoter value ->
            { model
                | proposalVoters =
                    List.Extra.updateIf (\{ profile } -> profile.account == proposalVoter)
                        (\voter -> { voter | isChecked = value })
                        model.proposalVoters
            }
                |> UR.init

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

        GotNewObjectiveDatePickerMsg subMsg ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update (datePickerSettings loggedIn.shared) subMsg model.newObjectiveExpirationDatePicker
            in
            case dateEvent of
                DatePicker.Picked newDate ->
                    { model | newObjectiveExpirationDate = newDate }
                        |> UR.init

                _ ->
                    { model | newObjectiveExpirationDatePicker = newDatePicker }
                        |> UR.init

        GotSearchSelectMsg subMsg ->
            let
                ( updatedVoters, cmd ) =
                    Select.update (searchProposalSelectConfiguration loggedIn.shared)
                        subMsg
                        model.searchState
            in
            { model | searchState = updatedVoters }
                |> UR.init
                |> UR.addCmd cmd

        SelectedProposalSearch maybeProfile ->
            let
                queryProposals =
                    case maybeProfile of
                        Nothing ->
                            identity

                        Just profile ->
                            Api.Eos.Proposal profile.account
                                |> Api.Eos.MultiSig
                                |> Api.Eos.query loggedIn.shared
                                    CompletedLoadProposals
                                    proposalRowDecoder
                                |> UR.addCmd
            in
            { model
                | selectedSearch = maybeProfile
                , proposals =
                    case maybeProfile of
                        Nothing ->
                            model.proposals

                        Just _ ->
                            RemoteData.Loading
            }
                |> UR.init
                |> queryProposals



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
            , viewProposalCard loggedIn.shared community model
            ]
        ]


viewChangePermissions : LoggedIn.Model -> Community.Model -> Model -> Html Msg
viewChangePermissions ({ shared } as loggedIn) community model =
    div [ class "bg-white rounded-sm shadow p-4 my-10" ]
        [ p [ class "font-bold mb-2" ]
            [ text "This will change the permission on the account you're currently logged in to. Since this is still a WIP, make sure you're using a test account, otherwise you may need to manually submit a transaction to the blockchain to revert your actions." ]
        , p [ class "mb-4" ] [ text "Here you can set the threshold of a specific permission. You also choose the accounts that will be associated with that permission" ]
        , Input.init
            { label = "Threshold"
            , id = "threshold-input"
            , onInput = EnteredThreshold
            , disabled = False
            , value = model.threshold |> String.fromInt
            , placeholder = Nothing
            , problems = Nothing
            , translators = shared.translators
            }
            |> Input.withType Input.Number
            |> Input.toHtml
        , Radio.init
            { label = "Target permission"
            , name = "permission-radio"
            , optionToString = Eos.Permission.toString
            , activeOption = model.targetPermission
            , onSelect = SelectedTargetPermission
            , areOptionsEqual = (==)
            }
            |> Radio.withOptions permissionOptions
            |> Radio.withAttrs [ class "mb-10" ]
            |> Radio.withLabelAttrs [ class "mr-29 capitalize" ]
            |> Radio.withRowAttrs [ class "mt-2" ]
            |> Radio.toHtml shared.translators
        , View.Form.label "voter-select-input" "Voters"
        , viewAutoCompleteAccount shared community model
        , div [ class "flex flex-wrap gap-6 my-6" ]
            (List.map (viewVoter loggedIn) model.selectedVoters)
        , button
            [ class "button button-primary w-full"
            , onClick ClickedChangeAccountPermissions
            ]
            [ text "Change account permissions" ]
        ]


viewAutoCompleteAccount : Shared -> Community.Model -> Model -> Html Msg
viewAutoCompleteAccount shared community model =
    Select.view
        (voterSelectConfiguration shared)
        model.voterState
        community.members
        (model.selectedVoters |> List.map .profile)
        |> Html.map GotVoterSelectMsg


voterSelectConfiguration : Shared -> Select.Config Msg Profile.Minimal
voterSelectConfiguration shared =
    Profile.selectConfig
        (Select.newConfig
            { onSelect = SelectedVoter
            , toLabel = .account >> Eos.Account.nameToString
            , filter = Profile.selectFilter 2 (.account >> Eos.Account.nameToString)
            }
            |> Select.withMultiSelection True
            |> Select.withInputId "voter-select-input"
        )
        shared
        False


viewVoter :
    LoggedIn.Model
    -> { profile : Profile.Minimal, permission : Eos.Permission.PermissionType, weight : Int }
    -> Html Msg
viewVoter ({ shared } as loggedIn) { profile, permission, weight } =
    div [ class "flex flex-col items-center border p-4 rounded" ]
        [ Avatar.view profile.avatar "w-10 h-10 mb-2"
        , Profile.viewProfileNameTag shared loggedIn.accountName profile
        , Input.init
            { label = "Weight"
            , id = "profile-weight-" ++ Eos.Account.nameToString profile.account
            , onInput = EnteredVoterWeight profile.account
            , disabled = False
            , value = weight |> String.fromInt
            , placeholder = Nothing
            , problems = Nothing
            , translators = shared.translators
            }
            |> Input.withType Input.Number
            |> Input.withContainerAttrs [ class "!my-4" ]
            |> Input.toHtml
        , Radio.init
            { label = "Permission"
            , name = "voter-permission-radio-" ++ Eos.Account.nameToString profile.account
            , optionToString = Eos.Permission.toString
            , activeOption = permission
            , onSelect = SelectedVoterPermission profile.account
            , areOptionsEqual = (==)
            }
            |> Radio.withOptions permissionOptions
            |> Radio.withAttrs [ class "w-full" ]
            |> Radio.withRowAttrs [ class "mt-2 justify-between capitalize" ]
            |> Radio.toHtml shared.translators
        , button
            [ class "button button-danger w-full mt-4"
            , onClick (RemovedVoter profile.account)
            ]
            [ text "Remove" ]
        ]


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
        , Input.init
            { label = "New objective name"
            , id = "new-objective-name-input"
            , onInput = EnteredNewObjectiveName
            , disabled = False
            , value = model.newObjectiveName
            , placeholder = Nothing
            , problems = Nothing
            , translators = loggedIn.shared.translators
            }
            |> Input.withCounter 12
            |> Input.withCounterType Input.CountLetters
            |> Input.toHtml
        , Radio.init
            { label = "Permission to use"
            , name = "proposal-permission-radio"
            , optionToString = Eos.Permission.toString
            , activeOption = model.proposalPermission
            , onSelect = SelectedProposalPermission
            , areOptionsEqual = (==)
            }
            |> Radio.withOptions permissionOptions
            |> Radio.withAttrs [ class "mb-4" ]
            |> Radio.withLabelAttrs [ class "mr-29 capitalize" ]
            |> Radio.withRowAttrs [ class "mt-2" ]
            |> Radio.toHtml loggedIn.shared.translators
        , div [ class "flex gap-4 mb-10" ]
            (List.map (viewProposalVoter loggedIn) model.proposalVoters)
        , View.Form.label "newobjective-expiration-picker" "Expiration (until when is the proposal valid)"
        , div [ class "relative mb-10" ]
            [ DatePicker.view (Just model.newObjectiveExpirationDate)
                (datePickerSettings loggedIn.shared)
                model.newObjectiveExpirationDatePicker
                |> Html.map GotNewObjectiveDatePickerMsg
            , img
                [ class "absolute right-0 top-0 h-full cursor-pointer"
                , src "/icons/calendar.svg"
                , tabindex -1
                ]
                []
            ]
        , button
            [ class "button button-primary w-full"
            , onClick ClickedProposeNewObjective
            , disabled (not canPropose)
            ]
            [ text "Propose new objective" ]
        ]


datePickerSettings : Shared -> DatePicker.Settings
datePickerSettings shared =
    let
        defaultSettings =
            DatePicker.defaultSettings
    in
    { defaultSettings
        | changeYear = DatePicker.off
        , placeholder = shared.translators.t "payment_history.pick_date"
        , inputClassList = [ ( "input w-full", True ) ]
        , inputId = Just "newobjective-expiration-picker"
        , dateFormatter = Date.format "E, d MMM y"
        , firstDayOfWeek = Time.Mon
    }


viewProposalVoter :
    LoggedIn.Model
    -> { profile : Profile.Minimal, isChecked : Bool, permission : Eos.Permission.PermissionType }
    -> Html Msg
viewProposalVoter loggedIn { profile, isChecked } =
    Checkbox.init
        { description =
            div [ class "flex flex-col items-center p-2" ]
                [ Avatar.view profile.avatar "w-10 h-10 mb-2"
                , Profile.viewProfileNameTag loggedIn.shared loggedIn.accountName profile
                ]
        , id = "proposal-voter-" ++ Eos.Account.nameToString profile.account
        , value = isChecked
        , disabled = False
        , onCheck = CheckedProposalVoter profile.account
        }
        |> Checkbox.withContainerAttrs [ class "flex flex-col-reverse items-center gap-2 border p-4 rounded-sm" ]
        |> Checkbox.toHtml


viewProposalCard : Shared -> Community.Model -> Model -> Html Msg
viewProposalCard shared community model =
    div [ class "bg-white rounded-sm shadow p-4 my-10" ]
        [ p [ class "text-sm mb-4" ]
            [ text "Here you can use this select element to search for proposals proposed by some user. "
            , text "Available actions are `Approve`, `Unapprove`, `Execute` and `Cancel`. "
            , text "All of them are straight forward, except for `Unapprove`. It doesn't mean \"Vote No\", it means \"Remove my Yes vote (if there is one)\""
            ]
        , View.Form.label "proposal-search-select" "Search proposals by user"
        , viewSearchProposal shared community model
        , div [ class "grid gap-4 grid-cols-2 mt-4" ]
            (case model.proposals of
                RemoteData.Success proposals ->
                    List.map (viewProposal shared) proposals

                RemoteData.Failure _ ->
                    [ text "Something went wrong when fetching proposals" ]

                RemoteData.Loading ->
                    [ text "Loading" ]

                RemoteData.NotAsked ->
                    []
            )
        ]


viewSearchProposal : Shared -> Community.Model -> Model -> Html Msg
viewSearchProposal shared community model =
    Select.view
        (searchProposalSelectConfiguration shared)
        model.searchState
        community.members
        (model.selectedSearch
            |> Maybe.map List.singleton
            |> Maybe.withDefault []
        )
        |> Html.map GotSearchSelectMsg


searchProposalSelectConfiguration : Shared -> Select.Config Msg Profile.Minimal
searchProposalSelectConfiguration shared =
    Profile.selectConfig
        (Select.newConfig
            { onSelect = SelectedProposalSearch
            , toLabel = .account >> Eos.Account.nameToString
            , filter = Profile.selectFilter 2 (.account >> Eos.Account.nameToString)
            }
            |> Select.withInputId "proposal-search-select"
        )
        shared
        False


viewProposal : Shared -> Proposal -> Html Msg
viewProposal shared proposal =
    div [ class "bg-white rounded border p-4" ]
        [ h1 [ class "font-bold" ] [ text proposal.name ]
        , View.Components.dateViewer [ class "text-sm text-gray-900" ]
            identity
            shared
            proposal.expiration
        , div [ class "my-4" ] (List.map viewAction proposal.actions)
        , div [ class "flex justify-between gap-4" ]
            [ button
                [ class "button w-full button-primary"
                , onClick (ClickedApproveProposal proposal)
                ]
                [ text "Approve" ]
            , button
                [ class "button w-full button-danger"
                , onClick (ClickedUnapproveProposal proposal)
                ]
                [ text "Disapprove" ]
            , button
                [ class "button w-full button-secondary"
                , onClick (ClickedExecuteProposal proposal)
                ]
                [ text "Execute" ]
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


permissionOptions : List ( Eos.Permission.PermissionType, Bool -> Html Msg )
permissionOptions =
    List.map
        (\permission ->
            ( permission, \_ -> text (Eos.Permission.toString permission) )
        )
        Eos.Permission.list



-- JSON


type alias ProposalRow =
    { proposalName : String, serializedTransaction : String }


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
        -- TODO - Not hardcode proposer
        |> Json.Decode.Pipeline.hardcoded (Eos.Account.stringToName "henriquebuss")
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
    ->
        Maybe
            { profile : Profile.Minimal
            , isChecked : Bool
            , permission : Eos.Permission.PermissionType
            }
makeVoter permission member =
    List.Extra.find
        (\{ authorization } -> authorization.actor == member.account)
        permission.accounts
        |> Maybe.map
            (\{ authorization } ->
                { profile = member
                , isChecked = True
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
        "ClickedChangeAccountPermissions" :: _ ->
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

        [ "ClickedProposeNewObjective" ] ->
            val
                |> Json.Decode.decodeValue (Json.Decode.field "transactionId" Json.Decode.string)
                |> Result.map (\_ -> ())
                |> CompletedProposingNewObjective
                |> Just

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        EnteredThreshold _ ->
            [ "EnteredThreshold" ]

        SelectedVoter _ ->
            [ "SelectedVoter" ]

        GotVoterSelectMsg _ ->
            [ "GotVoterSelectMsg" ]

        EnteredVoterWeight _ _ ->
            [ "EnteredVoterWeight" ]

        SelectedVoterPermission _ _ ->
            [ "SelectedVoterPermission" ]

        RemovedVoter _ ->
            [ "RemovedVoter" ]

        SelectedTargetPermission _ ->
            [ "SelectedTargetPermission" ]

        ClickedChangeAccountPermissions ->
            [ "ClickedChangeAccountPermissions" ]

        CompletedChangeAccountPermissions r ->
            [ "CompletedChangeAccountPermissions", UR.resultToString r ]

        ClickedProposeNewObjective ->
            [ "ClickedProposeNewObjective" ]

        CompletedLoadProposals r ->
            [ "CompletedLoadProposals", UR.resultToString r ]

        DeserializedProposals _ ->
            [ "DeserializedProposals" ]

        ClickedApproveProposal _ ->
            [ "ClickedApproveProposal" ]

        ClickedUnapproveProposal _ ->
            [ "ClickedUnapproveProposal" ]

        ClickedExecuteProposal _ ->
            [ "ClickedExecuteProposal" ]

        EnteredNewObjectiveName _ ->
            [ "EnteredNewObjectiveName" ]

        CompletedProposingNewObjective r ->
            [ "CompletedProposingNewObjective", UR.resultToString r ]

        SelectedProposalPermission _ ->
            [ "SelectedProposalPermission" ]

        CheckedProposalVoter _ _ ->
            [ "CheckedProposalVoter" ]

        GotNewObjectiveDatePickerMsg _ ->
            [ "GotNewObjectiveDatePickerMsg" ]

        CompletedLoadCommunityCreatorPermissions _ ->
            [ "CompletedLoadCommunityCreatorPermissions" ]

        GotSearchSelectMsg _ ->
            [ "GotSearchSelectMsg" ]

        SelectedProposalSearch _ ->
            [ "SelectedProposalSearch" ]
