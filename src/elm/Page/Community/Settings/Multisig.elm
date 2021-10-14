module Page.Community.Settings.Multisig exposing (Model, Msg, init, jsAddressToMsg, msgToString, update, view)

import Api.Eos
import Avatar
import Community
import Eos.Account
import Html exposing (Html, button, div, h1, h2, p, text)
import Html.Attributes exposing (class)
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
import RemoteData
import Select
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import Time
import UpdateResult as UR
import View.Components
import View.Feedback as Feedback
import View.Form
import View.Form.Input as Input
import View.Form.Radio as Radio



-- MODEL


type alias Model =
    { proposals : List Proposal
    , threshold : Int
    , voterState : Select.State
    , selectedVoters :
        List
            { profile : Profile.Minimal
            , permission : Api.Eos.Permission
            , weight : Int
            }
    , targetPermission : Api.Eos.Permission
    , newObjectiveName : String
    }


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( { proposals = []
      , threshold = 1
      , voterState = Select.newState "voter-select"
      , selectedVoters = []
      , targetPermission = Api.Eos.defaultPermission
      , newObjectiveName = ""
      }
    , Api.Eos.query loggedIn.shared
        CompletedLoadProposals
        proposalRowDecoder
        (Api.Eos.MultiSig (Api.Eos.Proposal (Eos.Account.stringToName "henriquebuss")))
    )



-- TYPES


type Msg
    = NoOp
    | EnteredThreshold String
    | SelectedVoter (Maybe Profile.Minimal)
    | GotVoterSelectMsg (Select.Msg Profile.Minimal)
    | EnteredVoterWeight Eos.Account.Name String
    | SelectedVoterPermission Eos.Account.Name Api.Eos.Permission
    | RemovedVoter Eos.Account.Name
    | SelectedTargetPermission Api.Eos.Permission
    | ClickedChangeAccountPermissions
    | CompletedChangeAccountPermissions (Result Json.Decode.Error ())
    | ClickedProposeNewObjective
    | CompletedLoadProposals (Result Http.Error (List ProposalRow))
    | DeserializedProposals (Result Json.Decode.Error (List Proposal))
    | ClickedApproveProposal Proposal
    | ClickedUnapproveProposal Proposal
    | ClickedExecuteProposal Proposal
    | EnteredNewObjectiveName String
    | CompletedProposingNewObjective


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
                        , permission = Api.Eos.defaultPermission
                        , weight = 1
                        }
                            :: model.selectedVoters
            in
            { model | selectedVoters = newVoters }
                |> UR.init

        GotVoterSelectMsg subMsg ->
            let
                ( updatedVoters, cmd ) =
                    Select.update (selectConfiguration loggedIn.shared)
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
                            { actor = loggedIn.accountName, permission = Api.Eos.Owner }
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
                              , { actor = community.creator, permission = Api.Eos.Active }
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
                                    -- TODO - Not hardcode requestedVotes
                                    [ { actor = Eos.Account.stringToName "henriquebus2"
                                      , permission = Api.Eos.Active
                                      }
                                    , { actor = Eos.Account.stringToName "henriquebus4"
                                      , permission = Api.Eos.Active
                                      }
                                    ]
                                , expiration =
                                    loggedIn.shared.now
                                        |> Time.posixToMillis
                                        -- now + 10 days
                                        |> (\now -> now + 1000 * 60 * 60 * 24 * 10)
                                        |> Time.millisToPosix
                                , actions = proposedActions
                                }
                                |> Api.Eos.MultiSigAction
                                |> Api.Eos.transact loggedIn.shared
                                    { actor = loggedIn.accountName, permission = Api.Eos.Active }
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
            { model | proposals = proposals }
                |> UR.init

        DeserializedProposals (Err err) ->
            -- TODO
            UR.init model

        ClickedApproveProposal proposal ->
            UR.init model
                |> UR.addPort
                    (Api.Eos.Approve
                        { proposer = proposal.proposer
                        , proposalName = proposal.name
                        }
                        |> Api.Eos.MultiSigAction
                        |> Api.Eos.transact loggedIn.shared
                            { actor = loggedIn.accountName, permission = Api.Eos.Active }
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
                            { actor = loggedIn.accountName, permission = Api.Eos.Active }
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
                            { actor = loggedIn.accountName, permission = Api.Eos.Active }
                            msg
                    )
                |> LoggedIn.withAuthentication loggedIn
                    model
                    { successMsg = msg, errorMsg = NoOp }

        EnteredNewObjectiveName newObjectiveName ->
            { model | newObjectiveName = newObjectiveName }
                |> UR.init

        CompletedProposingNewObjective ->
            { model | newObjectiveName = "" }
                |> UR.init
                |> UR.addCmd
                    (Api.Eos.query loggedIn.shared
                        CompletedLoadProposals
                        proposalRowDecoder
                        (Api.Eos.MultiSig (Api.Eos.Proposal (Eos.Account.stringToName "henriquebuss")))
                    )



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
view_ ({ shared } as loggedIn) community model =
    div [ class "container mx-auto" ]
        [ div [ class "px-4" ]
            [ viewChangePermissions loggedIn community model
            , Input.init
                { label = "New objective name"
                , id = "new-objective-name-input"
                , onInput = EnteredNewObjectiveName
                , disabled = False
                , value = model.newObjectiveName
                , placeholder = Nothing
                , problems = Nothing
                , translators = shared.translators
                }
                |> Input.withCounter 12
                |> Input.withCounterType Input.CountLetters
                |> Input.toHtml
            , button
                [ class "button button-primary w-full mt-10"
                , onClick ClickedProposeNewObjective
                ]
                [ text "Propose new objective" ]
            , div [ class "grid gap-4 grid-cols-2 mt-4" ]
                (List.map (viewProposal shared) model.proposals)
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
            , optionToString = Api.Eos.permissionToString
            , activeOption = model.targetPermission
            , onSelect = SelectedTargetPermission
            , areOptionsEqual = (==)
            }
            |> Radio.withOptions
                (List.map
                    (\permission_ ->
                        ( permission_
                        , \_ -> text (Api.Eos.permissionToString permission_)
                        )
                    )
                    Api.Eos.listPermissions
                )
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
        (selectConfiguration shared)
        model.voterState
        community.members
        (model.selectedVoters |> List.map .profile)
        |> Html.map GotVoterSelectMsg


selectConfiguration : Shared -> Select.Config Msg Profile.Minimal
selectConfiguration shared =
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
    -> { profile : Profile.Minimal, permission : Api.Eos.Permission, weight : Int }
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
            , optionToString = Api.Eos.permissionToString
            , activeOption = permission
            , onSelect = SelectedVoterPermission profile.account
            , areOptionsEqual = (==)
            }
            |> Radio.withOptions
                (List.map
                    (\permission_ ->
                        ( permission_
                        , \_ -> text (Api.Eos.permissionToString permission_)
                        )
                    )
                    Api.Eos.listPermissions
                )
            |> Radio.withAttrs [ class "w-full" ]
            |> Radio.withRowAttrs [ class "mt-2 justify-between capitalize" ]
            |> Radio.toHtml shared.translators
        , button
            [ class "button button-danger w-full mt-4"
            , onClick (RemovedVoter profile.account)
            ]
            [ text "Remove" ]
        ]


viewProposal : Shared -> Proposal -> Html Msg
viewProposal shared proposal =
    div [ class "bg-white rounded shadow p-4" ]
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
    div [ class "bg-white rounded shadow p-4" ]
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
            Just CompletedProposingNewObjective

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

        CompletedProposingNewObjective ->
            [ "CompletedProposingNewObjective" ]
