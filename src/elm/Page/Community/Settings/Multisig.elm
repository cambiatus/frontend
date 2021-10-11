module Page.Community.Settings.Multisig exposing (Model, Msg, init, jsAddressToMsg, msgToString, update, view)

import Api.Eos
import Eos
import Eos.Account
import Html exposing (Html, button, div, h1, h2, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Iso8601
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode as Encode
import Page
import RemoteData
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import Time
import UpdateResult as UR
import View.Components
import View.Form.Input as Input



-- MODEL


type alias Model =
    { proposals : List Proposal
    , newObjectiveName : String
    }


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( { proposals = []
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
    | ClickedChangeAccountPermissions
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

        ClickedChangeAccountPermissions ->
            model
                |> UR.init
                |> UR.addPort
                    (Api.Eos.UpdateAuth
                        { targetAccount = loggedIn.accountName
                        , targetPermission = Api.Eos.Active
                        , threshold = 2
                        , accounts =
                            [ { account = Eos.Account.stringToName "henriquebus2"
                              , permission = Api.Eos.Active
                              , weight = 1
                              }
                            , { account = Eos.Account.stringToName "henriquebus4"
                              , permission = Api.Eos.Active
                              , weight = 1
                              }
                            , { account = Eos.Account.stringToName "henriquebuss"
                              , permission = Api.Eos.Active
                              , weight = 1
                              }
                            ]
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
                        , view_ loggedIn.shared model
                        ]

                RemoteData.Loading ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.NotAsked ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.Failure err ->
                    Page.fullPageGraphQLError title err
    in
    { title = title, content = content }


view_ : Shared -> Model -> Html Msg
view_ shared model =
    div [ class "container mx-auto" ]
        [ div [ class "px-4" ]
            [ button
                [ class "button button-danger w-full my-10"
                , onClick ClickedChangeAccountPermissions
                ]
                [ text "Change account permissions" ]
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

        ClickedChangeAccountPermissions ->
            [ "ClickedChangeAccountPermissions" ]

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
