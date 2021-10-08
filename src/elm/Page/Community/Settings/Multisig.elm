module Page.Community.Settings.Multisig exposing (Model, Msg, init, jsAddressToMsg, msgToString, receiveBroadcast, update, view)

import Api
import Community
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



-- MODEL


type alias Model =
    { proposals : List Proposal
    }


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( { proposals = [] }
    , Api.getFromBlockchain loggedIn.shared
        { code = "eosio.msig"
        , scope = "henriquebuss"
        , table = "proposal"
        , limit = 100
        }
        proposalRowsDecoder
        CompletedLoadProposals
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


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


type alias Proposal =
    { name : String
    , expiration : Time.Posix
    , actions : List Action
    }


type alias Vote =
    { proposer : Eos.Account.Name
    , proposalName : String
    , permissionLevel : Eos.Authorization
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
                    { responseAddress = ClickedChangeAccountPermissions
                    , responseData = Encode.null
                    , data =
                        Eos.encodeTransaction
                            [ Eos.updateAuth
                                { actor = loggedIn.accountName
                                , permissionName = Eos.Account.ownerPermission
                                }
                                -- TODO - Not hardcode this
                                (Eos.Account.stringToName "eosmsigadmin")
                                2
                                -- TODO - Not hardcode these
                                [ { name = Eos.Account.stringToName "henriquebuss"
                                  , weight = 1
                                  }
                                , { name = Eos.Account.stringToName "henriquebus2"
                                  , weight = 1
                                  }
                                , { name = Eos.Account.stringToName "henriquebus4"
                                  , weight = 1
                                  }
                                ]
                            ]
                    }
                |> LoggedIn.withAuthentication loggedIn
                    model
                    { successMsg = ClickedChangeAccountPermissions
                    , errorMsg = NoOp
                    }

        ClickedProposeNewObjective ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    model
                        |> UR.init
                        |> UR.addPort
                            { responseAddress = ClickedProposeNewObjective
                            , responseData = Encode.null
                            , data =
                                Eos.encodeTransaction
                                    [ Eos.proposeTransaction
                                        loggedIn.accountName
                                        "createobj"
                                        -- TODO - Not hardcode these
                                        [ { actor = Eos.Account.stringToName "henriquebus2"
                                          , permissionName = Eos.Account.samplePermission
                                          }
                                        , { actor = Eos.Account.stringToName "henriquebus4"
                                          , permissionName = Eos.Account.samplePermission
                                          }
                                        ]
                                        (loggedIn.shared.now
                                            |> Time.posixToMillis
                                            -- now + 10 days
                                            |> (\now -> now + 1000 * 60 * 60 * 24 * 10)
                                            |> Time.millisToPosix
                                        )
                                        -- TODO - Not hardcode these
                                        [ { accountName = loggedIn.shared.contracts.community
                                          , name = "newobjective"
                                          , authorization =
                                                { actor = Eos.Account.stringToName "eosmsigadmin"
                                                , permissionName = Eos.Account.samplePermission
                                                }
                                          , data =
                                                { asset = { amount = 0, symbol = community.symbol }
                                                , description = "Test objective with multisig transaction"
                                                , creator = loggedIn.accountName
                                                }
                                                    |> Community.encodeCreateObjectiveAction
                                          }
                                        ]
                                    ]
                            }
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
                    { responseAddress = msg
                    , responseData = Encode.null
                    , data =
                        Eos.encodeTransaction
                            [ { accountName = "eosio.msig"
                              , name = "approve"
                              , authorization =
                                    { actor = loggedIn.accountName
                                    , permissionName = Eos.Account.samplePermission
                                    }
                              , data =
                                    encodeVote
                                        { proposer = Eos.Account.stringToName "henriquebuss"
                                        , proposalName = proposal.name
                                        , permissionLevel =
                                            { actor = loggedIn.accountName
                                            , permissionName = Eos.Account.samplePermission
                                            }
                                        }
                              }
                            ]
                    }
                |> LoggedIn.withAuthentication loggedIn
                    model
                    { successMsg = msg, errorMsg = NoOp }

        ClickedUnapproveProposal proposal ->
            UR.init model
                |> UR.addPort
                    { responseAddress = msg
                    , responseData = Encode.null
                    , data =
                        Eos.encodeTransaction
                            [ { accountName = "eosio.msig"
                              , name = "unapprove"
                              , authorization =
                                    { actor = loggedIn.accountName
                                    , permissionName = Eos.Account.samplePermission
                                    }
                              , data =
                                    encodeVote
                                        { proposer = Eos.Account.stringToName "henriquebuss"
                                        , proposalName = proposal.name
                                        , permissionLevel =
                                            { actor = loggedIn.accountName
                                            , permissionName = Eos.Account.samplePermission
                                            }
                                        }
                              }
                            ]
                    }
                |> LoggedIn.withAuthentication loggedIn
                    model
                    { successMsg = msg, errorMsg = NoOp }

        ClickedExecuteProposal proposal ->
            UR.init model
                |> UR.addPort
                    { responseAddress = msg
                    , responseData = Encode.null
                    , data =
                        Eos.encodeTransaction
                            [ { accountName = "eosio.msig"
                              , name = "exec"
                              , authorization =
                                    { actor = loggedIn.accountName
                                    , permissionName = Eos.Account.samplePermission
                                    }
                              , data =
                                    Encode.object
                                        [ ( "proposer", Encode.string "henriquebuss" )
                                        , ( "proposal_name", Encode.string proposal.name )
                                        , ( "executer", Eos.Account.encodeName loggedIn.accountName )
                                        ]
                              }
                            ]
                    }
                |> LoggedIn.withAuthentication loggedIn
                    model
                    { successMsg = msg, errorMsg = NoOp }



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
            [ h2 [ class "font-bold" ] [ text "Proposal name" ]
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


proposalRowsDecoder : Json.Decode.Decoder (List ProposalRow)
proposalRowsDecoder =
    Json.Decode.field "rows" (Json.Decode.list proposalRowDecoder)


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


encodeVote : Vote -> Encode.Value
encodeVote vote =
    Encode.object
        [ ( "proposer", Eos.Account.encodeName vote.proposer )
        , ( "proposal_name", Encode.string vote.proposalName )
        , ( "level", Eos.encodeAuthorization vote.permissionLevel )
        ]



-- UTILS


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        _ ->
            Nothing


jsAddressToMsg : List String -> Encode.Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "CompletedLoadOwnProposals" :: _ ->
            Json.Decode.decodeValue
                (Json.Decode.field "deserializedProposals"
                    (Json.Decode.list proposalDecoder)
                )
                val
                |> DeserializedProposals
                |> Just

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
            [ "CompletedLoadOwnProposals", UR.resultToString r ]

        DeserializedProposals _ ->
            [ "DeserializedProposals" ]

        ClickedApproveProposal _ ->
            [ "ClickedApproveProposal" ]

        ClickedUnapproveProposal _ ->
            [ "ClickedUnapproveProposal" ]

        ClickedExecuteProposal _ ->
            [ "ClickedExecuteProposal" ]
