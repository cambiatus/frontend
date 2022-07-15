module Action2 exposing (ClaimingStatus, Msg, msgToString, notClaiming, update)

import Action exposing (Action)
import Eos
import Eos.Account
import Form
import Form.File
import Graphql.Http
import Json.Encode
import RemoteData exposing (RemoteData)
import Route
import Session.Shared exposing (Shared)
import Sha256
import Time
import Time.Extra
import UpdateResult as UR
import View.Feedback


notClaiming : ClaimingStatus
notClaiming =
    NotClaiming


type ClaimingStatus
    = NotClaiming
    | Claiming { action : Action, proof : Proof }


type Proof
    = NoProofNecessary
    | WithProof (Form.Model Form.File.SingleModel) ProofCodeStatus


type ProofCodeStatus
    = NoCodeNecessary
    | GeneratingCode
    | WithCode ProofCode


type alias ProofCode =
    { code : String
    , expiration : Time.Posix
    , generation : Time.Posix
    }


type Msg
    = NoOp
    | ClickedClaimAction Action
    | GotClaimingActionMsg ClaimingActionMsg


type ClaimingActionMsg
    = ConfirmedClaimAction
    | ConfirmedClaimActionWithPhotoProof { photoProofUrl : String, proofCode : Maybe ProofCode }
    | GotPhotoProofFormMsg (Form.Msg Form.File.SingleModel)
    | GotUint64Name String
    | CompletedClaimingAction (Result Json.Encode.Value ())


type alias UpdateResult =
    UR.UpdateResult ClaimingStatus Msg ExternalMsg


type ExternalMsg
    = SetUpdateTimeEvery Float
    | ShowFeedback View.Feedback.Model


type alias Community community =
    { community | symbol : Eos.Symbol }


type alias LoggedIn loggedIn community =
    { loggedIn
        | accountName : Eos.Account.Name
        , shared : Shared
        , selectedCommunity : RemoteData (Graphql.Http.Error (Maybe (Community community))) (Community community)
    }


update : Msg -> ClaimingStatus -> LoggedIn loggedIn community -> UpdateResult
update msg status loggedIn =
    case msg of
        NoOp ->
            UR.init status

        ClickedClaimAction action ->
            let
                form =
                    { fileUrl = Nothing
                    , aspectRatio = Nothing
                    }
                        |> Form.File.initSingle
                        |> Form.init

                proofCode =
                    if action.hasProofCode then
                        GeneratingCode

                    else
                        NoCodeNecessary

                proof =
                    if action.hasProofPhoto then
                        WithProof form proofCode

                    else
                        NoProofNecessary

                generateProofCodePort =
                    if action.hasProofCode then
                        UR.addPort
                            { responseAddress = msg
                            , responseData = Json.Encode.null
                            , data =
                                Json.Encode.object
                                    [ ( "name", Json.Encode.string "accountNameToUint64" )
                                    , ( "accountName", Eos.Account.encodeName loggedIn.accountName )
                                    ]
                            }

                    else
                        identity
            in
            Claiming { action = action, proof = proof }
                |> UR.init
                |> UR.addExt (SetUpdateTimeEvery 1000)
                |> generateProofCodePort

        GotClaimingActionMsg subMsg ->
            case status of
                NotClaiming ->
                    -- TODO - Error
                    UR.init status

                Claiming { action, proof } ->
                    updateClaimingAction subMsg action proof loggedIn


updateClaimingAction : ClaimingActionMsg -> Action -> Proof -> LoggedIn loggedIn community -> UpdateResult
updateClaimingAction msg action proof loggedIn =
    let
        status =
            Claiming { action = action, proof = proof }
    in
    case msg of
        ConfirmedClaimAction ->
            UR.init status
                -- TODO - Add this
                -- |> LoggedIn.withPrivateKey loggedIn
                --     [ Permission.Claim ]
                --     model
                --     { successMsg = msg, errorMsg = ClickedCloseClaimModal }
                |> UR.addPort
                    { responseAddress = GotClaimingActionMsg msg
                    , responseData = Json.Encode.null
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

        ConfirmedClaimActionWithPhotoProof { photoProofUrl, proofCode } ->
            let
                claimPort maybeProofCode =
                    { responseAddress = GotClaimingActionMsg msg
                    , responseData = Json.Encode.null
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
                                                { photo = photoProofUrl
                                                , proofCode = maybeProofCode
                                                }
                                        }
                              }
                            ]
                    }
            in
            case proofCode of
                Nothing ->
                    if action.hasProofCode then
                        -- TODO - Error
                        UR.init status

                    else
                        UR.init status
                            -- TODO - Add this
                            -- |> LoggedIn.withPrivateKey loggedIn
                            --     [ Permission.Claim ]
                            --     model
                            --     { successMsg = msg, errorMsg = ClickedCloseClaimModal }
                            |> UR.addPort (claimPort Nothing)

                Just code ->
                    UR.init status
                        -- TODO - Add this
                        -- |> LoggedIn.withPrivateKey loggedIn
                        --     [ Permission.Claim ]
                        --     model
                        --     { successMsg = msg, errorMsg = ClickedCloseClaimModal }
                        |> UR.addPort
                            (claimPort
                                (Just
                                    { code = code.code
                                    , time = code.generation
                                    }
                                )
                            )

        GotPhotoProofFormMsg subMsg ->
            case proof of
                NoProofNecessary ->
                    UR.init status

                WithProof photoProofForm proofCodeStatus ->
                    Form.update loggedIn.shared subMsg photoProofForm
                        |> UR.fromChild
                            (\newForm ->
                                Claiming { action = action, proof = WithProof newForm proofCodeStatus }
                            )
                            (GotClaimingActionMsg << GotPhotoProofFormMsg)
                            (\ext -> UR.addExt (ShowFeedback ext))
                            status

        GotUint64Name uint64Name ->
            case proof of
                NoProofNecessary ->
                    UR.init status

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

                        proofCodeStatus =
                            if action.hasProofCode then
                                WithCode
                                    { code = proofCode
                                    , expiration = expiration
                                    , generation = loggedIn.shared.now
                                    }

                            else
                                NoCodeNecessary
                    in
                    Claiming
                        { action = action
                        , proof = WithProof formModel proofCodeStatus
                        }
                        |> UR.init

        CompletedClaimingAction (Ok ()) ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    NotClaiming
                        |> UR.init
                        |> UR.addExt
                            (ShowFeedback
                                (View.Feedback.Visible View.Feedback.Success
                                    (loggedIn.shared.translators.tr
                                        "dashboard.check_claim.success"
                                        [ ( "symbolCode", Eos.symbolToSymbolCodeString community.symbol ) ]
                                    )
                                )
                            )
                        |> UR.addExt (SetUpdateTimeEvery (60 * 1000))
                        |> UR.addCmd
                            (Eos.Account.nameToString loggedIn.accountName
                                |> Route.ProfileClaims
                                |> Route.pushUrl loggedIn.shared.navKey
                            )

                _ ->
                    NotClaiming
                        |> UR.init
                        |> UR.addExt (SetUpdateTimeEvery (60 * 1000))
                        |> UR.logImpossible (GotClaimingActionMsg msg)
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
            NotClaiming
                |> UR.init
                |> UR.addExt (ShowFeedback (View.Feedback.Visible View.Feedback.Failure (loggedIn.shared.translators.t "dashboard.check_claim.failure")))
                |> UR.addExt (SetUpdateTimeEvery (60 * 1000))
                |> UR.logJsonValue (GotClaimingActionMsg msg)
                    (Just loggedIn.accountName)
                    "Got an error when claiming an action"
                    { moduleName = "Page.Community.Objectives", function = "update" }
                    []
                    val


generateProofCode : Action -> String -> Time.Posix -> String
generateProofCode action claimerAccountUint64 time =
    (String.fromInt action.id
        ++ claimerAccountUint64
        ++ String.fromInt (Time.posixToMillis time // 1000)
    )
        |> Sha256.sha256
        |> String.slice 0 8


msgToString : Msg -> List String
msgToString msg =
    case msg of
        _ ->
            [ "TODO" ]
