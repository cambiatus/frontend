module Action exposing
    ( Action
    , ClaimConfirmationModalStatus(..)
    , Model
    , Msg(..)
    , encodeClaimAction
    , init
    , jsAddressToMsg
    , msgToString
    , selectionSet
    , update
    , viewClaimConfirmation
    )

import Cambiatus.Enum.VerificationType exposing (VerificationType)
import Cambiatus.Object
import Cambiatus.Object.Action as ActionObject
import Cambiatus.Scalar exposing (DateTime)
import Eos
import Eos.Account as Eos
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, classList, disabled)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Profile
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Translators)
import UpdateResult as UR
import View.Modal as Modal


type ClaimConfirmationModalStatus
    = Open Action
    | InProgress
    | Closed


type alias Action =
    { id : Int
    , description : String
    , reward : Float
    , verifierReward : Float
    , creator : Eos.Name
    , validators : List Profile.Minimal
    , usages : Int
    , usagesLeft : Int
    , deadline : Maybe DateTime
    , verificationType : VerificationType
    , verifications : Int
    , isCompleted : Bool
    , hasProofPhoto : Bool
    , hasProofCode : Bool
    , photoProofInstructions : Maybe String
    , position : Maybe Int
    }


type alias ClaimedAction =
    { actionId : Int
    , maker : Eos.Name
    , proofPhoto : String
    , proofCode : String
    , proofTime : Int
    }


type alias Model =
    { claimConfirmationModalStatus : ClaimConfirmationModalStatus
    , action : Action
    }


init : Action -> Model
init action =
    { claimConfirmationModalStatus = Closed
    , action = action
    }


type Msg
    = NoOp
    | OpenClaimConfirmation Action
    | CloseClaimConfirmation
    | ClaimAction Action
    | GotClaimActionResponse (Result Value String)


viewClaimConfirmation : Translators -> ClaimConfirmationModalStatus -> Html Msg
viewClaimConfirmation { t } claimConfirmationModalStatus =
    let
        text_ s =
            text (t s)

        modalContent acceptMsg isInProgress =
            div []
                [ Modal.initWith
                    { closeMsg = CloseClaimConfirmation
                    , isVisible = True
                    }
                    |> Modal.withHeader (t "claim.modal.title")
                    |> Modal.withBody [ text_ "dashboard.check_claim.body" ]
                    |> Modal.withFooter
                        [ button
                            [ class "modal-cancel"
                            , classList [ ( "button-disabled", isInProgress ) ]
                            , onClick
                                (if isInProgress then
                                    NoOp

                                 else
                                    CloseClaimConfirmation
                                )
                            , disabled isInProgress
                            ]
                            [ text_ "dashboard.check_claim.no" ]
                        , button
                            [ class "modal-accept"
                            , classList [ ( "button-disabled", isInProgress ) ]
                            , onClick
                                (if isInProgress then
                                    NoOp

                                 else
                                    acceptMsg
                                )
                            , disabled isInProgress
                            ]
                            [ text (t "dashboard.check_claim.yes")
                            ]
                        ]
                    |> Modal.toHtml
                ]
    in
    case claimConfirmationModalStatus of
        Open action ->
            let
                acceptMsg =
                    if action.hasProofPhoto then
                        --OpenProofSection action
                        -- TODO: Go to the Claim with Photo page
                        NoOp

                    else
                        ClaimAction action
            in
            modalContent acceptMsg False

        InProgress ->
            modalContent NoOp True

        Closed ->
            text ""



-- UPDATE


update : LoggedIn.Model -> Msg -> Model -> UR.UpdateResult Model Msg (External Msg)
update ({ shared } as loggedIn) msg model =
    let
        { t } =
            shared.translators
    in
    case msg of
        OpenClaimConfirmation action ->
            { model | claimConfirmationModalStatus = Open action }
                |> UR.init

        CloseClaimConfirmation ->
            { model | claimConfirmationModalStatus = Closed }
                |> UR.init

        ClaimAction action ->
            let
                newModel =
                    { model | claimConfirmationModalStatus = InProgress }
            in
            if LoggedIn.isAuth loggedIn then
                newModel
                    |> UR.init
                    |> UR.addPort
                        { responseAddress = ClaimAction action
                        , responseData = Encode.null
                        , data =
                            Eos.encodeTransaction
                                [ { accountName = shared.contracts.community
                                  , name = "claimaction"
                                  , authorization =
                                        { actor = loggedIn.accountName
                                        , permissionName = Eos.samplePermission
                                        }
                                  , data =
                                        { actionId = action.id
                                        , maker = loggedIn.accountName
                                        , proofPhoto = ""
                                        , proofCode = ""
                                        , proofTime = 0
                                        }
                                            |> encodeClaimAction
                                  }
                                ]
                        }

            else
                newModel
                    |> UR.init
                    |> UR.addExt (Just (ClaimAction action) |> RequiredAuthentication)

        GotClaimActionResponse (Ok _) ->
            let
                message =
                    shared.translators.tr "dashboard.check_claim.success"
                        [ ( "symbolCode", Eos.symbolToSymbolCodeString loggedIn.selectedCommunity ) ]
            in
            { model
                | claimConfirmationModalStatus = Closed
            }
                |> UR.init
                |> UR.addExt (ShowFeedback LoggedIn.Success message)

        GotClaimActionResponse (Err _) ->
            { model
                | claimConfirmationModalStatus = Closed
            }
                |> UR.init
                |> UR.addExt (ShowFeedback LoggedIn.Failure (t "dashboard.check_claim.failure"))

        NoOp ->
            model |> UR.init


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "ClaimAction" :: [] ->
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.field "transactionId" Decode.string |> Decode.map Ok
                    , Decode.succeed (Err val)
                    ]
                )
                val
                |> Result.map (Just << GotClaimActionResponse)
                |> Result.withDefault Nothing

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        OpenClaimConfirmation _ ->
            [ "OpenClaimConfirmation" ]

        CloseClaimConfirmation ->
            [ "CloseClaimConfirmation" ]

        ClaimAction _ ->
            [ "ClaimAction" ]

        GotClaimActionResponse r ->
            [ "GotClaimActionResponse", UR.resultToString r ]


encodeClaimAction : ClaimedAction -> Encode.Value
encodeClaimAction c =
    Encode.object
        [ ( "action_id", Encode.int c.actionId )
        , ( "maker", Eos.encodeName c.maker )
        , ( "proof_photo", Encode.string c.proofPhoto )
        , ( "proof_code", Encode.string c.proofCode )
        , ( "proof_time", Encode.int c.proofTime )
        ]


selectionSet : SelectionSet Action Cambiatus.Object.Action
selectionSet =
    SelectionSet.succeed Action
        |> with ActionObject.id
        |> with ActionObject.description
        |> with ActionObject.reward
        |> with ActionObject.verifierReward
        |> with (Eos.nameSelectionSet ActionObject.creatorId)
        |> with (ActionObject.validators Profile.minimalSelectionSet)
        |> with ActionObject.usages
        |> with ActionObject.usagesLeft
        |> with ActionObject.deadline
        |> with ActionObject.verificationType
        --|> with (SelectionSet.map ActionObject.objective actionObjectiveIdSelectionSet)
        |> with ActionObject.verifications
        |> with ActionObject.isCompleted
        |> with (SelectionSet.map (Maybe.withDefault False) ActionObject.hasProofPhoto)
        |> with (SelectionSet.map (Maybe.withDefault False) ActionObject.hasProofCode)
        |> with ActionObject.photoProofInstructions
        |> with ActionObject.position
