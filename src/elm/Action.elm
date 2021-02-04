module Action exposing
    ( Action
    , ClaimConfirmationModalStatus(..)
    , Model
    , Msg(..)
    , claimActionPort
    , encodeClaimAction
    , getClaimWithPhotoRoute
    , initModel
    , jsAddressToMsg
    , msgToString
    , selectionSet
    , update
    , viewClaimButton
    , viewClaimConfirmation
    )

import Cambiatus.Enum.VerificationType exposing (VerificationType)
import Cambiatus.Object
import Cambiatus.Object.Action as ActionObject
import Cambiatus.Object.Objective
import Cambiatus.Scalar exposing (DateTime)
import Eos
import Eos.Account as Eos exposing (Name)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, a, button, div, span, text)
import Html.Attributes exposing (class, classList, disabled)
import Html.Events exposing (onClick)
import Icons
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Ports
import Profile
import Route
import Session.Shared exposing (Translators)
import UpdateResult as UR
import View.Modal as Modal


type ClaimConfirmationModalStatus
    = Open Action
    | InProgress
    | Closed


type alias Action =
    { id : Int
    , objectiveId : Int
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


type alias ObjectiveId =
    Int


initModel : Action -> Model
initModel action =
    { claimConfirmationModalStatus = Closed
    , action = action
    }


type Msg
    = NoOp
    | ClaimConfirmationOpen Action
    | ClaimConfirmationClosed
    | ActionClaimed
    | GotActionClaimedResponse (Result Value String)
    | ActionWithPhotoLinkClicked


getClaimWithPhotoRoute : Eos.Symbol -> Int -> Int -> Route.Route
getClaimWithPhotoRoute community objectiveId actionId =
    Route.ClaimAction
        community
        objectiveId
        actionId


viewClaimConfirmation : Translators -> ClaimConfirmationModalStatus -> Html Msg
viewClaimConfirmation { t } claimConfirmationModalStatus =
    let
        text_ s =
            text (t s)

        modalContent acceptMsg isInProgress =
            div []
                [ Modal.initWith
                    { closeMsg = ClaimConfirmationClosed
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
                                    ClaimConfirmationClosed
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
                        ActionClaimed
            in
            modalContent acceptMsg False

        InProgress ->
            modalContent NoOp True

        Closed ->
            text ""



-- UPDATE


update : Translators -> Msg -> Model -> Model
update { t } msg model =
    case msg of
        -- TODO: this update function looks to simple to have it here...
        ClaimConfirmationOpen action ->
            { model | claimConfirmationModalStatus = Open action }

        ClaimConfirmationClosed ->
            { model | claimConfirmationModalStatus = Closed }

        ActionClaimed ->
            { model | claimConfirmationModalStatus = InProgress }

        GotActionClaimedResponse (Ok _) ->
            { model | claimConfirmationModalStatus = Closed }

        GotActionClaimedResponse (Err _) ->
            { model | claimConfirmationModalStatus = Closed }

        ActionWithPhotoLinkClicked ->
            model

        NoOp ->
            model


claimActionPort : msg -> Action -> String -> Name -> Ports.JavascriptOutModel msg
claimActionPort msg action contractsCommunity accountName =
    { responseAddress = msg
    , responseData = Encode.null
    , data =
        Eos.encodeTransaction
            [ { accountName = contractsCommunity
              , name = "claimaction"
              , authorization =
                    { actor = accountName
                    , permissionName = Eos.samplePermission
                    }
              , data =
                    { actionId = action.id
                    , maker = accountName
                    , proofPhoto = ""
                    , proofCode = ""
                    , proofTime = 0
                    }
                        |> encodeClaimAction
              }
            ]
    }


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
                |> Result.map (Just << GotActionClaimedResponse)
                |> Result.withDefault Nothing

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        ClaimConfirmationOpen _ ->
            [ "OpenClaimConfirmation" ]

        ClaimConfirmationClosed ->
            [ "CloseClaimConfirmation" ]

        ActionClaimed ->
            [ "ClaimAction" ]

        ActionWithPhotoLinkClicked ->
            [ "ActionWithPhotoLinkClicked" ]

        GotActionClaimedResponse r ->
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


type alias Objective =
    { id : Int
    }


objectiveSelectionSet : SelectionSet Objective Cambiatus.Object.Objective
objectiveSelectionSet =
    SelectionSet.succeed Objective
        |> with Cambiatus.Object.Objective.id


selectionSet : SelectionSet Action Cambiatus.Object.Action
selectionSet =
    SelectionSet.succeed Action
        |> with ActionObject.id
        |> with (SelectionSet.map (\s -> s.id) (ActionObject.objective objectiveSelectionSet))
        |> with ActionObject.description
        |> with ActionObject.reward
        |> with ActionObject.verifierReward
        |> with (Eos.nameSelectionSet ActionObject.creatorId)
        |> with (ActionObject.validators Profile.minimalSelectionSet)
        |> with ActionObject.usages
        |> with ActionObject.usagesLeft
        |> with ActionObject.deadline
        |> with ActionObject.verificationType
        |> with ActionObject.verifications
        |> with ActionObject.isCompleted
        |> with (SelectionSet.map (Maybe.withDefault False) ActionObject.hasProofPhoto)
        |> with (SelectionSet.map (Maybe.withDefault False) ActionObject.hasProofCode)
        |> with ActionObject.photoProofInstructions
        |> with ActionObject.position


viewClaimButton : Action -> Eos.Symbol -> Html Msg
viewClaimButton action symbol =
    -- TODO: Handle action.deadline and action.isCompleted.
    if action.hasProofPhoto then
        a
            [ Route.href (getClaimWithPhotoRoute symbol action.objectiveId action.id)
            , onClick ActionWithPhotoLinkClicked
            , class "self-end button button-primary"
            ]
            [ span [ class "inline-block w-4 align-middle mr-2" ] [ Icons.camera "" ]
            , span [ class "inline-block align-middle" ] [ text "Claim" ]
            ]

    else
        button
            [ onClick (ClaimConfirmationOpen action)
            , class "self-end button button-primary"
            ]
            [ span [ class "inline-block align-middle" ] [ text "Claim" ]
            ]
