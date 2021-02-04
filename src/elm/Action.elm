module Action exposing
    ( Action
    , ClaimConfirmationModalStatus(..)
    , Model
    , Msg(..)
    , claimActionPort
    , encodeClaimAction
    , getClaimWithPhotoRoute
    , initClaimingActionModel
    , jsAddressToMsg
    , msgToString
    , selectionSet
    , update
    , viewClaimButton
    , viewClaimConfirmation
    , viewSearchActions
    )

import Cambiatus.Enum.VerificationType exposing (VerificationType)
import Cambiatus.Object
import Cambiatus.Object.Action as ActionObject
import Cambiatus.Object.Objective
import Cambiatus.Scalar exposing (DateTime)
import Eos exposing (Symbol)
import Eos.Account as Eos exposing (Name)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, a, br, button, div, i, li, p, span, text, ul)
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


initClaimingActionModel : Action -> Model
initClaimingActionModel action =
    { claimConfirmationModalStatus = Open action
    , action = action
    }


type Msg
    = NoOp
    | ClaimConfirmationOpen Action
    | ClaimConfirmationClosed
    | ActionClaimed
    | GotActionClaimedResponse (Result Value String)
    | ActionWithPhotoLinkClicked Route.Route


getClaimWithPhotoRoute : Eos.Symbol -> Int -> Int -> Route.Route
getClaimWithPhotoRoute community objectiveId actionId =
    Route.ClaimAction
        community
        objectiveId
        actionId


viewClaimConfirmation : Eos.Symbol -> Translators -> ClaimConfirmationModalStatus -> Html Msg
viewClaimConfirmation symbol { t } claimConfirmationModalStatus =
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
                        ActionWithPhotoLinkClicked (getClaimWithPhotoRoute symbol action.objectiveId action.id)

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
        NoOp ->
            model

        ClaimConfirmationOpen action ->
            { model | claimConfirmationModalStatus = Open action }

        ActionClaimed ->
            { model | claimConfirmationModalStatus = InProgress }

        ClaimConfirmationClosed ->
            { model | claimConfirmationModalStatus = Closed }

        GotActionClaimedResponse (Ok _) ->
            { model | claimConfirmationModalStatus = Closed }

        GotActionClaimedResponse (Err _) ->
            { model | claimConfirmationModalStatus = Closed }

        ActionWithPhotoLinkClicked _ ->
            { model | claimConfirmationModalStatus = Closed }


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

        ActionWithPhotoLinkClicked _ ->
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
            [ --Route.href (getClaimWithPhotoRoute symbol action.objectiveId action.id)
              onClick (ActionWithPhotoLinkClicked (getClaimWithPhotoRoute symbol action.objectiveId action.id))
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


viewSearchActions : Symbol -> List Action -> Html Msg
viewSearchActions symbol actions =
    let
        viewAction action =
            li [ class "relative mb-10 w-full sm:px-2 sm:w-1/2 lg:w-1/3" ]
                [ i [ class "absolute top-0 left-0 right-0 -mt-6" ] [ Icons.flag "w-full fill-green" ]
                , div [ class "px-4 pt-8 pb-4 text-sm font-light bg-purple-500 rounded-lg text-white" ]
                    [ p [ class "mb-8" ] [ text action.description ]
                    , div [ class "flex justify-between" ]
                        [ p []
                            [ text "You gain"
                            , br [] []
                            , span [ class "text-green font-medium" ] [ text <| String.fromFloat action.reward ]
                            , text " "
                            , text <| Eos.symbolToSymbolCodeString symbol
                            ]
                        , viewClaimButton action symbol
                        ]
                    ]
                ]
    in
    ul [ class "flex px-4 sm:px-2 pt-12 flex-wrap justify-left" ]
        (List.map viewAction actions)
