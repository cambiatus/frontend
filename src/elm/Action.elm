module Action exposing
    ( Action
    , ClaimingActionStatus(..)
    , Msg(..)
    , Proof(..)
    , ProofPhotoStatus(..)
    , ReasonToClose(..)
    , claimActionPort
    , getClaimWithPhotoRoute
    , jsAddressToMsg
    , msgToString
    , selectionSet
    , subscriptions
    , update
    , viewClaimButton
    , viewClaimConfirmation
    , viewClaimWithProofs
    , viewSearchActions
    )

import Cambiatus.Enum.VerificationType exposing (VerificationType)
import Cambiatus.Object
import Cambiatus.Object.Action as ActionObject
import Cambiatus.Object.Objective
import Cambiatus.Scalar exposing (DateTime)
import Eos exposing (Symbol)
import Eos.Account as Eos
import File exposing (File)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Attribute, Html, br, button, div, i, input, label, li, p, span, text, ul)
import Html.Attributes exposing (accept, class, classList, disabled, multiple, style, type_)
import Html.Events exposing (on, onClick)
import Http
import Icons
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Ports
import Profile
import Route
import Session.Shared exposing (Translators)
import Sha256 exposing (sha256)
import Time exposing (Posix, posixToMillis)
import UpdateResult as UR
import Utils
import View.Modal as Modal



-- TYPES


type ClaimingActionStatus
    = ConfirmationOpen Action
    | InProgress Action (Maybe Proof)
    | PhotoProofShowed Action Proof
    | Closed


type Proof
    = Proof ProofPhotoStatus (Maybe ProofCode)


type alias ProofCode =
    { code_ : Maybe String
    , claimTimestamp : Int
    , secondsAfterClaim : Int
    , availabilityPeriod : Int
    }


type ProofPhotoStatus
    = NoPhotoAdded
    | Uploading
    | UploadFailed Http.Error
    | Uploaded String


type ReasonToClose
    = CancelClicked
    | TimerEnded


type alias Action =
    { id : Int
    , description : String
    , objective : Objective
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



-- UPDATE


type Msg
    = NoOp
      -- General Claim Messages
    | ClaimConfirmationOpen Action
    | ClaimConfirmationClosed ReasonToClose
    | ActionClaimed Action (Maybe Proof)
    | GotActionClaimedResponse (Result Value String)
      -- Claim with Proof Messages
    | AgreedToClaimWithProof Action
    | GotProofTime Posix
    | AskedForUint64Name
    | GotUint64Name (Result Value String)
    | Tick Time.Posix
    | PhotoAdded (List File)
    | PhotoUploaded (Result Http.Error String)


update : Translators -> Msg -> ClaimingActionStatus -> ClaimingActionStatus
update ({ t } as translators) msg model =
    case ( msg, model ) of
        ( GotProofTime posix, PhotoProofShowed action _ ) ->
            let
                initProofCodeParts =
                    Just
                        { code_ = Nothing
                        , claimTimestamp = Time.posixToMillis posix // 1000
                        , secondsAfterClaim = 0
                        , availabilityPeriod = 30 * 60
                        }
            in
            PhotoProofShowed action (Proof NoPhotoAdded initProofCodeParts)

        ( GotUint64Name (Ok uint64name), PhotoProofShowed action (Proof photoStatus (Just proofCode)) ) ->
            let
                verificationCode =
                    generateVerificationCode action.id uint64name proofCode.claimTimestamp

                newProofCode =
                    Just
                        { proofCode
                            | code_ = Just verificationCode
                        }
            in
            PhotoProofShowed action (Proof photoStatus newProofCode)

        ( Tick timer, PhotoProofShowed action (Proof photoStatus (Just proofCode)) ) ->
            let
                secondsAfterClaim =
                    (Time.posixToMillis timer // 1000) - proofCode.claimTimestamp

                isProofCodeActive =
                    (proofCode.availabilityPeriod - secondsAfterClaim) > 0

                newProofCode =
                    { proofCode
                        | secondsAfterClaim = secondsAfterClaim
                    }
                        |> Just
            in
            if isProofCodeActive then
                PhotoProofShowed action (Proof photoStatus newProofCode)

            else
                update translators (ClaimConfirmationClosed TimerEnded) model

        ( PhotoAdded (_ :: _), PhotoProofShowed action (Proof _ proofCode) ) ->
            PhotoProofShowed action (Proof Uploading proofCode)

        ( PhotoUploaded (Ok url), PhotoProofShowed action (Proof _ proofCode) ) ->
            PhotoProofShowed action (Proof (Uploaded url) proofCode)

        ( PhotoUploaded (Err error), PhotoProofShowed action (Proof _ proofCode) ) ->
            PhotoProofShowed action (Proof (UploadFailed error) proofCode)

        _ ->
            model



-- GRAPHQL


type alias Objective =
    { id : Int
    , description : String
    }


objectiveSelectionSet : SelectionSet Objective Cambiatus.Object.Objective
objectiveSelectionSet =
    SelectionSet.succeed Objective
        |> with Cambiatus.Object.Objective.id
        |> with Cambiatus.Object.Objective.description


selectionSet : SelectionSet Action Cambiatus.Object.Action
selectionSet =
    SelectionSet.succeed Action
        |> with ActionObject.id
        |> with ActionObject.description
        |> with (SelectionSet.map (\o -> { id = o.id, description = o.description }) (ActionObject.objective objectiveSelectionSet))
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



-- VIEW


viewClaimConfirmation : Translators -> ClaimingActionStatus -> Html Msg
viewClaimConfirmation { t } claimConfirmationModalStatus =
    let
        text_ s =
            text (t s)

        modalContent acceptMsg isInProgress =
            div []
                [ Modal.initWith
                    { closeMsg = ClaimConfirmationClosed CancelClicked
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
                                    ClaimConfirmationClosed CancelClicked
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
        ConfirmationOpen action ->
            let
                acceptMsg =
                    if action.hasProofPhoto then
                        AgreedToClaimWithProof action

                    else
                        ActionClaimed action Nothing
            in
            modalContent acceptMsg False

        InProgress _ _ ->
            modalContent NoOp True

        PhotoProofShowed _ _ ->
            text ""

        Closed ->
            text ""


viewClaimButton : Translators -> Maybe Posix -> Action -> Html Msg
viewClaimButton { t } maybeToday action =
    let
        posixDeadline : Posix
        posixDeadline =
            action.deadline
                |> Utils.posixDateTime

        pastDeadline : Bool
        pastDeadline =
            case action.deadline of
                Just _ ->
                    case maybeToday of
                        Just today ->
                            posixToMillis today > posixToMillis posixDeadline

                        Nothing ->
                            False

                Nothing ->
                    False

        isClosed =
            pastDeadline
                || (action.usages > 0 && action.usagesLeft == 0)

        ( buttonMsg, buttonClasses, buttonText ) =
            if isClosed then
                ( NoOp, "button-disabled", "dashboard.closed" )

            else
                ( ClaimConfirmationOpen action, "button button-primary", "dashboard.claim" )
    in
    button
        [ onClick buttonMsg
        , class "self-end button"
        , class buttonClasses
        ]
        [ if action.hasProofPhoto then
            span [ class "inline-block w-4 align-middle mr-2" ] [ Icons.camera "" ]

          else
            text ""
        , span [ class "inline-block align-middle" ] [ text (t buttonText) ]
        ]


viewSearchActions : Translators -> Symbol -> Maybe Posix -> List Action -> Html Msg
viewSearchActions translators symbol maybeToday actions =
    let
        viewAction action =
            if action.isCompleted then
                text ""

            else
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
                            , viewClaimButton translators maybeToday action
                            ]
                        ]
                    ]
    in
    ul [ class "flex px-4 sm:px-2 pt-12 flex-wrap justify-left" ]
        (List.map viewAction actions)


viewClaimWithProofs : Proof -> Translators -> Bool -> Action -> Html Msg
viewClaimWithProofs ((Proof photoStatus proofCode) as proof) ({ t } as translators) isAuth action =
    let
        isUploadingInProgress =
            case photoStatus of
                Uploading ->
                    True

                _ ->
                    False
    in
    div [ class "bg-white border-t border-gray-300" ]
        [ div [ class "container p-4 mx-auto" ]
            [ div [ class "heading-bold leading-7 font-bold" ] [ text <| t "community.actions.proof.title" ]
            , p [ class "mb-4" ]
                [ text (Maybe.withDefault "" action.photoProofInstructions) ]
            , case proofCode of
                Just { code_, secondsAfterClaim, availabilityPeriod } ->
                    case code_ of
                        Just c ->
                            viewProofCode
                                translators
                                c
                                secondsAfterClaim
                                availabilityPeriod

                        _ ->
                            text ""

                _ ->
                    text ""
            , div [ class "mb-4" ]
                [ span [ class "input-label block mb-2" ]
                    [ text (t "community.actions.proof.photo") ]
                , viewPhotoUploader translators photoStatus
                ]
            , div [ class "md:flex" ]
                [ button
                    [ class "modal-cancel"
                    , onClick
                        (if isUploadingInProgress then
                            NoOp

                         else
                            ClaimConfirmationClosed CancelClicked
                        )
                    , classList [ ( "button-disabled", isUploadingInProgress ) ]
                    , disabled isUploadingInProgress
                    ]
                    [ text (t "menu.cancel") ]
                , button
                    [ class "modal-accept"
                    , classList [ ( "button-disabled", isUploadingInProgress ) ]
                    , onClick
                        (if isUploadingInProgress then
                            NoOp

                         else
                            ActionClaimed action (Just proof)
                        )
                    , disabled isUploadingInProgress
                    ]
                    [ text (t "menu.send") ]
                ]
            ]
        ]


viewPhotoUploader : Translators -> ProofPhotoStatus -> Html Msg
viewPhotoUploader { t } proofPhotoStatus =
    let
        uploadedAttrs =
            case proofPhotoStatus of
                Uploaded url ->
                    [ class "bg-no-repeat bg-center bg-cover"
                    , style "background-image" ("url(" ++ url ++ ")")
                    ]

                _ ->
                    []

        onFileChange : (List File -> msg) -> Attribute msg
        onFileChange toMsg =
            Decode.list File.decoder
                |> Decode.at [ "target", "files" ]
                |> Decode.map toMsg
                |> on "change"
    in
    label
        (class "relative bg-purple-500 w-full md:w-2/3 h-56 rounded-sm flex justify-center items-center cursor-pointer"
            :: uploadedAttrs
        )
        [ input
            [ class "hidden-img-input"
            , type_ "file"
            , accept "image/*"
            , onFileChange PhotoAdded
            , multiple False
            ]
            []
        , div []
            [ case proofPhotoStatus of
                Uploading ->
                    div [ class "spinner spinner-light" ] []

                Uploaded _ ->
                    span [ class "absolute bottom-0 right-0 mr-4 mb-4 bg-orange-300 w-8 h-8 p-2 rounded-full" ]
                        [ Icons.camera "" ]

                _ ->
                    div [ class "text-white text-body font-bold text-center" ]
                        [ div [ class "w-10 mx-auto mb-2" ] [ Icons.camera "" ]
                        , div [] [ text (t "community.actions.proof.upload_photo_hint") ]
                        ]
            ]
        ]


viewProofCode : Translators -> String -> Int -> Int -> Html msg
viewProofCode { t } proofCode secondsAfterClaim proofCodeValiditySeconds =
    let
        remainingSeconds =
            proofCodeValiditySeconds - secondsAfterClaim

        timerMinutes =
            remainingSeconds // 60

        timerSeconds =
            remainingSeconds - (timerMinutes * 60)

        toString timeVal =
            if timeVal < 10 then
                "0" ++ String.fromInt timeVal

            else
                String.fromInt timeVal

        timer =
            toString timerMinutes ++ ":" ++ toString timerSeconds
    in
    div [ class "mb-4" ]
        [ span [ class "input-label block mb-1" ]
            [ text (t "community.actions.form.verification_code") ]
        , div [ class "text-2xl text-black font-bold inline-block align-middle mr-2" ]
            [ text proofCode ]
        , span [ class "whitespace-no-wrap text-body rounded-full bg-lightred px-3 py-1 text-white" ]
            [ text (t "community.actions.proof.code_period_label")
            , text " "
            , text timer
            ]
        ]



-- INTEROP


claimActionPort : msg -> String -> ClaimedAction -> Ports.JavascriptOutModel msg
claimActionPort msg contractsCommunity { actionId, maker, proofPhoto, proofCode, proofTime } =
    { responseAddress = msg
    , responseData = Encode.null
    , data =
        Eos.encodeTransaction
            [ { accountName = contractsCommunity
              , name = "claimaction"
              , authorization =
                    { actor = maker
                    , permissionName = Eos.samplePermission
                    }
              , data =
                    { actionId = actionId
                    , maker = maker
                    , proofPhoto = proofPhoto
                    , proofCode = proofCode
                    , proofTime = proofTime
                    }
                        |> encodeClaimAction
              }
            ]
    }


type alias ClaimedAction =
    { actionId : Int
    , maker : Eos.Name
    , proofPhoto : String
    , proofCode : String
    , proofTime : Int
    }


encodeClaimAction : ClaimedAction -> Encode.Value
encodeClaimAction c =
    Encode.object
        [ ( "action_id", Encode.int c.actionId )
        , ( "maker", Eos.encodeName c.maker )
        , ( "proof_photo", Encode.string c.proofPhoto )
        , ( "proof_code", Encode.string c.proofCode )
        , ( "proof_time", Encode.int c.proofTime )
        ]


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "ActionClaimed" :: [] ->
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.field "transactionId" Decode.string |> Decode.map Ok
                    , Decode.succeed (Err val)
                    ]
                )
                val
                |> Result.map (Just << GotActionClaimedResponse)
                |> Result.withDefault Nothing

        "AskedForUint64Name" :: [] ->
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.field "uint64name" Decode.string |> Decode.map Ok
                    , Decode.succeed (Err val)
                    ]
                )
                val
                |> Result.map (Just << GotUint64Name)
                |> Result.withDefault Nothing

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        ClaimConfirmationOpen _ ->
            [ "ClaimConfirmationOpen" ]

        ClaimConfirmationClosed _ ->
            [ "ClaimConfirmationClosed" ]

        ActionClaimed _ _ ->
            [ "ActionClaimed" ]

        AgreedToClaimWithProof _ ->
            [ "ActionWithPhotoLinkClicked" ]

        GotActionClaimedResponse r ->
            [ "GotActionClaimedResponse", UR.resultToString r ]

        Tick _ ->
            [ "Tick" ]

        GotProofTime _ ->
            [ "GotProofTime" ]

        PhotoAdded _ ->
            [ "PhotoAdded" ]

        PhotoUploaded r ->
            [ "PhotoUploaded", UR.resultToString r ]

        AskedForUint64Name ->
            [ "AskedForUint64Name" ]

        GotUint64Name n ->
            [ "GotUint64Name", UR.resultToString n ]



-- HELPERS


generateVerificationCode : Int -> String -> Int -> String
generateVerificationCode actionId makerAccountUint64 proofTimeSeconds =
    (String.fromInt actionId
        ++ makerAccountUint64
        ++ String.fromInt proofTimeSeconds
    )
        |> sha256
        |> String.slice 0 8



-- SUBSCRIPTIONS


subscriptions : ClaimingActionStatus -> Sub Msg
subscriptions model =
    case model of
        PhotoProofShowed _ (Proof _ (Just _)) ->
            Time.every 1000 Tick

        _ ->
            -- No timer needed if there's no proof code.
            Sub.none
