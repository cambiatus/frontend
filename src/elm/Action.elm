module Action exposing
    ( Action
    , ActionFeedback(..)
    , ClaimingActionStatus(..)
    , Community
    , Model
    , Msg(..)
    , Objective
    , Proof(..)
    , isClosed
    , isPastDeadline
    , jsAddressToMsg
    , msgToString
    , selectionSet
    , subscriptions
    , update
    , updateAction
    , viewClaimConfirmation
    , viewClaimWithProofs
    , viewSearchActions
    )

import Cambiatus.Enum.VerificationType as VerificationType exposing (VerificationType)
import Cambiatus.Object
import Cambiatus.Object.Action as ActionObject
import Cambiatus.Object.Community
import Cambiatus.Object.Objective
import Cambiatus.Scalar exposing (DateTime)
import Eos exposing (Symbol)
import Eos.Account as Eos
import Form
import Form.File
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, br, button, div, i, li, p, span, text, ul)
import Html.Attributes exposing (class, classList, disabled, type_)
import Html.Events exposing (onClick)
import Icons
import Json.Decode as Decode
import Json.Encode as Encode
import Markdown exposing (Markdown)
import Ports
import Profile
import Route
import Session.Shared exposing (Shared, Translators)
import Sha256 exposing (sha256)
import Task
import Time
import UpdateResult as UR
import Utils
import View.Feedback as Feedback
import View.Modal as Modal



-- TYPES


type alias Model =
    { status : ClaimingActionStatus
    , feedback : Maybe ActionFeedback
    , needsPinConfirmation : Bool
    }


type ClaimingActionStatus
    = ConfirmationOpen Action
    | ClaimInProgress Action (Maybe { proof : Proof, image : Maybe String })
    | PhotoUploaderShowed Action Proof
    | NotAsked


type ActionFeedback
    = Failure String
    | Success String


type Proof
    = Proof (Form.Model Form.File.Model) (Maybe ProofCode)


type alias ProofCode =
    { code_ : Maybe String
    , claimTimestamp : Int
    , secondsAfterClaim : Int
    , availabilityPeriod : Int
    }


type alias Action =
    { id : Int
    , description : Markdown
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
    , photoProofInstructions : Maybe Markdown
    , position : Maybe Int
    }



-- UPDATE


type Msg
    = NoOp
      -- General Claim Messages
    | ClaimButtonClicked Action
    | ClaimConfirmationClosed
    | ActionClaimed Action (Maybe { proof : Proof, image : Maybe String })
    | GotActionClaimedResponse (Result Encode.Value String)
      -- Claim with Proof Messages
    | AgreedToClaimWithProof Action
    | GotProofTime Time.Posix
    | AskedForUint64Name
    | GotUint64Name (Result Encode.Value String)
    | GotFormMsg (Form.Msg Form.File.Model)
    | Tick Time.Posix


update :
    Bool
    -> Shared
    -> Symbol
    -> Eos.Name
    -> Msg
    -> Model
    -> UR.UpdateResult Model Msg Feedback.Model
update isPinConfirmed shared selectedCommunity accName msg model =
    let
        { t, tr } =
            shared.translators

        claimOrAskForPin actionId photoUrl code time m =
            if isPinConfirmed then
                m
                    |> UR.init
                    |> UR.addPort
                        (claimActionPort
                            msg
                            shared.contracts.community
                            { actionId = actionId
                            , maker = accName
                            , proofPhoto = photoUrl
                            , proofCode = code
                            , proofTime = time
                            }
                        )

            else
                m |> UR.init
    in
    case ( msg, model.status ) of
        ( ClaimButtonClicked action, _ ) ->
            { model
                | status = ConfirmationOpen action
                , feedback = Nothing
            }
                |> UR.init

        ( ActionClaimed action Nothing, _ ) ->
            { model
                | status = ClaimInProgress action Nothing
                , feedback = Nothing
                , needsPinConfirmation = not isPinConfirmed
            }
                |> claimOrAskForPin action.id "" "" 0

        ( ActionClaimed action (Just proofRecord), _ ) ->
            let
                ( proofCode, time ) =
                    case proofRecord.proof of
                        Proof _ (Just { code_, claimTimestamp }) ->
                            ( Maybe.withDefault "" code_, claimTimestamp )

                        Proof _ Nothing ->
                            ( "", 0 )
            in
            case proofRecord.image of
                Just image ->
                    { model
                        | status = ClaimInProgress action (Just proofRecord)
                        , feedback = Nothing
                        , needsPinConfirmation = not isPinConfirmed
                    }
                        |> claimOrAskForPin action.id image proofCode time

                Nothing ->
                    { model
                        | feedback = Failure (t "community.actions.proof.no_upload_error") |> Just
                        , needsPinConfirmation = False
                    }
                        |> UR.init

        ( AgreedToClaimWithProof action, _ ) ->
            { model
                | status =
                    Proof (Form.init (Form.File.initModel Nothing)) Nothing
                        |> PhotoUploaderShowed action
                , feedback = Nothing
                , needsPinConfirmation = False
            }
                |> UR.init
                |> UR.addCmd (Task.perform GotProofTime Time.now)

        ( GotActionClaimedResponse (Ok _), _ ) ->
            let
                feedback =
                    tr "dashboard.check_claim.success" [ ( "symbolCode", Eos.symbolToSymbolCodeString selectedCommunity ) ]
                        |> Success
            in
            { model
                | status = NotAsked
                , feedback = Just feedback
                , needsPinConfirmation = False
            }
                |> UR.init
                |> UR.addCmd
                    (Eos.nameToString accName
                        |> Route.ProfileClaims
                        |> Route.pushUrl shared.navKey
                    )

        ( GotActionClaimedResponse (Err val), _ ) ->
            let
                feedback =
                    Failure (t "dashboard.check_claim.failure")
            in
            { model
                | status = NotAsked
                , feedback = Just feedback
                , needsPinConfirmation = False
            }
                |> UR.init
                |> UR.logJsonValue msg
                    (Just accName)
                    "Got an error when claiming an action"
                    { moduleName = "Action", function = "update" }
                    []
                    val

        ( ClaimConfirmationClosed, _ ) ->
            { model
                | status = NotAsked
                , feedback = Nothing
                , needsPinConfirmation = False
            }
                |> UR.init

        ( GotProofTime posix, PhotoUploaderShowed action _ ) ->
            let
                initProofCodeParts =
                    Just
                        { code_ = Nothing
                        , claimTimestamp = Time.posixToMillis posix // 1000
                        , secondsAfterClaim = 0
                        , availabilityPeriod = 30 * 60
                        }
            in
            { model
                | status =
                    Proof (Form.init (Form.File.initModel Nothing)) initProofCodeParts
                        |> PhotoUploaderShowed action
                , feedback = Nothing
                , needsPinConfirmation = False
            }
                |> UR.init
                |> UR.addPort
                    { responseAddress = AskedForUint64Name
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "accountNameToUint64" )
                            , ( "accountName", Encode.string (Eos.nameToString accName) )
                            ]
                    }

        ( GotUint64Name (Ok uint64Name), PhotoUploaderShowed action (Proof photoStatus (Just proofCode)) ) ->
            let
                verificationCode =
                    generateVerificationCode action.id uint64Name proofCode.claimTimestamp

                newProofCode =
                    Just
                        { proofCode
                            | code_ = Just verificationCode
                        }
            in
            { model
                | status = PhotoUploaderShowed action (Proof photoStatus newProofCode)
                , feedback = Nothing
                , needsPinConfirmation = False
            }
                |> UR.init

        ( GotUint64Name (Err err), _ ) ->
            { model
                | feedback = Just <| Failure "Failed while creating proof code."
                , needsPinConfirmation = False
            }
                |> UR.init
                |> UR.logJsonValue msg
                    (Just accName)
                    "Got error when converting name to Uint64"
                    { moduleName = "Action", function = "update" }
                    []
                    err

        ( GotFormMsg subMsg, ClaimInProgress action (Just proofRecord) ) ->
            let
                formModel =
                    case proofRecord.proof of
                        Proof formModel_ _ ->
                            formModel_
            in
            Form.update shared subMsg formModel
                |> UR.fromChild
                    (\newForm ->
                        { model
                            | status =
                                ClaimInProgress action
                                    (Just
                                        { proofRecord
                                            | proof =
                                                case proofRecord.proof of
                                                    Proof _ proofCode ->
                                                        Proof newForm proofCode
                                        }
                                    )
                        }
                    )
                    GotFormMsg
                    UR.addExt
                    model

        ( GotFormMsg subMsg, PhotoUploaderShowed action (Proof formModel proofCode) ) ->
            Form.update shared subMsg formModel
                |> UR.fromChild
                    (\newForm ->
                        { model | status = PhotoUploaderShowed action (Proof newForm proofCode) }
                    )
                    GotFormMsg
                    UR.addExt
                    model

        ( Tick timer, PhotoUploaderShowed action (Proof photoStatus (Just proofCode)) ) ->
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
            (if isProofCodeActive then
                { model
                    | status = PhotoUploaderShowed action (Proof photoStatus newProofCode)
                    , needsPinConfirmation = False
                }

             else
                { model
                    | status = NotAsked
                    , feedback = Failure (t "community.actions.proof.time_expired") |> Just
                    , needsPinConfirmation = False
                }
            )
                |> UR.init

        _ ->
            { model
                | needsPinConfirmation = False
            }
                |> UR.init



-- GRAPHQL


type alias Community =
    { symbol : Symbol }


communitySelectionSet : SelectionSet Community Cambiatus.Object.Community
communitySelectionSet =
    SelectionSet.succeed Community
        |> with (Eos.symbolSelectionSet Cambiatus.Object.Community.symbol)


type alias Objective =
    { id : Int
    , description : Markdown
    , community : Community
    , isCompleted : Bool
    }


objectiveSelectionSet : SelectionSet Objective Cambiatus.Object.Objective
objectiveSelectionSet =
    SelectionSet.succeed Objective
        |> with Cambiatus.Object.Objective.id
        |> with (Markdown.selectionSet Cambiatus.Object.Objective.description)
        |> with (Cambiatus.Object.Objective.community communitySelectionSet)
        |> with Cambiatus.Object.Objective.isCompleted


selectionSet : SelectionSet Action Cambiatus.Object.Action
selectionSet =
    SelectionSet.succeed Action
        |> with ActionObject.id
        |> with (Markdown.selectionSet ActionObject.description)
        |> with
            (SelectionSet.map
                (\o ->
                    { id = o.id
                    , description = o.description
                    , community = o.community
                    , isCompleted = o.isCompleted
                    }
                )
                (ActionObject.objective objectiveSelectionSet)
            )
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
        |> with (Markdown.maybeSelectionSet ActionObject.photoProofInstructions)
        |> with ActionObject.position



-- VIEW


viewClaimConfirmation : Translators -> Model -> Html Msg
viewClaimConfirmation { t } model =
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
    case model.status of
        ConfirmationOpen action ->
            let
                acceptMsg =
                    if action.hasProofPhoto then
                        AgreedToClaimWithProof action

                    else
                        ActionClaimed action Nothing
            in
            modalContent acceptMsg False

        ClaimInProgress _ _ ->
            text ""

        PhotoUploaderShowed _ _ ->
            text ""

        NotAsked ->
            text ""


viewClaimButton : Translators -> Time.Posix -> Action -> Html Msg
viewClaimButton { t } now action =
    let
        ( buttonMsg, buttonClasses, buttonText ) =
            if isClosed action now then
                ( NoOp, "button-disabled", "dashboard.closed" )

            else
                ( ClaimButtonClicked action, "button button-primary", "dashboard.claim" )
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


viewSearchActions : Translators -> Time.Posix -> List Action -> Html Msg
viewSearchActions ({ t } as translators) today actions =
    let
        viewAction action =
            if action.isCompleted then
                text ""

            else
                li [ class "relative mb-10 w-full sm:px-2 sm:w-1/2 lg:w-1/3" ]
                    [ i [ class "absolute top-0 left-0 right-0 -mt-6" ] [ Icons.flag "w-full fill-current text-green" ]
                    , div [ class "px-4 pt-8 pb-4 text-sm font-light bg-purple-500 rounded-lg text-white" ]
                        [ Markdown.view [ class "mb-8" ] action.description
                        , div [ class "flex justify-between" ]
                            [ p []
                                [ text (t "menu.search.gain")
                                , br [] []
                                , span [ class "text-green font-semibold" ] [ text <| String.fromFloat action.reward ]
                                , text " "
                                , text <| Eos.symbolToSymbolCodeString action.objective.community.symbol
                                ]
                            , viewClaimButton translators today action
                            ]
                        ]
                    ]
    in
    ul [ class "flex px-4 sm:px-2 pt-12 flex-wrap justify-left" ]
        (List.map viewAction actions)


claimWithProofsForm : Translators -> Form.Form msg Form.File.Model String
claimWithProofsForm translators =
    Form.File.init
        { label = translators.t "community.actions.proof.upload"
        , id = "proof-photo-uploader"
        }
        |> Form.File.withContainerAttrs [ class "mb-4 md:w-2/3" ]
        |> Form.File.withFileTypes [ Form.File.Image, Form.File.PDF ]
        |> Form.file
            { translators = translators
            , value = identity
            , update = \newValue _ -> newValue
            , externalError = always Nothing
            }


viewClaimWithProofs : Proof -> Translators -> Bool -> Action -> Html Msg
viewClaimWithProofs ((Proof photoStatus proofCode) as proof) ({ t } as translators) isLoading action =
    div [ class "bg-white border-t border-gray-300" ]
        [ div [ class "container p-4 mx-auto" ]
            [ div [ class "text-lg font-bold my-3" ] [ text <| t "community.actions.proof.title" ]
            , action.photoProofInstructions
                |> Maybe.withDefault Markdown.empty
                |> Markdown.view [ class "mb-4" ]
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
            , Form.view []
                translators
                (\submitButton ->
                    [ div [ class "md:flex" ]
                        [ button
                            [ class "modal-cancel"
                            , type_ "button"
                            , onClick ClaimConfirmationClosed
                            , disabled isLoading
                            ]
                            [ text (t "menu.cancel") ]
                        , submitButton
                            [ class "modal-accept"
                            , disabled isLoading
                            ]
                            [ text (t "menu.send") ]
                        ]
                    ]
                )
                (claimWithProofsForm translators)
                photoStatus
                { toMsg = GotFormMsg
                , onSubmit =
                    \image ->
                        ActionClaimed action (Just { proof = proof, image = Just image })
                }
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
        [ span [ class "label block" ]
            [ text (t "community.actions.form.verification_code") ]
        , div [ class "text-2xl text-black font-bold inline-block align-middle mr-2" ]
            [ text proofCode ]
        , span [ class "whitespace-nowrap rounded-full bg-lightred px-3 py-1 text-white" ]
            [ text (t "community.actions.proof.code_period_label")
            , text " "
            , text timer
            ]
        ]



-- INTEROP


encode : Action -> Encode.Value
encode action =
    let
        makeAsset : Float -> Eos.Asset
        makeAsset amount =
            { symbol = action.objective.community.symbol, amount = amount }
    in
    Encode.object
        [ ( "action_id", Encode.int action.id )
        , ( "objective_id", Encode.int action.objective.id )
        , ( "description", Markdown.encode action.description )
        , ( "reward", Eos.encodeAsset (makeAsset action.reward) )
        , ( "verifier_reward", Eos.encodeAsset (makeAsset action.verifierReward) )
        , ( "deadline"
          , Utils.fromMaybeDateTime action.deadline
                |> Time.posixToMillis
                |> Encode.int
          )
        , ( "usages", Encode.int action.usages )
        , ( "usages_left", Encode.int action.usagesLeft )
        , ( "verifications", Encode.int action.verifications )
        , ( "verification_type"
          , action.verificationType
                |> VerificationType.toString
                |> String.toLower
                |> Encode.string
          )
        , ( "validators_str"
          , action.validators
                |> List.map (\v -> Eos.nameToString v.account)
                |> String.join "-"
                |> Encode.string
          )
        , ( "is_completed", Eos.encodeEosBool (Eos.boolToEosBool action.isCompleted) )
        , ( "creator", Eos.encodeName action.creator )
        , ( "has_proof_photo", Eos.encodeEosBool (Eos.boolToEosBool action.hasProofPhoto) )
        , ( "has_proof_code", Eos.encodeEosBool (Eos.boolToEosBool action.hasProofCode) )
        , ( "photo_proof_instructions", Markdown.encode (Maybe.withDefault Markdown.empty action.photoProofInstructions) )
        ]


updateAction : Eos.Name -> Shared -> Action -> Eos.Action
updateAction accountName shared action =
    { accountName = shared.contracts.community
    , name = "upsertaction"
    , authorization =
        { actor = accountName
        , permissionName = Eos.samplePermission
        }
    , data = encode action
    }


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


jsAddressToMsg : List String -> Encode.Value -> Maybe Msg
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

        ClaimButtonClicked _ ->
            [ "ClaimButtonClicked" ]

        ClaimConfirmationClosed ->
            [ "ClaimConfirmationClosed" ]

        ActionClaimed _ _ ->
            [ "ActionClaimed" ]

        AgreedToClaimWithProof _ ->
            [ "AgreedToClaimWithProof" ]

        GotActionClaimedResponse r ->
            [ "GotActionClaimedResponse", UR.resultToString r ]

        Tick _ ->
            [ "Tick" ]

        GotProofTime _ ->
            [ "GotProofTime" ]

        AskedForUint64Name ->
            [ "AskedForUint64Name" ]

        GotUint64Name n ->
            [ "GotUint64Name", UR.resultToString n ]

        GotFormMsg subMsg ->
            "GotFormMsg" :: Form.msgToString subMsg



-- HELPERS


generateVerificationCode : Int -> String -> Int -> String
generateVerificationCode actionId makerAccountUint64 proofTimeSeconds =
    (String.fromInt actionId
        ++ makerAccountUint64
        ++ String.fromInt proofTimeSeconds
    )
        |> sha256
        |> String.slice 0 8


isPastDeadline : Action -> Time.Posix -> Bool
isPastDeadline action now =
    case action.deadline of
        Just _ ->
            Time.posixToMillis now > Time.posixToMillis (Utils.fromMaybeDateTime action.deadline)

        Nothing ->
            False


isClosed : Action -> Time.Posix -> Bool
isClosed action now =
    isPastDeadline action now
        || (action.usages > 0 && action.usagesLeft == 0)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.status of
        PhotoUploaderShowed _ (Proof _ (Just _)) ->
            Time.every 1000 Tick

        _ ->
            -- No timer needed if there's no proof code.
            Sub.none
