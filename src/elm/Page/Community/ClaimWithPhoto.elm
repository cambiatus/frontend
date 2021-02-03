module Page.Community.ClaimWithPhoto exposing (..)

import Action exposing (Action, ClaimConfirmationModalStatus(..))
import Api
import Eos exposing (Symbol)
import Eos.Account as Eos
import File exposing (File)
import Html exposing (Html, button, div, input, label, p, span, text)
import Html.Attributes exposing (accept, class, classList, disabled, multiple, style, type_)
import Html.Events exposing (onClick)
import Http
import Icons
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Page
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Translators)
import Sha256 exposing (sha256)
import Task
import Time exposing (Posix)
import UpdateResult as UR


type alias Model =
    { claimConfirmationModalStatus : Action.ClaimConfirmationModalStatus
    , action : Maybe Action -- TODO: Use result since action must be loaded
    , proof : Proof
    }


type alias ObjectiveId =
    Int


type alias ActionId =
    Int


init : LoggedIn.Model -> Symbol -> ObjectiveId -> Maybe ActionId -> ( Model, Cmd Msg )
init loggedIn symbol objectiveId actionId =
    ( { claimConfirmationModalStatus = Action.Closed
      , action = Nothing
      , proof = Proof NoPhotoAdded Nothing
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | OpenProofSection Action
    | CloseProofSection ReasonToCloseProofSection
    | GotProofTime Int Posix
    | GetUint64Name String
    | GotUint64Name (Result Value String)
    | Tick Time.Posix
    | EnteredPhoto (List File)
    | CompletedPhotoUpload (Result Http.Error String)
    | ClaimAction Action
    | GotClaimActionResponse (Result Value String)


type Proof
    = Proof ProofPhotoStatus (Maybe ProofCode)


type alias ProofCode =
    { code : Maybe String
    , claimTimestamp : Int
    , secondsAfterClaim : Int
    , availabilityPeriod : Int
    }


type ProofPhotoStatus
    = NoPhotoAdded
    | Uploading
    | UploadFailed Http.Error
    | Uploaded String


type ReasonToCloseProofSection
    = CancelClicked
    | TimerExpired


viewPhotoUploader : Translators -> ProofPhotoStatus -> Html Msg
viewPhotoUploader { t } proofPhotoStatus =
    let
        uploadedAttrs =
            case proofPhotoStatus of
                Uploaded url ->
                    [ class " bg-no-repeat bg-center bg-cover"
                    , style "background-image" ("url(" ++ url ++ ")")
                    ]

                _ ->
                    []
    in
    label
        (class "relative bg-purple-500 w-full md:w-2/3 h-56 rounded-sm flex justify-center items-center cursor-pointer"
            :: uploadedAttrs
        )
        [ input
            [ class "hidden-img-input"
            , type_ "file"
            , accept "image/*"
            , Page.onFileChange EnteredPhoto
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


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    { title = "Claim with photo"
    , content =
        case model.action of
            Just a ->
                viewClaimWithProofs model.proof loggedIn.shared.translators a

            Nothing ->
                text "this is claim with photo page"
    }


viewClaimWithProofs : Proof -> Translators -> Action -> Html Msg
viewClaimWithProofs proofs translators action =
    let
        { t } =
            translators

        isUploadingInProgress =
            case proofs of
                Proof Uploading _ ->
                    True

                _ ->
                    False
    in
    div [ class "bg-white border-t border-gray-300" ]
        [ div [ class "container p-4 mx-auto" ]
            [ div [ class "heading-bold leading-7 font-bold" ] [ text <| t "community.actions.proof.title" ]
            , p [ class "mb-4" ]
                [ text <|
                    Maybe.withDefault "" action.photoProofInstructions
                ]
            , case proofs of
                Proof _ (Just { code, secondsAfterClaim, availabilityPeriod }) ->
                    case code of
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
                , case proofs of
                    Proof photoStatus _ ->
                        viewPhotoUploader translators photoStatus
                ]
            , div [ class "md:flex" ]
                [ button
                    [ class "modal-cancel"
                    , onClick
                        (if isUploadingInProgress then
                            NoOp

                         else
                            CloseProofSection CancelClicked
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
                            ClaimAction action
                        )
                    , disabled isUploadingInProgress
                    ]
                    [ text (t "menu.send") ]
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick


update : LoggedIn.Model -> Msg -> Model -> UR.UpdateResult Model Msg (External Msg)
update ({ shared } as loggedIn) msg model =
    let
        { t } =
            shared.translators
    in
    case msg of
        OpenProofSection action ->
            let
                runProofCodeTimer =
                    Task.perform (GotProofTime action.id) Time.now
            in
            if action.hasProofPhoto then
                { model
                    | claimConfirmationModalStatus = Action.Closed
                    , proof = Proof NoPhotoAdded Nothing
                }
                    |> UR.init
                    |> UR.addCmd
                        (if action.hasProofCode then
                            runProofCodeTimer

                         else
                            Cmd.none
                        )
                    |> UR.addPort
                        { responseAddress = NoOp
                        , responseData = Encode.null
                        , data =
                            Encode.object
                                [ ( "id", Encode.string "communityPage" )
                                , ( "name", Encode.string "scrollIntoView" )
                                ]
                        }

            else
                model |> UR.init

        EnteredPhoto (file :: _) ->
            let
                uploadImage =
                    Api.uploadImage shared file CompletedPhotoUpload

                newProof =
                    case model.proof of
                        Proof _ proofCode ->
                            Proof Uploading proofCode
            in
            { model
                | proof = newProof
            }
                |> UR.init
                |> UR.addCmd uploadImage
                |> UR.addExt HideFeedback

        EnteredPhoto [] ->
            model
                |> UR.init

        CompletedPhotoUpload (Ok url) ->
            let
                newProofs =
                    case model.proof of
                        Proof _ proofCode ->
                            Proof (Uploaded url) proofCode
            in
            { model | proof = newProofs }
                |> UR.init

        CompletedPhotoUpload (Err error) ->
            let
                newProofs =
                    case model.proof of
                        Proof _ proofCode ->
                            Proof (UploadFailed error) proofCode
            in
            { model | proof = newProofs }
                |> UR.init
                |> UR.logHttpError msg error

        CloseProofSection reason ->
            { model
                | claimConfirmationModalStatus = Action.Closed
            }
                -- TODO: Show expired message
                |> UR.init
                |> UR.addExt
                    (case reason of
                        TimerExpired ->
                            ShowFeedback LoggedIn.Failure (t "community.actions.proof.time_expired")

                        CancelClicked ->
                            HideFeedback
                    )

        GetUint64Name _ ->
            model |> UR.init

        GotUint64Name (Ok uint64name) ->
            case ( model.proof, model.action ) of
                ( Proof proofPhoto (Just proofCode), Just action ) ->
                    let
                        verificationCode =
                            generateVerificationCode action.id uint64name proofCode.claimTimestamp

                        newProofCode =
                            Just
                                { proofCode
                                    | code = Just verificationCode
                                }
                    in
                    { model | proof = Proof proofPhoto newProofCode }
                        |> UR.init

                _ ->
                    model
                        |> UR.init

        GotUint64Name (Err _) ->
            model |> UR.init

        Tick timer ->
            case model.proof of
                Proof proofPhoto (Just proofCode) ->
                    let
                        secondsAfterClaim =
                            (Time.posixToMillis timer // 1000) - proofCode.claimTimestamp

                        isProofCodeActive =
                            (proofCode.availabilityPeriod - secondsAfterClaim) > 0
                    in
                    if isProofCodeActive then
                        let
                            newProofCode =
                                Just
                                    { proofCode
                                        | secondsAfterClaim = secondsAfterClaim
                                    }
                        in
                        { model | proof = Proof proofPhoto newProofCode } |> UR.init

                    else
                        update loggedIn (CloseProofSection TimerExpired) model

                _ ->
                    model |> UR.init

        GotProofTime actionId posix ->
            let
                initProofCodeParts =
                    Just
                        { code = Nothing
                        , claimTimestamp = Time.posixToMillis posix // 1000
                        , secondsAfterClaim = 0
                        , availabilityPeriod = 30 * 60
                        }
            in
            { model | proof = Proof NoPhotoAdded initProofCodeParts }
                |> UR.init
                |> UR.addPort
                    { responseAddress = GetUint64Name (Eos.nameToString loggedIn.accountName)
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "accountNameToUint64" )
                            , ( "accountName", Encode.string (Eos.nameToString loggedIn.accountName) )
                            ]
                    }

        ClaimAction action ->
            -- TODO: Remove duplication in port
            let
                hasPhotoError =
                    case model.proof of
                        Proof (Uploaded _) _ ->
                            False

                        Proof _ _ ->
                            -- Error: photo wasn't uploaded while claiming with proof
                            True

                newModel =
                    case model.proof of
                        Proof _ _ ->
                            -- Claim with proof has no confirmation
                            model

                ( proofPhotoUrl, proofCode_, proofTime ) =
                    case model.proof of
                        Proof (Uploaded url) (Just { code, claimTimestamp }) ->
                            ( url, Maybe.withDefault "" code, claimTimestamp )

                        Proof (Uploaded url) Nothing ->
                            ( url, "", 0 )

                        _ ->
                            ( "", "", 0 )
            in
            if hasPhotoError then
                model
                    |> UR.init
                    |> UR.addExt (ShowFeedback LoggedIn.Failure (t "community.actions.proof.no_photo_error"))

            else if LoggedIn.isAuth loggedIn then
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
                                        , proofPhoto = proofPhotoUrl
                                        , proofCode = proofCode_
                                        , proofTime = proofTime
                                        }
                                            |> Action.encodeClaimAction
                                  }
                                ]
                        }

            else
                newModel
                    |> UR.init
                    |> UR.addExt (Just (ClaimAction action) |> RequiredAuthentication)

        NoOp ->
            model |> UR.init

        GotClaimActionResponse (Ok _) ->
            -- TODO: remove duplicates
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
            -- TODO: remove duplicates
            { model
                | claimConfirmationModalStatus = Closed
            }
                |> UR.init
                |> UR.addExt (ShowFeedback LoggedIn.Failure (t "dashboard.check_claim.failure"))


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    -- TODO: Remove duplicates
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

        "GetUint64Name" :: [] ->
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
        Tick _ ->
            [ "Tick" ]

        GotProofTime _ _ ->
            [ "GotProofTime" ]

        OpenProofSection _ ->
            [ "OpenProofSection" ]

        CloseProofSection _ ->
            [ "CloseProofSection" ]

        EnteredPhoto _ ->
            [ "EnteredPhoto" ]

        CompletedPhotoUpload r ->
            [ "CompletedPhotoUpload", UR.resultToString r ]

        GetUint64Name _ ->
            [ "GetUint64Name" ]

        GotUint64Name n ->
            [ "GotUint64Name", UR.resultToString n ]

        GotClaimActionResponse r ->
            [ "GotClaimActionResponse", UR.resultToString r ]

        NoOp ->
            [ "NoOp" ]

        ClaimAction _ ->
            [ "ClaimAction" ]


generateVerificationCode : Int -> String -> Int -> String
generateVerificationCode actionId makerAccountUint64 proofTimeSeconds =
    (String.fromInt actionId
        ++ makerAccountUint64
        ++ String.fromInt proofTimeSeconds
    )
        |> sha256
        |> String.slice 0 8
