module Page.Community.ClaimWithPhoto exposing
    ( Model
    , Msg(..)
    , init
    , jsAddressToMsg
    , msgToString
    , subscriptions
    , update
    , view
    )

import Action exposing (Action, ClaimConfirmationModalStatus(..))
import Api
import Community
import Eos exposing (Symbol)
import Eos.Account as Eos
import File exposing (File)
import Graphql.Http
import Html exposing (Html, button, div, input, label, p, span, text)
import Html.Attributes exposing (accept, class, classList, disabled, multiple, style, type_)
import Html.Events exposing (onClick)
import Http
import Icons
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Page
import Ports exposing (JavascriptOutModel)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Translators)
import Sha256 exposing (sha256)
import Task
import Time exposing (Posix)
import UpdateResult as UR


type alias Model =
    { status : Status
    , proof : Proof
    }


type alias ObjectiveId =
    Int


type alias ActionId =
    Int


init : LoggedIn.Model -> Symbol -> ObjectiveId -> Maybe ActionId -> ( Model, Cmd Msg )
init loggedIn symbol objectiveId actionId =
    let
        ( status, cmd ) =
            case loggedIn.actionToClaim of
                Just actionModel ->
                    if actionModel.action.hasProofPhoto then
                        ( Loaded actionModel, Task.succeed (OpenProofSection actionModel.action) |> Task.perform identity )

                    else
                        ( NotFound, Cmd.none )

                Nothing ->
                    ( NotFound, Cmd.none )
    in
    ( { status = status
      , proof = Proof NoPhotoAdded Nothing
      }
      --, Api.Graphql.query loggedIn.shared
      --    (Community.communityQuery symbol)
      --    CommunityLoaded
    , cmd
    )


type Msg
    = NoOp
    | CommunityLoaded (Result (Graphql.Http.Error (Maybe Community.Model)) (Maybe Community.Model))
    | OpenProofSection Action
    | CloseProofSection ReasonToCloseProofSection
    | GotProofTime Posix
    | GetUint64Name String
    | GotUint64Name (Result Value String)
    | Tick Time.Posix
    | EnteredPhoto (List File)
    | CompletedPhotoUpload (Result Http.Error String)
      --| ClaimAction Action
    | GotClaimActionResponse (Result Value String)
    | GotActionWithPhotoMsg Action.Msg


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
view ({ shared } as loggedIn) model =
    let
        { t } =
            shared.translators

        content =
            case model.status of
                Loading ->
                    Page.fullPageLoading shared

                TimeExpired ->
                    Page.fullPageNotFound "Time has expired" "hello subtitle"

                Loaded _ ->
                    div [ class "bg-white" ]
                        [ Page.viewHeader loggedIn (t "community.actions.title") (Route.Community loggedIn.selectedCommunity)
                        , case loggedIn.actionToClaim of
                            Just actionModel ->
                                viewClaimWithProofs model.proof loggedIn.shared.translators actionModel.action

                            Nothing ->
                                text "this is claim with photo page"
                        ]

                LoadFailed err ->
                    Page.fullPageGraphQLError (t "error.invalidSymbol") err

                NotFound ->
                    Page.fullPageNotFound (t "community.actions.form.not_found") ""
    in
    { title = "Claim with photo"
    , content = content
    }


viewClaimWithProofs : Proof -> Translators -> Action -> Html Msg
viewClaimWithProofs (Proof photoStatus proofCode) translators action =
    let
        { t } =
            translators

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
                [ text <|
                    Maybe.withDefault "" action.photoProofInstructions
                ]
            , case proofCode of
                Just { code, secondsAfterClaim, availabilityPeriod } ->
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
                , viewPhotoUploader translators photoStatus
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
                            --GotActionWithPhotoMsg Action.ActionClaimed
                            (GotActionWithPhotoMsg << Action.ClaimConfirmationOpen) action
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
subscriptions model =
    case model.proof of
        Proof _ (Just _) ->
            Time.every 1000 Tick

        _ ->
            -- No proof code, no timer needed
            Sub.none


type Status
    = Loading
    | Loaded Action.Model
    | LoadFailed (Graphql.Http.Error (Maybe Community.Model))
    | NotFound
    | TimeExpired


update : Msg -> Model -> LoggedIn.Model -> UR.UpdateResult Model Msg (External Msg)
update msg model ({ shared } as loggedIn) =
    let
        (Proof photoStatus proofCode) =
            model.proof

        { t } =
            shared.translators
    in
    case msg of
        CommunityLoaded (Err err) ->
            { model | status = LoadFailed err }
                |> UR.init
                |> UR.logGraphqlError msg err

        CommunityLoaded (Ok c) ->
            case c of
                Just community ->
                    { model
                        | status = Loading
                    }
                        |> UR.init

                Nothing ->
                    { model | status = NotFound }
                        |> UR.init

        OpenProofSection action ->
            let
                runProofCodeTimer =
                    if action.hasProofCode then
                        Task.perform GotProofTime Time.now

                    else
                        Cmd.none
            in
            model
                |> UR.init
                |> UR.addCmd runProofCodeTimer
                -- Don't show any messages at first
                |> UR.addExt HideFeedback

        EnteredPhoto (file :: _) ->
            let
                uploadImage =
                    Api.uploadImage shared file CompletedPhotoUpload
            in
            { model
                | proof = Proof Uploading proofCode
            }
                |> UR.init
                |> UR.addCmd uploadImage
                |> UR.addExt HideFeedback

        EnteredPhoto [] ->
            model
                |> UR.init

        CompletedPhotoUpload (Ok url) ->
            { model
                | proof = Proof (Uploaded url) proofCode
            }
                |> UR.init

        CompletedPhotoUpload (Err error) ->
            { model
                | proof = Proof (UploadFailed error) proofCode
            }
                |> UR.init
                |> UR.logHttpError msg error

        CloseProofSection reason ->
            let
                ( status, ext ) =
                    case reason of
                        TimerExpired ->
                            ( TimeExpired, ShowFeedback LoggedIn.Failure (t "community.actions.proof.time_expired") )

                        CancelClicked ->
                            -- TODO: Redirect to the place where the user came from
                            ( NotFound, HideFeedback )
            in
            { model
                | status = status
                , proof = Proof NoPhotoAdded Nothing
            }
                |> UR.init
                |> UR.addExt ext

        GetUint64Name _ ->
            model |> UR.init

        GotUint64Name (Ok uint64name) ->
            case ( proofCode, loggedIn.actionToClaim ) of
                ( Just pc, Just actionModel ) ->
                    let
                        verificationCode =
                            generateVerificationCode actionModel.action.id uint64name pc.claimTimestamp

                        newProofCode =
                            Just
                                { pc
                                    | code = Just verificationCode
                                }
                    in
                    { model | proof = Proof photoStatus newProofCode }
                        |> UR.init

                _ ->
                    model
                        |> UR.init

        GotUint64Name (Err _) ->
            model |> UR.init

        Tick timer ->
            case proofCode of
                Just pc ->
                    let
                        secondsAfterClaim =
                            (Time.posixToMillis timer // 1000) - pc.claimTimestamp

                        isProofCodeActive =
                            (pc.availabilityPeriod - secondsAfterClaim) > 0
                    in
                    if isProofCodeActive then
                        let
                            newProofCode =
                                Just
                                    { pc
                                        | secondsAfterClaim = secondsAfterClaim
                                    }
                        in
                        { model | proof = Proof photoStatus newProofCode } |> UR.init

                    else
                        update (CloseProofSection TimerExpired) model loggedIn

                _ ->
                    model |> UR.init

        GotProofTime posix ->
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

        GotActionWithPhotoMsg actionMsg ->
            let
                _ =
                    Debug.log "photo model" model

                hasPhotoError =
                    case model.proof of
                        Proof (Uploaded _) _ ->
                            False

                        _ ->
                            -- Error: photo wasn't uploaded while claiming with proof
                            True

                proofData =
                    if hasPhotoError then
                        Nothing

                    else
                        let
                            ( proofPhotoUrl, proofCode_, proofTime ) =
                                case model.proof of
                                    Proof (Uploaded url) (Just { code, claimTimestamp }) ->
                                        ( url, Maybe.withDefault "" code, claimTimestamp )

                                    Proof (Uploaded url) Nothing ->
                                        ( url, "", 0 )

                                    _ ->
                                        ( "", "", 0 )
                        in
                        Just
                            { proofPhoto = proofPhotoUrl
                            , proofCode = proofCode_
                            , proofTime = proofTime
                            }

                addProofData actionModel pd =
                    { actionModel | proofData = pd }

                loggedInWithUpdatedClaimingAction =
                    case loggedIn.actionToClaim of
                        Just actionModel ->
                            { loggedIn
                                | actionToClaim =
                                    Action.update shared.translators
                                        actionMsg
                                        (addProofData actionModel proofData)
                                        |> Just
                            }

                        Nothing ->
                            case actionMsg of
                                Action.ClaimConfirmationOpen a ->
                                    { loggedIn
                                        | actionToClaim =
                                            addProofData (Action.initClaimingActionModel a) proofData
                                                |> Just
                                    }

                                _ ->
                                    loggedIn

                ext =
                    if hasPhotoError then
                        ShowFeedback LoggedIn.Failure (t "community.actions.proof.no_photo_error")

                    else
                        UpdatedLoggedIn (Debug.log "loggedInWithUpdatedClaimingAction" loggedInWithUpdatedClaimingAction)
            in
            model
                |> UR.init
                |> UR.addExt ext

        NoOp ->
            model |> UR.init

        GotClaimActionResponse (Ok _) ->
            -- TODO: remove duplicates
            let
                message =
                    shared.translators.tr "dashboard.check_claim.success"
                        [ ( "symbolCode", Eos.symbolToSymbolCodeString loggedIn.selectedCommunity ) ]
            in
            model
                --claimConfirmationModalStatus = Closed
                |> UR.init
                |> UR.addExt (ShowFeedback LoggedIn.Success message)

        GotClaimActionResponse (Err _) ->
            -- TODO: remove duplicates
            model
                -- claimConfirmationModalStatus = Closed
                |> UR.init
                |> UR.addExt (ShowFeedback LoggedIn.Failure (t "dashboard.check_claim.failure"))


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
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
        CommunityLoaded res ->
            [ "CommunityLoaded" ]

        GotActionWithPhotoMsg _ ->
            [ "GotActionWithPhotoMsg" ]

        Tick _ ->
            [ "Tick" ]

        GotProofTime _ ->
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


generateVerificationCode : Int -> String -> Int -> String
generateVerificationCode actionId makerAccountUint64 proofTimeSeconds =
    (String.fromInt actionId
        ++ makerAccountUint64
        ++ String.fromInt proofTimeSeconds
    )
        |> sha256
        |> String.slice 0 8
