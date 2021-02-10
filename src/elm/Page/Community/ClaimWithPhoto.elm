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

import Action exposing (Action, Model(..))
import Api
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
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Translators)
import Sha256 exposing (sha256)
import Task
import Time exposing (Posix)
import UpdateResult as UR



-- MODEL


type alias Model =
    { status : Status
    , proof : Proof
    }


init : LoggedIn.Model -> Symbol -> ObjectiveId -> Maybe ActionId -> ( Model, Cmd Msg )
init loggedIn symbol objectiveId actionId =
    let
        ( status, cmd ) =
            --case loggedIn.actionToClaim of
            --    Just actionModel ->
            --        if actionModel.action.hasProofPhoto then
            --            ( Loaded actionModel, Task.succeed (PageShowed actionModel.action) |> Task.perform identity )
            --
            --        else
            --            ( NotFound, Cmd.none )
            --
            --    Nothing ->
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



-- TYPES


type Status
    = Loading
    | Loaded Action.Model
    | LoadFailed (Graphql.Http.Error (Maybe Action.Model))
    | NotFound
    | Expired


type alias ObjectiveId =
    Int


type alias ActionId =
    Int


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


type ReasonToCloseProofSection
    = CancelClicked
    | TimerEnded



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view ({ shared } as loggedIn) model =
    let
        { t } =
            shared.translators

        content =
            case model.status of
                Loaded _ ->
                    div [ class "bg-white" ]
                        -- TODO: why loggedIn here? Why not `Loaded actionModel`?
                        [--case loggedIn.actionToClaim of
                         --    Just actionModel ->
                         --        viewClaimWithProofs model.proof
                         --            loggedIn.shared.translators
                         --            (LoggedIn.isAuth loggedIn)
                         --            actionModel.action
                         --
                         --    Nothing ->
                         --        -- TODO: FIXIT: We see this after claiming
                         --        text "this is claim with photo page"
                        ]

                LoadFailed err ->
                    Page.fullPageGraphQLError (t "error.invalidSymbol") err

                Loading ->
                    Page.fullPageLoading shared

                Expired ->
                    Page.fullPageNotFound "Time has expired" ""

                NotFound ->
                    Page.fullPageNotFound (t "community.actions.form.not_found") ""
    in
    { title = "Claim action with photo"
    , content = content
    }


viewClaimWithProofs : Proof -> Translators -> Bool -> Action -> Html Msg
viewClaimWithProofs (Proof photoStatus proofCode) ({ t } as translators) isAuth action =
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
                            ClaimingCancelled CancelClicked
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
                            GotActionMsg (Action.ActionClaimed action Nothing)
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
    in
    label
        (class "relative bg-purple-500 w-full md:w-2/3 h-56 rounded-sm flex justify-center items-center cursor-pointer"
            :: uploadedAttrs
        )
        [ input
            [ class "hidden-img-input"
            , type_ "file"
            , accept "image/*"
            , Page.onFileChange PhotoAdded
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



-- UPDATE


type Msg
    = NoOp
    | ActionLoaded (Result (Graphql.Http.Error (Maybe Action.Model)) (Maybe Action.Model))
    | PageShowed Action
    | ClaimingCancelled ReasonToCloseProofSection
    | GotProofTime Posix
    | AskedForUint64Name
    | GotUint64Name (Result Value String)
    | Tick Time.Posix
    | PhotoAdded (List File)
    | PhotoUploaded (Result Http.Error String)
    | GotActionMsg Action.Msg


update : Msg -> Model -> LoggedIn.Model -> UR.UpdateResult Model Msg (External Msg)
update msg model ({ shared } as loggedIn) =
    let
        (Proof photoStatus proofCode) =
            model.proof

        { t } =
            shared.translators
    in
    case msg of
        ActionLoaded (Err err) ->
            { model | status = LoadFailed err }
                |> UR.init
                |> UR.logGraphqlError msg err

        ActionLoaded (Ok a) ->
            -- TODO: This is a placeholder. Use real loaded action.
            case a of
                Just action ->
                    { model
                        | status = Loading
                    }
                        |> UR.init

                Nothing ->
                    { model | status = NotFound }
                        |> UR.init

        PageShowed action ->
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

        PhotoAdded (file :: _) ->
            { model
                | proof = Proof Uploading proofCode
            }
                |> UR.init
                |> UR.addCmd (Api.uploadImage shared file PhotoUploaded)

        PhotoAdded [] ->
            model
                |> UR.init

        PhotoUploaded (Ok url) ->
            { model
                | proof = Proof (Uploaded url) proofCode
            }
                |> UR.init

        PhotoUploaded (Err error) ->
            { model
                | proof = Proof (UploadFailed error) proofCode
            }
                |> UR.init
                |> UR.logHttpError msg error

        ClaimingCancelled reason ->
            let
                ( status, doNext ) =
                    case reason of
                        TimerEnded ->
                            ( Expired, UR.addExt <| ShowFeedback LoggedIn.Failure (t "community.actions.proof.time_expired") )

                        CancelClicked ->
                            ( NotFound, UR.addCmd <| Route.replaceUrl loggedIn.shared.navKey Route.Dashboard )
            in
            { model
                | status = status
                , proof = Proof NoPhotoAdded Nothing
            }
                |> UR.init
                |> doNext

        --|> UR.addExt
        --    (UpdatedLoggedIn
        --        { loggedIn
        --            | actionToClaim = Nothing -- Remove cancelled claiming action from the global state
        --        }
        --    )
        AskedForUint64Name ->
            model |> UR.init

        GotUint64Name (Ok uint64name) ->
            --case ( proofCode, model.status ) of
            --    ( Just pc, Loaded actionModel ) ->
            --        let
            --            verificationCode =
            --                generateVerificationCode actionModel.action.id uint64name pc.claimTimestamp
            --
            --            newProofCode =
            --                Just
            --                    { pc
            --                        | code_ = Just verificationCode
            --                    }
            --        in
            --        { model | proof = Proof photoStatus newProofCode }
            --            |> UR.init
            --
            --    _ ->
            model
                |> UR.init

        GotUint64Name (Err err) ->
            model
                |> UR.init
                |> UR.addExt (ShowFeedback LoggedIn.Failure "Failed while creating proof code.")
                |> UR.logDebugValue msg err

        Tick timer ->
            case proofCode of
                Just proofCode_ ->
                    let
                        secondsAfterClaim =
                            (Time.posixToMillis timer // 1000) - proofCode_.claimTimestamp

                        isProofCodeActive =
                            (proofCode_.availabilityPeriod - secondsAfterClaim) > 0
                    in
                    if isProofCodeActive then
                        let
                            newProofCode =
                                Just
                                    { proofCode_
                                        | secondsAfterClaim = secondsAfterClaim
                                    }
                        in
                        { model | proof = Proof photoStatus newProofCode }
                            |> UR.init

                    else
                        update (ClaimingCancelled TimerEnded) model loggedIn

                Nothing ->
                    model |> UR.init

        GotProofTime posix ->
            let
                initProofCodeParts =
                    Just
                        { code_ = Nothing
                        , claimTimestamp = Time.posixToMillis posix // 1000
                        , secondsAfterClaim = 0
                        , availabilityPeriod = 30 * 60
                        }
            in
            { model | proof = Proof NoPhotoAdded initProofCodeParts }
                |> UR.init
                |> UR.addPort
                    { responseAddress = AskedForUint64Name
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "accountNameToUint64" )
                            , ( "accountName", Encode.string (Eos.nameToString loggedIn.accountName) )
                            ]
                    }

        GotActionMsg actionMsg ->
            handleActionMsg loggedIn actionMsg model

        NoOp ->
            model |> UR.init


handleActionMsg : LoggedIn.Model -> Action.Msg -> Model -> UR.UpdateResult Model Msg (External Msg)
handleActionMsg ({ shared } as loggedIn) actionMsg model =
    let
        { t } =
            shared.translators
    in
    case actionMsg of
        Action.ActionClaimed _ _ ->
            if True then
                model
                    |> UR.init
                --|> UR.addExt
                --    (Action.ActionClaimed { isPinConfirmed = True }
                --        |> GotActionMsg
                --        |> Just
                --        |> RequiredAuthentication
                --    )

            else
                case ( model.status, model.proof ) of
                    ( Loaded actionModel, Proof (Uploaded url) proofCodeMatch ) ->
                        let
                            ( code, time ) =
                                case proofCodeMatch of
                                    Just { code_, claimTimestamp } ->
                                        ( Maybe.withDefault "" code_, claimTimestamp )

                                    Nothing ->
                                        ( "", 0 )
                        in
                        { model
                            | status = Loaded (Action.update shared.translators actionMsg actionModel)
                        }
                            |> UR.init
                            |> UR.addPort
                                (Action.claimActionPort
                                    (GotActionMsg actionMsg)
                                    shared.contracts.community
                                    { actionId = 0 --actionModel.action.id
                                    , maker = loggedIn.accountName
                                    , proofPhoto = url
                                    , proofCode = code
                                    , proofTime = time
                                    }
                                )

                    _ ->
                        model
                            |> UR.init
                            -- No photo uploaded, show the error message
                            |> UR.addExt (ShowFeedback LoggedIn.Failure (t "community.actions.proof.no_photo_error"))

        Action.GotActionClaimedResponse (Ok _) ->
            let
                message =
                    shared.translators.tr "dashboard.check_claim.success"
                        [ ( "symbolCode", Eos.symbolToSymbolCodeString loggedIn.selectedCommunity ) ]
            in
            model
                |> UR.init
                -- Remove claimed action from the global state
                --|> UR.addExt (UpdatedLoggedIn { loggedIn | actionToClaim = Nothing })
                |> UR.addExt (ShowFeedback LoggedIn.Success message)

        Action.GotActionClaimedResponse (Err _) ->
            model
                |> UR.init
                |> UR.addExt (ShowFeedback LoggedIn.Failure (t "dashboard.check_claim.failure"))

        _ ->
            case model.status of
                Loaded actionModel ->
                    { model
                        | status = Loaded (Action.update shared.translators actionMsg actionModel)
                    }
                        |> UR.init

                _ ->
                    model |> UR.init



-- INTEROP


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
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

        "GotActionMsg" :: remainAddress ->
            Action.jsAddressToMsg remainAddress val
                |> Maybe.map GotActionMsg

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        ActionLoaded _ ->
            [ "ActionLoaded" ]

        GotActionMsg actionMsg ->
            "GotActionMsg" :: Action.msgToString actionMsg

        Tick _ ->
            [ "Tick" ]

        GotProofTime _ ->
            [ "GotProofTime" ]

        PageShowed _ ->
            [ "PageShowed" ]

        ClaimingCancelled _ ->
            [ "ClaimingCancelled" ]

        PhotoAdded _ ->
            [ "PhotoAdded" ]

        PhotoUploaded r ->
            [ "PhotoUploaded", UR.resultToString r ]

        AskedForUint64Name ->
            [ "AskedForUint64Name" ]

        GotUint64Name n ->
            [ "GotUint64Name", UR.resultToString n ]

        NoOp ->
            [ "NoOp" ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.proof of
        Proof _ (Just _) ->
            Time.every 1000 Tick

        _ ->
            -- No timer needed if there's no proof code.
            Sub.none



-- HELPERS


generateVerificationCode : Int -> String -> Int -> String
generateVerificationCode actionId makerAccountUint64 proofTimeSeconds =
    (String.fromInt actionId
        ++ makerAccountUint64
        ++ String.fromInt proofTimeSeconds
    )
        |> sha256
        |> String.slice 0 8
