module Action2 exposing (Action, ClaimingStatus, ExternalMsg(..), Msg, ObjectiveId, completeObjectiveSelectionSet, encodeClaimAction, encodeObjectiveId, isClaimable, isPastDeadline, msgToString, notClaiming, objectiveIdFromInt, objectiveIdSelectionSet, objectiveIdToInt, selectionSet, update, updateAction, viewCard, viewClaimModal)

import Auth
import Cambiatus.Enum.Permission as Permission exposing (Permission)
import Cambiatus.Enum.VerificationType as VerificationType exposing (VerificationType)
import Cambiatus.Mutation
import Cambiatus.Object
import Cambiatus.Object.Action
import Cambiatus.Object.Community
import Cambiatus.Object.Objective
import Cambiatus.Scalar exposing (DateTime)
import Eos
import Eos.Account
import Form
import Form.File
import Form.Text
import Graphql.Http
import Graphql.Operation exposing (RootMutation)
import Graphql.OptionalArgument as OptionalArgument
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html, button, div, h4, img, li, p, span, text)
import Html.Attributes exposing (alt, class, classList, disabled, id, src, tabindex)
import Html.Attributes.Aria exposing (ariaHidden)
import Html.Events exposing (onClick)
import Icons
import Json.Encode
import Markdown exposing (Markdown)
import Profile
import RemoteData exposing (RemoteData)
import Route
import Session.Shared exposing (Shared)
import Sha256
import Time
import Time.Extra
import Translation
import UpdateResult as UR
import Url
import Utils
import View.Feedback
import View.Modal


type alias Action =
    -- TODO - Use opaque type for id
    { id : Int
    , description : Markdown
    , image : Maybe String
    , objective : Objective
    , reward : Float
    , verifierReward : Float
    , creator : Eos.Account.Name
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
    , claimCount : Int
    }


type alias Objective =
    { id : ObjectiveId
    , description : Markdown
    , community : Community
    , isCompleted : Bool
    }


type alias Community =
    { symbol : Eos.Symbol
    , name : String
    }


type ObjectiveId
    = ObjectiveId Int


type ClaimingStatus
    = NotClaiming
    | Claiming { action : Action, proof : Proof }


notClaiming : ClaimingStatus
notClaiming =
    NotClaiming


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
    | ClickedShareAction Action
    | GotClaimingActionMsg ClaimingActionMsg


type ClaimingActionMsg
    = ClickedCloseClaimModal
    | ConfirmedClaimAction
    | ConfirmedClaimActionWithPhotoProof { photoProofUrl : String, proofCode : Maybe ProofCode }
    | GotPhotoProofFormMsg (Form.Msg Form.File.SingleModel)
    | GotUint64Name String
    | CompletedClaimingAction (Result Json.Encode.Value ())


type alias UpdateResult =
    UR.UpdateResult ClaimingStatus Msg ExternalMsg


type ExternalMsg
    = SetUpdateTimeEvery Float
    | ShowFeedback View.Feedback.Model
    | RequiredPrivateKey Msg
    | ShowInsufficientPermissionsModal


type alias ExtensibleCommunity community =
    { community | symbol : Eos.Symbol }


type alias LoggedIn loggedIn community =
    { loggedIn
        | accountName : Eos.Account.Name
        , shared : Shared
        , selectedCommunity : RemoteData (Graphql.Http.Error (Maybe (ExtensibleCommunity community))) (ExtensibleCommunity community)
        , auth : Auth.Model
        , profile : RemoteData (Graphql.Http.Error (Maybe Profile.Model)) Profile.Model
    }


update : Msg -> ClaimingStatus -> LoggedIn loggedIn community -> UpdateResult
update msg status loggedIn =
    case msg of
        NoOp ->
            UR.init status

        ClickedShareAction action ->
            let
                sharePort =
                    -- TODO - Add port listener
                    if loggedIn.shared.canShare then
                        { responseAddress = msg
                        , responseData = Json.Encode.null
                        , data =
                            Json.Encode.object
                                [ ( "name", Json.Encode.string "share" )
                                , ( "title", Markdown.encode action.description )
                                , ( "url"
                                  , Route.CommunityObjectives
                                        (Route.WithObjectiveSelected
                                            { id = objectiveIdToInt action.objective.id
                                            , action = Just action.id
                                            }
                                        )
                                        |> Route.addRouteToUrl loggedIn.shared
                                        |> Url.toString
                                        |> Json.Encode.string
                                  )
                                ]
                        }

                    else
                        { responseAddress = msg
                        , responseData = Json.Encode.int action.id
                        , data =
                            Json.Encode.object
                                [ ( "name", Json.Encode.string "copyToClipboard" )
                                , ( "id", Json.Encode.string (shareActionFallbackId action.id) )
                                ]
                        }
            in
            UR.init status
                |> UR.addPort sharePort

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



-- CLAIMING ACTIONS


updateClaimingAction : ClaimingActionMsg -> Action -> Proof -> LoggedIn loggedIn community -> UpdateResult
updateClaimingAction msg action proof loggedIn =
    let
        status =
            Claiming { action = action, proof = proof }

        withPrivateKey : List Permission -> UpdateResult -> UpdateResult
        withPrivateKey requiredPermissions ur =
            Auth.withPrivateKey loggedIn.auth
                { requiredPermissions = requiredPermissions
                , currentPermissions =
                    RemoteData.toMaybe loggedIn.profile
                        |> Maybe.map (.roles >> List.concatMap .permissions)
                }
                { onAskedPrivateKey = UR.addExt (RequiredPrivateKey (GotClaimingActionMsg msg))
                , onInsufficientPermissions = UR.addExt ShowInsufficientPermissionsModal
                , onAbsentPermissions =
                    UR.logImpossible (GotClaimingActionMsg msg)
                        "Tried signing eos transaction, but profile wasn't loaded"
                        (Just loggedIn.accountName)
                        { moduleName = "Action2"
                        , function = "updateClaimingAction"
                        }
                        []
                , defaultModel = Claiming { action = action, proof = proof }
                }
                (\_ -> ur)
    in
    case msg of
        ClickedCloseClaimModal ->
            UR.init NotClaiming

        ConfirmedClaimAction ->
            -- TODO - Add port listener
            UR.init status
                -- TODO - ErrorMsg?
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
                                    encodeClaimAction
                                        { communityId = action.objective.community.symbol
                                        , actionId = action.id
                                        , claimer = loggedIn.accountName
                                        , proof = Nothing
                                        }
                              }
                            ]
                    }
                |> withPrivateKey [ Permission.Claim ]

        ConfirmedClaimActionWithPhotoProof { photoProofUrl, proofCode } ->
            -- TODO - Add port listener
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
                                    encodeClaimAction
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
                            |> UR.addPort (claimPort Nothing)
                            |> withPrivateKey [ Permission.Claim ]

                Just code ->
                    UR.init status
                        |> UR.addPort
                            (claimPort
                                (Just
                                    { code = code.code
                                    , time = code.generation
                                    }
                                )
                            )
                        |> withPrivateKey [ Permission.Claim ]

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



-- VIEW


viewCard :
    LoggedIn loggedIn community
    ->
        { containerAttrs : List (Html.Attribute msg)
        , sideIcon : Html msg
        , toMsg : Msg -> msg
        }
    -> Action
    -> Html msg
viewCard loggedIn { containerAttrs, sideIcon, toMsg } action =
    let
        ({ t, tr } as translators) =
            loggedIn.shared.translators
    in
    li (class "bg-white rounded self-start w-full flex-shrink-0" :: containerAttrs)
        [ case action.image of
            Nothing ->
                text ""

            Just "" ->
                text ""

            Just image ->
                div [ class "mt-2 mx-2 relative" ]
                    [ img [ src image, alt "", class "rounded" ] []
                    , div [ class "bg-gradient-to-t from-[#01003a14] to-[#01003a00] absolute top-0 left-0 w-full h-full rounded" ] []
                    ]
        , div [ class "px-4 pt-4 pb-6" ]
            [ div [ class "flex" ]
                [ sideIcon
                , div [ class "ml-5 mt-1 min-w-0 w-full" ]
                    [ h4 [ Html.Attributes.title (Markdown.toRawString action.description) ]
                        [ Markdown.view [ class "line-clamp-3 hide-children-from-2" ] action.description ]
                    , span [ class "sr-only" ]
                        [ text <|
                            t "community.objectives.reward"
                                ++ ": "
                                ++ Eos.assetToString translators
                                    { amount = action.reward
                                    , symbol = action.objective.community.symbol
                                    }
                        ]
                    , span
                        [ class "font-bold text-sm text-gray-900 uppercase block mt-6"
                        , Html.Attributes.Aria.ariaHidden True
                        ]
                        [ text <| t "community.objectives.reward" ]
                    , div
                        [ class "mt-1 text-green font-bold"
                        , Html.Attributes.Aria.ariaHidden True
                        ]
                        [ span [ class "text-2xl mr-1" ]
                            [ text
                                (Eos.formatSymbolAmount
                                    translators
                                    action.objective.community.symbol
                                    action.reward
                                )
                            ]
                        , text (Eos.symbolToSymbolCodeString action.objective.community.symbol)
                        ]
                    ]
                ]
            , div
                [ class "grid grid-cols-1 sm:grid-cols-2 gap-x-4 gap-y-2 mt-6"
                , classList [ ( "sm:grid-cols-1", not (isClaimable action) ) ]
                ]
                [ button
                    [ class "button button-secondary w-full"
                    , onClick (ClickedShareAction action |> toMsg)
                    , id (shareActionButtonId action.id)
                    ]
                    [ Icons.share "mr-2 flex-shrink-0"
                    , text <| t "share"
                    ]
                , if isClaimable action then
                    button
                        [ class "button button-primary w-full sm:col-span-1"
                        , onClick (ClickedClaimAction action |> toMsg)
                        ]
                        [ if action.hasProofPhoto then
                            Icons.camera "w-4 mr-2 flex-shrink-0"

                          else
                            text ""
                        , text <| t "dashboard.claim"
                        ]

                  else
                    text ""
                ]
            ]
        , if not loggedIn.shared.canShare then
            Form.Text.view
                (Form.Text.init
                    { label = ""
                    , id = shareActionFallbackId action.id
                    }
                    |> Form.Text.withExtraAttrs
                        [ class "absolute opacity-0 left-[-9999em]"
                        , tabindex -1
                        , ariaHidden True
                        ]
                    |> Form.Text.withContainerAttrs [ class "mb-0 overflow-hidden" ]
                    |> Form.Text.withInputElement (Form.Text.TextareaInput { submitOnEnter = False })
                )
                { onChange = \_ -> NoOp
                , onBlur = NoOp
                , value =
                    tr
                        "community.objectives.share_action"
                        [ ( "community_name", action.objective.community.name )
                        , ( "objective_description", Markdown.toRawString action.objective.description )
                        , ( "action_description", Markdown.toRawString action.description )
                        , ( "url"
                          , Route.WithObjectiveSelected
                                { id = objectiveIdToInt action.objective.id
                                , action = Just action.id
                                }
                                |> Route.CommunityObjectives
                                |> Route.addRouteToUrl loggedIn.shared
                                |> Url.toString
                          )
                        ]
                , error = text ""
                , hasError = False
                , translators = translators
                , isRequired = False
                }
                |> Html.map toMsg

          else
            text ""
        ]


viewClaimModal : Shared -> { position : Maybe Int } -> ClaimingStatus -> Html Msg
viewClaimModal shared { position } status =
    case status of
        NotClaiming ->
            text ""

        Claiming { action, proof } ->
            let
                { t } =
                    shared.translators

                ( onClaimClick, isClaimDisabled ) =
                    case proof of
                        WithProof formModel _ ->
                            ( Form.parse (claimWithPhotoForm shared.translators)
                                formModel
                                { onError = GotPhotoProofFormMsg
                                , onSuccess =
                                    \photoProofUrl ->
                                        ConfirmedClaimActionWithPhotoProof
                                            { photoProofUrl = photoProofUrl
                                            , proofCode =
                                                case proof of
                                                    WithProof _ (WithCode proofCode) ->
                                                        Just proofCode

                                                    _ ->
                                                        Nothing
                                            }
                                }
                            , Form.hasFieldsLoading formModel
                            )

                        NoProofNecessary ->
                            ( ConfirmedClaimAction, False )
            in
            View.Modal.initWith
                { closeMsg = ClickedCloseClaimModal
                , isVisible = True
                }
                |> View.Modal.withBody
                    [ case action.image of
                        Nothing ->
                            text ""

                        Just "" ->
                            text ""

                        Just image ->
                            div [ class "mb-4 relative" ]
                                [ img
                                    [ src image
                                    , alt ""
                                    , class "max-w-full mx-auto object-scale-down rounded"
                                    ]
                                    []
                                , div [ class "bg-gradient-to-t from-[#01003a14] to-[#01003a00] absolute top-0 left-0 w-full h-full rounded" ] []
                                ]
                    , div
                        [ class "flex"
                        , classList [ ( "md:mb-6", proof == NoProofNecessary ) ]
                        ]
                        [ case position of
                            Nothing ->
                                -- TODO
                                text ""

                            Just validPosition ->
                                span [ class "text-lg text-gray-500 font-bold" ]
                                    [ text (String.fromInt validPosition ++ ".") ]
                        , div [ class "ml-5 mt-1 min-w-0 w-full" ]
                            [ Markdown.view [] action.description
                            , div [ class "md:flex md:justify-between md:w-full" ]
                                [ div []
                                    [ span [ class "font-bold text-sm text-gray-900 uppercase block mt-6" ]
                                        [ text <| t "community.objectives.reward" ]
                                    , div [ class "text-green font-bold" ]
                                        [ span [ class "text-2xl mr-1" ]
                                            [ text
                                                (Eos.formatSymbolAmount shared.translators
                                                    action.objective.community.symbol
                                                    action.reward
                                                )
                                            ]
                                        , text (Eos.symbolToSymbolCodeString action.objective.community.symbol)
                                        ]
                                    ]
                                , viewClaimCount shared.translators [ class "hidden md:flex md:self-end md:mr-8" ] action
                                ]
                            ]
                        ]
                    , viewClaimCount shared.translators
                        [ class "md:hidden"
                        , classList [ ( "mb-6", proof == NoProofNecessary ) ]
                        ]
                        action
                    , case proof of
                        WithProof formModel proofCode ->
                            let
                                timeLeft =
                                    case proofCode of
                                        NoCodeNecessary ->
                                            Nothing

                                        GeneratingCode ->
                                            Nothing

                                        WithCode { expiration } ->
                                            let
                                                minutes =
                                                    Time.Extra.diff Time.Extra.Minute
                                                        shared.timezone
                                                        shared.now
                                                        expiration

                                                seconds =
                                                    Time.Extra.diff Time.Extra.Second
                                                        shared.timezone
                                                        shared.now
                                                        expiration
                                                        |> modBy 60
                                            in
                                            Just { minutes = minutes, seconds = seconds }

                                isTimeOver =
                                    case timeLeft of
                                        Nothing ->
                                            False

                                        Just { minutes } ->
                                            minutes < 0
                            in
                            div []
                                [ p [ class "text-lg font-bold text-gray-333 mt-6 mb-4 md:text-center" ]
                                    [ text <| t "community.actions.proof.title" ]
                                , case action.photoProofInstructions of
                                    Just instructions ->
                                        Markdown.view [] instructions

                                    Nothing ->
                                        p [] [ text <| t "community.actions.proof.upload_hint" ]
                                , case proofCode of
                                    NoCodeNecessary ->
                                        text ""

                                    GeneratingCode ->
                                        div [ class "p-4 mt-4 bg-gray-100 rounded-sm flex flex-col items-center justify-center md:w-1/2 md:mx-auto" ]
                                            [ span [ class "uppercase text-gray-333 font-bold text-sm" ]
                                                [ text <| t "community.actions.form.verification_code" ]
                                            , span [ class "bg-gray-333 animate-skeleton-loading h-10 w-44 mt-2" ] []
                                            ]

                                    WithCode { code } ->
                                        div []
                                            [ div [ class "p-4 mt-4 bg-gray-100 rounded-sm flex flex-col items-center justify-center md:w-1/2 md:mx-auto" ]
                                                [ span [ class "uppercase text-gray-333 font-bold text-sm" ]
                                                    [ text <| t "community.actions.form.verification_code" ]
                                                , span [ class "font-bold text-xl text-gray-333" ] [ text code ]
                                                ]
                                            , p
                                                [ class "text-purple-500 text-center mt-4"
                                                , classList [ ( "text-red", isTimeOver ) ]
                                                ]
                                                [ text <| t "community.actions.proof.code_period_label"
                                                , text " "
                                                , span [ class "font-bold" ]
                                                    [ case timeLeft of
                                                        Nothing ->
                                                            text "30:00"

                                                        Just { minutes, seconds } ->
                                                            (Utils.padInt 2 minutes ++ ":" ++ Utils.padInt 2 seconds)
                                                                |> text
                                                    ]
                                                ]
                                            ]
                                , Form.viewWithoutSubmit [ class "mb-6" ]
                                    shared.translators
                                    (\_ -> [])
                                    (claimWithPhotoForm shared.translators)
                                    formModel
                                    { toMsg = GotPhotoProofFormMsg }
                                ]

                        NoProofNecessary ->
                            text ""
                    ]
                |> View.Modal.withFooter
                    [ div [ class "w-full grid md:grid-cols-2 gap-4" ]
                        [ if isClaimable action then
                            button
                                [ class "button button-secondary w-full"
                                , onClick ClickedCloseClaimModal
                                ]
                                [ text <| t "menu.cancel" ]

                          else
                            text ""
                        , button
                            [ onClick onClaimClick
                            , class "button button-primary w-full"
                            , disabled isClaimDisabled
                            ]
                            [ text <| t "dashboard.claim" ]
                        ]
                    ]
                |> View.Modal.withSize View.Modal.FullScreen
                |> View.Modal.toHtml
                |> Html.map GotClaimingActionMsg


viewClaimCount : Translation.Translators -> List (Html.Attribute msg) -> Action -> Html msg
viewClaimCount { t, tr } attrs action =
    div
        (class "mt-4 p-2 bg-gray-100 flex items-center justify-center text-gray-900 font-semibold text-sm rounded-sm"
            :: attrs
        )
        [ img
            [ src "/images/doggo_holding_coins.svg"
            , alt ""
            , class "w-8 mr-2"
            ]
            []
        , p []
            [ text <| t "community.objectives.claim_count"
            , text " "
            , span [ class "text-base ml-1 font-bold" ]
                [ if action.claimCount == 1 then
                    text <| t "community.objectives.claim_count_times_singular"

                  else
                    text <|
                        tr "community.objectives.claim_count_times"
                            [ ( "count", String.fromInt action.claimCount ) ]
                ]
            ]
        ]


claimWithPhotoForm : Translation.Translators -> Form.Form msg Form.File.SingleModel String
claimWithPhotoForm translators =
    Form.succeed identity
        |> Form.with
            (Form.File.init { id = "photo-proof-input" }
                |> Form.File.withFileTypes [ Form.File.Image, Form.File.Pdf ]
                |> Form.File.withContainerAttributes [ class "w-full bg-gray-100 grid place-items-center mt-2" ]
                |> Form.File.withEntryContainerAttributes (\_ -> [ class "h-56 rounded-sm overflow-hidden w-full grid place-items-center" ])
                |> Form.File.withImageClass "h-56"
                |> Form.File.withAddImagesView
                    [ div [ class "w-full h-56 bg-gray-100 rounded-sm flex flex-col justify-center items-center" ]
                        [ Icons.addPhoto "fill-current text-body-black w-10 mb-2"
                        , p [ class "px-4 font-bold" ] [ text <| translators.t "community.actions.proof.upload_hint" ]
                        ]
                    ]
                |> Form.File.withAddImagesContainerAttributes [ class "!w-full rounded-sm" ]
                |> Form.File.withImageCropperAttributes [ class "rounded-sm" ]
                |> Form.file
                    { parser = Ok
                    , translators = translators
                    , value = identity
                    , update = \newModel _ -> newModel
                    , externalError = always Nothing
                    }
            )



-- GRAPHQL


selectionSet : SelectionSet Action Cambiatus.Object.Action
selectionSet =
    SelectionSet.succeed Action
        |> SelectionSet.with Cambiatus.Object.Action.id
        |> SelectionSet.with (Markdown.selectionSet Cambiatus.Object.Action.description)
        |> SelectionSet.with Cambiatus.Object.Action.image
        |> SelectionSet.with
            (SelectionSet.map
                (\o ->
                    { id = o.id
                    , description = o.description
                    , community = o.community
                    , isCompleted = o.isCompleted
                    }
                )
                (Cambiatus.Object.Action.objective objectiveSelectionSet)
            )
        |> SelectionSet.with Cambiatus.Object.Action.reward
        |> SelectionSet.with Cambiatus.Object.Action.verifierReward
        |> SelectionSet.with (Eos.Account.nameSelectionSet Cambiatus.Object.Action.creatorId)
        |> SelectionSet.with (Cambiatus.Object.Action.validators Profile.minimalSelectionSet)
        |> SelectionSet.with Cambiatus.Object.Action.usages
        |> SelectionSet.with Cambiatus.Object.Action.usagesLeft
        |> SelectionSet.with Cambiatus.Object.Action.deadline
        |> SelectionSet.with Cambiatus.Object.Action.verificationType
        |> SelectionSet.with Cambiatus.Object.Action.verifications
        |> SelectionSet.with Cambiatus.Object.Action.isCompleted
        |> SelectionSet.with (SelectionSet.map (Maybe.withDefault False) Cambiatus.Object.Action.hasProofPhoto)
        |> SelectionSet.with (SelectionSet.map (Maybe.withDefault False) Cambiatus.Object.Action.hasProofCode)
        |> SelectionSet.with (Markdown.maybeSelectionSet Cambiatus.Object.Action.photoProofInstructions)
        |> SelectionSet.with Cambiatus.Object.Action.position
        |> SelectionSet.with (Cambiatus.Object.Action.claimCount (\optionals -> { optionals | status = OptionalArgument.Absent }))


objectiveSelectionSet : SelectionSet Objective Cambiatus.Object.Objective
objectiveSelectionSet =
    SelectionSet.succeed Objective
        |> SelectionSet.with objectiveIdSelectionSet
        |> SelectionSet.with (Markdown.selectionSet Cambiatus.Object.Objective.description)
        |> SelectionSet.with (Cambiatus.Object.Objective.community communitySelectionSet)
        |> SelectionSet.with Cambiatus.Object.Objective.isCompleted


objectiveIdSelectionSet : SelectionSet ObjectiveId Cambiatus.Object.Objective
objectiveIdSelectionSet =
    Cambiatus.Object.Objective.id |> SelectionSet.map ObjectiveId


communitySelectionSet : SelectionSet Community Cambiatus.Object.Community
communitySelectionSet =
    SelectionSet.succeed Community
        |> SelectionSet.with (Eos.symbolSelectionSet Cambiatus.Object.Community.symbol)
        |> SelectionSet.with Cambiatus.Object.Community.name


completeObjectiveSelectionSet : ObjectiveId -> SelectionSet decodesTo Cambiatus.Object.Objective -> SelectionSet (Maybe decodesTo) RootMutation
completeObjectiveSelectionSet (ObjectiveId id) =
    Cambiatus.Mutation.completeObjective { id = id }



-- EOS


updateAction : Eos.Account.Name -> Shared -> Action -> Eos.Action
updateAction accountName shared action =
    { accountName = shared.contracts.community
    , name = "upsertaction"
    , authorization =
        { actor = accountName
        , permissionName = Eos.Account.samplePermission
        }
    , data = encode action
    }



-- JSON


type alias ClaimedAction =
    { communityId : Eos.Symbol
    , actionId : Int
    , claimer : Eos.Account.Name
    , proof :
        Maybe
            { photo : String
            , proofCode :
                Maybe
                    { code : String
                    , time : Time.Posix
                    }
            }
    }


encodeClaimAction : ClaimedAction -> Json.Encode.Value
encodeClaimAction c =
    let
        encodeProofItem getter default encoder =
            c.proof
                |> Maybe.map getter
                |> Maybe.withDefault default
                |> encoder

        encodeProofCodeItem getter default encoder =
            c.proof
                |> Maybe.andThen .proofCode
                |> Maybe.map getter
                |> Maybe.withDefault default
                |> encoder
    in
    Json.Encode.object
        [ ( "community_id", Eos.encodeSymbol c.communityId )
        , ( "action_id", Json.Encode.int c.actionId )
        , ( "maker", Eos.Account.encodeName c.claimer )
        , ( "proof_photo", encodeProofItem .photo "" Json.Encode.string )
        , ( "proof_code", encodeProofCodeItem .code "" Json.Encode.string )
        , ( "proof_time"
          , encodeProofCodeItem (.time >> Time.posixToMillis >> (\time -> time // 1000))
                0
                Json.Encode.int
          )
        ]


encodeObjectiveId : ObjectiveId -> Json.Encode.Value
encodeObjectiveId (ObjectiveId id) =
    Json.Encode.int id


encode : Action -> Json.Encode.Value
encode action =
    let
        makeAsset : Float -> Eos.Asset
        makeAsset amount =
            { symbol = action.objective.community.symbol, amount = amount }
    in
    Json.Encode.object
        [ ( "community_id", Eos.encodeSymbol action.objective.community.symbol )
        , ( "action_id", Json.Encode.int action.id )
        , ( "objective_id", encodeObjectiveId action.objective.id )
        , ( "description", Markdown.encode action.description )
        , ( "reward", Eos.encodeAsset (makeAsset action.reward) )
        , ( "verifier_reward", Eos.encodeAsset (makeAsset action.verifierReward) )
        , ( "deadline"
          , Utils.fromMaybeDateTime action.deadline
                |> Time.posixToMillis
                |> Json.Encode.int
          )
        , ( "usages", Json.Encode.int action.usages )
        , ( "usages_left", Json.Encode.int action.usagesLeft )
        , ( "verifications", Json.Encode.int action.verifications )
        , ( "verification_type"
          , action.verificationType
                |> VerificationType.toString
                |> String.toLower
                |> Json.Encode.string
          )
        , ( "validators_str"
          , action.validators
                |> List.map (\v -> Eos.Account.nameToString v.account)
                |> String.join "-"
                |> Json.Encode.string
          )
        , ( "is_completed", Eos.encodeEosBool (Eos.boolToEosBool action.isCompleted) )
        , ( "creator", Eos.Account.encodeName action.creator )
        , ( "has_proof_photo", Eos.encodeEosBool (Eos.boolToEosBool action.hasProofPhoto) )
        , ( "has_proof_code", Eos.encodeEosBool (Eos.boolToEosBool action.hasProofCode) )
        , ( "photo_proof_instructions", Markdown.encode (Maybe.withDefault Markdown.empty action.photoProofInstructions) )
        , ( "image", Json.Encode.string "" )
        ]



-- UTILS


isClaimable : Action -> Bool
isClaimable action =
    action.verificationType == VerificationType.Claimable


isPastDeadline : Action -> Time.Posix -> Bool
isPastDeadline action now =
    case action.deadline of
        Just _ ->
            Time.posixToMillis now > Time.posixToMillis (Utils.fromMaybeDateTime action.deadline)

        Nothing ->
            False


shareActionButtonId : Int -> String
shareActionButtonId actionId =
    "share-action-button-" ++ String.fromInt actionId


shareActionFallbackId : Int -> String
shareActionFallbackId actionId =
    "share-action-fallback-" ++ String.fromInt actionId


generateProofCode : Action -> String -> Time.Posix -> String
generateProofCode action claimerAccountUint64 time =
    (String.fromInt action.id
        ++ claimerAccountUint64
        ++ String.fromInt (Time.posixToMillis time // 1000)
    )
        |> Sha256.sha256
        |> String.slice 0 8


objectiveIdToInt : ObjectiveId -> Int
objectiveIdToInt (ObjectiveId id) =
    id


objectiveIdFromInt : Int -> ObjectiveId
objectiveIdFromInt id =
    ObjectiveId id


msgToString : Msg -> List String
msgToString msg =
    case msg of
        _ ->
            [ "TODO" ]
