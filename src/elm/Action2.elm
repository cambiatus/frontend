module Action2 exposing (ClaimingStatus, ExternalMsg(..), Msg, msgToString, notClaiming, update)

import Auth
import Cambiatus.Enum.Permission as Permission exposing (Permission)
import Cambiatus.Enum.VerificationType as VerificationType exposing (VerificationType)
import Cambiatus.Scalar exposing (DateTime)
import Eos
import Eos.Account
import Form
import Form.File
import Form.Text
import Graphql.Http
import Html exposing (Html, button, div, h4, img, li, span, text)
import Html.Attributes exposing (alt, class, classList, id, src, tabindex)
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
import View.Feedback


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
        ConfirmedClaimAction ->
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



-- UTILS


isClaimable : Action -> Bool
isClaimable action =
    action.verificationType == VerificationType.Claimable


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


msgToString : Msg -> List String
msgToString msg =
    case msg of
        _ ->
            [ "TODO" ]
