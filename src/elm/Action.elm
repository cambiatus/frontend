module Action exposing
    ( Action, Objective
    , Id, encodeId, idFromInt, idFromString, idToInt, idToString
    , ObjectiveId, encodeObjectiveId, objectiveIdSelectionSet, objectiveIdFromInt, objectiveIdToInt
    , isClosed, isPastDeadline
    , selectionSet, completeObjectiveSelectionSet, updateAction
    , ClaimingStatus, notClaiming, startClaiming, Msg, update, ExternalMsg(..), msgToString, jsAddressToMsg
    , viewCard, viewClaimModal
    )

{-|


## Main types

@docs Action, Objective


### Ids


#### Action Ids

@docs Id, encodeId, idFromInt, idFromString, idToInt, idToString


#### Objective Ids

@docs ObjectiveId, encodeObjectiveId, objectiveIdSelectionSet, objectiveIdFromInt, objectiveIdToInt


## Helper functions

@docs isClosed, isPastDeadline


## GraphQL & EOS

@docs selectionSet, completeObjectiveSelectionSet, updateAction


## Claiming actions

@docs ClaimingStatus, notClaiming, startClaiming, Msg, update, ExternalMsg, msgToString, jsAddressToMsg


### Views

@docs viewCard, viewClaimModal

-}

import Auth
import Browser.Dom
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
import Json.Decode
import Json.Encode
import Markdown exposing (Markdown)
import Profile
import RemoteData exposing (RemoteData)
import Route
import Session.Shared exposing (Shared)
import Sha256
import Task
import Time
import Time.Extra
import Translation
import UpdateResult as UR
import Url
import Utils
import View.Feedback
import View.Modal


type alias Action =
    { id : Id
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


type Id
    = Id Int


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
    | Claiming
        { action : Action
        , proof : Proof
        , position : Maybe Int
        }


notClaiming : ClaimingStatus
notClaiming =
    NotClaiming


startClaiming : { position : Maybe Int } -> Action -> Msg
startClaiming position action =
    ClickedClaimAction position action


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
    | ClickedClaimAction { position : Maybe Int } Action
    | ClickedShareAction Action
    | CopiedShareLinkToClipboard Id
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
                                            , action = Just (idToInt action.id)
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
                        , responseData = encodeId action.id
                        , data =
                            Json.Encode.object
                                [ ( "name", Json.Encode.string "copyToClipboard" )
                                , ( "id", Json.Encode.string (shareActionFallbackId action.id) )
                                ]
                        }
            in
            UR.init status
                |> UR.addPort sharePort

        CopiedShareLinkToClipboard actionId ->
            status
                |> UR.init
                |> UR.addExt
                    (ShowFeedback
                        (View.Feedback.Visible View.Feedback.Success
                            (loggedIn.shared.translators.t "copied_to_clipboard")
                        )
                    )
                |> UR.addCmd
                    (Browser.Dom.focus (shareActionButtonId actionId)
                        |> Task.attempt (\_ -> NoOp)
                    )

        ClickedClaimAction { position } action ->
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
            { action = action
            , proof = proof
            , position = position
            }
                |> Claiming
                |> UR.init
                |> UR.addExt (SetUpdateTimeEvery 1000)
                |> generateProofCodePort

        GotClaimingActionMsg subMsg ->
            case status of
                NotClaiming ->
                    UR.init status
                        |> UR.logIncompatibleMsg msg
                            (Just loggedIn.accountName)
                            { moduleName = "Action", function = "update" }
                            []

                Claiming { action, proof, position } ->
                    updateClaimingAction subMsg action proof { position = position } loggedIn



-- CLAIMING ACTIONS


updateClaimingAction : ClaimingActionMsg -> Action -> Proof -> { position : Maybe Int } -> LoggedIn loggedIn community -> UpdateResult
updateClaimingAction msg action proof { position } loggedIn =
    let
        status =
            { action = action, proof = proof, position = position }

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
                        { moduleName = "Action"
                        , function = "updateClaimingAction"
                        }
                        []
                , defaultModel = Claiming status
                }
                (\_ -> ur)
    in
    case msg of
        ClickedCloseClaimModal ->
            UR.init NotClaiming
                |> UR.addExt (SetUpdateTimeEvery (60 * 1000))

        ConfirmedClaimAction ->
            Claiming status
                |> UR.init
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
                        Claiming status
                            |> UR.init
                            |> UR.logImpossible (GotClaimingActionMsg msg)
                                "Claimed action that requires proof code, but no proof code was provided"
                                (Just loggedIn.accountName)
                                { moduleName = "Action"
                                , function = "updateClaimingAction"
                                }
                                []

                    else
                        Claiming status
                            |> UR.init
                            |> UR.addPort (claimPort Nothing)
                            |> withPrivateKey [ Permission.Claim ]

                Just code ->
                    Claiming status
                        |> UR.init
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
                    Claiming status
                        |> UR.init

                WithProof photoProofForm proofCodeStatus ->
                    Form.update loggedIn.shared subMsg photoProofForm
                        |> UR.fromChild
                            (\newForm ->
                                Claiming { status | proof = WithProof newForm proofCodeStatus }
                            )
                            (GotClaimingActionMsg << GotPhotoProofFormMsg)
                            (\ext -> UR.addExt (ShowFeedback ext))
                            (Claiming status)

        GotUint64Name uint64Name ->
            case proof of
                NoProofNecessary ->
                    Claiming status
                        |> UR.init

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
                    Claiming { status | proof = WithProof formModel proofCodeStatus }
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
                            { moduleName = "Action", function = "updateClaimingAction" }
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
                    { moduleName = "Action", function = "updateClaimingAction" }
                    []
                    val



-- VIEW


viewCard :
    LoggedIn loggedIn community
    ->
        { containerAttrs : List (Html.Attribute msg)
        , position : Maybe Int
        , toMsg : Msg -> msg
        }
    -> Action
    -> Html msg
viewCard loggedIn { containerAttrs, position, toMsg } action =
    let
        ({ t, tr } as translators) =
            loggedIn.shared.translators

        canBeClaimed =
            isClaimable action && not (isClosed action loggedIn.shared.now) && not action.isCompleted
    in
    li (class "bg-white rounded self-start w-full flex-shrink-0" :: containerAttrs)
        [ case action.image of
            Nothing ->
                text ""

            Just "" ->
                text ""

            Just image ->
                div [ class "mt-2 mx-2 relative h-36 flex" ]
                    [ img
                        [ src image
                        , alt ""
                        , class "rounded object-cover mx-auto"
                        ]
                        []
                    , div [ class "bg-gradient-to-t from-[#01003a14] to-[#01003a00] absolute top-0 left-0 w-full h-full rounded" ] []
                    ]
        , div [ class "px-4 pt-4 pb-6" ]
            [ div [ class "flex mb-6" ]
                [ case position of
                    Nothing ->
                        if not (isClosed action loggedIn.shared.now) && not action.isCompleted then
                            Icons.flag "w-8 text-green fill-current"

                        else
                            Icons.flag "w-8 text-gray-900 fill-current"

                    Just validPosition ->
                        span
                            [ class "text-lg text-gray-500 font-bold"
                            , ariaHidden True
                            ]
                            [ text (String.fromInt validPosition ++ ".") ]
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
                        , ariaHidden True
                        ]
                        [ text <| t "community.objectives.reward" ]
                    , div
                        [ class "mt-1 text-green font-bold"
                        , ariaHidden True
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
            , if isClosed action loggedIn.shared.now || action.isCompleted then
                viewNotAbleToClaimNotice (t "community.objectives.action_completed_notice")

              else if not (isClaimable action) then
                viewNotAbleToClaimNotice (t "community.objectives.action_automatic_notice")

              else
                text ""
            , div
                [ class "grid grid-cols-1 sm:grid-cols-2 gap-x-4 gap-y-2"
                , classList [ ( "sm:grid-cols-1", not canBeClaimed ) ]
                ]
                [ button
                    [ class "button button-secondary w-full"
                    , onClick (ClickedShareAction action |> toMsg)
                    , id (shareActionButtonId action.id)
                    ]
                    [ Icons.share "mr-2 flex-shrink-0"
                    , text <| t "share"
                    ]
                , if canBeClaimed then
                    button
                        [ class "button button-primary w-full sm:col-span-1"
                        , onClick
                            (ClickedClaimAction { position = position } action
                                |> toMsg
                            )
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
                        [ tabindex -1
                        , ariaHidden True
                        ]
                    |> Form.Text.withContainerAttrs
                        [ class "h-0 absolute opacity-0 left-[-9999em] !mb-0 overflow-hidden"
                        , ariaHidden True
                        ]
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
                                , action = Just (idToInt action.id)
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


viewNotAbleToClaimNotice : String -> Html msg
viewNotAbleToClaimNotice noticeText =
    div [ class "flex items-center bg-gray-100 rounded-sm p-2 mb-4" ]
        [ img
            [ src "/images/transfer-doggo.svg"
            , alt ""
            , class "w-8 mr-2"
            ]
            []
        , span [ class "text-sm text-gray-900" ] [ text noticeText ]
        ]


viewClaimModal : Shared -> ClaimingStatus -> Html Msg
viewClaimModal shared status =
    case status of
        NotClaiming ->
            text ""

        Claiming { action, proof, position } ->
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
                                Icons.flag "w-8 text-green fill-current"

                            Just validPosition ->
                                span
                                    [ class "text-lg text-gray-500 font-bold"
                                    , ariaHidden True
                                    ]
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
                |> Form.File.withGrayBoxVariant translators
                |> Form.File.withContainerAttributes [ class "w-full mt-2" ]
                |> Form.File.withEntryContainerAttributes (\_ -> [ class "h-56 w-full" ])
                |> Form.File.withImageClass "h-56"
                |> Form.File.withAddImagesContainerAttributes [ class "h-56 rounded-sm" ]
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
        |> SelectionSet.with idSelectionSet
        |> SelectionSet.with (Markdown.selectionSet Cambiatus.Object.Action.description)
        |> SelectionSet.with (SelectionSet.map emptyStringToNothing Cambiatus.Object.Action.image)
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


emptyStringToNothing : Maybe String -> Maybe String
emptyStringToNothing maybeString =
    case maybeString of
        Nothing ->
            Nothing

        Just "" ->
            Nothing

        Just nonEmptyString ->
            Just nonEmptyString


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


idSelectionSet : SelectionSet Id Cambiatus.Object.Action
idSelectionSet =
    Cambiatus.Object.Action.id |> SelectionSet.map Id


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
    , actionId : Id
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
        , ( "action_id", encodeId c.actionId )
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


encodeId : Id -> Json.Encode.Value
encodeId (Id id) =
    Json.Encode.int id


decodeId : Json.Decode.Decoder Id
decodeId =
    Json.Decode.map Id Json.Decode.int


encode : Action -> Json.Encode.Value
encode action =
    let
        makeAsset : Float -> Eos.Asset
        makeAsset amount =
            { symbol = action.objective.community.symbol, amount = amount }
    in
    Json.Encode.object
        [ ( "community_id", Eos.encodeSymbol action.objective.community.symbol )
        , ( "action_id", encodeId action.id )
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


isClosed : Action -> Time.Posix -> Bool
isClosed action now =
    isPastDeadline action now
        || (action.usages > 0 && action.usagesLeft == 0)


shareActionButtonId : Id -> String
shareActionButtonId actionId =
    "share-action-button-" ++ String.fromInt (idToInt actionId)


shareActionFallbackId : Id -> String
shareActionFallbackId actionId =
    "share-action-fallback-" ++ String.fromInt (idToInt actionId)


generateProofCode : Action -> String -> Time.Posix -> String
generateProofCode action claimerAccountUint64 time =
    (String.fromInt (idToInt action.id)
        ++ claimerAccountUint64
        ++ String.fromInt (Time.posixToMillis time // 1000)
    )
        |> Sha256.sha256
        |> String.slice 0 8


idToInt : Id -> Int
idToInt (Id id) =
    id


idFromInt : Int -> Id
idFromInt id =
    Id id


idToString : Id -> String
idToString (Id id) =
    String.fromInt id


idFromString : String -> Maybe Id
idFromString =
    String.toInt
        >> Maybe.map Id


objectiveIdToInt : ObjectiveId -> Int
objectiveIdToInt (ObjectiveId id) =
    id


objectiveIdFromInt : Int -> ObjectiveId
objectiveIdFromInt id =
    ObjectiveId id


jsAddressToMsg : List String -> Json.Encode.Value -> Maybe Msg
jsAddressToMsg addr val =
    let
        decodeConfirmedClaimAction =
            Json.Decode.decodeValue (Json.Decode.field "transactionId" Json.Decode.string) val
                |> Result.map (\_ -> ())
                |> Result.mapError (\_ -> val)
                |> CompletedClaimingAction
                |> GotClaimingActionMsg
                |> Just
    in
    case addr of
        "ClickedShareAction" :: _ ->
            case
                Json.Decode.decodeValue
                    (Json.Decode.map2
                        (\hasCopied actionId ->
                            if hasCopied then
                                Just actionId

                            else
                                Nothing
                        )
                        (Json.Decode.field "copied" Json.Decode.bool)
                        (Json.Decode.field "addressData" decodeId)
                    )
                    val
            of
                Ok (Just actionId) ->
                    Just (CopiedShareLinkToClipboard actionId)

                Ok Nothing ->
                    Just NoOp

                Err _ ->
                    Just NoOp

        "ClickedClaimAction" :: _ ->
            Json.Decode.decodeValue (Json.Decode.field "uint64name" Json.Decode.string) val
                |> Result.map (GotUint64Name >> GotClaimingActionMsg)
                |> Result.toMaybe

        "GotClaimingActionMsg" :: "ConfirmedClaimAction" :: _ ->
            decodeConfirmedClaimAction

        "GotClaimingActionMsg" :: "ConfirmedClaimActionWithPhotoProof" :: _ ->
            decodeConfirmedClaimAction

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        ClickedClaimAction _ _ ->
            [ "ClickedClaimAction" ]

        ClickedShareAction _ ->
            [ "ClickedShareAction" ]

        CopiedShareLinkToClipboard _ ->
            [ "CopiedShareLinkToClipboard" ]

        GotClaimingActionMsg subMsg ->
            "GotClaimingActionMsg" :: claimingActionMsgToString subMsg


claimingActionMsgToString : ClaimingActionMsg -> List String
claimingActionMsgToString msg =
    case msg of
        ClickedCloseClaimModal ->
            [ "ClickedCloseClaimModal" ]

        ConfirmedClaimAction ->
            [ "ConfirmedClaimAction" ]

        ConfirmedClaimActionWithPhotoProof _ ->
            [ "ConfirmedClaimActionWithPhotoProof" ]

        GotPhotoProofFormMsg subMsg ->
            "GotPhotoProofFormMsg" :: Form.msgToString subMsg

        GotUint64Name _ ->
            [ "GotUint64Name" ]

        CompletedClaimingAction r ->
            [ "CompletedClaimingAction", UR.resultToString r ]
