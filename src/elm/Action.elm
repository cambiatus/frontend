module Action exposing
    ( Action, Objective
    , Id, idFromInt, idFromString, idToInt, idToString
    , ObjectiveId, objectiveIdSelectionSet, objectiveIdFromInt, objectiveIdToInt
    , isClosed, isPastDeadline, isObjectiveClosed, daysLeft, limitNotice
    , selectionSet, selectionSetWithoutEligibility
    , Msg, update, ExternalMsg(..), msgToString, jsAddressToMsg
    , viewCard
    )

{-|


## Main types

@docs Action, Objective


### Ids


#### Action Ids

@docs Id, idFromInt, idFromString, idToInt, idToString


#### Objective Ids

@docs ObjectiveId, objectiveIdSelectionSet, objectiveIdFromInt, objectiveIdToInt


## Helper functions

@docs isClosed, isPastDeadline, isObjectiveClosed, daysLeft, limitNotice


## GraphQL & EOS

@docs selectionSet, selectionSetWithoutEligibility


## Claiming actions

@docs Msg, update, ExternalMsg, msgToString, jsAddressToMsg


### Views

@docs viewCard

-}

import Auth
import Browser.Dom
import Cambiatus.Enum.ClaimEligibilityStatus as ClaimEligibilityStatus exposing (ClaimEligibilityStatus)
import Cambiatus.Enum.VerificationType as VerificationType exposing (VerificationType)
import Cambiatus.Object
import Cambiatus.Object.Action
import Cambiatus.Object.ClaimEligibility
import Cambiatus.Object.Community
import Cambiatus.Object.Objective
import Cambiatus.Scalar exposing (DateTime)
import ClaimMoved
import Eos
import Eos.Account
import Form.Text
import Graphql.Http
import Graphql.OptionalArgument as OptionalArgument
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html, button, div, h4, img, li, span, text)
import Html.Attributes exposing (alt, class, classList, id, src, tabindex)
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
import Task
import Time
import Translation
import UpdateResult as UR
import Url
import Utils
import View.Feedback


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
    , maxClaimsPerAccount : Maybe Int
    , myClaimEligibility : Maybe ClaimEligibility
    }


type alias ClaimEligibility =
    { status : ClaimEligibilityStatus
    , availableAt : Maybe DateTime
    , approvedCount : Int
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


type Msg
    = NoOp
    | ClickedShareAction Action
    | CopiedShareLinkToClipboard Id


type alias UpdateResult =
    UR.UpdateResult () Msg ExternalMsg


type ExternalMsg
    = ShowFeedback View.Feedback.Model


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


update : Msg -> LoggedIn loggedIn community -> UpdateResult
update msg loggedIn =
    case msg of
        NoOp ->
            UR.init ()

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
            UR.init ()
                |> UR.addPort sharePort

        CopiedShareLinkToClipboard actionId ->
            UR.init ()
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

        maybeLimitNotice =
            limitNotice translators loggedIn.shared.now action

        canBeClaimed =
            isClaimable action && isOpenForNewClaims action loggedIn.shared.now && maybeLimitNotice == Nothing
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
                        if isOpenForNewClaims action loggedIn.shared.now then
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
                    , viewClaimProgress translators action
                    ]
                ]
            , case maybeLimitNotice of
                Just notice ->
                    viewNotAbleToClaimNotice notice

                Nothing ->
                    if not (isOpenForNewClaims action loggedIn.shared.now) then
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
                    ClaimMoved.viewClaimLink translators loggedIn.shared.endpoints.api (idToInt action.id)

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


viewClaimProgress : Translation.Translators -> Action -> Html msg
viewClaimProgress { tr } action =
    case ( action.maxClaimsPerAccount, action.myClaimEligibility ) of
        ( Just maxClaims, Just eligibility ) ->
            if maxClaims > 1 && eligibility.status == ClaimEligibilityStatus.Available && eligibility.approvedCount > 0 then
                span [ class "text-sm text-gray-900" ]
                    [ text
                        (tr "community.objectives.claim_limit.progress"
                            [ ( "done", String.fromInt eligibility.approvedCount )
                            , ( "max", String.fromInt maxClaims )
                            ]
                        )
                    ]

            else
                text ""

        _ ->
            text ""



-- GRAPHQL


selectionSet : SelectionSet Action Cambiatus.Object.Action
selectionSet =
    actionFieldsSelectionSet
        |> SelectionSet.with Cambiatus.Object.Action.maxClaimsPerAccount
        |> SelectionSet.with (Cambiatus.Object.Action.myClaimEligibility claimEligibilitySelectionSet)


{-| The same as `selectionSet`, but without the claim limit fields. Use it where the
eligibility isn't needed (for example, on claim pages), so those queries don't depend on the
eligibility resolver.
-}
selectionSetWithoutEligibility : SelectionSet Action Cambiatus.Object.Action
selectionSetWithoutEligibility =
    actionFieldsSelectionSet
        |> SelectionSet.hardcoded Nothing
        |> SelectionSet.hardcoded Nothing


actionFieldsSelectionSet : SelectionSet (Maybe Int -> Maybe ClaimEligibility -> Action) Cambiatus.Object.Action
actionFieldsSelectionSet =
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


claimEligibilitySelectionSet : SelectionSet ClaimEligibility Cambiatus.Object.ClaimEligibility
claimEligibilitySelectionSet =
    SelectionSet.succeed ClaimEligibility
        |> SelectionSet.with Cambiatus.Object.ClaimEligibility.status
        |> SelectionSet.with Cambiatus.Object.ClaimEligibility.availableAt
        |> SelectionSet.with Cambiatus.Object.ClaimEligibility.approvedCount


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



-- JSON


encodeId : Id -> Json.Encode.Value
encodeId (Id id) =
    Json.Encode.int id


decodeId : Json.Decode.Decoder Id
decodeId =
    Json.Decode.map Id Json.Decode.int



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


{-| Whether the objective holding this action has been completed.

The contract has no idea this concept exists — `cambiatus.cm`'s objective table
has no completion column, and `claimaction` only ever inspects the action — so
completing an objective closes its actions on chain one `upsertaction` at a time.
An action that pass missed stays claimable forever, which is why closing an
objective is not enough on its own to keep it out of the UI.

-}
isObjectiveClosed : Action -> Bool
isObjectiveClosed action =
    action.objective.isCompleted


{-| Whether a fresh claim on this action would still be accepted: the action is
open on chain AND its objective is still live.

This is deliberately not folded into [`isClosed`](#isClosed), which also decides
whether an _existing_ claim can still be voted on. A claim filed before its
objective was completed still has to reach a verdict, so closing the objective
must stop new claims without stranding the ones already open.

-}
isOpenForNewClaims : Action -> Time.Posix -> Bool
isOpenForNewClaims action now =
    not (isClosed action now)
        && not action.isCompleted
        && not (isObjectiveClosed action)


{-| Whole days until `availableAt`, rounded up, and never less than one
-}
daysLeft : Time.Posix -> Time.Posix -> Int
daysLeft now availableAt =
    let
        msLeft =
            Time.posixToMillis availableAt - Time.posixToMillis now
    in
    max 1 (ceiling (toFloat msLeft / 86400000))


{-| The message explaining why a member can't claim this action because of a claim limit.
`Nothing` means the member may claim it.
-}
limitNotice : Translation.Translators -> Time.Posix -> Action -> Maybe String
limitNotice { t, tr } now action =
    case action.myClaimEligibility of
        Nothing ->
            Nothing

        Just eligibility ->
            case eligibility.status of
                ClaimEligibilityStatus.Completed ->
                    Just (t "community.objectives.claim_limit.completed")

                ClaimEligibilityStatus.Pending ->
                    Just (t "community.objectives.claim_limit.pending")

                ClaimEligibilityStatus.Cooldown ->
                    case eligibility.availableAt of
                        Nothing ->
                            Just (tr "community.objectives.claim_limit.cooldown_one" [])

                        Just _ ->
                            let
                                availableAt =
                                    Utils.fromMaybeDateTime eligibility.availableAt
                            in
                            if Time.posixToMillis availableAt <= Time.posixToMillis now then
                                Nothing

                            else
                                case daysLeft now availableAt of
                                    1 ->
                                        Just (tr "community.objectives.claim_limit.cooldown_one" [])

                                    days ->
                                        Just (tr "community.objectives.claim_limit.cooldown" [ ( "count", String.fromInt days ) ])

                ClaimEligibilityStatus.Available ->
                    Nothing


shareActionButtonId : Id -> String
shareActionButtonId actionId =
    "share-action-button-" ++ String.fromInt (idToInt actionId)


shareActionFallbackId : Id -> String
shareActionFallbackId actionId =
    "share-action-fallback-" ++ String.fromInt (idToInt actionId)


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

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        ClickedShareAction _ ->
            [ "ClickedShareAction" ]

        CopiedShareLinkToClipboard _ ->
            [ "CopiedShareLinkToClipboard" ]
