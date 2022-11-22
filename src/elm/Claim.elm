module Claim exposing
    ( Check
    , ClaimId
    , ClaimProfileSummaries
    , ClaimStatus(..)
    , ModalStatus(..)
    , Model
    , Msg(..)
    , Paginated
    , claimPaginatedSelectionSet
    , encodeVerification
    , initClaimProfileSummaries
    , isOpenForVotes
    , isValidated
    , isVotable
    , paginatedPageInfo
    , paginatedToList
    , selectionSet
    , updateClaimModalStatus
    , updateProfileSummaries
    , viewClaimCard
    , viewPhotoModal
    , viewVoteClaimModal
    , viewVotingProgress
    )

import Action exposing (Action)
import Api.Relay as Relay
import Cambiatus.Enum.ClaimStatus as ClaimStatus
import Cambiatus.Object
import Cambiatus.Object.Check as Check
import Cambiatus.Object.Claim as Claim
import Cambiatus.Object.ClaimConnection
import Cambiatus.Object.ClaimEdge
import Cambiatus.Scalar exposing (DateTime)
import Date
import Eos
import Eos.Account as Eos
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, a, button, div, p, span, strong, text)
import Html.Attributes exposing (class, classList, disabled, href, id, style, target)
import Html.Events exposing (onClick)
import Icons
import Json.Encode as Encode
import List.Extra as List
import Markdown
import Profile
import Profile.Summary
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared, Translators)
import Time
import Utils
import View.Components
import View.Modal as Modal


type alias Model =
    { id : ClaimId
    , status : ClaimStatus
    , claimer : Profile.Minimal
    , action : Action
    , checks : List Check
    , createdAt : DateTime
    , proofPhoto : Maybe String
    , proofCode : Maybe String
    }


type ModalStatus
    = Loading ClaimId Bool
    | VoteConfirmationModal ClaimId Bool
    | PhotoModal Model
    | Closed


type ClaimStatus
    = Approved
    | Rejected
    | Pending
    | Cancelled CancelReason


type CancelReason
    = DeadlineReached
    | UsagesReached
    | ActionCompleted


type alias ClaimId =
    Int


type alias Check =
    { isApproved : Bool
    , validator : Profile.Minimal
    }


isValidated : Model -> Eos.Name -> Bool
isValidated claim user =
    claim.status /= Pending || List.any (\c -> c.validator.account == user) claim.checks


isValidator : Eos.Name -> Model -> Bool
isValidator accountName claim =
    claim.action.validators
        |> List.any
            (\v -> v.account == accountName)


isVotable : Model -> Eos.Name -> Time.Posix -> Bool
isVotable claim accountName now =
    isValidator accountName claim
        && not (isValidated claim accountName)
        && not (Action.isClosed claim.action now)
        && not claim.action.isCompleted


isOpenForVotes : Time.Posix -> Model -> Bool
isOpenForVotes now claim =
    let
        isOpen =
            case claim.status of
                Pending ->
                    True

                _ ->
                    False
    in
    not (Action.isClosed claim.action now)
        && not claim.action.isCompleted
        && isOpen


pendingValidators : Model -> List Profile.Minimal
pendingValidators claim =
    List.filter
        (\validator ->
            List.map (\c -> c.validator.account) claim.checks
                |> List.member validator.account
                |> not
        )
        claim.action.validators


encodeVerification : ClaimId -> Eos.Name -> Bool -> Eos.Symbol -> Encode.Value
encodeVerification claimId validator vote communityId =
    let
        encodedClaimId : Encode.Value
        encodedClaimId =
            Encode.int claimId

        encodedVerifier : Encode.Value
        encodedVerifier =
            Eos.encodeName validator

        encodedVote : Encode.Value
        encodedVote =
            vote
                |> Eos.boolToEosBool
                |> Eos.encodeEosBool
    in
    Encode.object
        [ ( "community_id", Eos.encodeSymbol communityId )
        , ( "claim_id", encodedClaimId )
        , ( "verifier", encodedVerifier )
        , ( "vote", encodedVote )
        ]



-- GraphQL


type alias Paginated =
    { edges : Maybe (List (Maybe (Relay.Edge Model)))
    , pageInfo : Relay.PageInfo
    , count : Maybe Int
    }


claimPaginatedSelectionSet : Time.Posix -> SelectionSet Paginated Cambiatus.Object.ClaimConnection
claimPaginatedSelectionSet now =
    SelectionSet.succeed Paginated
        |> with (Cambiatus.Object.ClaimConnection.edges (claimEdgeSelectionSet now))
        |> with (Cambiatus.Object.ClaimConnection.pageInfo Relay.pageInfoSelectionSet)
        |> with Cambiatus.Object.ClaimConnection.count


claimEdgeSelectionSet : Time.Posix -> SelectionSet (Relay.Edge Model) Cambiatus.Object.ClaimEdge
claimEdgeSelectionSet now =
    SelectionSet.succeed Relay.Edge
        |> with Cambiatus.Object.ClaimEdge.cursor
        |> with (Cambiatus.Object.ClaimEdge.node (selectionSet now))


selectionSet : Time.Posix -> SelectionSet Model Cambiatus.Object.Claim
selectionSet now =
    SelectionSet.succeed Model
        |> with Claim.id
        |> with (SelectionSet.map claimStatusMap Claim.status)
        |> with (Claim.claimer Profile.minimalSelectionSet)
        |> with (Claim.action Action.selectionSet)
        |> with (Claim.checks (\_ -> { input = Absent }) checkSelectionSet)
        |> with Claim.createdAt
        |> with (SelectionSet.map emptyStringToNothing Claim.proofPhoto)
        |> with (SelectionSet.map emptyStringToNothing Claim.proofCode)
        |> SelectionSet.map
            (\claim ->
                case claim.status of
                    Pending ->
                        if claim.action.isCompleted then
                            { claim | status = Cancelled ActionCompleted }

                        else if claim.action.usagesLeft == 0 && claim.action.usages > 0 then
                            { claim | status = Cancelled UsagesReached }

                        else
                            case claim.action.deadline of
                                Nothing ->
                                    claim

                                Just deadline ->
                                    if Time.posixToMillis (Utils.fromDateTime deadline) < Time.posixToMillis now then
                                        { claim | status = Cancelled DeadlineReached }

                                    else
                                        claim

                    _ ->
                        claim
            )


emptyStringToNothing : Maybe String -> Maybe String
emptyStringToNothing maybeString =
    maybeString
        |> Maybe.andThen
            (\s ->
                if String.isEmpty s then
                    Nothing

                else
                    Just s
            )


claimStatusMap : ClaimStatus.ClaimStatus -> ClaimStatus
claimStatusMap v =
    case v of
        ClaimStatus.Approved ->
            Approved

        ClaimStatus.Rejected ->
            Rejected

        ClaimStatus.Pending ->
            Pending


checkSelectionSet : SelectionSet Check Cambiatus.Object.Check
checkSelectionSet =
    SelectionSet.succeed Check
        |> with Check.isVerified
        |> with (Check.validator Profile.minimalSelectionSet)


paginatedToList : Maybe Paginated -> List Model
paginatedToList maybeObj =
    let
        toMaybeEdges : Maybe Paginated -> Maybe (List (Maybe (Relay.Edge Model)))
        toMaybeEdges maybeConn =
            Maybe.andThen
                (\a -> a.edges)
                maybeConn

        toEdges : Maybe (List (Maybe (Relay.Edge Model))) -> List (Maybe (Relay.Edge Model))
        toEdges maybeEdges =
            Maybe.withDefault
                []
                maybeEdges

        toMaybeNodes : List (Maybe (Relay.Edge Model)) -> List (Maybe Model)
        toMaybeNodes edges =
            List.map
                (\a ->
                    Maybe.andThen
                        (\b -> b.node)
                        a
                )
                edges

        toNodes : List (Maybe Model) -> List Model
        toNodes maybeNodes =
            List.filterMap identity maybeNodes
    in
    maybeObj
        |> toMaybeEdges
        |> toEdges
        |> toMaybeNodes
        |> toNodes


paginatedPageInfo : Maybe Paginated -> Maybe Relay.PageInfo
paginatedPageInfo maybePaginated =
    Maybe.map
        (\a -> a.pageInfo)
        maybePaginated



-- CLAIM CARD


type alias ClaimProfileSummaries =
    { cardSummary : Profile.Summary.Model
    , topModalSummary : Profile.Summary.Model
    , votersSummaries : List Profile.Summary.Model
    , pendingSummaries : List Profile.Summary.Model
    , showClaimModal : Bool
    }


initClaimProfileSummaries : Model -> ClaimProfileSummaries
initClaimProfileSummaries claim =
    { cardSummary = Profile.Summary.init False
    , topModalSummary = Profile.Summary.init True
    , votersSummaries =
        List.length claim.checks
            |> Profile.Summary.initMany False
    , pendingSummaries =
        List.length (pendingValidators claim)
            |> Profile.Summary.initMany False
    , showClaimModal = False
    }


type ProfileSummaryKind
    = CardSummary
    | TopModalSummary
    | VotersSummaries Int
    | PendingSummaries Int


type alias VoteClaimModalOptions msg =
    { voteMsg : ClaimId -> Bool -> msg
    , closeMsg : msg
    , claimId : ClaimId
    , isApproving : Bool
    , isInProgress : Bool
    }


type Msg
    = OpenVoteModal ClaimId Bool
    | CloseClaimModals
    | OpenPhotoModal Model
    | GotExternalMsg ExternalMsg


type ExternalMsg
    = OpenClaimModal
    | CloseClaimModal
    | GotProfileSummaryMsg ProfileSummaryKind Profile.Summary.Msg


updateClaimModalStatus : Msg -> { m | claimModalStatus : ModalStatus } -> { m | claimModalStatus : ModalStatus }
updateClaimModalStatus msg model =
    case msg of
        OpenVoteModal claimId vote ->
            { model | claimModalStatus = VoteConfirmationModal claimId vote }

        CloseClaimModals ->
            { model | claimModalStatus = Closed }

        OpenPhotoModal claimId ->
            { model | claimModalStatus = PhotoModal claimId }

        GotExternalMsg _ ->
            model


updateProfileSummaries : ExternalMsg -> ClaimProfileSummaries -> ClaimProfileSummaries
updateProfileSummaries externalMsg claimProfileSummaries =
    case externalMsg of
        OpenClaimModal ->
            { claimProfileSummaries | showClaimModal = True }

        CloseClaimModal ->
            { claimProfileSummaries | showClaimModal = False }

        GotProfileSummaryMsg profileSummaryKind subMsg ->
            case profileSummaryKind of
                CardSummary ->
                    { claimProfileSummaries | cardSummary = Profile.Summary.update subMsg claimProfileSummaries.cardSummary }

                TopModalSummary ->
                    { claimProfileSummaries | topModalSummary = Profile.Summary.update subMsg claimProfileSummaries.topModalSummary }

                VotersSummaries index ->
                    { claimProfileSummaries | votersSummaries = List.updateAt index (Profile.Summary.update subMsg) claimProfileSummaries.votersSummaries }

                PendingSummaries index ->
                    { claimProfileSummaries | pendingSummaries = List.updateAt index (Profile.Summary.update subMsg) claimProfileSummaries.pendingSummaries }


{-| Claim card with a short claim overview. Used on Dashboard and Analysis pages.
-}
viewClaimCard : List (Html.Attribute Msg) -> LoggedIn.Model -> ClaimProfileSummaries -> Model -> Html Msg
viewClaimCard attributes loggedIn profileSummaries claim =
    let
        { t, tr } =
            loggedIn.shared.translators

        completionStatus =
            { approved =
                List.filter .isApproved claim.checks
                    |> List.length
            , disapproved =
                List.filter (not << .isApproved) claim.checks
                    |> List.length
            , verifications =
                claim.action.verifications
            , claimStatus = claim.status
            }

        isCancelled =
            case claim.status of
                Cancelled _ ->
                    True

                _ ->
                    False

        claimAging =
            let
                createdAtDate =
                    Utils.fromDateTime claim.createdAt
                        |> Date.fromPosix loggedIn.shared.timezone
            in
            loggedIn.shared.now
                |> Date.fromPosix loggedIn.shared.timezone
                |> Date.diff Date.Days createdAtDate

        claimAgingText =
            if claimAging < 1 then
                t "claim.opened_today"

            else if claimAging == 1 then
                tr "claim.day_ago" [ ( "day_count", String.fromInt claimAging ) ]

            else
                tr "claim.days_ago" [ ( "day_count", String.fromInt claimAging ) ]
    in
    div attributes
        [ viewClaimModal loggedIn profileSummaries claim
        , div
            [ class "flex flex-col p-4 my-2 rounded-lg bg-white hover:shadow cursor-pointer"
            , id ("claim" ++ String.fromInt claim.id)
            , Utils.onClickNoBubble (GotExternalMsg OpenClaimModal)
            ]
            [ div
                [ class "flex mb-4"
                , case claim.proofPhoto of
                    Just _ ->
                        class "justify-between"

                    Nothing ->
                        class "justify-center"
                ]
                [ Profile.Summary.view loggedIn.shared.translators loggedIn.accountName claim.claimer profileSummaries.cardSummary
                    |> Html.map (GotProfileSummaryMsg CardSummary >> GotExternalMsg)
                , case claim.proofPhoto of
                    Just url ->
                        div [ class "claim-photo-thumb" ]
                            [ View.Components.pdfViewer
                                [ Utils.onClickNoBubble (OpenPhotoModal claim)
                                , class "w-full h-full rounded-sm"
                                ]
                                { url = url
                                , childClass = "max-w-full max-h-full"
                                , maybeTranslators = Nothing
                                , onFileTypeDiscovered = Nothing
                                }
                            ]

                    Nothing ->
                        text ""
                ]
            , if isCancelled then
                span [ class "bg-gray-900 text-white font-bold uppercase text-sm py-1 px-5 rounded-label self-center" ]
                    [ text (t "claim.cancelled_texts.cancelled_claim") ]

              else
                span [ class "text-purple-500 font-bold uppercase text-sm self-center" ]
                    [ text claimAgingText ]
            , Markdown.view
                [ class "truncate-children mb-2 mt-6"
                , classList [ ( "text-gray-900", isCancelled ) ]
                ]
                claim.action.description
            , View.Components.dateViewer [ class "text-gray-900 font-bold text-sm uppercase flex mb-6" ]
                identity
                loggedIn.shared
                (Utils.fromDateTime claim.createdAt)
            , case claim.status of
                Cancelled reason ->
                    let
                        reasonText =
                            case reason of
                                DeadlineReached ->
                                    "claim.cancelled_texts.deadline_reached"

                                UsagesReached ->
                                    "claim.cancelled_texts.usages_reached"

                                ActionCompleted ->
                                    "claim.cancelled_texts.action_completed"
                    in
                    p [ class "bg-gray-900 rounded p-4 text-white mb-6" ]
                        ([ "claim.cancelled_texts.was_cancelled"
                         , reasonText
                         , "claim.cancelled_texts.not_available"
                         ]
                            |> List.map t
                            |> String.join ". "
                            |> text
                            |> List.singleton
                        )

                _ ->
                    viewVotingProgress loggedIn.shared completionStatus
            , if
                isValidated claim loggedIn.accountName
                    || not (isValidator loggedIn.accountName claim)
                    || claim.action.isCompleted
                    || Action.isClosed claim.action loggedIn.shared.now
              then
                button
                    [ class "button button-secondary w-full font-semibold mb-2"
                    , Utils.onClickNoBubble (GotExternalMsg OpenClaimModal)
                    ]
                    [ text (t "all_analysis.more_details") ]

              else
                div [ class "flex justify-between space-x-4" ]
                    [ button
                        [ class "button button-danger"
                        , Utils.onClickNoBubble (OpenVoteModal claim.id False)
                        , disabled (not loggedIn.hasAcceptedCodeOfConduct)
                        ]
                        [ text (t "dashboard.reject") ]
                    , button
                        [ class "button button-primary"
                        , Utils.onClickNoBubble (OpenVoteModal claim.id True)
                        , disabled (not loggedIn.hasAcceptedCodeOfConduct)
                        ]
                        [ text (t "dashboard.verify") ]
                    ]
            ]
        ]


type alias CompletionStatus =
    { approved : Int
    , disapproved : Int
    , verifications : Int
    , claimStatus : ClaimStatus
    }


viewVotingProgress : Shared -> CompletionStatus -> Html msg
viewVotingProgress shared completionStatus =
    let
        { t } =
            shared.translators

        totalVotes =
            toFloat completionStatus.verifications

        approvedWidth =
            (toFloat completionStatus.approved / totalVotes) * 100

        disapprovedWidth =
            (toFloat completionStatus.disapproved / totalVotes) * 100

        votingLeft =
            completionStatus.verifications - completionStatus.approved - completionStatus.disapproved

        voteNumberTitleConditional compStatus =
            if compStatus == 1 then
                text (t "claim.vote")

            else
                text (t "claim.votes")

        viewVotesBar =
            case completionStatus.claimStatus of
                Pending ->
                    div []
                        [ div
                            [ class "flex" ]
                            [ p
                                [ class "w-full text-right text-gray-600" ]
                                [ text (t "claim.pending") ]
                            ]
                        , div [ class "w-full h-2 bg-gray-500 flex rounded-full my-2" ]
                            [ div
                                [ class "flex rounded-full overflow-hidden h-2"
                                , style "width" (String.fromFloat (approvedWidth + disapprovedWidth) ++ "%")
                                ]
                                [ div [ class "bg-green", style "width" (String.fromFloat (toFloat completionStatus.approved / toFloat (completionStatus.approved + completionStatus.disapproved) * 100) ++ "%") ] []
                                , div
                                    [ class "bg-red"
                                    , style "width" (String.fromFloat (toFloat completionStatus.disapproved / toFloat (completionStatus.approved + completionStatus.disapproved) * 100) ++ "%")
                                    ]
                                    []
                                ]
                            ]
                        ]

                Cancelled _ ->
                    div []
                        [ div [ class "flex" ]
                            [ p [ class "w-full text-right text-gray-600" ]
                                [ text (t "claim.cancelled") ]
                            ]
                        , div
                            [ class "w-full h-2 flex bg-gray-500 rounded-full my-2" ]
                            [ div
                                [ class "flex rounded-full bg-gray-900 h-2"
                                , style "width" (String.fromFloat (approvedWidth + disapprovedWidth) ++ "%")
                                ]
                                []
                            ]
                        ]

                Approved ->
                    div []
                        [ p
                            [ class "w-full text-right text-green" ]
                            [ text (t "claim.approved") ]
                        , div [ class "w-full h-2 bg-green flex rounded-full my-2" ]
                            []
                        ]

                Rejected ->
                    div []
                        [ p
                            [ class "w-full text-right text-red" ]
                            [ text (t "claim.disapproved") ]
                        , div [ class "w-full h-2 bg-red flex rounded-full my-2" ]
                            []
                        ]
    in
    div
        [ class "flex flex-col mb-4" ]
        [ viewVotesBar
        , case completionStatus.claimStatus of
            Approved ->
                p
                    [ class "ml-auto text-green text-right" ]
                    [ span [ class "font-bold mr-2" ]
                        [ text (String.fromInt completionStatus.approved)
                        ]
                    , span []
                        [ voteNumberTitleConditional completionStatus.approved ]
                    ]

            Rejected ->
                p
                    [ class "ml-auto text-red text-right" ]
                    [ span [ class "font-bold mr-2" ]
                        [ text (String.fromInt completionStatus.disapproved)
                        ]
                    , span []
                        [ voteNumberTitleConditional completionStatus.disapproved ]
                    ]

            Pending ->
                div [ class "flex" ]
                    [ if completionStatus.approved == 0 then
                        text ""

                      else
                        p
                            [ style "width" (String.fromFloat approvedWidth ++ "%") ]
                            [ span [ class "font-bold text-green" ]
                                [ text (String.fromInt completionStatus.approved)
                                ]
                            ]
                    , if completionStatus.disapproved == 0 then
                        text ""

                      else
                        p
                            [ style "width" (String.fromFloat disapprovedWidth ++ "%") ]
                            [ span [ class "font-bold text-red" ]
                                [ text (String.fromInt completionStatus.disapproved)
                                ]
                            ]
                    , p
                        [ class "ml-auto text-gray-600 text-right" ]
                        [ span [ class "font-bold mr-2" ]
                            [ text (String.fromInt votingLeft)
                            ]
                        , span []
                            [ voteNumberTitleConditional votingLeft ]
                        ]
                    ]

            Cancelled _ ->
                p [ class "w-full text-gray-600 text-right space-x-1" ]
                    [ span [ class "font-bold" ] [ text (String.fromInt votingLeft) ]
                    , span [] [ text (t "claim.cancelled_voting_bar") ]
                    ]
        ]


viewClaimModal : LoggedIn.Model -> ClaimProfileSummaries -> Model -> Html Msg
viewClaimModal ({ shared, accountName } as loggedIn) profileSummaries claim =
    let
        { t } =
            shared.translators

        claimVerifiersSectionClass =
            "block my-6 h-auto space-y-4"

        claimVerifiersProfileListContainerClass =
            "grid grid-cols-3 md:flex md:flex-wrap md:space-x-6 py-2"

        claimVerifiersFewerProfiles =
            "flex flex-wrap space-x-6"

        profileSummaryEmpty =
            div [ class claimVerifiersFewerProfiles ]
                [ div [ class "mb-10" ] [ Profile.viewEmpty shared.translators ]
                ]

        viewProfileSummary profile_ profileSummary_ =
            profileSummary_
                |> Profile.Summary.withPreventScrolling View.Components.PreventScrollAlways
                |> Profile.Summary.withRelativeSelector ".modal-content"
                |> Profile.Summary.withScrollSelector ".modal-body-lg"
                |> Profile.Summary.view shared.translators accountName profile_

        viewClaimerProfileSummary =
            viewProfileSummary claim.claimer profileSummaries.topModalSummary
                |> Html.map (GotProfileSummaryMsg TopModalSummary >> GotExternalMsg)

        viewClaimDateAndState =
            let
                ( claimStatusPhrase, claimStatus, textColor ) =
                    case claim.status of
                        Approved ->
                            ( t "claim.title_approved.1", t "claim.title_approved.2", "text-lg font-bold lowercase text-green" )

                        Rejected ->
                            ( t "claim.title_rejected.1", t "claim.title_rejected.2", "text-lg font-bold lowercase text-red" )

                        Pending ->
                            ( t "claim.title_under_review.1", t "claim.pending", "text-lg font-bold lowercase text-gray-600" )

                        Cancelled _ ->
                            ( t "claim.title_cancelled.1", t "claim.title_cancelled.2", "text-lg font-bold lowercase text-gray-900" )
            in
            div [ class "block" ]
                [ View.Components.dateViewer [ class "text-xs uppercase block text-gray-900" ]
                    (\translations ->
                        { translations
                            | today = Just (t "claim.claimed_today")
                            , yesterday = Just (t "claim.claimed_yesterday")
                            , other = t "claim.claimed_on"
                        }
                    )
                    shared
                    (Utils.fromDateTime claim.createdAt)
                , p [ class "text-lg font-bold" ]
                    [ text claimStatusPhrase ]
                , div [ class textColor ] [ text claimStatus ]
                ]

        viewCancelledNotice =
            case claim.status of
                Cancelled reason ->
                    let
                        reasonText =
                            case reason of
                                DeadlineReached ->
                                    "claim.cancelled_texts.deadline_reached"

                                UsagesReached ->
                                    "claim.cancelled_texts.usages_reached"

                                ActionCompleted ->
                                    "claim.cancelled_texts.action_completed"
                    in
                    p [ class "text-gray-900 my-10" ]
                        [ span [ class "text-black" ] [ text (t "claim.attention") ]
                        , [ reasonText
                          , "claim.cancelled_texts.not_available"
                          ]
                            |> List.map t
                            |> String.join ". "
                            |> text
                        ]

                _ ->
                    text ""

        viewRewardInfo =
            let
                rewardTxtClass =
                    "my-2 font-bold text-lg"

                makeAsset float =
                    { amount = float
                    , symbol = claim.action.objective.community.symbol
                    }
            in
            div
                [ class "text-center flex justify-center bg-gray-100 rounded-md my-10 p-4 space-x-16" ]
                [ div
                    []
                    [ p [ class rewardTxtClass ]
                        [ makeAsset claim.action.reward
                            |> Eos.assetToString shared.translators
                            |> text
                        ]
                    , p [ class "text-sm text-green uppercase" ] [ text (t "community.actions.reward") ]
                    ]
                , div []
                    [ p [ class rewardTxtClass ]
                        [ makeAsset claim.action.verifierReward
                            |> Eos.assetToString shared.translators
                            |> text
                        ]
                    , p [ class "text-sm text-green uppercase" ] [ text (t "claim.analyst_reward") ]
                    ]
                ]

        viewApprovedByProfileSummaryList =
            let
                cls =
                    if List.length claim.checks > 3 then
                        claimVerifiersProfileListContainerClass

                    else
                        claimVerifiersFewerProfiles
            in
            div
                [ class claimVerifiersSectionClass ]
                [ p [ class "label" ] [ text (t "claim.approved_by") ]
                , div []
                    [ if List.any .isApproved claim.checks |> not then
                        profileSummaryEmpty

                      else
                        div [ class cls ]
                            (List.map3
                                (\profileSummary index c ->
                                    if c.isApproved then
                                        div [ class "mb-4" ]
                                            [ viewProfileSummary c.validator profileSummary
                                                |> Html.map (GotProfileSummaryMsg (VotersSummaries index) >> GotExternalMsg)
                                            ]

                                    else
                                        text ""
                                )
                                profileSummaries.votersSummaries
                                (List.range 0 (List.length profileSummaries.votersSummaries))
                                claim.checks
                            )
                    ]
                ]

        viewDisapprovedByProfileSummaryList =
            let
                cls =
                    if List.length claim.checks > 3 then
                        claimVerifiersProfileListContainerClass

                    else
                        claimVerifiersFewerProfiles
            in
            div
                [ class claimVerifiersSectionClass ]
                [ p [ class "label" ] [ text (t "claim.disapproved_by") ]
                , div []
                    [ if List.filter (\check -> check.isApproved == False) claim.checks |> List.isEmpty then
                        profileSummaryEmpty

                      else
                        div [ class cls ]
                            (List.map3
                                (\profileSummary index c ->
                                    if not c.isApproved then
                                        div [ class "mb-4" ]
                                            [ viewProfileSummary c.validator profileSummary
                                                |> Html.map (GotProfileSummaryMsg (VotersSummaries index) >> GotExternalMsg)
                                            ]

                                    else
                                        text ""
                                )
                                profileSummaries.votersSummaries
                                (List.range 0 (List.length profileSummaries.votersSummaries))
                                claim.checks
                            )
                    ]
                ]

        viewPendingVotersProfileSummaryList =
            let
                cls =
                    if List.length profileSummaries.pendingSummaries > 3 then
                        claimVerifiersProfileListContainerClass

                    else
                        claimVerifiersFewerProfiles
            in
            div
                [ class claimVerifiersSectionClass ]
                [ p [ class "label" ] [ text (t "claim.pending_vote") ]
                , div []
                    [ if List.length claim.checks == claim.action.verifications then
                        profileSummaryEmpty

                      else
                        div [ class cls ]
                            (pendingValidators claim
                                |> List.map3
                                    (\profileSummary index v ->
                                        div [ class "mb-4" ]
                                            [ viewProfileSummary v profileSummary
                                                |> Html.map (GotProfileSummaryMsg (PendingSummaries index) >> GotExternalMsg)
                                            ]
                                    )
                                    profileSummaries.pendingSummaries
                                    (List.range 0 (List.length profileSummaries.pendingSummaries))
                            )
                    ]
                ]

        viewActionDetails =
            let
                proofCode =
                    case claim.proofCode of
                        Just code ->
                            code

                        Nothing ->
                            ""
            in
            div
                [ class "block mt-6" ]
                [ p [ class "label" ] [ text (t "claim.action") ]
                , Markdown.view [ class "text-left mt-2 mb-6 text-lg w-full" ]
                    claim.action.description
                , case claim.proofPhoto of
                    Just url ->
                        div
                            [ class "relative h-auto w-auto text-center text-white"
                            , Utils.onClickNoBubble (OpenPhotoModal claim)
                            ]
                            [ div
                                [ class "z-10 absolute bottom-4 left-4 bg-black bg-opacity-60 p-4" ]
                                [ p [ class "text-sm text-left w-full uppercase" ] [ text (t "community.actions.form.verification_code") ]
                                , p [ class "font-bold font-normal text-left w-full" ] [ text proofCode ]
                                ]
                            , View.Components.pdfViewer
                                [ Utils.onClickNoBubble (OpenPhotoModal claim)
                                , class "w-full h-full min-h-48 rounded-sm bg-gray-300"
                                ]
                                { url = url
                                , childClass = "max-w-full max-h-full"
                                , maybeTranslators = Nothing
                                , onFileTypeDiscovered = Nothing
                                }
                            ]

                    Nothing ->
                        text ""
                ]

        header =
            div [ class "flex space-x-6 md:justify-center md:space-x-8 md:mt-4" ]
                [ viewClaimerProfileSummary
                , viewClaimDateAndState
                ]

        completionStatus =
            { approved =
                List.filter .isApproved claim.checks
                    |> List.length
            , disapproved =
                List.filter (not << .isApproved) claim.checks
                    |> List.length
            , verifications =
                claim.action.verifications
            , claimStatus = claim.status
            }

        claimRoute =
            Route.Claim
                (Action.objectiveIdToInt claim.action.objective.id)
                (Action.idToInt claim.action.id)
                claim.id

        claimDetailsButton =
            a
                [ class "button button-primary w-full mt-4"
                , target "_blank"
                , Route.href claimRoute
                ]
                [ text (t "claim.modal.view_details")
                , Icons.externalLink "inline-block ml-4 h-3 fill-current"
                ]

        footer =
            div [ class "block w-full my-4 sm:w-1/2 sm:mx-auto" ]
                [ if isVotable claim accountName shared.now then
                    div [ class "flex space-x-4" ]
                        [ button
                            [ class "w-full button button-danger"
                            , onClick (OpenVoteModal claim.id False)
                            , disabled (not loggedIn.hasAcceptedCodeOfConduct)
                            ]
                            [ text (t "dashboard.reject") ]
                        , button
                            [ class "w-full button button-primary"
                            , onClick (OpenVoteModal claim.id True)
                            , disabled (not loggedIn.hasAcceptedCodeOfConduct)
                            ]
                            [ text (t "dashboard.verify") ]
                        ]

                  else
                    text ""
                , claimDetailsButton
                ]
    in
    div
        []
        [ Modal.initWith
            { closeMsg = GotExternalMsg CloseClaimModal
            , isVisible = profileSummaries.showClaimModal
            }
            |> Modal.withBody
                [ header
                , viewCancelledNotice
                , viewVotingProgress shared completionStatus
                , viewRewardInfo
                , viewApprovedByProfileSummaryList
                , viewDisapprovedByProfileSummaryList
                , viewPendingVotersProfileSummaryList
                , viewActionDetails
                ]
            |> Modal.withFooter [ footer ]
            |> Modal.withSize Modal.Large
            |> Modal.toHtml
        ]


viewPhotoModal : LoggedIn.Model -> Model -> Html Msg
viewPhotoModal loggedIn claim =
    let
        { t } =
            loggedIn.shared.translators

        body =
            [ div [ class "md:flex md:justify-start md:space-x-4" ]
                [ case claim.proofPhoto of
                    Just url ->
                        div [ class "sm:w-1/2" ]
                            [ View.Components.pdfViewer
                                [ class "min-h-[100px] max-h-[42vh] rounded-sm" ]
                                { url = url
                                , childClass = "max-w-full max-h-full"
                                , maybeTranslators = Just loggedIn.shared.translators
                                , onFileTypeDiscovered = Nothing
                                }
                            , a
                                [ class "underline inline-block py-1 text-gray"
                                , href url
                                , target "_blank"
                                ]
                                [ text (t "community.actions.proof.upload_full")
                                , Icons.externalLink "inline-block ml-1 h-3 fill-current"
                                ]
                            ]

                    Nothing ->
                        text ""
                , case claim.proofCode of
                    Just proofCode ->
                        div []
                            [ p [ class "mt-4 md:mt-0 label md:text-xl block" ]
                                [ text (t "community.actions.form.verification_code") ]
                            , strong [ class "text-xl md:text-3xl" ] [ text proofCode ]
                            ]

                    Nothing ->
                        text ""
                ]
            ]

        withPhotoModalFooter =
            if isVotable claim loggedIn.accountName loggedIn.shared.now then
                Modal.withFooter
                    [ button
                        [ class "modal-cancel"
                        , onClick (OpenVoteModal claim.id False)
                        ]
                        [ text <| t "dashboard.reject" ]
                    , button
                        [ class "modal-accept"
                        , onClick (OpenVoteModal claim.id True)
                        ]
                        [ text <| t "dashboard.verify" ]
                    ]

            else
                -- Don't show footer if the Claim is already validated by the current user
                -- or if current user is not a validator.
                identity
    in
    Modal.initWith
        { closeMsg = CloseClaimModals
        , isVisible = True
        }
        |> Modal.withHeader (t "dashboard.claim")
        |> Modal.withBody body
        |> withPhotoModalFooter
        |> Modal.toHtml


viewVoteClaimModal : Translators -> VoteClaimModalOptions msg -> Html msg
viewVoteClaimModal { t } { voteMsg, closeMsg, claimId, isApproving, isInProgress } =
    let
        text_ s =
            text (t s)

        body =
            [ if isApproving then
                text_ "claim.modal.message_approve"

              else
                text_ "claim.modal.message_disapprove"
            ]

        footer =
            [ button
                [ class "modal-cancel"
                , onClick closeMsg
                , classList [ ( "button-disabled", isInProgress ) ]
                , disabled isInProgress
                ]
                [ text_ "claim.modal.secondary" ]
            , button
                [ class "modal-accept"
                , classList [ ( "button-disabled", isInProgress ) ]
                , disabled isInProgress
                , onClick (voteMsg claimId isApproving)
                ]
                [ if isApproving then
                    text_ "claim.modal.primary_approve"

                  else
                    text_ "claim.modal.primary_disapprove"
                ]
            ]
    in
    Modal.initWith
        { closeMsg = closeMsg
        , isVisible = True
        }
        |> Modal.withHeader (t "claim.modal.title")
        |> Modal.withBody body
        |> Modal.withFooter footer
        |> Modal.toHtml
