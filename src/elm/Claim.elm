module Claim exposing
    ( ClaimStatus(..)
    , Model
    , Paginated
    , claimPaginatedSelectionSet
    , encodeVerification
    , isValidated
    , paginatedPageInfo
    , paginatedToList
    , selectionSet
    , viewClaimCard
    , viewVoteClaimModal
    )

import Api.Relay exposing (Edge, PageConnection)
import Cambiatus.Enum.ClaimStatus as ClaimStatus
import Cambiatus.Enum.VerificationType exposing (VerificationType(..))
import Cambiatus.Object
import Cambiatus.Object.Action as Action
import Cambiatus.Object.Check as Check
import Cambiatus.Object.Claim as Claim
import Cambiatus.Object.ClaimConnection
import Cambiatus.Object.ClaimEdge
import Cambiatus.Scalar exposing (DateTime(..))
import Community exposing (Objective)
import Eos exposing (Symbol)
import Eos.Account
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, a, button, div, p, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Json.Encode as Encode
import Profile exposing (Profile)
import Route
import Session.Shared exposing (Shared, Translators)
import Strftime
import Time
import Utils
import View.Modal as Modal


type alias Model =
    { id : Int
    , status : ClaimStatus
    , claimer : Profile
    , action : Action
    , checks : List Check
    , createdAt : DateTime
    }


type ClaimStatus
    = Approved
    | Rejected
    | Pending


type alias Check =
    { isApproved : Bool
    , validator : Profile
    }


type alias Action =
    { id : Int
    , description : String
    , reward : Float
    , verifierReward : Float
    , validators : List Profile
    , verifications : Int
    , verificationType : VerificationType
    , objective : Objective
    , createdAt : DateTime
    }


isValidated : Model -> Eos.Account.Name -> Bool
isValidated claim user =
    claim.status /= Pending || List.any (\c -> c.validator.account == user) claim.checks


encodeVerification : Int -> Eos.Account.Name -> Bool -> Encode.Value
encodeVerification claimId validator vote =
    let
        encodedClaimId : Encode.Value
        encodedClaimId =
            Encode.int claimId

        encodedVerifier : Encode.Value
        encodedVerifier =
            Eos.Account.encodeName validator

        encodedVote : Encode.Value
        encodedVote =
            vote
                |> Eos.boolToEosBool
                |> Eos.encodeEosBool
    in
    Encode.object
        [ ( "claim_id", encodedClaimId )
        , ( "verifier", encodedVerifier )
        , ( "vote", encodedVote )
        ]



-- GraphQL


type alias Paginated =
    PageConnection Model


claimPaginatedSelectionSet : SelectionSet Paginated Cambiatus.Object.ClaimConnection
claimPaginatedSelectionSet =
    SelectionSet.succeed PageConnection
        |> with (Cambiatus.Object.ClaimConnection.edges claimEdgeSelectionSet)
        |> with (Cambiatus.Object.ClaimConnection.pageInfo Api.Relay.pageInfoSelectionSet)


claimEdgeSelectionSet : SelectionSet (Edge Model) Cambiatus.Object.ClaimEdge
claimEdgeSelectionSet =
    SelectionSet.succeed Edge
        |> with Cambiatus.Object.ClaimEdge.cursor
        |> with (Cambiatus.Object.ClaimEdge.node selectionSet)


selectionSet : SelectionSet Model Cambiatus.Object.Claim
selectionSet =
    SelectionSet.succeed Model
        |> with Claim.id
        |> with (SelectionSet.map claimStatusMap Claim.status)
        |> with (Claim.claimer Profile.selectionSet)
        |> with (Claim.action actionSelectionSet)
        |> with (Claim.checks (\_ -> { input = Absent }) checkSelectionSet)
        |> with Claim.createdAt


claimStatusMap : ClaimStatus.ClaimStatus -> ClaimStatus
claimStatusMap v =
    case v of
        ClaimStatus.Approved ->
            Approved

        ClaimStatus.Rejected ->
            Rejected

        ClaimStatus.Pending ->
            Pending


actionSelectionSet : SelectionSet Action Cambiatus.Object.Action
actionSelectionSet =
    SelectionSet.succeed Action
        |> with Action.id
        |> with Action.description
        |> with Action.reward
        |> with Action.verifierReward
        |> with (Action.validators Profile.selectionSet)
        |> with Action.verifications
        |> with Action.verificationType
        |> with (Action.objective Community.objectiveSelectionSet)
        |> with Action.createdAt


checkSelectionSet : SelectionSet Check Cambiatus.Object.Check
checkSelectionSet =
    SelectionSet.succeed Check
        |> with Check.isVerified
        |> with (Check.validator Profile.selectionSet)


paginatedToList : Maybe Paginated -> List Model
paginatedToList maybeObj =
    let
        toMaybeEdges : Maybe Paginated -> Maybe (List (Maybe (Edge Model)))
        toMaybeEdges maybeConn =
            Maybe.andThen
                (\a -> a.edges)
                maybeConn

        toEdges : Maybe (List (Maybe (Edge Model))) -> List (Maybe (Edge Model))
        toEdges maybeEdges =
            Maybe.withDefault
                []
                maybeEdges

        toMaybeNodes : List (Maybe (Edge Model)) -> List (Maybe Model)
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


paginatedPageInfo : Maybe Paginated -> Maybe Api.Relay.PageInfo
paginatedPageInfo maybePaginated =
    Maybe.map
        (\a -> a.pageInfo)
        maybePaginated



-- CLAIM CARD


type alias ClaimCardOptions msg =
    { claim : Model
    , selectedCommunity : Symbol
    , shared : Shared
    , accountName : Eos.Account.Name
    , openConfirmationModalMsg : Int -> Bool -> msg
    }


type alias VoteClaimModalOptions msg =
    { voteMsg : Int -> Bool -> msg
    , closeMsg : msg
    , claimId : Int
    , isApproving : Bool
    }


{-| Claim card with a short claim overview. Used on Dashboard and Analysis pages.
-}
viewClaimCard : ClaimCardOptions msg -> Html msg
viewClaimCard { claim, selectedCommunity, shared, accountName, openConfirmationModalMsg } =
    let
        { t } =
            shared.translators

        date dateTime =
            Just dateTime
                |> Utils.posixDateTime
                |> Strftime.format "%d %b %Y" Time.utc

        ( claimStatus, textColor ) =
            case claim.status of
                Approved ->
                    ( t "all_analysis.approved", "text-green" )

                Rejected ->
                    ( t "all_analysis.disapproved", "text-red" )

                Pending ->
                    ( t "all_analysis.pending", "text-black" )

        claimRoute =
            Route.Claim
                selectedCommunity
                claim.action.objective.id
                claim.action.id
                claim.id
    in
    div [ class "w-full sm:w-1/2 lg:w-1/3 xl:w-1/4 px-2 mb-4" ]
        [ div
            [ class "flex flex-col p-4 my-2 rounded-lg bg-white hover:shadow"
            , id ("claim" ++ String.fromInt claim.id)
            ]
            [ a [ Route.href claimRoute ]
                [ div [ class "flex justify-center mb-8" ]
                    [ Profile.view shared accountName claim.claimer ]
                , div [ class "bg-gray-100 flex items-center justify-center h-6 w-32 mb-2" ]
                    [ p
                        [ class ("text-caption uppercase " ++ textColor) ]
                        [ text claimStatus ]
                    ]
                , div [ class "mb-6" ]
                    [ p [ class "text-body" ]
                        [ text claim.action.description ]
                    , p
                        [ class "text-gray-900 text-caption uppercase" ]
                        [ text (date claim.createdAt) ]
                    ]
                ]
            , if isValidated claim accountName then
                a
                    [ class "button button-secondary w-full font-medium mb-2"
                    , Route.href claimRoute
                    ]
                    [ text (t "all_analysis.more_details") ]

              else
                div [ class "flex justify-between space-x-4" ]
                    [ button
                        [ class "button button-danger"
                        , onClick (openConfirmationModalMsg claim.id False)
                        ]
                        [ text (t "dashboard.reject") ]
                    , button
                        [ class "button button-primary"
                        , onClick (openConfirmationModalMsg claim.id True)
                        ]
                        [ text (t "dashboard.verify") ]
                    ]
            ]
        ]


viewVoteClaimModal : Translators -> VoteClaimModalOptions msg -> Html msg
viewVoteClaimModal { t } { voteMsg, closeMsg, claimId, isApproving } =
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
            [ button [ class "modal-cancel", onClick closeMsg ]
                [ text_ "claim.modal.secondary" ]
            , button [ class "modal-accept", onClick (voteMsg claimId isApproving) ]
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
