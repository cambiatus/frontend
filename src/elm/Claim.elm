module Claim exposing
    ( ClaimStatus(..)
    , ModalStatus(..)
    , Model
    , Msg(..)
    , Paginated
    , claimPaginatedSelectionSet
    , encodeVerification
    , isValidated
    , paginatedPageInfo
    , paginatedToList
    , selectionSet
    , updateClaimModalStatus
    , viewClaimCard
    , viewPhotoModal
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
import Eos
import Eos.Account
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, a, button, div, img, label, p, strong, text)
import Html.Attributes exposing (class, classList, disabled, id, src, style)
import Html.Events exposing (onClick)
import Json.Encode as Encode
import Profile exposing (Profile)
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Translators)
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


type ModalStatus
    = Loading Int Bool
    | VoteConfirmationModal Int Bool
    | PhotoModal Model
    | Closed


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


type alias VoteClaimModalOptions msg =
    { voteMsg : Int -> Bool -> msg
    , closeMsg : msg
    , claimId : Int
    , isApproving : Bool
    , isInProgress : Bool
    }


type Msg
    = OpenVoteModal Int Bool
    | CloseClaimModals
    | OpenPhotoModal Model


updateClaimModalStatus : Msg -> { m | claimModalStatus : ModalStatus } -> { m | claimModalStatus : ModalStatus }
updateClaimModalStatus msg model =
    case msg of
        OpenVoteModal claimId vote ->
            { model | claimModalStatus = VoteConfirmationModal claimId vote }

        CloseClaimModals ->
            { model | claimModalStatus = Closed }

        OpenPhotoModal claimId ->
            { model | claimModalStatus = PhotoModal claimId }


{-| Claim card with a short claim overview. Used on Dashboard and Analysis pages.
-}
viewClaimCard : LoggedIn.Model -> (Int -> Bool -> msg) -> (Model -> msg) -> Model -> Html msg
viewClaimCard { selectedCommunity, shared, accountName } openConfirmationModalMsg openPhotoMsg claim =
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

        hasPhotoProof =
            -- TODO: replace this placeholder with the real data
            claim.id == 41 || claim.id == 47
    in
    div [ class "w-full sm:w-1/2 lg:w-1/3 xl:w-1/4 px-2 mb-4" ]
        [ div
            [ class "flex flex-col p-4 my-2 rounded-lg bg-white hover:shadow"
            , id ("claim" ++ String.fromInt claim.id)
            ]
            [ -- a [ Route.href claimRoute ]
              div []
                [ div
                    [ class "flex mb-8"
                    , classList
                        [ ( "justify-center", not <| hasPhotoProof )
                        , ( "justify-between", hasPhotoProof )
                        ]
                    ]
                    [ Profile.view shared accountName claim.claimer
                    , if hasPhotoProof then
                        div [ class "claim-photo-thumb" ]
                            [ img [ onClick (openPhotoMsg claim), src "http://cambiatus.miskov.ru/trash.png" ] [] ]

                      else
                        text ""
                    ]
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


viewPhotoModal : LoggedIn.Model -> Model -> Html Msg
viewPhotoModal loggedIn claim =
    let
        { t } =
            loggedIn.shared.translators

        body =
            [ div [ class "md:flex md:justify-start md:space-x-4" ]
                [ img [ style "max-height" "42vh", src "http://cambiatus.miskov.ru/trash.png" ] []
                , div []
                    [ label [ class "mt-6 md:mt-0 input-label md:text-xl block" ]
                        [ text "verification number"
                        ]
                    , strong [ class "text-xl md:text-3xl" ] [ text "82378463" ]
                    ]
                ]
            ]

        withPhotoModalFooter =
            if isValidated claim loggedIn.accountName then
                -- Don't show footer if the Claim is already validated by the current user
                identity

            else
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
