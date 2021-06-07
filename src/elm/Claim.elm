module Claim exposing
    ( ClaimId
    , ClaimStatus(..)
    , ModalStatus(..)
    , Model
    , Msg(..)
    , Paginated
    , claimPaginatedSelectionSet
    , encodeVerification
    , isValidated
    , isValidator
    , isVotable
    , paginatedPageInfo
    , paginatedToList
    , selectionSet
    , updateClaimModalStatus
    , viewClaimCard
    , viewClaimModal
    , viewPhotoModal
    , viewVoteClaimModal
    )

import Action exposing (Action, isClosed)
import Api.Relay exposing (Edge, PageConnection)
import Cambiatus.Enum.ClaimStatus as ClaimStatus
import Cambiatus.Object
import Cambiatus.Object.Check as Check
import Cambiatus.Object.Claim as Claim
import Cambiatus.Object.ClaimConnection
import Cambiatus.Object.ClaimEdge
import Cambiatus.Scalar exposing (DateTime(..))
import Eos
import Eos.Account as Eos
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, a, button, div, img, label, p, strong, text)
import Html.Attributes exposing (class, classList, disabled, href, id, src, style, target)
import Html.Events exposing (onClick)
import Icons
import Json.Encode as Encode
import Profile
import Route exposing (Route)
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Translators)
import Strftime
import Time
import Utils
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
    | ClaimModal Model
    | Closed


type ClaimStatus
    = Approved
    | Rejected
    | Pending


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
        && not (isClosed claim.action now)
        && not claim.action.isCompleted


encodeVerification : ClaimId -> Eos.Name -> Bool -> Encode.Value
encodeVerification claimId validator vote =
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
        |> with (Claim.claimer Profile.minimalSelectionSet)
        |> with (Claim.action Action.selectionSet)
        |> with (Claim.checks (\_ -> { input = Absent }) checkSelectionSet)
        |> with Claim.createdAt
        |> with (SelectionSet.map emptyStringToNothing Claim.proofPhoto)
        |> with (SelectionSet.map emptyStringToNothing Claim.proofCode)


emptyStringToNothing : Maybe String -> Maybe String
emptyStringToNothing s =
    case s of
        Just "" ->
            Nothing

        Just nonEmpty ->
            Just nonEmpty

        Nothing ->
            Nothing


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
    | OpenClaimModal Model
    | RouteOpened Route


updateClaimModalStatus : Msg -> { m | claimModalStatus : ModalStatus } -> { m | claimModalStatus : ModalStatus }
updateClaimModalStatus msg model =
    case msg of
        OpenVoteModal claimId vote ->
            { model | claimModalStatus = VoteConfirmationModal claimId vote }

        CloseClaimModals ->
            { model | claimModalStatus = Closed }

        OpenPhotoModal claimId ->
            { model | claimModalStatus = PhotoModal claimId }

        OpenClaimModal claimId ->
            { model | claimModalStatus = ClaimModal claimId }

        RouteOpened _ ->
            model


{-| Claim card with a short claim overview. Used on Dashboard and Analysis pages.
-}
viewClaimCard : LoggedIn.Model -> Model -> Bool -> Html Msg
viewClaimCard loggedIn claim showClaimModal =
    let
        t =
            loggedIn.shared.translators.t

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
    in
    div [ class "w-full sm:w-1/2 lg:w-1/3 xl:w-1/4 px-2" ]
        [ viewClaimModal loggedIn claim showClaimModal
        , div
            [ class "flex flex-col p-4 my-2 rounded-lg bg-white hover:shadow cursor-pointer"
            , id ("claim" ++ String.fromInt claim.id)
            , Utils.onClickNoBubble (OpenClaimModal claim)
            ]
            [ div
                [ class "flex mb-8"
                , case claim.proofPhoto of
                    Just _ ->
                        class "justify-between"

                    Nothing ->
                        class "justify-center"
                ]
                [ Profile.view loggedIn.shared loggedIn.accountName claim.claimer
                , case claim.proofPhoto of
                    Just url ->
                        div [ class "claim-photo-thumb" ]
                            [ img
                                [ Utils.onClickNoBubble (OpenPhotoModal claim)
                                , src url
                                ]
                                []
                            ]

                    Nothing ->
                        text ""
                ]
            , div [ class "bg-gray-100 flex items-center justify-center h-6 w-32 mb-2" ]
                [ p
                    [ class ("text-caption uppercase " ++ textColor) ]
                    [ text claimStatus ]
                ]
            , div [ class "mb-6" ]
                [ p [ class "text-body overflow-ellipsis overflow-hidden" ]
                    [ text claim.action.description ]
                , p
                    [ class "text-gray-900 text-caption uppercase" ]
                    [ text (date claim.createdAt) ]
                ]
            , if
                isValidated claim loggedIn.accountName
                    || not (isValidator loggedIn.accountName claim)
                    || claim.action.isCompleted
                    || Action.isClosed claim.action loggedIn.shared.now
                    || Action.isPastDeadline claim.action loggedIn.shared.now
              then
                a
                    [ class "button button-secondary w-full font-medium mb-2"
                    , Utils.onClickNoBubble (OpenClaimModal claim)
                    ]
                    [ text (t "all_analysis.more_details") ]

              else
                div [ class "flex justify-between space-x-4" ]
                    [ button
                        [ class "button button-danger"
                        , Utils.onClickNoBubble (OpenVoteModal claim.id False)
                        ]
                        [ text (t "dashboard.reject") ]
                    , button
                        [ class "button button-primary"
                        , Utils.onClickNoBubble (OpenVoteModal claim.id True)
                        ]
                        [ text (t "dashboard.verify") ]
                    ]
            ]
        ]


viewClaimModal : LoggedIn.Model -> Model -> Bool -> Html Msg
viewClaimModal loggedIn claim showClaimModal =
    let
        t =
            loggedIn.shared.translators.t

        greenTextTitleStyle =
            "uppercase text-green text-xs"

        claimVerifiersList =
            "block mt-6 h-32 space-y-4"

        photoAndTagName =
            div []
                [ Profile.viewLarge loggedIn.shared loggedIn.accountName claim.claimer ]

        claimDateAndState =
            let
                date datetime =
                    Just datetime
                        |> Utils.posixDateTime
                        |> Strftime.format "%d %b %Y" Time.utc

                ( claimStatusPhrase, claimStatus, textColor ) =
                    case claim.status of
                        Approved ->
                            ( t "claim.title_approved.1", t "claim.approved", "text-2xl font-bold lowercase text-green" )

                        Rejected ->
                            ( t "claim.title_rejected.1", t "claim.disapproved", "text-2xl font-bold lowercase text-red" )

                        Pending ->
                            ( t "claim.title_under_review.1", t "claim.pending", "text-2xl font-bold lowercase text-gray-600" )

                claimDate =
                    let
                        claimedOn =
                            t "claim.claimed_on"

                        dateTime =
                            date claim.createdAt
                    in
                    claimedOn ++ " " ++ dateTime
            in
            div [ class "block" ]
                [ p [ class "text-xs uppercase" ] [ text claimDate ]
                , label [ class "text-2xl font-bold" ]
                    [ text claimStatusPhrase
                    ]
                , div [ class textColor ] [ text claimStatus ]
                ]

        rewardInfo =
            let
                rewardTxtStyle =
                    "my-2 font-bold text-lg"
            in
            div
                [ class "text-center flex justify-center bg-gray-100 rounded-md p-4 space-x-16" ]
                [ div
                    []
                    [ p [ class rewardTxtStyle ] [ text (String.fromFloat claim.action.reward) ]
                    , p [ class greenTextTitleStyle ] [ text (t "community.actions.reward") ]
                    ]
                , div []
                    [ p [ class rewardTxtStyle ] [ text (String.fromFloat claim.action.verifierReward) ]
                    , p [ class greenTextTitleStyle ] [ text (t "claim.analyst_reward") ]
                    ]
                ]

        approvedBy =
            div
                [ class claimVerifiersList ]
                [ p [ class greenTextTitleStyle ] [ text (t "claim.approved_by") ]
                , div [ class "overflow-x-auto" ]
                    [ if List.isEmpty claim.checks then
                        div [ class "flex mb-10" ]
                            [ div [ class "pt-2" ] [ Profile.viewEmpty loggedIn.shared ]
                            ]

                      else
                        div [ class "flex -mx-2 pt-2" ]
                            (List.map
                                (\c ->
                                    if c.isApproved then
                                        div [ class "px-2" ]
                                            [ Profile.view loggedIn.shared loggedIn.accountName c.validator
                                            ]

                                    else
                                        text ""
                                )
                                claim.checks
                            )
                    ]
                ]

        disapprovedBy =
            div
                [ class claimVerifiersList ]
                [ p [ class greenTextTitleStyle ] [ text (t "claim.disapproved_by") ]
                , div [ class "overflow-x-auto" ]
                    [ if List.filter (\check -> check.isApproved == False) claim.checks |> List.isEmpty then
                        div [ class "flex mb-10 " ]
                            [ div [ class "pt-2" ] [ Profile.viewEmpty loggedIn.shared ] ]

                      else
                        div [ class "flex -mx-2 pt-2" ]
                            (List.map
                                (\c ->
                                    if not c.isApproved then
                                        div [ class "px-2" ]
                                            [ Profile.view loggedIn.shared loggedIn.accountName c.validator
                                            ]

                                    else
                                        text ""
                                )
                                claim.checks
                            )
                    ]
                ]

        pendingVote =
            let
                pendingValidators =
                    List.filter
                        (\p -> not <| List.member p.account (List.map (\c -> c.validator.account) claim.checks))
                        claim.action.validators
            in
            div
                [ class claimVerifiersList ]
                [ p [ class greenTextTitleStyle ] [ text (t "claim.pending_vote") ]
                , div [ class "overflow-x-auto" ]
                    [ if List.length claim.checks == claim.action.verifications then
                        div [ class "flex mb-10 " ]
                            [ div [ class "pt-2" ] [ Profile.viewEmpty loggedIn.shared ] ]

                      else
                        div [ class "flex space-x-6" ]
                            (List.map (\v -> Profile.view loggedIn.shared loggedIn.accountName v) pendingValidators)
                    ]
                ]

        actionDetail =
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
                [ p [ class greenTextTitleStyle ] [ text (t "claim.action") ]
                , p [ class "text-left mt-2 text-lg w-full" ] [ text claim.action.description ]
                , case claim.proofPhoto of
                    Just url ->
                        div
                            [ class "relative h-auto w-auto text-center text-white"
                            , Utils.onClickNoBubble (OpenPhotoModal claim)
                            ]
                            [ div
                                [ class "z-10 absolute bottom-1 left-1 bg-black bg-opacity-60 p-4" ]
                                [ p [ class "text-xs text-left w-full uppercase" ] [ text (t "community.actions.form.verification_code") ]
                                , p [ class "text-base font-bold font-normal text-left w-full" ] [ text proofCode ]
                                ]
                            , div []
                                [ img
                                    [ class "z-0 rounded-md my-4 w-full"
                                    , style "height" "200px"
                                    , src url
                                    ]
                                    []
                                ]
                            ]

                    Nothing ->
                        text ""
                ]

        header =
            div [ class "flex mt-16 space-x-8 justify-center" ]
                [ photoAndTagName
                , claimDateAndState
                ]

        body =
            div
                [ class "block" ]
                [ rewardInfo
                , approvedBy
                , disapprovedBy
                , pendingVote
                , actionDetail
                ]

        footer =
            if isVotable claim loggedIn.accountName loggedIn.shared.now then
                div [ class "flex justify-between space-x-4 mt-8 object-bottom" ]
                    [ button
                        [ class "button button-danger"
                        , onClick (OpenVoteModal claim.id False)
                        ]
                        [ text (t "dashboard.reject") ]
                    , button
                        [ class "button button-primary"
                        , onClick (OpenVoteModal claim.id True)
                        ]
                        [ text (t "dashboard.verify") ]
                    ]

            else
                div [] []
    in
    div
        []
        [ Modal.initWith
            { closeMsg = CloseClaimModals
            , isVisible = showClaimModal
            }
            |> Modal.withBody
                [ div
                    [ class "block space-y-6 "
                    ]
                    [ header
                    , body
                    ]
                ]
            |> Modal.withFooter [ footer ]
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
                            [ img
                                [ style "max-height" "42vh"
                                , src url
                                ]
                                []
                            , a
                                [ class "underline inline-block py-1 text-gray"
                                , href url
                                , target "_blank"
                                ]
                                [ text (t "community.actions.proof.photo_full")
                                , Icons.externalLink "inline-block ml-1 h-3 fill-current"
                                ]
                            ]

                    Nothing ->
                        text ""
                , case claim.proofCode of
                    Just proofCode ->
                        div []
                            [ label [ class "mt-4 md:mt-0 input-label md:text-xl block" ]
                                [ text (t "community.actions.form.verification_code")
                                ]
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
