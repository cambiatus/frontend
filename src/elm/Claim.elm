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
    , isVotable
    , paginatedPageInfo
    , paginatedToList
    , selectionSet
    , updateClaimModalStatus
    , viewClaimCard
    , viewPhotoModal
    , viewVoteClaimModal
    )

import Action exposing (Action)
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
import Html exposing (Html, a, button, div, label, p, strong, text)
import Html.Attributes exposing (class, classList, disabled, href, id, target)
import Html.Events exposing (onClick)
import Icons
import Json.Encode as Encode
import Profile
import Profile.Summary
import Route exposing (Route)
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Translators)
import Strftime
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
    | RouteOpened Route
    | GotProfileSummaryMsg Profile.Summary.Msg


updateClaimModalStatus : Msg -> { m | claimModalStatus : ModalStatus } -> { m | claimModalStatus : ModalStatus }
updateClaimModalStatus msg model =
    case msg of
        OpenVoteModal claimId vote ->
            { model | claimModalStatus = VoteConfirmationModal claimId vote }

        CloseClaimModals ->
            { model | claimModalStatus = Closed }

        OpenPhotoModal claimId ->
            { model | claimModalStatus = PhotoModal claimId }

        RouteOpened _ ->
            model

        GotProfileSummaryMsg _ ->
            model


{-| Claim card with a short claim overview. Used on Dashboard and Analysis pages.
-}
viewClaimCard : LoggedIn.Model -> Profile.Summary.Model -> Model -> Html Msg
viewClaimCard { shared, accountName } profileSummary claim =
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
                    if claim.action.isCompleted then
                        ( t "community.actions.completed", "text-black" )

                    else
                        ( t "all_analysis.pending", "text-black" )

        claimRoute =
            Route.Claim
                claim.action.objective.id
                claim.action.id
                claim.id
    in
    div [ class "w-full sm:w-1/2 lg:w-1/3 xl:w-1/4 px-2" ]
        [ div
            [ class "flex flex-col p-4 my-2 rounded-lg bg-white hover:shadow cursor-pointer"
            , id ("claim" ++ String.fromInt claim.id)

            -- We can't just use `a` with `href` here because there are other `a`-nodes inside.
            , onClick <| RouteOpened claimRoute
            ]
            [ div
                [ class "flex mb-8"
                , case claim.proofPhoto of
                    Just _ ->
                        class "justify-between"

                    Nothing ->
                        class "justify-center"
                ]
                [ div [ class "flex items-center justify-center" ]
                    [ Profile.Summary.view shared accountName claim.claimer profileSummary
                        |> Html.map GotProfileSummaryMsg
                    ]
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
                                }
                            ]

                    Nothing ->
                        text ""
                ]
            , a [ Route.href claimRoute ]
                [ div [ class "bg-gray-100 flex items-center justify-center h-6 w-32 mb-2" ]
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
                ]
            , if
                isValidated claim accountName
                    || not (isValidator accountName claim)
                    || claim.action.isCompleted
                    || Action.isClosed claim.action shared.now
                    || Action.isPastDeadline claim.action shared.now
              then
                a
                    [ class "button button-secondary w-full font-medium mb-2"
                    , Route.href claimRoute
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
