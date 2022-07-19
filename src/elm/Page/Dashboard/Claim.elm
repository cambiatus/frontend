module Page.Dashboard.Claim exposing (Model, Msg, init, jsAddressToMsg, msgToString, update, view)

import Action
import Cambiatus.Enum.Permission as Permission
import Cambiatus.Query
import Claim
import Dict
import Eos
import Eos.Account as Eos
import Eos.EosError as EosError
import Graphql.Http
import Html exposing (Html, button, div, h3, p, span, strong, text)
import Html.Attributes exposing (class, classList, disabled)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import List.Extra as List
import Log
import Markdown
import Page
import Profile
import Profile.Summary
import RemoteData exposing (RemoteData)
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared, Translators)
import Strftime
import String.Extra
import UpdateResult as UR
import Utils
import View.Components
import View.Feedback as Feedback



-- INIT


init : LoggedIn.Model -> Claim.ClaimId -> UpdateResult
init loggedIn claimId =
    let
        model =
            initModel claimId
    in
    model
        |> UR.init
        |> UR.addExt (fetchClaim claimId loggedIn)



-- MODEL


type alias Model =
    { claimId : Claim.ClaimId
    , statusClaim : Status
    , claimModalStatus : Claim.ModalStatus
    , isValidated : Bool
    }


initModel : Claim.ClaimId -> Model
initModel claimId =
    { claimId = claimId
    , statusClaim = Loading
    , claimModalStatus = Claim.Closed
    , isValidated = False
    }


type Status
    = Loading
    | Loaded Claim.Model ProfileSummaries
    | Failed (Graphql.Http.Error Claim.Model)


type alias ProfileSummaries =
    { claimer : Profile.Summary.Model
    , pending : List Profile.Summary.Model
    , voter : List Profile.Summary.Model
    }


type ProfileSummaryKind
    = ClaimerSummary
    | PendingSummary Int
    | VoterSummary Int



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view ({ shared } as loggedIn) model =
    let
        { t } =
            shared.translators

        title =
            case model.statusClaim of
                Loaded claim _ ->
                    claim.action.description
                        |> Markdown.toUnformattedString
                        |> String.Extra.softEllipsis 20

                _ ->
                    ""

        content =
            div []
                [ case model.statusClaim of
                    Loading ->
                        Page.fullPageLoading shared

                    Loaded claim profileSummaries ->
                        div [ class "bg-gray-100" ]
                            [ Page.viewHeader loggedIn title
                            , div [ class "mt-10 mb-8 flex items-center justify-center" ]
                                [ Profile.Summary.view shared.translators
                                    loggedIn.accountName
                                    claim.claimer
                                    profileSummaries.claimer
                                    |> Html.map (GotProfileSummaryMsg ClaimerSummary)
                                ]
                            , div [ class "mx-auto container px-4" ]
                                [ viewTitle shared model claim
                                , viewProofs shared.translators claim
                                , viewDetails loggedIn model claim
                                , viewVoters loggedIn profileSummaries claim
                                ]
                            , case model.claimModalStatus of
                                Claim.PhotoModal c ->
                                    Claim.viewPhotoModal loggedIn c
                                        |> Html.map ClaimMsg

                                _ ->
                                    if
                                        Claim.isVotable claim loggedIn.accountName shared.now
                                            && not model.isValidated
                                    then
                                        viewVoteButtons loggedIn claim.id model.claimModalStatus

                                    else
                                        text ""
                            ]

                    Failed err ->
                        Page.fullPageGraphQLError (t "error.unknown") err
                ]
    in
    { title = title
    , content =
        case RemoteData.map .hasObjectives loggedIn.selectedCommunity of
            RemoteData.Success True ->
                content

            RemoteData.Success False ->
                Page.fullPageNotFound
                    (t "error.pageNotFound")
                    (t "community.objectives.disabled.description")

            RemoteData.Loading ->
                Page.fullPageLoading shared

            RemoteData.NotAsked ->
                Page.fullPageLoading shared

            RemoteData.Failure e ->
                Page.fullPageGraphQLError (t "community.error_loading") e
    }


viewProofs : Translators -> Claim.Model -> Html Msg
viewProofs { t } claim =
    let
        viewProofCode =
            case claim.proofCode of
                Just proofCode ->
                    div [ class "ml-4" ]
                        [ p [ class "label" ]
                            [ text (t "community.actions.form.verification_code") ]
                        , strong [ class "text-lg block" ] [ text proofCode ]
                        ]

                Nothing ->
                    text ""
    in
    case claim.proofPhoto of
        Just url ->
            div [ class "mb-8 flex" ]
                [ div [ class "claim-photo-thumb" ]
                    [ View.Components.pdfViewer
                        [ onClick (ClaimMsg <| Claim.OpenPhotoModal claim), class "h-full w-full" ]
                        { url = url
                        , childClass = "max-w-full max-h-full"
                        , maybeTranslators = Nothing
                        , onFileTypeDiscovered = Nothing
                        }
                    ]
                , viewProofCode
                ]

        Nothing ->
            text ""


viewVoteButtons : LoggedIn.Model -> Claim.ClaimId -> Claim.ModalStatus -> Html Msg
viewVoteButtons loggedIn claimId modalStatus =
    let
        { t } =
            loggedIn.shared.translators

        viewVoteModal : Bool -> Bool -> Html Msg
        viewVoteModal isApproving isInProgress =
            Claim.viewVoteClaimModal
                loggedIn.shared.translators
                { voteMsg = VoteClaim
                , closeMsg = ClaimMsg Claim.CloseClaimModals
                , claimId = claimId
                , isApproving = isApproving
                , isInProgress = isInProgress
                }
    in
    div [ class "mb-8 border-t pt-8" ]
        [ h3 [ class "font-bold mb-6 text-center" ]
            [ text <| t "claim.voteTitle" ]
        , div [ class "flex justify-around sm:justify-center sm:space-x-3" ]
            [ button
                [ class "button button-secondary text-red"
                , onClick (ClaimMsg <| Claim.OpenVoteModal claimId False)
                , disabled (not loggedIn.hasAcceptedCodeOfConduct)
                ]
                [ text <| t "dashboard.reject" ]
            , button
                [ class "button button-primary"
                , onClick (ClaimMsg <| Claim.OpenVoteModal claimId True)
                , disabled (not loggedIn.hasAcceptedCodeOfConduct)
                ]
                [ text <| t "dashboard.verify" ]
            ]
        , case modalStatus of
            Claim.Loading _ isApproving ->
                viewVoteModal isApproving True

            Claim.VoteConfirmationModal _ isApproving ->
                viewVoteModal isApproving False

            _ ->
                text ""
        ]


viewTitle : Shared -> Model -> Claim.Model -> Html msg
viewTitle shared _ claim =
    let
        text_ s =
            text (shared.translators.t s)
    in
    div [ class "text-lg font-bold text-center mb-4" ]
        [ case claim.status of
            Claim.Approved ->
                div [ class "inline-block" ]
                    [ text_ "claim.title_approved.1"
                    , span [ class "text-green ml-1" ] [ text_ "claim.title_approved.2" ]
                    ]

            Claim.Cancelled _ ->
                div [ class "inline-block" ]
                    [ text_ "claim.title_action_completed" ]

            Claim.Rejected ->
                div [ class "inline-block" ]
                    [ text_ "claim.title_rejected.1"
                    , span [ class "text-red ml-1" ] [ text_ "claim.title_rejected.2" ]
                    ]

            Claim.Pending ->
                if Action.isClosed claim.action shared.now then
                    div
                        [ class "inline-block" ]
                        [ text_ "claim.title_action_closed"
                        ]

                else
                    div [ class "inline-block" ]
                        [ text_ "claim.title_under_review.1"
                        , span [ class "text-gray ml-1" ] [ text_ "claim.title_under_review.2" ]
                        , span [ class "mr-1" ] [ text_ "claim.title_under_review.3" ]
                        ]
        ]


viewDetails : LoggedIn.Model -> Model -> Claim.Model -> Html msg
viewDetails { shared } _ claim =
    let
        text_ s =
            text (shared.translators.t s)

        isRejected =
            claim.status == Claim.Rejected

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
    in
    div []
        [ Claim.viewVotingProgress shared completionStatus
        , div [ class "mb-8" ]
            [ p [ class "label" ] [ text_ "claim.action" ]
            , Markdown.view [ class "mb-2" ] claim.action.description
            , if claim.action.isCompleted then
                div [ class "flex mb-2" ]
                    [ div [ class "tag bg-green" ] [ text_ "community.actions.completed" ]
                    ]

              else if Action.isClosed claim.action shared.now then
                div [ class "flex mb-2" ]
                    [ div
                        [ class "tag bg-gray-500 text-red" ]
                        [ text_ "community.actions.closed" ]
                    ]

              else
                text ""
            ]
        , div [ class "mb-8" ]
            [ div
                [ class "flex justify-between lg:justify-start" ]
                [ div [ class "mr-6" ]
                    [ p [ class "label" ] [ text_ "claim.date" ]
                    , p []
                        [ text
                            (claim.createdAt
                                |> Utils.fromDateTime
                                |> Strftime.format "%d %b %Y %H:%M" shared.timezone
                            )
                        ]
                    ]
                , div []
                    [ p [ class "label" ] [ text_ "claim.claimer_reward" ]
                    , p [ classList [ ( "text-red line-through", isRejected ) ] ]
                        [ text
                            (String.fromFloat claim.action.reward
                                ++ " "
                                ++ Eos.symbolToSymbolCodeString claim.action.objective.community.symbol
                            )
                        ]
                    ]
                ]
            ]
        , div [ class "mb-8" ]
            [ p [ class "label" ] [ text_ "claim.objective" ]
            , Markdown.view [] claim.action.objective.description
            ]
        , div [ class "mb-8" ]
            [ p [ class "label" ] [ text_ "claim.your_reward" ]
            , p []
                [ text
                    (String.fromFloat claim.action.verifierReward
                        ++ " "
                        ++ Eos.symbolToSymbolCodeString claim.action.objective.community.symbol
                    )
                ]
            ]
        ]


viewVoters : LoggedIn.Model -> ProfileSummaries -> Claim.Model -> Html Msg
viewVoters ({ shared } as loggedIn) profileSummaries claim =
    let
        text_ s =
            text (shared.translators.t s)
    in
    div []
        [ div [ class "mb-8" ]
            [ p [ class "label" ] [ text_ "claim.approved_by" ]
            , if List.isEmpty claim.checks then
                div [ class "flex mb-10" ] [ Profile.viewEmpty shared.translators ]

              else
                div [ class "flex flex-wrap -mx-2" ]
                    (List.map3
                        (\profileSummary index c ->
                            if c.isApproved then
                                div [ class "px-2" ]
                                    [ Profile.Summary.view shared.translators
                                        loggedIn.accountName
                                        c.validator
                                        profileSummary
                                        |> Html.map (GotProfileSummaryMsg (VoterSummary index))
                                    ]

                            else
                                text ""
                        )
                        profileSummaries.voter
                        (List.range 0 (List.length profileSummaries.voter))
                        claim.checks
                    )
            ]
        , div [ class "mb-8" ]
            [ p [ class "label" ] [ text_ "claim.disapproved_by" ]
            , if List.filter (\check -> check.isApproved == False) claim.checks |> List.isEmpty then
                div [ class "flex mb-10" ] [ Profile.viewEmpty shared.translators ]

              else
                div [ class "flex flex-wrap mb-10 -mx-2 pt-2" ]
                    (List.map3
                        (\profileSummary index c ->
                            if not c.isApproved then
                                div [ class "px-2" ]
                                    [ Profile.Summary.view shared.translators loggedIn.accountName c.validator profileSummary
                                        |> Html.map (GotProfileSummaryMsg (VoterSummary index))
                                    ]

                            else
                                text ""
                        )
                        profileSummaries.voter
                        (List.range 0 (List.length profileSummaries.voter))
                        claim.checks
                    )
            ]
        , div [ class "mb-8" ]
            [ p [ class "label" ] [ text_ "claim.pending" ]
            , if List.length claim.checks == claim.action.verifications then
                div [ class "flex" ] [ Profile.viewEmpty shared.translators ]

              else
                div [ class "flex flex-row flex-wrap space-x-6" ]
                    (pendingValidators claim
                        |> List.map3
                            (\profileSummary index v ->
                                Profile.Summary.view shared.translators loggedIn.accountName v profileSummary
                                    |> Html.map (GotProfileSummaryMsg (PendingSummary index))
                            )
                            profileSummaries.pending
                            (List.range 0 (List.length profileSummaries.pending))
                    )
            ]
        ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


type Msg
    = ClaimLoaded (RemoteData (Graphql.Http.Error Claim.Model) Claim.Model)
    | ClosedAuthModal
    | VoteClaim Claim.ClaimId Bool
    | GotVoteResult (Result (Maybe Value) String)
    | ClaimMsg Claim.Msg
    | GotProfileSummaryMsg ProfileSummaryKind Profile.Summary.Msg


pendingValidators : Claim.Model -> List Profile.Minimal
pendingValidators claim =
    List.filter
        (\validator ->
            List.map (\c -> c.validator.account) claim.checks
                |> List.member validator.account
                |> not
        )
        claim.action.validators


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    let
        { tr } =
            loggedIn.shared.translators
    in
    case msg of
        ClaimLoaded (RemoteData.Success response) ->
            let
                claimerSummary =
                    Profile.Summary.init True

                pendingCount =
                    pendingValidators response |> List.length

                voterSummaries =
                    List.length response.checks
                        |> Profile.Summary.initMany False

                pendingSummaries =
                    pendingCount
                        |> Profile.Summary.initMany False
            in
            { model
                | statusClaim =
                    Loaded response
                        { claimer = claimerSummary
                        , voter = voterSummaries
                        , pending = pendingSummaries
                        }
                , isValidated = Claim.isValidated response loggedIn.accountName
            }
                |> UR.init

        ClaimLoaded (RemoteData.Failure err) ->
            { model | statusClaim = Failed err }
                |> UR.init
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when trying to load claim"
                    { moduleName = "Page.Dashboard.Claim", function = "update" }
                    []
                    err

        ClaimLoaded _ ->
            UR.init model

        ClosedAuthModal ->
            { model | claimModalStatus = Claim.Closed }
                |> UR.init

        ClaimMsg m ->
            Claim.updateClaimModalStatus m model
                |> UR.init

        VoteClaim claimId vote ->
            case model.statusClaim of
                Loaded claim _ ->
                    let
                        newModel =
                            { model
                                | claimModalStatus = Claim.Loading claimId vote
                            }
                    in
                    UR.init newModel
                        |> UR.addPort
                            { responseAddress = msg
                            , responseData = Encode.null
                            , data =
                                Eos.encodeTransaction
                                    [ { accountName = loggedIn.shared.contracts.community
                                      , name = "verifyclaim"
                                      , authorization =
                                            { actor = loggedIn.accountName
                                            , permissionName = Eos.samplePermission
                                            }
                                      , data =
                                            Claim.encodeVerification claimId
                                                loggedIn.accountName
                                                vote
                                                claim.action.objective.community.symbol
                                      }
                                    ]
                            }
                        |> UR.addBreadcrumb
                            { type_ = Log.QueryBreadcrumb
                            , category = msg
                            , message = "Requested to vote on claim"
                            , data =
                                Dict.fromList
                                    [ ( "claimId", Encode.int claimId )
                                    , ( "vote", Encode.bool vote )
                                    ]
                            , level = Log.Info
                            }
                        |> LoggedIn.withPrivateKey loggedIn
                            [ Permission.Verify ]
                            model
                            { successMsg = msg, errorMsg = ClosedAuthModal }

                _ ->
                    model
                        |> UR.init

        GotVoteResult (Ok _) ->
            case model.statusClaim of
                Loaded claim _ ->
                    let
                        message val =
                            [ ( "value", val ) ]
                                |> tr "claim.reward"

                        value =
                            String.fromFloat claim.action.verifierReward
                                ++ " "
                                ++ Eos.symbolToSymbolCodeString claim.action.objective.community.symbol
                    in
                    { model
                        | claimModalStatus = Claim.Closed
                        , isValidated = True
                    }
                        |> UR.init
                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Success (message value))

                _ ->
                    { model
                        | claimModalStatus = Claim.Closed
                    }
                        |> UR.init

        GotVoteResult (Err eosErrorString) ->
            let
                errorMsg =
                    EosError.parseClaimError loggedIn.shared.translators eosErrorString
            in
            case model.statusClaim of
                Loaded claim profileSummaries ->
                    { model
                        | statusClaim = Loaded claim profileSummaries
                        , claimModalStatus = Claim.Closed
                    }
                        |> UR.init
                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure errorMsg)

                _ ->
                    model |> UR.init

        GotProfileSummaryMsg profileSummaryKind subMsg ->
            case model.statusClaim of
                Loaded claim profileSummaries ->
                    case profileSummaryKind of
                        PendingSummary index ->
                            { model
                                | statusClaim =
                                    Loaded claim
                                        { profileSummaries
                                            | pending =
                                                List.updateAt index (Profile.Summary.update subMsg) profileSummaries.pending
                                        }
                            }
                                |> UR.init

                        VoterSummary index ->
                            { model
                                | statusClaim =
                                    Loaded claim
                                        { profileSummaries
                                            | voter =
                                                List.updateAt index (Profile.Summary.update subMsg) profileSummaries.voter
                                        }
                            }
                                |> UR.init

                        ClaimerSummary ->
                            { model | statusClaim = Loaded claim { profileSummaries | claimer = Profile.Summary.update subMsg profileSummaries.claimer } }
                                |> UR.init

                _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Got a Profile.Summary.Msg, but claims aren't loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Dashboard.Claim", function = "update" }
                            []



-- HELPERS


fetchClaim : Claim.ClaimId -> LoggedIn.Model -> LoggedIn.External Msg
fetchClaim claimId loggedIn =
    LoggedIn.query
        loggedIn
        (Cambiatus.Query.claim { id = claimId } (Claim.selectionSet loggedIn.shared.now))
        ClaimLoaded


jsAddressToMsg : List String -> Encode.Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "VoteClaim" :: _ ->
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.field "transactionId" Decode.string
                        |> Decode.map Ok
                    , Decode.field "error" (Decode.nullable Decode.value)
                        |> Decode.map Err
                    ]
                )
                val
                |> Result.map (Just << GotVoteResult)
                |> Result.withDefault Nothing

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        ClaimLoaded r ->
            [ "ClaimLoaded", UR.remoteDataToString r ]

        ClosedAuthModal ->
            [ "ClosedAuthModal" ]

        VoteClaim claimId _ ->
            [ "VoteClaim", String.fromInt claimId ]

        GotVoteResult r ->
            [ "GotVoteResult", UR.resultToString r ]

        ClaimMsg _ ->
            [ "ClaimMsg" ]

        GotProfileSummaryMsg _ subMsg ->
            "GotProfileSummaryMsg" :: Profile.Summary.msgToString subMsg
