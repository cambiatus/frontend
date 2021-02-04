module Page.Dashboard.Claim exposing (Model, Msg, init, jsAddressToMsg, msgToString, update, view, viewVoters)

import Api.Graphql
import Cambiatus.Query
import Claim
import Eos
import Eos.Account as Eos
import Eos.EosError as EosError
import Graphql.Http
import Html exposing (Html, button, div, h3, img, label, p, span, strong, text)
import Html.Attributes exposing (class, classList, src)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import Page
import Profile
import Route
import Session.LoggedIn as LoggedIn exposing (External)
import Session.Shared exposing (Shared, Translators)
import Strftime
import Time
import UpdateResult as UR
import Utils



-- INIT


init : LoggedIn.Model -> Claim.ClaimId -> ( Model, Cmd Msg )
init { shared } claimId =
    ( initModel claimId
    , fetchClaim claimId shared
    )



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
    | Loaded Claim.Model
    | Failed (Graphql.Http.Error Claim.Model)



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view ({ shared } as loggedIn) model =
    let
        { t } =
            shared.translators

        title =
            case model.statusClaim of
                Loaded claim ->
                    claim.action.description

                _ ->
                    ""

        content =
            div []
                [ case model.statusClaim of
                    Loading ->
                        Page.fullPageLoading shared

                    Loaded claim ->
                        div [ class "bg-gray-100" ]
                            [ Page.viewHeader loggedIn claim.action.description Route.Analysis
                            , div [ class "mt-10 mb-8" ]
                                [ Profile.viewLarge shared loggedIn.accountName claim.claimer
                                ]
                            , div [ class "mx-auto container px-4" ]
                                [ viewTitle shared claim
                                , viewProofs shared.translators claim
                                , viewDetails loggedIn model claim
                                , viewVoters loggedIn claim
                                ]
                            , case model.claimModalStatus of
                                Claim.PhotoModal c ->
                                    Claim.viewPhotoModal loggedIn c
                                        |> Html.map ClaimMsg

                                _ ->
                                    if
                                        Claim.isVotable claim loggedIn.accountName
                                            && not model.isValidated
                                    then
                                        viewVoteButtons shared.translators claim.id model.claimModalStatus

                                    else
                                        text ""
                            ]

                    Failed err ->
                        Page.fullPageGraphQLError (t "error.unknown") err
                ]
    in
    { title = title
    , content =
        case loggedIn.hasObjectives of
            LoggedIn.FeatureLoaded True ->
                content

            LoggedIn.FeatureLoaded False ->
                Page.fullPageNotFound
                    (t "error.pageNotFound")
                    (t "community.objectives.disabled.description")

            LoggedIn.FeatureLoading ->
                Page.fullPageLoading shared
    }


viewProofs : Translators -> Claim.Model -> Html Msg
viewProofs { t } claim =
    let
        viewProofCode =
            case claim.proofCode of
                Just proofCode ->
                    div [ class "ml-4" ]
                        [ label [ class "input-label block" ]
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
                    [ img
                        [ onClick (ClaimMsg <| Claim.OpenPhotoModal claim)
                        , src url
                        ]
                        []
                    ]
                , viewProofCode
                ]

        Nothing ->
            text ""


viewVoteButtons : Translators -> Claim.ClaimId -> Claim.ModalStatus -> Html Msg
viewVoteButtons ({ t } as translators) claimId modalStatus =
    let
        viewVoteModal : Bool -> Bool -> Html Msg
        viewVoteModal isApproving isInProgress =
            Claim.viewVoteClaimModal
                translators
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
                ]
                [ text <| t "dashboard.reject" ]
            , button
                [ class "button button-primary"
                , onClick (ClaimMsg <| Claim.OpenVoteModal claimId True)
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


viewTitle : Shared -> Claim.Model -> Html msg
viewTitle shared claim =
    let
        text_ s =
            text (shared.translators.t s)
    in
    div [ class "text-heading font-bold text-center mb-4" ]
        [ case claim.status of
            Claim.Approved ->
                div [ class "inline-block" ]
                    [ text_ "claim.title_approved.1"
                    , span [ class "text-green ml-1" ] [ text_ "claim.title_approved.2" ]
                    ]

            Claim.Rejected ->
                div [ class "inline-block" ]
                    [ text_ "claim.title_rejected.1"
                    , span [ class "text-red ml-1" ] [ text_ "claim.title_rejected.2" ]
                    ]

            Claim.Pending ->
                div [ class "inline-block" ]
                    [ text_ "claim.title_under_review.1"
                    , span [ class "text-gray ml-1" ] [ text_ "claim.title_under_review.2" ]
                    , span [ class "mr-1" ] [ text_ "claim.title_under_review.3" ]
                    ]
        ]


viewDetails : LoggedIn.Model -> Model -> Claim.Model -> Html msg
viewDetails { shared, selectedCommunity } model claim =
    let
        text_ s =
            text (shared.translators.t s)

        isRejected =
            claim.status == Claim.Rejected
    in
    div []
        [ div [ class "mb-8" ]
            [ p
                [ class "text-caption uppercase text-green" ]
                [ text_ "claim.action" ]
            , div [ class "mb-2" ]
                [ p [ class "pt-2 text-body" ]
                    [ text claim.action.description ]
                ]
            ]
        , div [ class "mb-8" ]
            [ div
                [ class "flex justify-between lg:justify-start" ]
                [ div [ class "mr-6" ]
                    [ p [ class "text-caption uppercase text-green" ]
                        [ text_ "claim.date" ]
                    , p [ class "pt-2 text-body" ]
                        [ text
                            (claim.createdAt
                                |> Just
                                |> Utils.posixDateTime
                                |> Strftime.format "%d %b %Y %H:%M" Time.utc
                            )
                        ]
                    ]
                , div []
                    [ p
                        [ class "text-caption uppercase text-green" ]
                        [ text_ "claim.claimer_reward" ]
                    , p
                        [ class "pt-2 text-body"
                        , classList [ ( "text-red line-through", isRejected ) ]
                        ]
                        [ text
                            (String.fromFloat claim.action.reward
                                ++ " "
                                --++ Eos.symbolToSymbolCodeString claim.action.objective.community.symbol
                                ++ Eos.symbolToSymbolCodeString selectedCommunity
                            )
                        ]
                    ]
                ]
            ]
        , div [ class "mb-8" ]
            [ p
                [ class "text-caption uppercase text-green" ]
                [ text_ "claim.objective" ]
            , p
                [ class "pt-2 text-body" ]
                [ text claim.action.objective.description
                ]
            ]
        , div [ class "mb-8" ]
            [ p
                [ class "text-caption uppercase text-green" ]
                [ text_ "claim.your_reward" ]
            , p
                [ class "pt-2 text-body" ]
                [ text
                    (String.fromFloat claim.action.verifierReward
                        ++ " "
                        --++ Eos.symbolToSymbolCodeString claim.action.objective.community.symbol
                        ++ Eos.symbolToSymbolCodeString selectedCommunity
                    )
                ]
            ]
        ]


viewVoters : LoggedIn.Model -> Claim.Model -> Html msg
viewVoters ({ shared } as loggedIn) claim =
    let
        text_ s =
            text (shared.translators.t s)

        pendingValidators =
            List.filter
                (\p -> not <| List.member p.account (List.map (\c -> c.validator.account) claim.checks))
                claim.action.validators
    in
    div []
        [ div [ class "mb-8" ]
            [ p [ class "text-caption uppercase text-green" ]
                [ text_ "claim.approved_by" ]
            , div []
                [ if List.isEmpty claim.checks then
                    div [ class "flex mb-10" ]
                        [ div [ class "pt-2" ] [ Profile.viewEmpty shared ]
                        ]

                  else
                    div [ class "flex flex-wrap -mx-2 pt-2" ]
                        (List.map
                            (\c ->
                                if c.isApproved then
                                    div [ class "px-2" ]
                                        [ Profile.view shared loggedIn.accountName c.validator
                                        ]

                                else
                                    text ""
                            )
                            claim.checks
                        )
                ]
            ]
        , div [ class "mb-8" ]
            [ p [ class "text-caption uppercase text-green" ]
                [ text_ "claim.disapproved_by" ]
            , div [ class "flex mb-10 " ]
                [ if List.filter (\check -> check.isApproved == False) claim.checks |> List.isEmpty then
                    div [ class "pt-2" ] [ Profile.viewEmpty shared ]

                  else
                    div [ class "flex flex-wrap -mx-2 pt-2" ]
                        (List.map
                            (\c ->
                                if not c.isApproved then
                                    div [ class "px-2" ]
                                        [ Profile.view shared loggedIn.accountName c.validator
                                        ]

                                else
                                    text ""
                            )
                            claim.checks
                        )
                ]
            ]
        , div [ class "mb-8" ]
            [ p [ class "text-caption uppercase text-green" ]
                [ text_ "claim.pending" ]
            , div [ class "pt-2" ]
                [ if List.length claim.checks == claim.action.verifications then
                    div [ class "flex" ]
                        [ Profile.viewEmpty shared
                        ]

                  else
                    div [ class "flex flex-row flex-wrap space-x-6" ]
                        (List.map (\v -> Profile.view shared loggedIn.accountName v) pendingValidators)
                ]
            ]
        ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = ClaimLoaded (Result (Graphql.Http.Error Claim.Model) Claim.Model)
    | VoteClaim Claim.ClaimId Bool
    | GotVoteResult Claim.ClaimId (Result (Maybe Value) String)
    | ClaimMsg Claim.Msg


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    let
        { t, tr } =
            loggedIn.shared.translators
    in
    case msg of
        ClaimLoaded (Ok response) ->
            { model
                | statusClaim = Loaded response
                , isValidated = Claim.isValidated response loggedIn.accountName
            }
                |> UR.init

        ClaimLoaded (Err err) ->
            { model | statusClaim = Failed err }
                |> UR.init
                |> UR.logGraphqlError msg err

        ClaimMsg m ->
            Claim.updateClaimModalStatus m model
                |> UR.init

        VoteClaim claimId vote ->
            case model.statusClaim of
                Loaded _ ->
                    let
                        newModel =
                            { model
                                | claimModalStatus = Claim.Loading claimId vote
                            }
                    in
                    if LoggedIn.isAuth loggedIn then
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
                                          , data = Claim.encodeVerification claimId loggedIn.accountName vote
                                          }
                                        ]
                                }

                    else
                        UR.init newModel
                            |> UR.addExt (Just (VoteClaim claimId vote) |> LoggedIn.RequiredAuthentication)

                _ ->
                    model
                        |> UR.init

        GotVoteResult _ (Ok _) ->
            case model.statusClaim of
                Loaded claim ->
                    let
                        message val =
                            [ ( "value", val ) ]
                                |> tr "claim.reward"

                        value =
                            String.fromFloat claim.action.verifierReward
                                ++ " "
                                --++ Eos.symbolToSymbolCodeString claim.action.objective.community.symbol
                                ++ Eos.symbolToSymbolCodeString loggedIn.selectedCommunity
                    in
                    { model
                        | claimModalStatus = Claim.Closed
                        , isValidated = True
                    }
                        |> UR.init
                        |> UR.addExt (LoggedIn.ShowFeedback LoggedIn.Success (message value))

                _ ->
                    { model
                        | claimModalStatus = Claim.Closed
                    }
                        |> UR.init

        GotVoteResult _ (Err eosErrorString) ->
            let
                errorMsg =
                    EosError.parseClaimError loggedIn.shared.translators eosErrorString
            in
            case model.statusClaim of
                Loaded claim ->
                    { model
                        | statusClaim = Loaded claim
                        , claimModalStatus = Claim.Closed
                    }
                        |> UR.init
                        |> UR.addExt (LoggedIn.ShowFeedback LoggedIn.Failure errorMsg)

                _ ->
                    model |> UR.init



-- HELPERS


fetchClaim : Claim.ClaimId -> Shared -> Cmd Msg
fetchClaim claimId shared =
    Api.Graphql.query
        shared
        (Cambiatus.Query.claim { input = { id = claimId } } Claim.selectionSet)
        ClaimLoaded


jsAddressToMsg : List String -> Encode.Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "VoteClaim" :: claimId :: _ ->
            let
                id =
                    String.toInt claimId
                        |> Maybe.withDefault 0
            in
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.field "transactionId" Decode.string
                        |> Decode.map Ok
                    , Decode.field "error" (Decode.nullable Decode.value)
                        |> Decode.map Err
                    ]
                )
                val
                |> Result.map (Just << GotVoteResult id)
                |> Result.withDefault Nothing

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        ClaimLoaded r ->
            [ "ClaimLoaded", UR.resultToString r ]

        VoteClaim claimId _ ->
            [ "VoteClaim", String.fromInt claimId ]

        GotVoteResult _ r ->
            [ "GotVoteResult", UR.resultToString r ]

        ClaimMsg _ ->
            [ "ClaimMsg" ]
