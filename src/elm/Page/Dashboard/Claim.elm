module Page.Dashboard.Claim exposing (Model, Msg, init, jsAddressToMsg, msgToString, update, view, viewVoters)

import Api.Graphql
import Cambiatus.Query
import Claim
import Eos exposing (Symbol)
import Eos.Account as Eos
import Graphql.Http
import Html exposing (Html, button, div, h3, p, span, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import I18Next
import Json.Decode as Decode
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


init : LoggedIn.Model -> Symbol -> Int -> ( Model, Cmd Msg )
init { shared } communityId claimId =
    ( initModel communityId claimId
    , fetchClaim claimId shared
    )


type ModalStatus
    = ModalClosed
    | ModalLoading Bool
    | ModalOpened Bool



-- MODEL


type alias Model =
    { communityId : Symbol
    , claimId : Int
    , statusClaim : Status
    , modalStatus : ModalStatus
    , isValidated : Bool
    }


initModel : Symbol -> Int -> Model
initModel communityId claimId =
    { communityId = communityId
    , claimId = claimId
    , statusClaim = Loading
    , modalStatus = ModalClosed
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
        t : String -> String
        t =
            I18Next.t shared.translations

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
                        Page.fullPageLoading

                    Loaded claim ->
                        let
                            isCurrentUserValidator =
                                claim.action.validators
                                    |> List.any
                                        (\v -> v.account == loggedIn.accountName)
                        in
                        div [ class "bg-gray-100" ]
                            [ Page.viewHeader loggedIn claim.action.description Route.Analysis
                            , div [ class "mt-10 mb-8" ]
                                [ Profile.viewLarge shared loggedIn.accountName claim.claimer
                                ]
                            , div [ class "mx-auto container px-4" ]
                                [ viewTitle shared claim
                                , viewDetails shared model claim
                                , viewVoters loggedIn claim
                                ]
                            , if model.isValidated || not isCurrentUserValidator then
                                text ""

                              else
                                viewVoteButtons shared.translators claim.id model.modalStatus
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
                Page.fullPageLoading
    }


viewVoteButtons : Translators -> Int -> ModalStatus -> Html Msg
viewVoteButtons ({ t } as translators) claimId modalStatus =
    let
        viewVoteModal : Bool -> Bool -> Html Msg
        viewVoteModal isApproving isInProgress =
            Claim.viewVoteClaimModal
                translators
                { voteMsg = VoteClaim
                , closeMsg = CloseModal
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
                , onClick (OpenModal False)
                ]
                [ text <| t "dashboard.reject" ]
            , button
                [ class "button button-primary"
                , onClick (OpenModal True)
                ]
                [ text <| t "dashboard.verify" ]
            ]
        , case modalStatus of
            ModalClosed ->
                text ""

            ModalLoading isApproving ->
                viewVoteModal isApproving True

            ModalOpened isApproving ->
                viewVoteModal isApproving False
        ]


viewTitle : Shared -> Claim.Model -> Html msg
viewTitle shared claim =
    let
        text_ s =
            text (I18Next.t shared.translations s)
    in
    div [ class "text-heading font-bold text-center mb-8" ]
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


viewDetails : Shared -> Model -> Claim.Model -> Html msg
viewDetails shared model claim =
    let
        text_ s =
            text (I18Next.t shared.translations s)

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
                        [ text (String.fromFloat claim.action.reward ++ " " ++ Eos.symbolToSymbolCodeString model.communityId) ]
                    ]
                ]
            ]
        , div [ class "mb-8" ]
            [ p
                [ class "text-caption uppercase text-green" ]
                [ text_ "claim.objective" ]
            , p
                [ class "pt-2 text-body" ]
                [ text claim.action.objective.description ]
            ]
        , div [ class "mb-8" ]
            [ p
                [ class "text-caption uppercase text-green" ]
                [ text_ "claim.your_reward" ]
            , p
                [ class "pt-2 text-body" ]
                [ text (String.fromFloat claim.action.verifierReward ++ " " ++ Eos.symbolToSymbolCodeString model.communityId) ]
            ]
        ]


viewVoters : LoggedIn.Model -> Claim.Model -> Html msg
viewVoters ({ shared } as loggedIn) claim =
    let
        text_ s =
            text (I18Next.t shared.translations s)

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
    | VoteClaim Int Bool
    | GotVoteResult Int (Result Decode.Value String)
    | OpenModal Bool
    | CloseModal


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

        OpenModal vote ->
            { model | modalStatus = ModalOpened vote } |> UR.init

        CloseModal ->
            { model | modalStatus = ModalClosed } |> UR.init

        VoteClaim claimId vote ->
            case model.statusClaim of
                Loaded _ ->
                    let
                        newModel =
                            { model
                                | modalStatus = ModalLoading vote
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
                                ++ Eos.symbolToString claim.action.objective.community.symbol
                    in
                    { model
                        | modalStatus = ModalClosed
                        , isValidated = True
                    }
                        |> UR.init
                        |> UR.addExt (LoggedIn.ShowFeedback LoggedIn.Success (message value))

                _ ->
                    { model
                        | modalStatus = ModalClosed
                    }
                        |> UR.init

        GotVoteResult _ (Err v) ->
            case model.statusClaim of
                Loaded claim ->
                    { model
                        | statusClaim = Loaded claim
                        , modalStatus = ModalClosed
                    }
                        |> UR.init
                        |> UR.logDebugValue msg v
                        |> UR.addExt (LoggedIn.ShowFeedback LoggedIn.Failure (t "community.verifyClaim.error"))

                _ ->
                    model
                        |> UR.init
                        |> UR.logDebugValue msg v



-- HELPERS


fetchClaim : Int -> Shared -> Cmd Msg
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
                    , Decode.succeed (Err Encode.null)
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

        OpenModal _ ->
            [ "OpenModal" ]

        CloseModal ->
            [ "CloseModal" ]
