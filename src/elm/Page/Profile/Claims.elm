module Page.Profile.Claims exposing
    ( Model
    , Msg(..)
    , init
    , jsAddressToMsg
    , msgToString
    , receiveBroadcast
    , update
    , view
    )

import Api.Graphql
import Cambiatus.Object
import Cambiatus.Object.User as Profile
import Cambiatus.Query
import Claim
import Community
import Eos
import Eos.Account as Eos
import Eos.EosError as EosError
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, src)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import List.Extra as List
import Page
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import UpdateResult as UR
import View.Feedback as Feedback


init : LoggedIn.Model -> String -> ( Model, Cmd Msg )
init loggedIn account =
    ( initModel account
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )


type alias Model =
    { status : Status
    , accountString : String
    , claimModalStatus : Claim.ModalStatus
    }


initModel : String -> Model
initModel account =
    { status = Loading
    , accountString = account
    , claimModalStatus = Claim.Closed
    }


type Status
    = Loading
    | Loaded (List Claim.ClaimProfileSummaries) ProfileClaims
    | NotFound
    | Failed (Graphql.Http.Error (Maybe ProfileClaims))



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        { t } =
            loggedIn.shared.translators

        pageTitle =
            t "profile.claims.title"

        content =
            case model.status of
                Loading ->
                    Page.fullPageLoading loggedIn.shared

                Loaded profileSummaries profileClaims ->
                    div []
                        [ Page.viewHeader loggedIn pageTitle
                        , viewResults loggedIn profileSummaries profileClaims
                        , viewClaimVoteModal loggedIn model
                        ]

                NotFound ->
                    Page.fullPageNotFound
                        (t "profile.claims.not_found.title")
                        (t "profile.claims.not_found.subtitle")

                Failed e ->
                    Page.fullPageGraphQLError pageTitle e
    in
    { title = pageTitle, content = content }


viewResults : LoggedIn.Model -> List Claim.ClaimProfileSummaries -> List Claim.Model -> Html Msg
viewResults loggedIn profileSummaries claims =
    let
        viewClaim profileSummary claimIndex claim =
            Claim.viewClaimCard loggedIn profileSummary claim
                |> Html.map (ClaimMsg claimIndex)
    in
    div [ class "container mx-auto px-4 mb-10" ]
        [ if List.length claims > 0 then
            div [ class "flex flex-wrap -mx-2 pt-4" ]
                (claims
                    |> List.reverse
                    |> List.map3 viewClaim profileSummaries (List.range 0 (List.length profileSummaries))
                )

          else
            viewEmptyResults loggedIn
        ]


viewClaimVoteModal : LoggedIn.Model -> Model -> Html Msg
viewClaimVoteModal loggedIn model =
    let
        viewVoteModal claimId isApproving isLoading =
            Claim.viewVoteClaimModal
                loggedIn.shared.translators
                { voteMsg = VoteClaim
                , closeMsg = ClaimMsg 0 Claim.CloseClaimModals
                , claimId = claimId
                , isApproving = isApproving
                , isInProgress = isLoading
                }
    in
    case model.claimModalStatus of
        Claim.VoteConfirmationModal claimId vote ->
            viewVoteModal claimId vote False

        Claim.Loading claimId vote ->
            viewVoteModal claimId vote True

        Claim.PhotoModal claimId ->
            Claim.viewPhotoModal loggedIn claimId
                |> Html.map (ClaimMsg 0)

        _ ->
            text ""


viewEmptyResults : LoggedIn.Model -> Html Msg
viewEmptyResults { shared } =
    let
        text_ s =
            text (shared.translators.t s)
    in
    div [ class "w-full text-center" ]
        [ div [ class "w-full flex justify-center" ]
            [ img [ src "/images/empty-analysis.svg", class "object-contain h-32 mb-3" ] []
            ]
        , div [ class "inline-block text-gray" ]
            [ text_ "profile.claims.empty_results"
            ]
        ]


type alias ProfileClaims =
    List Claim.Model


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = ClaimsLoaded (RemoteData (Graphql.Http.Error (Maybe ProfileClaims)) (Maybe ProfileClaims))
    | ClosedAuthModal
    | CompletedLoadCommunity Community.Model
    | ClaimMsg Int Claim.Msg
    | VoteClaim Claim.ClaimId Bool
    | GotVoteResult Claim.ClaimId (Result (Maybe Value) String)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        ClaimsLoaded (RemoteData.Success results) ->
            case results of
                Just claims ->
                    let
                        profileSummaries =
                            List.map Claim.initClaimProfileSummaries claims
                    in
                    { model | status = Loaded profileSummaries (List.reverse claims) }
                        |> UR.init

                Nothing ->
                    { model | status = NotFound }
                        |> UR.init

        ClaimsLoaded (RemoteData.Failure e) ->
            { model | status = Failed e }
                |> UR.init

        ClaimsLoaded _ ->
            UR.init model

        ClosedAuthModal ->
            { model | claimModalStatus = Claim.Closed }
                |> UR.init

        CompletedLoadCommunity community ->
            UR.init model
                |> UR.addCmd (profileClaimQuery loggedIn model.accountString community)

        ClaimMsg claimIndex m ->
            let
                updatedModel =
                    case ( model.status, m ) of
                        ( Loaded profileSummaries profileClaims, Claim.GotExternalMsg subMsg ) ->
                            { model
                                | status =
                                    Loaded
                                        (List.updateAt claimIndex (Claim.updateProfileSummaries subMsg) profileSummaries)
                                        profileClaims
                            }

                        _ ->
                            model
            in
            updatedModel
                |> Claim.updateClaimModalStatus m
                |> UR.init

        VoteClaim claimId vote ->
            case model.status of
                Loaded _ _ ->
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
                                      , data = Claim.encodeVerification claimId loggedIn.accountName vote
                                      }
                                    ]
                            }
                        |> LoggedIn.withAuthentication loggedIn
                            model
                            { successMsg = msg, errorMsg = ClosedAuthModal }

                _ ->
                    model
                        |> UR.init

        GotVoteResult claimId (Ok _) ->
            case model.status of
                Loaded profileSummaries profileClaims ->
                    let
                        maybeClaim : Maybe Claim.Model
                        maybeClaim =
                            List.find (\c -> c.id == claimId) profileClaims

                        message val =
                            [ ( "value", val ) ]
                                |> loggedIn.shared.translators.tr "claim.reward"
                    in
                    case maybeClaim of
                        Just claim ->
                            let
                                value =
                                    String.fromFloat claim.action.verifierReward
                                        ++ " "
                                        ++ Eos.symbolToSymbolCodeString claim.action.objective.community.symbol
                            in
                            { model | status = Loaded profileSummaries profileClaims }
                                |> UR.init
                                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Success (message value))
                                |> UR.addCmd (Route.replaceUrl loggedIn.shared.navKey (Route.ProfileClaims model.accountString))

                        Nothing ->
                            model
                                |> UR.init

                _ ->
                    model |> UR.init

        GotVoteResult _ (Err eosErrorString) ->
            let
                errorMessage =
                    EosError.parseClaimError loggedIn.shared.translators eosErrorString
            in
            case model.status of
                Loaded profileSummaries claims ->
                    let
                        updateShowClaimModal profileSummary =
                            { profileSummary | showClaimModal = False }
                    in
                    { model
                        | status = Loaded (List.map updateShowClaimModal profileSummaries) claims
                        , claimModalStatus = Claim.Closed
                    }
                        |> UR.init
                        |> UR.addExt (ShowFeedback Feedback.Failure errorMessage)

                _ ->
                    model |> UR.init


profileClaimQuery : LoggedIn.Model -> String -> Community.Model -> Cmd Msg
profileClaimQuery { shared, authToken } accountName community =
    Api.Graphql.query shared
        (Just authToken)
        (Cambiatus.Query.user { account = accountName } (selectionSet community.symbol))
        ClaimsLoaded


selectionSet : Eos.Symbol -> SelectionSet ProfileClaims Cambiatus.Object.User
selectionSet communityId =
    Profile.claims (\_ -> { communityId = Present (Eos.symbolToString communityId) }) Claim.selectionSet


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityLoaded community ->
            Just (CompletedLoadCommunity community)

        _ ->
            Nothing


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
        ClaimsLoaded r ->
            [ "ClaimsLoaded", UR.remoteDataToString r ]

        ClosedAuthModal ->
            [ "ClosedAuthModal" ]

        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        ClaimMsg _ _ ->
            [ "ClaimMsg" ]

        VoteClaim claimId _ ->
            [ "VoteClaim", String.fromInt claimId ]

        GotVoteResult _ r ->
            [ "GotVoteResult", UR.resultToString r ]
