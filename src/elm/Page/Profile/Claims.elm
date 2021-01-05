module Page.Profile.Claims exposing
    ( Model
    , Msg(..)
    , init
    , jsAddressToMsg
    , msgToString
    , update
    , view
    )

import Api.Graphql
import Cambiatus.Object
import Cambiatus.Object.Profile as Profile
import Cambiatus.Query
import Claim exposing (viewVoteClaimModal)
import Eos
import Eos.Account as Eos
import Eos.EosError as EosError
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import List.Extra as List
import Page
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import UpdateResult as UR


init : LoggedIn.Model -> String -> ( Model, Cmd Msg )
init loggedIn account =
    ( initModel account
    , profileClaimQuery loggedIn account
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
    | Loaded ProfileClaims
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

                Loaded profile ->
                    div []
                        [ Page.viewHeader loggedIn pageTitle Route.Dashboard
                        , viewResults loggedIn profile.claims
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


viewResults : LoggedIn.Model -> List Claim.Model -> Html Msg
viewResults loggedIn claims =
    let
        viewClaim claim =
            Claim.viewClaimCard loggedIn claim
                |> Html.map ClaimMsg
    in
    div [ class "container mx-auto px-4 mb-10" ]
        [ if List.length claims > 0 then
            div [ class "flex flex-wrap -mx-2" ]
                (claims
                    |> List.reverse
                    |> List.map viewClaim
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
                , closeMsg = ClaimMsg Claim.CloseClaimModals
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
                |> Html.map ClaimMsg

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
    { claims : List Claim.Model }


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = ClaimsLoaded (Result (Graphql.Http.Error (Maybe ProfileClaims)) (Maybe ProfileClaims))
    | ClaimMsg Claim.Msg
    | VoteClaim Claim.ClaimId Bool
    | GotVoteResult Claim.ClaimId (Result (Maybe Value) String)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        ClaimsLoaded (Ok results) ->
            case results of
                Just claims ->
                    { model | status = Loaded claims }
                        |> UR.init

                Nothing ->
                    { model | status = NotFound }
                        |> UR.init

        ClaimsLoaded (Err e) ->
            { model | status = Failed e }
                |> UR.init

        ClaimMsg m ->
            let
                claimCmd =
                    case m of
                        Claim.RouteOpened r ->
                            Route.replaceUrl loggedIn.shared.navKey r

                        _ ->
                            Cmd.none
            in
            Claim.updateClaimModalStatus m model
                |> UR.init
                |> UR.addCmd claimCmd

        VoteClaim claimId vote ->
            case model.status of
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

        GotVoteResult claimId (Ok _) ->
            case model.status of
                Loaded profile ->
                    let
                        maybeClaim : Maybe Claim.Model
                        maybeClaim =
                            List.find (\c -> c.id == claimId) profile.claims

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
                            { model
                                | status = Loaded profile
                            }
                                |> UR.init
                                |> UR.addExt (LoggedIn.ShowFeedback LoggedIn.Success (message value))
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
                Loaded claims ->
                    { model
                        | status = Loaded claims
                        , claimModalStatus = Claim.Closed
                    }
                        |> UR.init
                        |> UR.addExt (ShowFeedback LoggedIn.Failure errorMessage)

                _ ->
                    model |> UR.init


profileClaimQuery : LoggedIn.Model -> String -> Cmd Msg
profileClaimQuery ({ shared } as loggedIn) accountName =
    Api.Graphql.query
        shared
        (Cambiatus.Query.profile { account = accountName } selectionSet)
        ClaimsLoaded


selectionSet : SelectionSet ProfileClaims Cambiatus.Object.Profile
selectionSet =
    SelectionSet.succeed ProfileClaims
        |> with (Profile.claims Claim.selectionSet)


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
            [ "ClaimsLoaded", UR.resultToString r ]

        ClaimMsg _ ->
            [ "ClaimMsg" ]

        VoteClaim claimId _ ->
            [ "VoteClaim", String.fromInt claimId ]

        GotVoteResult _ r ->
            [ "GotVoteResult", UR.resultToString r ]
