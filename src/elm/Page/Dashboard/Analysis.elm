module Page.Dashboard.Analysis exposing
    ( Model
    , Msg
    , init
    , jsAddressToMsg
    , msgToString
    , receiveBroadcast
    , update
    , view
    )

import Api.Graphql
import Api.Relay
import Cambiatus.Query
import Claim
import Community
import Eos
import Eos.Account as Eos
import Eos.EosError as EosError
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Html exposing (Html, button, div, img, option, select, span, text)
import Html.Attributes exposing (class, selected, src, value)
import Html.Events exposing (onClick)
import Http
import Icons
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import List.Extra as List
import Page
import Profile
import RemoteData exposing (RemoteData)
import Route
import Select
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import Simple.Fuzzy
import UpdateResult as UR


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( initModel
    , case loggedIn.selectedCommunity of
        RemoteData.Success community ->
            fetchAnalysis loggedIn initFilter Nothing community

        _ ->
            Cmd.none
    )



-- MODEL


type alias Model =
    { status : Status
    , claimModalStatus : Claim.ModalStatus
    , autoCompleteState : Select.State
    , reloadOnNextQuery : Bool
    , filters : Filter
    }


initModel : Model
initModel =
    { status = Loading
    , claimModalStatus = Claim.Closed
    , autoCompleteState = Select.newState ""
    , reloadOnNextQuery = False
    , filters = initFilter
    }


type Status
    = Loading
    | Loaded (List Claim.Model) (Maybe Api.Relay.PageInfo)
    | Failed


type alias Filter =
    { profile : Maybe Profile.Minimal
    , statusFilter : StatusFilter
    }


initFilter : Filter
initFilter =
    { profile = Nothing, statusFilter = All }


type StatusFilter
    = All
    | Approved
    | Rejected
    | Pending



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view ({ shared } as loggedIn) model =
    let
        t =
            shared.translators.t

        pageTitle =
            t "all_analysis.title"

        content =
            case model.status of
                Loading ->
                    Page.fullPageLoading shared

                Loaded claims pageInfo ->
                    let
                        viewClaim claim =
                            Claim.viewClaimCard loggedIn claim
                                |> Html.map ClaimMsg
                    in
                    div []
                        [ Page.viewHeader loggedIn pageTitle Route.Dashboard
                        , div [ class "container mx-auto px-4 mb-10" ]
                            [ viewFilters loggedIn model
                            , if List.length claims > 0 then
                                div []
                                    [ div [ class "flex flex-wrap -mx-2" ]
                                        (List.map viewClaim claims)
                                    , viewPagination loggedIn pageInfo
                                    ]

                              else
                                viewEmptyResults loggedIn
                            ]
                        , let
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

                            Claim.PhotoModal claim ->
                                Claim.viewPhotoModal loggedIn claim
                                    |> Html.map ClaimMsg

                            _ ->
                                text ""
                        ]

                Failed ->
                    Page.fullPageError (t "all_analysis.error") Http.Timeout
    in
    { title = pageTitle
    , content = content
    }


viewFilters : LoggedIn.Model -> Model -> Html Msg
viewFilters ({ shared } as loggedIn) model =
    let
        t =
            shared.translators.t

        text_ s =
            text (t s)
    in
    div [ class "mt-4 mb-12" ]
        [ div []
            [ span [ class "input-label" ]
                [ text_ "all_analysis.filter.user" ]
            , case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    let
                        selectedUsers =
                            Maybe.map (\v -> [ v ]) model.filters.profile
                                |> Maybe.withDefault []
                    in
                    div []
                        [ Html.map SelectMsg
                            (Select.view
                                (selectConfiguration shared False)
                                model.autoCompleteState
                                community.members
                                selectedUsers
                            )
                        , viewSelectedVerifiers loggedIn selectedUsers
                        ]

                _ ->
                    text ""
            ]
        , div [ class "mt-6" ]
            [ span [ class "input-label" ] [ text_ "all_analysis.filter.status.label" ]
            , select
                [ class "input w-full mb-2 border form-select border-gray-500 rounded-sm"
                , Html.Events.on "change"
                    (Decode.map
                        (\val ->
                            case val of
                                "approved" ->
                                    SelectStatusFilter Approved

                                "rejected" ->
                                    SelectStatusFilter Rejected

                                "pending" ->
                                    SelectStatusFilter Pending

                                _ ->
                                    SelectStatusFilter All
                        )
                        Html.Events.targetValue
                    )
                ]
                [ option
                    [ value ""
                    , selected (model.filters.statusFilter == All)
                    ]
                    [ text_ "all_analysis.all" ]
                , option
                    [ value "approved"
                    , selected (model.filters.statusFilter == Approved)
                    ]
                    [ text_ "all_analysis.approved" ]
                , option
                    [ value "rejected"
                    , selected (model.filters.statusFilter == Rejected)
                    ]
                    [ text_ "all_analysis.disapproved" ]
                , option
                    [ value "pending"
                    , selected (model.filters.statusFilter == Pending)
                    ]
                    [ text_ "all_analysis.pending" ]
                ]
            ]
        ]


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
            [ text_ "all_analysis.empty"
            , span [ class "underline text-orange-500", onClick ClearFilters ] [ text_ "all_analysis.clear_filters" ]
            ]
        ]


viewPagination : LoggedIn.Model -> Maybe Api.Relay.PageInfo -> Html Msg
viewPagination { shared } maybePageInfo =
    let
        text_ s =
            text (shared.translators.t s)
    in
    case maybePageInfo of
        Just pageInfo ->
            div [ class "flex justify-center" ]
                [ if pageInfo.hasNextPage then
                    button
                        [ Route.href Route.Analysis
                        , class "button button-primary uppercase w-full"
                        , onClick ShowMore
                        ]
                        [ text_ "all_analysis.show_more" ]

                  else
                    text ""
                ]

        Nothing ->
            text ""



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = ClaimsLoaded (RemoteData (Graphql.Http.Error (Maybe Claim.Paginated)) (Maybe Claim.Paginated))
    | CompletedLoadCommunity Community.Model
    | ClaimMsg Claim.Msg
    | VoteClaim Claim.ClaimId Bool
    | GotVoteResult Claim.ClaimId (Result (Maybe Value) String)
    | SelectMsg (Select.Msg Profile.Minimal)
    | OnSelectVerifier (Maybe Profile.Minimal)
    | ShowMore
    | ClearSelectSelection
    | SelectStatusFilter StatusFilter
    | ClearFilters


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        ClaimsLoaded (RemoteData.Success results) ->
            case model.status of
                Loaded claims _ ->
                    let
                        newClaims =
                            if model.reloadOnNextQuery then
                                Claim.paginatedToList results

                            else
                                claims ++ Claim.paginatedToList results
                    in
                    { model
                        | status = Loaded newClaims (Claim.paginatedPageInfo results)
                        , reloadOnNextQuery = False
                    }
                        |> UR.init

                _ ->
                    { model
                        | status =
                            Loaded (Claim.paginatedToList results) (Claim.paginatedPageInfo results)
                        , reloadOnNextQuery = False
                    }
                        |> UR.init

        ClaimsLoaded (RemoteData.Failure _) ->
            { model | status = Failed } |> UR.init

        ClaimsLoaded _ ->
            UR.init model

        CompletedLoadCommunity community ->
            UR.init model
                |> UR.addCmd (fetchAnalysis loggedIn model.filters Nothing community)

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
                Loaded _ _ ->
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
                Loaded claims pageInfo ->
                    let
                        maybeClaim : Maybe Claim.Model
                        maybeClaim =
                            List.find (\c -> c.id == claimId) claims

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

                                updatedClaims =
                                    List.updateIf (\c -> c.id == claim.id)
                                        (\_ -> claim)
                                        claims
                            in
                            { model
                                | status = Loaded updatedClaims pageInfo
                            }
                                |> UR.init
                                |> UR.addExt (LoggedIn.ShowFeedback LoggedIn.Success (message value))
                                |> UR.addCmd (Route.replaceUrl loggedIn.shared.navKey Route.Analysis)

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
                Loaded claims pageInfo ->
                    { model
                        | status = Loaded claims pageInfo
                        , claimModalStatus = Claim.Closed
                    }
                        |> UR.init
                        |> UR.addExt (ShowFeedback LoggedIn.Failure errorMessage)

                _ ->
                    model |> UR.init

        SelectMsg subMsg ->
            let
                ( updated, cmd ) =
                    Select.update (selectConfiguration loggedIn.shared False) subMsg model.autoCompleteState
            in
            UR.init { model | autoCompleteState = updated }
                |> UR.addCmd cmd

        OnSelectVerifier maybeProfile ->
            let
                oldFilters =
                    model.filters
            in
            case ( model.status, loggedIn.selectedCommunity ) of
                ( Loaded _ _, RemoteData.Success community ) ->
                    let
                        newModel =
                            { model
                                | status = Loading
                                , filters = { oldFilters | profile = maybeProfile }
                                , reloadOnNextQuery = True
                            }
                    in
                    newModel
                        |> UR.init
                        |> UR.addCmd (fetchAnalysis loggedIn newModel.filters Nothing community)

                _ ->
                    UR.init model

        ShowMore ->
            case ( model.status, loggedIn.selectedCommunity ) of
                ( Loaded _ pageInfo, RemoteData.Success community ) ->
                    let
                        cursor : Maybe String
                        cursor =
                            Maybe.andThen .endCursor pageInfo
                    in
                    model
                        |> UR.init
                        |> UR.addCmd (fetchAnalysis loggedIn model.filters cursor community)

                _ ->
                    UR.init model

        ClearSelectSelection ->
            case ( model.status, loggedIn.selectedCommunity ) of
                ( Loaded _ _, RemoteData.Success community ) ->
                    let
                        oldFilters =
                            model.filters

                        newModel =
                            { model
                                | status = Loading
                                , filters = { oldFilters | profile = Nothing }
                                , reloadOnNextQuery = True
                            }
                    in
                    newModel
                        |> UR.init
                        |> UR.addCmd (fetchAnalysis loggedIn newModel.filters Nothing community)

                _ ->
                    UR.init model

        SelectStatusFilter statusFilter ->
            let
                oldFilters =
                    model.filters

                newModel =
                    { model
                        | filters = { oldFilters | statusFilter = statusFilter }
                        , status = Loading
                        , reloadOnNextQuery = True
                    }
            in
            newModel
                |> UR.init
                |> UR.addCmd
                    (case loggedIn.selectedCommunity of
                        RemoteData.Success community ->
                            fetchAnalysis loggedIn newModel.filters Nothing community

                        _ ->
                            Cmd.none
                    )

        ClearFilters ->
            { model
                | filters = initFilter
                , reloadOnNextQuery = True
                , status = Loading
            }
                |> UR.init
                |> UR.addCmd
                    (case loggedIn.selectedCommunity of
                        RemoteData.Success community ->
                            fetchAnalysis loggedIn initFilter Nothing community

                        _ ->
                            Cmd.none
                    )


fetchAnalysis : LoggedIn.Model -> Filter -> Maybe String -> Community.Model -> Cmd Msg
fetchAnalysis { shared, authToken } { profile, statusFilter } maybeCursorAfter community =
    let
        optionalClaimer =
            case profile of
                Just p ->
                    Present (Eos.nameToString p.account)

                Nothing ->
                    Absent

        optionalStatus =
            case statusFilter of
                All ->
                    Absent

                Approved ->
                    Present "approved"

                Rejected ->
                    Present "rejected"

                Pending ->
                    Present "pending"

        filterRecord =
            { claimer = optionalClaimer
            , status = optionalStatus
            }

        filter =
            case ( filterRecord.claimer, filterRecord.status ) of
                ( Absent, Absent ) ->
                    Absent

                ( _, _ ) ->
                    Present filterRecord

        required =
            { communityId = Eos.symbolToString community.symbol }

        mapFn =
            \s ->
                if String.isEmpty s then
                    Nothing

                else
                    Just (Present s)

        optionals =
            \opts ->
                { opts
                    | first = Present 16
                    , after =
                        Maybe.andThen mapFn maybeCursorAfter
                            |> Maybe.withDefault Absent
                    , filter = filter
                }
    in
    Api.Graphql.query shared
        (Just authToken)
        (Cambiatus.Query.claimsAnalysisHistory optionals required Claim.claimPaginatedSelectionSet)
        ClaimsLoaded



-- Configure Select


selectFilter : Int -> (a -> String) -> String -> List a -> Maybe (List a)
selectFilter minChars toLabel query items =
    if String.length query < minChars then
        Nothing

    else
        items
            |> Simple.Fuzzy.filter toLabel query
            |> Just


selectConfiguration : Shared -> Bool -> Select.Config Msg Profile.Minimal
selectConfiguration shared isDisabled =
    Profile.selectConfig
        (Select.newConfig
            { onSelect = OnSelectVerifier
            , toLabel = \p -> Eos.nameToString p.account
            , filter = selectFilter 2 (\p -> Eos.nameToString p.account)
            }
            |> Select.withMultiSelection True
        )
        shared
        isDisabled


viewSelectedVerifiers : LoggedIn.Model -> List Profile.Minimal -> Html Msg
viewSelectedVerifiers ({ shared } as loggedIn) selectedVerifiers =
    if List.isEmpty selectedVerifiers then
        text ""

    else
        div [ class "flex flex-row mt-3 mb-10 flex-wrap" ]
            (selectedVerifiers
                |> List.map
                    (\p ->
                        div
                            [ class "flex justify-between flex-col m-3 items-center" ]
                            [ Profile.view shared loggedIn.accountName p
                            , div
                                [ onClick ClearSelectSelection
                                , class "h-6 w-6 flex items-center mt-4"
                                ]
                                [ Icons.trash "" ]
                            ]
                    )
            )


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityLoaded community ->
            Just (CompletedLoadCommunity community)


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
            [ "ChecksLoaded", UR.remoteDataToString r ]

        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        ClaimMsg _ ->
            [ "ClaimMsg" ]

        VoteClaim claimId _ ->
            [ "VoteClaim", String.fromInt claimId ]

        GotVoteResult _ r ->
            [ "GotVoteResult", UR.resultToString r ]

        SelectMsg _ ->
            [ "SelectMsg", "sub" ]

        OnSelectVerifier _ ->
            [ "OnSelectVerifier" ]

        ShowMore ->
            [ "ShowMore" ]

        ClearSelectSelection ->
            [ "ClearSelectSelection" ]

        SelectStatusFilter _ ->
            [ "SelectStatusFilter" ]

        ClearFilters ->
            [ "ClearFilters" ]
