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
import Cambiatus.Enum.Direction
import Cambiatus.Query
import Claim
import Community
import Eos
import Eos.Account as Eos
import Eos.EosError as EosError
import Graphql.Http
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (class, classList, src)
import Html.Events exposing (onClick)
import Icons
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import List.Extra as List
import Page
import Profile
import Profile.Summary
import RemoteData exposing (RemoteData)
import Select
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import Simple.Fuzzy
import UpdateResult as UR
import View.Components
import View.Feedback as Feedback
import View.Form.Select as Select
import View.Modal as Modal
import View.TabSelector


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( initModel
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )



-- MODEL


type alias Model =
    { status : RemoteData (Graphql.Http.Error (Maybe Claim.Paginated)) LoadedModel
    , claimModalStatus : Claim.ModalStatus
    , autoCompleteState : Select.State
    , reloadOnNextQuery : Bool
    , selectedTab : Tab
    , showFilterModal : Bool
    , filters : Filter
    , filtersBeingEdited : Filter
    , filterProfileSummary : Profile.Summary.Model
    , direction : FilterDirection
    , tabCounts : TabCounts
    }


initModel : Model
initModel =
    { status = RemoteData.Loading
    , claimModalStatus = Claim.Closed
    , autoCompleteState = Select.newState ""
    , reloadOnNextQuery = False
    , selectedTab = WaitingToVote
    , showFilterModal = False
    , filters = initFilter
    , filtersBeingEdited = initFilter
    , filterProfileSummary =
        Profile.Summary.init False
            |> Profile.Summary.withPreventScrolling View.Components.PreventScrollAlways
    , direction = DESC
    , tabCounts = { waitingToVote = Nothing, analyzed = Nothing }
    }


type alias LoadedModel =
    { claims : List Claim.Model
    , profileSummaries : List Claim.ClaimProfileSummaries
    , pageInfo : Maybe Api.Relay.PageInfo
    }


type alias TabCounts =
    { waitingToVote : Maybe Int, analyzed : Maybe Int }


type Tab
    = WaitingToVote
    | Analyzed


type alias Filter =
    { profile : Maybe Profile.Minimal
    , statusFilter : StatusFilter
    }


type FilterDirection
    = ASC
    | DESC


initFilter : Filter
initFilter =
    { profile = Nothing, statusFilter = All }


type StatusFilter
    = All
    | Approved
    | Rejected


statusFilterToString : StatusFilter -> String
statusFilterToString filter =
    case filter of
        All ->
            "all"

        Approved ->
            "approved"

        Rejected ->
            "rejected"



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view ({ shared } as loggedIn) model =
    let
        t =
            shared.translators.t

        pageTitle =
            t "all_analysis.title"

        viewLoading =
            div [ class "mb-10" ] [ Page.fullPageLoading shared ]

        content =
            case model.status of
                RemoteData.NotAsked ->
                    [ viewLoading ]

                RemoteData.Loading ->
                    [ viewLoading ]

                RemoteData.Success loadedModel ->
                    viewContent loggedIn loadedModel model

                RemoteData.Failure err ->
                    [ div [ class "text-center container mx-auto" ]
                        [ Page.fullPageGraphQLError (t "all_analysis.error") err ]
                    ]
    in
    { title = pageTitle
    , content =
        div []
            (Page.viewHeader loggedIn pageTitle
                :: viewFiltersModal loggedIn model
                :: viewHeaderAndOptions loggedIn model
                ++ content
            )
    }


viewHeaderAndOptions : LoggedIn.Model -> Model -> List (Html Msg)
viewHeaderAndOptions loggedIn model =
    [ div [ class "bg-white py-4" ]
        [ div [ class "container mx-auto px-4 flex justify-center" ]
            [ viewTabSelector loggedIn.shared model
            ]
        ]
    , div [ class "container mx-auto px-4" ]
        [ viewFilterAndOrder loggedIn model ]
    ]


viewContent : LoggedIn.Model -> LoadedModel -> Model -> List (Html Msg)
viewContent loggedIn { claims, profileSummaries, pageInfo } model =
    let
        viewClaim profileSummary claimIndex claim =
            Claim.viewClaimCard loggedIn profileSummary claim
                |> Html.map (ClaimMsg claimIndex)
    in
    [ div [ class "container mx-auto px-4 mb-10" ]
        [ if List.length claims > 0 then
            div []
                [ div [ class "flex flex-wrap -mx-2" ]
                    (List.map3 viewClaim
                        profileSummaries
                        (List.range 0 (List.length claims))
                        claims
                    )
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

        Claim.PhotoModal claim ->
            Claim.viewPhotoModal loggedIn claim
                |> Html.map (ClaimMsg 0)

        _ ->
            text ""
    ]


viewTabSelector : Shared -> Model -> Html Msg
viewTabSelector { translators } model =
    let
        count : Tab -> Maybe Int
        count tab =
            case tab of
                WaitingToVote ->
                    model.tabCounts.waitingToVote

                Analyzed ->
                    model.tabCounts.analyzed
    in
    View.TabSelector.init
        { tabs =
            [ { tab = WaitingToVote, label = translators.t "all_analysis.tabs.waiting_vote", count = count WaitingToVote }
            , { tab = Analyzed, label = translators.t "all_analysis.tabs.analyzed", count = count Analyzed }
            ]
        , selectedTab = model.selectedTab
        , onSelectTab = SelectedTab
        }
        |> View.TabSelector.withContainerAttrs [ class "w-full md:w-2/3 xl:w-2/5" ]
        |> View.TabSelector.toHtml


viewFilterAndOrder : LoggedIn.Model -> Model -> Html Msg
viewFilterAndOrder { shared } model =
    let
        ( filterDirectionIcon, filterDirectionLabel ) =
            case model.direction of
                ASC ->
                    ( Icons.sortAscending, "all_analysis.filter.sort.asc" )

                DESC ->
                    ( Icons.sortDescending, "all_analysis.filter.sort.desc" )

        viewButton label icon onClickMsg =
            button
                [ class "button button-secondary flex-grow-1 justify-between pl-4"
                , onClick onClickMsg
                ]
                [ text (shared.translators.t label)
                , icon
                ]
    in
    div [ class "w-full md:w-2/3 xl:w-1/3 mx-auto mt-4 mb-6 flex space-x-4" ]
        [ viewButton "all_analysis.filter.title" (Icons.arrowDown "fill-current") OpenedFilterModal
        , viewButton filterDirectionLabel (filterDirectionIcon "mr-2") ToggleSorting
        ]


viewFiltersModal : LoggedIn.Model -> Model -> Html Msg
viewFiltersModal ({ shared } as loggedIn) model =
    let
        { t } =
            shared.translators

        showFilterSelect =
            case model.selectedTab of
                WaitingToVote ->
                    False

                Analyzed ->
                    True

        showUserSelect =
            case model.selectedTab of
                WaitingToVote ->
                    True

                Analyzed ->
                    False
    in
    Modal.initWith
        { closeMsg = ClosedFilterModal
        , isVisible = model.showFilterModal
        }
        |> Modal.withHeader (t "all_analysis.filter.title")
        |> Modal.withBody
            [ if not showUserSelect then
                text ""

              else
                div []
                    (span [ class "input-label" ]
                        [ text (t "all_analysis.filter.user") ]
                        :: (case loggedIn.selectedCommunity of
                                RemoteData.Success community ->
                                    let
                                        selectedUsers =
                                            Maybe.map (\v -> [ v ]) model.filtersBeingEdited.profile
                                                |> Maybe.withDefault []

                                        addRelativeMenuClass =
                                            if not showFilterSelect && List.isEmpty selectedUsers then
                                                Select.withMenuClass "!relative"

                                            else
                                                identity
                                    in
                                    [ div [ classList [ ( "mb-10", not showFilterSelect && List.isEmpty selectedUsers ) ] ]
                                        [ Html.map SelectMsg
                                            (Select.view
                                                (selectConfiguration shared False
                                                    |> addRelativeMenuClass
                                                )
                                                model.autoCompleteState
                                                community.members
                                                selectedUsers
                                            )
                                        , viewSelectedVerifiers loggedIn model.filterProfileSummary selectedUsers
                                        ]
                                    ]

                                _ ->
                                    []
                           )
                    )
            , if not showFilterSelect then
                text ""

              else
                Select.init
                    { id = "status_filter_select"
                    , label = t "all_analysis.filter.status.label"
                    , onInput = SelectStatusFilter
                    , firstOption = { value = All, label = t "all_analysis.all" }
                    , value = model.filters.statusFilter
                    , valueToString = statusFilterToString
                    , disabled = False
                    , problems = Nothing
                    }
                    |> Select.withOptions
                        [ { value = Approved, label = t "all_analysis.approved" }
                        , { value = Rejected, label = t "all_analysis.disapproved" }
                        ]
                    |> Select.withContainerAttrs
                        [ class "mt-1"
                        , classList [ ( "mt-6", showUserSelect ) ]
                        ]
                    |> Select.toHtml
            , div []
                [ button
                    [ class "button button-primary w-full"
                    , onClick ClickedApplyFilters
                    ]
                    [ text (t "all_analysis.filter.apply") ]
                ]
            ]
        |> Modal.toHtml


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
            , button [ class "underline text-orange-500", onClick ClearFilters ] [ text_ "all_analysis.clear_filters" ]
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
                        [ class "button button-primary uppercase w-full"
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
    = ClaimsLoaded Tab (RemoteData (Graphql.Http.Error (Maybe Claim.Paginated)) (Maybe Claim.Paginated))
    | ClosedAuthModal
    | CompletedLoadCommunity Community.Model
    | ClaimMsg Int Claim.Msg
    | VoteClaim Claim.ClaimId Bool
    | GotVoteResult Claim.ClaimId (Result (Maybe Value) String)
    | OpenedFilterModal
    | ClosedFilterModal
    | ClickedApplyFilters
    | SelectMsg (Select.Msg Profile.Minimal)
    | SelectedTab Tab
    | OnSelectVerifier (Maybe Profile.Minimal)
    | ShowMore
    | ClearSelectSelection
    | SelectStatusFilter StatusFilter
    | ToggleSorting
    | ClearFilters
    | GotProfileSummaryMsg Profile.Summary.Msg


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        ClaimsLoaded tab (RemoteData.Success results) ->
            let
                initProfileSummaries claims =
                    List.map Claim.initClaimProfileSummaries claims

                oldTabCounts =
                    model.tabCounts

                newTabCounts =
                    let
                        newTabCount =
                            results
                                |> Maybe.andThen .count
                    in
                    case tab of
                        WaitingToVote ->
                            { oldTabCounts | waitingToVote = newTabCount }

                        Analyzed ->
                            { oldTabCounts | analyzed = newTabCount }
            in
            if tab /= model.selectedTab then
                { model | tabCounts = newTabCounts }
                    |> UR.init

            else
                case model.status of
                    RemoteData.Success { claims } ->
                        let
                            newClaims =
                                if model.reloadOnNextQuery then
                                    Claim.paginatedToList results

                                else
                                    claims ++ Claim.paginatedToList results
                        in
                        { model
                            | status =
                                RemoteData.Success
                                    { claims = newClaims
                                    , profileSummaries = initProfileSummaries newClaims
                                    , pageInfo = Claim.paginatedPageInfo results
                                    }
                            , tabCounts = newTabCounts
                            , reloadOnNextQuery = False
                        }
                            |> UR.init

                    _ ->
                        { model
                            | status =
                                RemoteData.Success
                                    { claims = Claim.paginatedToList results
                                    , profileSummaries = initProfileSummaries (Claim.paginatedToList results)
                                    , pageInfo = Claim.paginatedPageInfo results
                                    }
                            , tabCounts = newTabCounts
                            , reloadOnNextQuery = False
                        }
                            |> UR.init

        ClaimsLoaded _ (RemoteData.Failure err) ->
            { model | status = RemoteData.Failure err } |> UR.init

        ClaimsLoaded _ _ ->
            UR.init model

        ClosedAuthModal ->
            { model | claimModalStatus = Claim.Closed }
                |> UR.init

        CompletedLoadCommunity community ->
            UR.init model
                |> UR.addCmd (fetchAnalysis loggedIn model Nothing WaitingToVote community.symbol)
                |> UR.addCmd (fetchAnalysis loggedIn model Nothing Analyzed community.symbol)
                |> UR.addExt (LoggedIn.ReloadResource LoggedIn.TimeResource)

        ClaimMsg claimIndex m ->
            let
                updatedModel =
                    case ( model.status, m ) of
                        ( RemoteData.Success ({ profileSummaries } as loadedModel), Claim.GotExternalMsg subMsg ) ->
                            { model
                                | status =
                                    RemoteData.Success
                                        { loadedModel
                                            | profileSummaries =
                                                List.updateAt claimIndex (Claim.updateProfileSummaries subMsg) profileSummaries
                                        }
                            }

                        _ ->
                            model
            in
            updatedModel
                |> Claim.updateClaimModalStatus m
                |> UR.init

        VoteClaim claimId vote ->
            case model.status of
                RemoteData.Success _ ->
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
                RemoteData.Success ({ claims } as loadedModel) ->
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
                                symbol =
                                    claim.action.objective.community.symbol

                                value =
                                    Eos.assetToString
                                        { amount = claim.action.verifierReward
                                        , symbol = symbol
                                        }

                                updatedClaims =
                                    List.filter (\c -> c.id /= claim.id) claims

                                updateCount operator maybeCount =
                                    Maybe.map (\count -> operator count 1) maybeCount
                            in
                            { model
                                | status = RemoteData.Success { loadedModel | claims = updatedClaims }
                                , tabCounts =
                                    { waitingToVote = updateCount (-) model.tabCounts.waitingToVote
                                    , analyzed = updateCount (+) model.tabCounts.analyzed
                                    }
                                , claimModalStatus = Claim.Closed
                            }
                                |> UR.init
                                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Success (message value))

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
                RemoteData.Success loadedModel ->
                    let
                        hideClaimModal profileSummary =
                            { profileSummary | showClaimModal = False }
                    in
                    { model
                        | status = RemoteData.Success { loadedModel | profileSummaries = List.map hideClaimModal loadedModel.profileSummaries }
                        , claimModalStatus = Claim.Closed
                    }
                        |> UR.init
                        |> UR.addExt (ShowFeedback Feedback.Failure errorMessage)

                _ ->
                    model |> UR.init

        OpenedFilterModal ->
            { model | showFilterModal = True }
                |> UR.init

        ClosedFilterModal ->
            { model | showFilterModal = False }
                |> UR.init

        ClickedApplyFilters ->
            if model.filtersBeingEdited == model.filters then
                { model | showFilterModal = False }
                    |> UR.init

            else
                let
                    newModel =
                        { model
                            | showFilterModal = False
                            , filters = model.filtersBeingEdited
                            , reloadOnNextQuery = True
                            , status = RemoteData.Loading
                        }

                    addFetchCommand =
                        case loggedIn.selectedCommunity of
                            RemoteData.Success community ->
                                UR.addCmd (fetchAnalysis loggedIn newModel Nothing model.selectedTab community.symbol)

                            _ ->
                                identity
                in
                newModel
                    |> UR.init
                    |> addFetchCommand

        SelectMsg subMsg ->
            let
                ( updated, cmd ) =
                    Select.update (selectConfiguration loggedIn.shared False) subMsg model.autoCompleteState
            in
            UR.init { model | autoCompleteState = updated }
                |> UR.addCmd cmd

        SelectedTab tab ->
            let
                newModel =
                    { model
                        | selectedTab = tab
                        , filters = initFilter
                        , filtersBeingEdited = initFilter
                        , status = RemoteData.Loading
                        , reloadOnNextQuery = True
                    }

                addFetchCommand =
                    case loggedIn.selectedCommunity of
                        RemoteData.Success community ->
                            UR.addCmd (fetchAnalysis loggedIn newModel Nothing tab community.symbol)

                        _ ->
                            identity
            in
            newModel
                |> UR.init
                |> addFetchCommand

        OnSelectVerifier maybeProfile ->
            let
                oldFilters =
                    model.filtersBeingEdited
            in
            { model | filtersBeingEdited = { oldFilters | profile = maybeProfile } }
                |> UR.init

        ShowMore ->
            case ( model.status, loggedIn.selectedCommunity ) of
                ( RemoteData.Success { pageInfo }, RemoteData.Success community ) ->
                    let
                        cursor : Maybe String
                        cursor =
                            Maybe.andThen .endCursor pageInfo
                    in
                    model
                        |> UR.init
                        |> UR.addCmd (fetchAnalysis loggedIn model cursor model.selectedTab community.symbol)

                _ ->
                    UR.init model

        ClearSelectSelection ->
            let
                oldFilters =
                    model.filtersBeingEdited
            in
            { model | filtersBeingEdited = { oldFilters | profile = Nothing } }
                |> UR.init

        SelectStatusFilter statusFilter ->
            let
                oldFilters =
                    model.filtersBeingEdited
            in
            { model | filtersBeingEdited = { oldFilters | statusFilter = statusFilter } }
                |> UR.init

        ClearFilters ->
            let
                newModel =
                    { model
                        | filters = initFilter
                        , filtersBeingEdited = initFilter
                        , direction = DESC
                        , reloadOnNextQuery = True
                        , status = RemoteData.Loading
                    }
            in
            newModel
                |> UR.init
                |> UR.addCmd
                    (case loggedIn.selectedCommunity of
                        RemoteData.Success community ->
                            fetchAnalysis loggedIn newModel Nothing model.selectedTab community.symbol

                        _ ->
                            Cmd.none
                    )

        ToggleSorting ->
            let
                sortDirection =
                    if model.direction == ASC then
                        DESC

                    else
                        ASC

                newModel =
                    { model
                        | direction = sortDirection
                        , reloadOnNextQuery = True
                        , status = RemoteData.Loading
                    }

                fetchCmd =
                    case loggedIn.selectedCommunity of
                        RemoteData.Success community ->
                            fetchAnalysis loggedIn newModel Nothing model.selectedTab community.symbol

                        _ ->
                            Cmd.none
            in
            newModel
                |> UR.init
                |> UR.addCmd fetchCmd

        GotProfileSummaryMsg subMsg ->
            { model | filterProfileSummary = Profile.Summary.update subMsg model.filterProfileSummary }
                |> UR.init


fetchAnalysis : LoggedIn.Model -> Model -> Maybe String -> Tab -> Eos.Symbol -> Cmd Msg
fetchAnalysis { shared, authToken } model maybeCursorAfter tab symbol =
    let
        cursorAfter =
            case maybeCursorAfter of
                Nothing ->
                    Absent

                Just "" ->
                    Absent

                Just cursor ->
                    Present cursor

        optionalClaimer =
            model.filters.profile
                |> Maybe.map (.account >> Eos.nameToString)
                |> OptionalArgument.fromMaybe

        optionalStatus =
            case model.filters.statusFilter of
                All ->
                    Absent

                Approved ->
                    Present "approved"

                Rejected ->
                    Present "rejected"

        direction =
            case model.direction of
                ASC ->
                    Cambiatus.Enum.Direction.Asc

                DESC ->
                    Cambiatus.Enum.Direction.Desc

        filterRecord =
            { claimer = optionalClaimer
            , status = optionalStatus
            , direction = Present direction
            }

        optionals =
            \opts ->
                { opts
                    | first = Present 16
                    , after = cursorAfter
                    , filter = Present filterRecord
                }

        required =
            { communityId = Eos.symbolToString symbol }

        query =
            case tab of
                WaitingToVote ->
                    Cambiatus.Query.pendingClaims optionals required Claim.claimPaginatedSelectionSet

                Analyzed ->
                    Cambiatus.Query.analyzedClaims optionals required Claim.claimPaginatedSelectionSet
    in
    Api.Graphql.query shared (Just authToken) query (ClaimsLoaded tab)



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
            |> Select.withMenuClass "max-h-40 overflow-y-auto"
        )
        shared
        isDisabled


viewSelectedVerifiers : LoggedIn.Model -> Profile.Summary.Model -> List Profile.Minimal -> Html Msg
viewSelectedVerifiers ({ shared } as loggedIn) profileSummary selectedVerifiers =
    if List.isEmpty selectedVerifiers then
        text ""

    else
        div [ class "flex flex-row mt-3 mb-10 flex-wrap" ]
            (selectedVerifiers
                |> List.map
                    (\p ->
                        div
                            [ class "flex justify-between flex-col m-3 items-center" ]
                            [ profileSummary
                                |> Profile.Summary.withRelativeSelector ".modal-content"
                                |> Profile.Summary.withScrollSelector ".modal-body"
                                |> Profile.Summary.view shared loggedIn.accountName p
                                |> Html.map GotProfileSummaryMsg
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
        ClaimsLoaded _ r ->
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

        OpenedFilterModal ->
            [ "OpenedFilterModal" ]

        ClosedFilterModal ->
            [ "ClosedFilterModal" ]

        ClickedApplyFilters ->
            [ "ClickedApplyFilters" ]

        SelectMsg _ ->
            [ "SelectMsg", "sub" ]

        SelectedTab _ ->
            [ "SelectedTab" ]

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

        ToggleSorting ->
            [ "ToggleSorting" ]

        GotProfileSummaryMsg subMsg ->
            "GotProfileSummaryMsg" :: Profile.Summary.msgToString subMsg
