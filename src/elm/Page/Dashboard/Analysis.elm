module Page.Dashboard.Analysis exposing
    ( Model
    , Msg
    , init
    , jsAddressToMsg
    , msgToString
    , update
    , view
    )

import Api.Relay
import Cambiatus.Enum.Direction
import Cambiatus.Enum.Permission as Permission
import Cambiatus.Query
import Claim
import Dict
import Eos
import Eos.Account as Eos
import Eos.EosError as EosError
import Form
import Form.Select
import Form.UserPicker
import Graphql.Http
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Icons
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import List.Extra as List
import Log
import Page
import Profile
import Profile.Summary
import RemoteData exposing (RemoteData)
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import UpdateResult as UR
import View.Components
import View.Feedback as Feedback
import View.Modal as Modal
import View.TabSelector


init : LoggedIn.Model -> UpdateResult
init loggedIn =
    let
        model =
            initModel
    in
    model
        |> UR.init
        |> UR.addExt (fetchAnalysis loggedIn model Nothing WaitingToVote)
        |> UR.addExt (fetchAnalysis loggedIn model Nothing Analyzed)
        |> UR.addExt (LoggedIn.ReloadResource LoggedIn.TimeResource)



-- MODEL


type alias Model =
    { status : RemoteData (Graphql.Http.Error (Maybe Claim.Paginated)) LoadedModel
    , claimModalStatus : Claim.ModalStatus
    , reloadOnNextQuery : Bool
    , selectedTab : Tab
    , showFilterModal : Bool
    , filters : Filter
    , filterProfileSummary : Profile.Summary.Model
    , filtersForm : Form.Model FiltersFormInput
    , direction : FilterDirection
    , tabCounts : TabCounts
    }


initModel : Model
initModel =
    { status = RemoteData.Loading
    , claimModalStatus = Claim.Closed
    , reloadOnNextQuery = False
    , selectedTab = WaitingToVote
    , showFilterModal = False
    , filters = initFilter
    , filterProfileSummary =
        Profile.Summary.init False
            |> Profile.Summary.withPreventScrolling View.Components.PreventScrollAlways
    , filtersForm =
        Form.init
            { user = Form.UserPicker.initSingle { id = "filters-user-picker" }
            , status = All
            }
    , direction = ASC
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


statusFilterFromString : String -> Maybe StatusFilter
statusFilterFromString filter =
    case filter of
        "all" ->
            Just All

        "approved" ->
            Just Approved

        "rejected" ->
            Just Rejected

        _ ->
            Nothing



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
            Claim.viewClaimCard [ class "w-full self-start" ] loggedIn profileSummary claim
                |> Html.map (ClaimMsg claimIndex)
    in
    [ div [ class "container mx-auto px-4 mb-10" ]
        (if List.length claims > 0 then
            [ View.Components.masonryLayout [ View.Components.Sm ]
                { transitionWithParent = False }
                [ class "grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-x-4 mb-4 sm:auto-rows-[5px]" ]
                (List.map3 viewClaim
                    profileSummaries
                    (List.range 0 (List.length claims))
                    claims
                )
            , viewPagination loggedIn pageInfo
            ]

         else
            [ viewEmptyResults loggedIn ]
        )
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


type alias FiltersFormInput =
    { user : Form.UserPicker.SinglePickerModel
    , status : StatusFilter
    }


filtersForm : LoggedIn.Model -> List Profile.Minimal -> { showStatusSelect : Bool } -> Form.Form msg FiltersFormInput Filter
filtersForm loggedIn users { showStatusSelect } =
    let
        { t } =
            loggedIn.shared.translators
    in
    Form.succeed Filter
        |> Form.with
            (Form.UserPicker.init
                { label = t "all_analysis.filter.user"
                , currentUser = loggedIn.accountName
                , profiles = users
                }
                |> Form.UserPicker.withModalSelectors True
                |> Form.UserPicker.withMenuClass "max-h-44 overflow-y-auto !relative"
                |> Form.userPicker
                    { parser = Ok
                    , value = .user
                    , update = \user input -> { input | user = user }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (if not showStatusSelect then
                Form.succeed All

             else
                Form.Select.init
                    { label = t "all_analysis.filter.status.label"
                    , id = "status-select"
                    , optionToString = statusFilterToString
                    }
                    |> Form.Select.withOption All (t "all_analysis.all")
                    |> Form.Select.withOption Approved (t "all_analysis.approved")
                    |> Form.Select.withOption Rejected (t "all_analysis.disapproved")
                    |> Form.Select.withContainerAttrs [ class "mb-10" ]
                    |> Form.select (statusFilterFromString >> Maybe.withDefault All)
                        { parser = Ok
                        , value = .status
                        , update = \status input -> { input | status = status }
                        , externalError = always Nothing
                        }
            )


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

        profiles =
            loggedIn.selectedCommunity
                |> RemoteData.map .members
                |> RemoteData.withDefault []
    in
    Modal.initWith
        { closeMsg = ClosedFilterModal
        , isVisible = model.showFilterModal
        }
        |> Modal.withHeader (t "all_analysis.filter.title")
        |> Modal.withBody
            [ Form.view []
                shared.translators
                (\submitButton ->
                    [ submitButton [ class "button button-primary w-full" ]
                        [ text <| t "all_analysis.filter.apply" ]
                    ]
                )
                (filtersForm loggedIn profiles { showStatusSelect = showFilterSelect })
                model.filtersForm
                { toMsg = GotFilterFormMsg
                , onSubmit = SubmittedFilterForm
                }
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
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


type Msg
    = ClaimsLoaded Tab (RemoteData (Graphql.Http.Error (Maybe Claim.Paginated)) (Maybe Claim.Paginated))
    | ClosedAuthModal
    | ClaimMsg Int Claim.Msg
    | VoteClaim Claim.ClaimId Bool
    | GotVoteResult Claim.ClaimId (Result (Maybe Value) String)
    | OpenedFilterModal
    | ClosedFilterModal
    | GotFilterFormMsg (Form.Msg FiltersFormInput)
    | SubmittedFilterForm Filter
    | SelectedTab Tab
    | ShowMore
    | ToggleSorting
    | ClearFilters


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
            case ( model.status, loggedIn.selectedCommunity ) of
                ( RemoteData.Success _, RemoteData.Success community ) ->
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
                                      , data = Claim.encodeVerification claimId loggedIn.accountName vote community.symbol
                                      }
                                    ]
                            }
                        |> LoggedIn.withPrivateKey loggedIn
                            [ Permission.Verify ]
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
                                        loggedIn.shared.translators
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
                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure errorMessage)

                _ ->
                    model |> UR.init

        OpenedFilterModal ->
            { model | showFilterModal = True }
                |> UR.init

        ClosedFilterModal ->
            { model | showFilterModal = False }
                |> UR.init

        GotFilterFormMsg subMsg ->
            Form.update loggedIn.shared subMsg model.filtersForm
                |> UR.fromChild (\newForm -> { model | filtersForm = newForm })
                    GotFilterFormMsg
                    LoggedIn.addFeedback
                    model

        SubmittedFilterForm filters ->
            if filters == model.filters then
                { model | showFilterModal = False }
                    |> UR.init

            else
                let
                    newModel =
                        { model
                            | showFilterModal = False
                            , filters = filters
                            , reloadOnNextQuery = True
                            , status = RemoteData.Loading
                        }
                in
                newModel
                    |> UR.init
                    |> UR.addExt
                        (fetchAnalysis loggedIn
                            newModel
                            Nothing
                            model.selectedTab
                        )

        SelectedTab tab ->
            let
                newModel =
                    { model
                        | selectedTab = tab
                        , filters = initFilter
                        , status = RemoteData.Loading
                        , reloadOnNextQuery = True
                    }

                tabToString tab_ =
                    case tab_ of
                        WaitingToVote ->
                            "waiting to vote"

                        Analyzed ->
                            "analyzed"
            in
            newModel
                |> UR.init
                |> UR.addExt
                    (fetchAnalysis loggedIn
                        newModel
                        Nothing
                        tab
                    )
                |> UR.addBreadcrumb
                    { type_ = Log.InfoBreadcrumb
                    , category = msg
                    , message = "Selected tab"
                    , data = Dict.fromList [ ( "tab", tabToString tab |> Encode.string ) ]
                    , level = Log.Info
                    }

        ShowMore ->
            case model.status of
                RemoteData.Success { pageInfo } ->
                    let
                        cursor : Maybe String
                        cursor =
                            Maybe.andThen .endCursor pageInfo
                    in
                    model
                        |> UR.init
                        |> UR.addExt
                            (fetchAnalysis loggedIn
                                model
                                cursor
                                model.selectedTab
                            )
                        |> UR.addBreadcrumb
                            { type_ = Log.QueryBreadcrumb
                            , category = msg
                            , message = "Requested to show more items"
                            , data = Dict.empty
                            , level = Log.Info
                            }

                _ ->
                    UR.init model

        ClearFilters ->
            let
                newModel =
                    { model
                        | filters = initFilter
                        , direction = ASC
                        , reloadOnNextQuery = True
                        , status = RemoteData.Loading
                    }
            in
            newModel
                |> UR.init
                |> UR.addExt
                    (fetchAnalysis loggedIn
                        newModel
                        Nothing
                        model.selectedTab
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
            in
            newModel
                |> UR.init
                |> UR.addExt
                    (fetchAnalysis loggedIn
                        newModel
                        Nothing
                        model.selectedTab
                    )


fetchAnalysis : LoggedIn.Model -> Model -> Maybe String -> Tab -> LoggedIn.External Msg
fetchAnalysis loggedIn model maybeCursorAfter tab =
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

        queryFn =
            case tab of
                WaitingToVote ->
                    Cambiatus.Query.pendingClaims

                Analyzed ->
                    Cambiatus.Query.analyzedClaims

        query =
            queryFn optionals (Claim.claimPaginatedSelectionSet loggedIn.shared.now)
    in
    LoggedIn.query loggedIn query (ClaimsLoaded tab)


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

        GotFilterFormMsg subMsg ->
            "GotFilterFormMsg" :: Form.msgToString subMsg

        SubmittedFilterForm _ ->
            [ "SubmittedFilterForm" ]

        SelectedTab _ ->
            [ "SelectedTab" ]

        ShowMore ->
            [ "ShowMore" ]

        ClearFilters ->
            [ "ClearFilters" ]

        ToggleSorting ->
            [ "ToggleSorting" ]
