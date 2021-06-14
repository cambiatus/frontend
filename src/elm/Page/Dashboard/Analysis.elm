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
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Html exposing (Html, button, div, img, li, span, text, ul)
import Html.Attributes exposing (class, classList, src, value)
import Html.Events exposing (onClick)
import Icons
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import List.Extra as List
import Page
import Profile
import Profile.Summary
import RemoteData exposing (RemoteData)
import Route
import Select
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import Simple.Fuzzy
import UpdateResult as UR
import View.Components
import View.Feedback as Feedback
import View.Form.Select as Select
import View.Modal as Modal


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
    , filterProfileSummary : Profile.Summary.Model
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
    , filterProfileSummary =
        Profile.Summary.init False
            |> Profile.Summary.withPreventScrolling View.Components.PreventScrollAlways
    }


type alias LoadedModel =
    { claims : List Claim.Model
    , profileSummaries : List Profile.Summary.Model
    , pageInfo : Maybe Api.Relay.PageInfo
    , tabCounts : List ( Tab, Int )
    }


type Tab
    = WaitingToVote
    | Analyzed


allTabs : List Tab
allTabs =
    [ WaitingToVote, Analyzed ]


type alias Filter =
    { profile : Maybe Profile.Minimal
    , statusFilter : StatusFilter
    , direction : FilterDirection
    }


type FilterDirection
    = ASC
    | DESC


initFilter : Filter
initFilter =
    { profile = Nothing, statusFilter = All, direction = DESC }


type StatusFilter
    = All
    | Approved
    | Rejected
    | Pending


statusFilterToString : StatusFilter -> String
statusFilterToString filter =
    case filter of
        All ->
            "all"

        Approved ->
            "approved"

        Rejected ->
            "rejected"

        Pending ->
            "pending"



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
                RemoteData.NotAsked ->
                    [ Page.fullPageLoading shared ]

                RemoteData.Loading ->
                    [ Page.fullPageLoading shared ]

                RemoteData.Success loadedModel ->
                    viewContent loggedIn loadedModel model

                RemoteData.Failure err ->
                    -- TODO - Check this
                    [ div [ class "text-center container mx-auto" ]
                        [ Page.fullPageGraphQLError (t "all_analysis.error") err
                        ]
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
    [ div [ class "bg-white pt-4 md:pt-6 pb-6" ]
        [ div [ class "container mx-auto px-4 flex flex-col items-center" ]
            [ viewGoodPracticesCard
            , viewTabSelector model
            ]
        ]
    , div [ class "container mx-auto px-4" ]
        [ viewFilters loggedIn model ]
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


viewGoodPracticesCard : Html msg
viewGoodPracticesCard =
    div [ class "rounded shadow-lg w-full md:w-3/4 lg:w-2/3 bg-white" ]
        [ div [ class "flex items-center bg-yellow text-black font-medium p-2 rounded-t" ]
            [ Icons.lamp "mr-2", text "Lembre-se" ]
        , ul [ class "list-disc p-4 pl-8 pb-4 md:pb-11 space-y-4" ]
            -- TODO - I18N
            [ li [ class "pl-1" ] [ text "Não reivindique a mesma ação mais de uma vez no mesmo dia;" ]
            , li [ class "pl-1" ] [ text "Reivindique apenas ações que você realmente realizou;" ]
            , li [ class "pl-1" ] [ text "Conheça as boas práticas da Comunidade" ]
            ]
        ]


viewTabSelector : Model -> Html Msg
viewTabSelector model =
    let
        viewTab isLeft isRight tab =
            let
                isActive =
                    tab == model.selectedTab

                count =
                    case model.status of
                        RemoteData.Success loadedModel ->
                            List.filterMap
                                (\( tab_, tabCount ) ->
                                    if tab_ == tab then
                                        Just (" (" ++ String.fromInt tabCount ++ ")")

                                    else
                                        Nothing
                                )
                                loadedModel.tabCounts
                                |> List.head
                                |> Maybe.withDefault ""

                        _ ->
                            ""

                label =
                    case tab of
                        WaitingToVote ->
                            -- TODO - I18N
                            "Waiting vote"

                        Analyzed ->
                            -- TODO - I18N
                            "Analyzed"
            in
            button
                [ class "text-center py-3 px-8 w-1/2 focus:outline-none"
                , classList
                    [ ( "bg-orange-300 text-white cursor-default", isActive )
                    , ( "bg-gray-100 text-black hover:bg-gray-200", not isActive )
                    , ( "rounded-l-full", isLeft )
                    , ( "rounded-r-full", isRight )
                    ]
                , onClick (SelectedTab tab)
                ]
                [ text (label ++ count) ]
    in
    div [ class "mt-6 md:mt-8 w-full md:w-2/3 xl:w-1/3 flex" ]
        (List.indexedMap
            (\idx -> viewTab (idx == 0) (idx == List.length allTabs - 1))
            allTabs
        )


viewFilters : LoggedIn.Model -> Model -> Html Msg
viewFilters { shared } model =
    let
        filterDirectionIcon =
            case model.filters.direction of
                ASC ->
                    Icons.sortAscending

                DESC ->
                    Icons.sortDescending

        viewFilterButton label icon onClickMsg =
            button
                [ class "button button-secondary flex-grow-1 justify-between pl-4"
                , onClick onClickMsg
                ]
                [ text (shared.translators.t label)
                , icon
                ]
    in
    div [ class "w-full md:w-2/3 xl:w-1/3 mx-auto mt-4 mb-6 flex space-x-4" ]
        [ viewFilterButton "Filters" (Icons.arrowDown "fill-current") OpenedFilterModal -- TODO - Fix label
        , viewFilterButton "Sort by" (filterDirectionIcon "mr-2") ToggleSorting -- TODO - Fix label
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
    in
    Modal.initWith
        { closeMsg = ClosedFilterModal
        , isVisible = model.showFilterModal
        }
        |> Modal.withHeader "Filters"
        |> Modal.withHeaderAttrs [ class "px-5" ]
        |> Modal.withCloseButtonAttrs [ class "mx-5" ]
        -- TODO - I18N
        |> Modal.withBody
            [ div
                -- overflow-y-hidden makes it so overflow-x is interpreted as
                -- auto, so we need a minimal amount of padding for the focus
                -- ring not to be cropped
                [ class "px-1"
                , classList [ ( "overflow-y-hidden", not showFilterSelect ) ]
                ]
                (span [ class "input-label" ]
                    [ text (t "all_analysis.filter.user") ]
                    :: (case loggedIn.selectedCommunity of
                            RemoteData.Success community ->
                                let
                                    selectedUsers =
                                        Maybe.map (\v -> [ v ]) model.filters.profile
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
                    |> Select.withContainerAttrs [ class "mt-6" ]
                    |> Select.toHtml
            , div [ class "px-1" ]
                [ button
                    [ class "button button-primary w-full"
                    , onClick ClickedApplyFilters
                    ]
                    [ text "Apply" ]
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
        ClaimsLoaded (RemoteData.Success results) ->
            let
                initProfileSummaries claims =
                    List.length claims
                        |> Profile.Summary.initMany False
            in
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
                                , tabCounts = [ ( WaitingToVote, 2 ), ( Analyzed, 15 ) ] -- TODO
                                }
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
                                , tabCounts = [ ( WaitingToVote, 2 ), ( Analyzed, 15 ) ] -- TODO
                                }
                        , reloadOnNextQuery = False
                    }
                        |> UR.init

        ClaimsLoaded (RemoteData.Failure err) ->
            { model | status = RemoteData.Failure err } |> UR.init

        ClaimsLoaded _ ->
            UR.init model

        CompletedLoadCommunity community ->
            UR.init model
                |> UR.addCmd (fetchAnalysis loggedIn model.filters Nothing community)
                |> UR.addExt (LoggedIn.ReloadResource LoggedIn.TimeResource)

        ClaimMsg claimIndex m ->
            let
                claimCmd =
                    case m of
                        Claim.RouteOpened r ->
                            Route.replaceUrl loggedIn.shared.navKey r

                        _ ->
                            Cmd.none

                updatedModel =
                    case ( model.status, m ) of
                        ( RemoteData.Success ({ profileSummaries } as loadedModel), Claim.GotProfileSummaryMsg subMsg ) ->
                            { model
                                | status =
                                    RemoteData.Success
                                        { loadedModel
                                            | profileSummaries =
                                                List.updateAt claimIndex (Profile.Summary.update subMsg) profileSummaries
                                        }
                            }

                        _ ->
                            model
            in
            updatedModel
                |> Claim.updateClaimModalStatus m
                |> UR.init
                |> UR.addCmd claimCmd

        VoteClaim claimId vote ->
            case model.status of
                RemoteData.Success _ ->
                    let
                        newModel =
                            { model
                                | claimModalStatus = Claim.Loading claimId vote
                            }
                    in
                    if LoggedIn.hasPrivateKey loggedIn then
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
                                value =
                                    String.fromFloat claim.action.verifierReward
                                        ++ " "
                                        ++ Eos.symbolToSymbolCodeString claim.action.objective.community.symbol

                                updatedClaims =
                                    List.updateIf (\c -> c.id == claim.id)
                                        (\_ -> claim)
                                        claims
                            in
                            { model | status = RemoteData.Success { loadedModel | claims = updatedClaims } }
                                |> UR.init
                                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Success (message value))
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
                RemoteData.Success loadedModel ->
                    { model
                        | status = RemoteData.Success loadedModel
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
            -- TODO
            { model | showFilterModal = False }
                |> UR.init

        SelectMsg subMsg ->
            let
                ( updated, cmd ) =
                    Select.update (selectConfiguration loggedIn.shared False) subMsg model.autoCompleteState
            in
            UR.init { model | autoCompleteState = updated }
                |> UR.addCmd cmd

        SelectedTab tab ->
            -- TODO - Query again
            { model | selectedTab = tab }
                |> UR.init

        OnSelectVerifier maybeProfile ->
            let
                oldFilters =
                    model.filters
            in
            { model | filters = { oldFilters | profile = maybeProfile } }
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
                        |> UR.addCmd (fetchAnalysis loggedIn model.filters cursor community)

                _ ->
                    UR.init model

        ClearSelectSelection ->
            case ( model.status, loggedIn.selectedCommunity ) of
                ( RemoteData.Success _, RemoteData.Success community ) ->
                    let
                        oldFilters =
                            model.filters

                        newModel =
                            { model
                                | status = RemoteData.Loading
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
                        , status = RemoteData.Loading
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
                , status = RemoteData.Loading
            }
                |> UR.init
                |> UR.addCmd
                    (case loggedIn.selectedCommunity of
                        RemoteData.Success community ->
                            fetchAnalysis loggedIn initFilter Nothing community

                        _ ->
                            Cmd.none
                    )

        ToggleSorting ->
            let
                oldFilters =
                    model.filters

                sortDirection =
                    if model.filters.direction == ASC then
                        DESC

                    else
                        ASC

                newModel =
                    { model
                        | filters = { oldFilters | direction = sortDirection }
                        , reloadOnNextQuery = True
                        , status = RemoteData.Loading
                    }

                fetchCmd =
                    case loggedIn.selectedCommunity of
                        RemoteData.Success community ->
                            fetchAnalysis loggedIn newModel.filters Nothing community

                        _ ->
                            Cmd.none
            in
            newModel
                |> UR.init
                |> UR.addCmd fetchCmd

        GotProfileSummaryMsg subMsg ->
            { model | filterProfileSummary = Profile.Summary.update subMsg model.filterProfileSummary }
                |> UR.init


fetchAnalysis : LoggedIn.Model -> Filter -> Maybe String -> Community.Model -> Cmd Msg
fetchAnalysis { shared, authToken } { profile, statusFilter, direction } maybeCursorAfter community =
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
            , direction =
                case direction of
                    ASC ->
                        Present Cambiatus.Enum.Direction.Asc

                    DESC ->
                        Present Cambiatus.Enum.Direction.Desc
            }

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
                    , filter = Present filterRecord
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
                            -- We need `absolute` so we can display the dialog
                            -- bubble on hover
                            [ div [ class "absolute" ]
                                [ Profile.Summary.view shared loggedIn.accountName p profileSummary
                                    |> Html.map GotProfileSummaryMsg
                                ]

                            -- We need this invisible Profile.Summary so we know
                            -- the exact space the above one would take
                            , div [ class "opacity-0 pointer-events-none" ]
                                [ Profile.Summary.view shared loggedIn.accountName p profileSummary
                                    |> Html.map GotProfileSummaryMsg
                                ]
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
        ClaimsLoaded r ->
            [ "ClaimsLoaded", UR.remoteDataToString r ]

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
