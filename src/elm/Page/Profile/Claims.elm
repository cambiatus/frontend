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

import Cambiatus.Enum.Permission as Permission
import Cambiatus.Object
import Cambiatus.Object.User as Profile
import Cambiatus.Query
import Claim
import Community
import Dict
import Eos
import Eos.Account as Eos
import Eos.EosError as EosError
import Form.Select
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Html exposing (Html, a, button, div, img, li, text, ul)
import Html.Attributes exposing (class, classList, src)
import Html.Events exposing (onClick)
import Icons
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import List.Extra as List
import Log
import Page
import RemoteData exposing (RemoteData)
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import Time
import UpdateResult as UR
import View.Components
import View.Feedback as Feedback
import View.Modal as Modal
import View.TabSelector


init : LoggedIn.Model -> String -> ( Model, Cmd Msg )
init loggedIn account =
    ( initModel account
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )


type alias Model =
    { status : Status
    , accountString : String
    , claimModalStatus : Claim.ModalStatus
    , selectedTab : Tab
    , orderDirection : OrderDirection
    , showFiltersModal : Bool
    , currentStatusFilter : StatusFilter
    , editingStatusFilter : StatusFilter
    }


initModel : String -> Model
initModel account =
    { status = Loading
    , accountString = account
    , claimModalStatus = Claim.Closed
    , selectedTab = WaitingVote
    , orderDirection = Asc
    , showFiltersModal = False
    , currentStatusFilter = All
    , editingStatusFilter = All
    }


type Status
    = Loading
    | Loaded (List Claim.ClaimProfileSummaries) ProfileClaims
    | NotFound
    | Failed (Graphql.Http.Error (Maybe ProfileClaims))


type Tab
    = WaitingVote
    | Analyzed


type OrderDirection
    = Asc
    | Desc


type StatusFilter
    = All
    | Approved
    | Disapproved
    | Completed



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        { t } =
            loggedIn.shared.translators

        pageTitle =
            t "profile.claims.title"

        maybeClaims =
            case model.status of
                Loaded _ profileClaims ->
                    Just profileClaims

                _ ->
                    Nothing

        content =
            case model.status of
                Loading ->
                    [ Page.fullPageLoading loggedIn.shared ]

                Loaded profileSummaries profileClaims ->
                    [ viewResults loggedIn profileSummaries profileClaims model
                    , viewClaimVoteModal loggedIn model
                    ]

                NotFound ->
                    [ Page.fullPageNotFound
                        (t "profile.claims.not_found.title")
                        (t "profile.claims.not_found.subtitle")
                    ]

                Failed e ->
                    [ Page.fullPageGraphQLError pageTitle e ]
    in
    { title = pageTitle
    , content =
        div []
            (Page.viewHeader loggedIn pageTitle
                :: viewFiltersModal loggedIn.shared model
                :: viewHeaderAndOptions loggedIn maybeClaims model
                ++ content
            )
    }


viewHeaderAndOptions : LoggedIn.Model -> Maybe (List Claim.Model) -> Model -> List (Html Msg)
viewHeaderAndOptions ({ shared } as loggedIn) maybeClaims model =
    [ div
        [ class "bg-white pt-5 md:pt-6 pb-6" ]
        [ div [ class "container mx-auto px-4 flex flex-col items-center" ]
            [ if not loggedIn.hasAcceptedCodeOfConduct then
                LoggedIn.viewFrozenAccountCard shared.translators
                    { onClick = ClickedAcceptCodeOfConduct
                    , isHorizontal = True
                    }
                    [ class "shadow-lg" ]

              else
                viewGoodPracticesCard shared
            , viewTabSelector shared maybeClaims model
            ]
        ]
    , div [ class "container mx-auto px-4" ]
        [ viewFilterAndOrder shared model ]
    ]


viewGoodPracticesCard : Shared -> Html msg
viewGoodPracticesCard { translators, language } =
    let
        text_ =
            translators.t >> text
    in
    div [ class "rounded shadow-lg w-full md:w-3/4 lg:w-2/3 bg-white" ]
        [ div [ class "flex items-center bg-yellow text-black font-semibold p-2 rounded-t" ]
            [ Icons.lamp "mr-2", text_ "profile.claims.good_practices.title" ]
        , ul [ class "list-disc p-4 pl-8 pb-4 md:pb-11 space-y-4" ]
            [ li [ class "pl-1" ] [ text_ "profile.claims.good_practices.once_a_day" ]
            , li [ class "pl-1" ] [ text_ "profile.claims.good_practices.completed_action" ]
            , li [ class "pl-1" ]
                [ text_ "profile.claims.good_practices.know_the"
                , text " "
                , a
                    [ class "text-orange-300 hover:underline focus-ring rounded-sm"
                    , Html.Attributes.href (LoggedIn.codeOfConductUrl language)
                    ]
                    [ text_ "profile.claims.good_practices.good_practices" ]
                ]
            ]
        ]


viewTabSelector : Shared -> Maybe (List Claim.Model) -> Model -> Html Msg
viewTabSelector ({ translators } as shared) maybeClaims model =
    let
        tabCount : Tab -> Maybe Int
        tabCount tab =
            maybeClaims
                |> Maybe.map (\claims -> claimsToShow shared claims All tab |> List.length)
    in
    View.TabSelector.init
        { tabs =
            [ { tab = WaitingVote, label = translators.t "all_analysis.tabs.waiting_vote", count = tabCount WaitingVote }
            , { tab = Analyzed, label = translators.t "all_analysis.tabs.analyzed", count = tabCount Analyzed }
            ]
        , selectedTab = model.selectedTab
        , onSelectTab = SelectedTab
        }
        |> View.TabSelector.withContainerAttrs [ class "w-full md:w-2/3 xl:w-2/5 mt-6 lg:mt-8" ]
        |> View.TabSelector.toHtml


viewFilterAndOrder : Shared -> Model -> Html Msg
viewFilterAndOrder shared model =
    let
        ( filterDirectionIcon, filterDirectionLabel ) =
            case model.orderDirection of
                Asc ->
                    ( Icons.sortAscending, "all_analysis.filter.sort.asc" )

                Desc ->
                    ( Icons.sortDescending, "all_analysis.filter.sort.desc" )

        viewButton label icon onClickMsg =
            button
                [ class "button button-secondary flex-grow-1 justify-between pl-4"
                , onClick onClickMsg
                ]
                [ text (shared.translators.t label)
                , icon
                ]

        showFilters =
            case model.selectedTab of
                WaitingVote ->
                    False

                Analyzed ->
                    True
    in
    div
        [ class "w-full md:w-2/3 xl:w-1/3 mx-auto mt-4 mb-6 flex space-x-4"
        , classList [ ( "sm:w-1/2 md:w-2/6 xl:w-1/6", not showFilters ) ]
        ]
        [ if showFilters then
            viewButton "all_analysis.filter.title" (Icons.arrowDown "fill-current") OpenedFilterModal

          else
            text ""
        , viewButton filterDirectionLabel (filterDirectionIcon "mr-2") ToggledSorting
        ]


viewFiltersModal : Shared -> Model -> Html Msg
viewFiltersModal shared model =
    let
        { t } =
            shared.translators

        statusFilterToString statusFilter =
            case statusFilter of
                All ->
                    "all"

                Approved ->
                    "approved"

                Disapproved ->
                    "disapproved"

                Completed ->
                    "completed"
    in
    Modal.initWith
        { closeMsg = ClosedFilterModal
        , isVisible = model.showFiltersModal
        }
        |> Modal.withHeader (t "all_analysis.filter.title")
        |> Modal.withBody
            [ Form.Select.init
                { label = t "all_analysis.filter.status.label"
                , id = "status-filter-select"
                , optionToString = statusFilterToString
                }
                |> Form.Select.withOption All (t "all_analysis.all")
                |> Form.Select.withOption Approved (t "all_analysis.approved")
                |> Form.Select.withOption Disapproved (t "all_analysis.disapproved")
                |> Form.Select.withOption Completed (t "community.actions.completed")
                |> Form.Select.withContainerAttrs [ class "mb-10" ]
                |> (\options ->
                        Form.Select.view options
                            { onSelect = SelectedStatusFilter
                            , onBlur = NoOp
                            , value = model.editingStatusFilter
                            , error = text ""
                            , hasError = False
                            , isRequired = False
                            }
                   )
            , button
                [ class "button button-primary w-full"
                , onClick ClickedApplyFilters
                ]
                [ text (t "all_analysis.filter.apply") ]
            ]
        |> Modal.toHtml


claimsToShow : Shared -> List Claim.Model -> StatusFilter -> Tab -> List Claim.Model
claimsToShow shared claims statusFilter tab =
    case tab of
        WaitingVote ->
            List.filter (Claim.isOpenForVotes shared.now) claims

        Analyzed ->
            let
                satisfiesFilter claim =
                    case statusFilter of
                        All ->
                            True

                        Approved ->
                            claim.status == Claim.Approved

                        Disapproved ->
                            claim.status == Claim.Rejected

                        Completed ->
                            claim.status == Claim.Pending
            in
            List.filter
                (\claim ->
                    not (Claim.isOpenForVotes shared.now claim)
                        && satisfiesFilter claim
                )
                claims


viewResults : LoggedIn.Model -> List Claim.ClaimProfileSummaries -> List Claim.Model -> Model -> Html Msg
viewResults loggedIn profileSummaries claims model =
    let
        viewClaim profileSummary claimIndex claim =
            ( String.fromInt claim.id
            , Claim.viewClaimCard [ class "w-full self-start" ] loggedIn profileSummary claim
                |> Html.map (ClaimMsg claimIndex)
            )

        orderFunction =
            case model.orderDirection of
                Desc ->
                    List.reverse

                Asc ->
                    identity

        claimsList =
            claimsToShow loggedIn.shared claims model.currentStatusFilter model.selectedTab
    in
    div [ class "container mx-auto px-4 mb-10" ]
        [ if List.length claimsList > 0 then
            View.Components.keyedMasonryLayout [ View.Components.Sm ]
                { transitionWithParent = False }
                [ class "grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-x-4 mb-4 sm:auto-rows-[5px]" ]
                (claimsList
                    |> orderFunction
                    |> List.map3 viewClaim
                        profileSummaries
                        (List.range 0 (List.length profileSummaries))
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
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


type Msg
    = NoOp
    | ClaimsLoaded (RemoteData (Graphql.Http.Error (Maybe ProfileClaims)) (Maybe ProfileClaims))
    | ClosedAuthModal
    | CompletedLoadCommunity Community.Model
    | ClaimMsg Int Claim.Msg
    | VoteClaim Claim.ClaimId Bool
    | GotVoteResult Claim.ClaimId (Result (Maybe Value) String)
    | SelectedTab Tab
    | OpenedFilterModal
    | ClosedFilterModal
    | ToggledSorting
    | SelectedStatusFilter StatusFilter
    | ClickedApplyFilters
    | ClickedAcceptCodeOfConduct


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        NoOp ->
            UR.init model

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
                |> UR.addExt
                    (profileClaimQuery loggedIn
                        model.accountString
                        community.symbol
                    )

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
            case ( model.status, loggedIn.selectedCommunity ) of
                ( Loaded _ _, RemoteData.Success community ) ->
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
                                                community.symbol
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
                Loaded _ profileClaims ->
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
                                symbol =
                                    claim.action.objective.community.symbol

                                value =
                                    Eos.assetToString loggedIn.shared.translators
                                        { amount = claim.action.verifierReward
                                        , symbol = symbol
                                        }
                            in
                            { model
                                | status = Loading
                                , claimModalStatus = Claim.Closed
                            }
                                |> UR.init
                                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Success (message value))
                                |> UR.addExt
                                    (profileClaimQuery loggedIn
                                        model.accountString
                                        symbol
                                    )

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
                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure errorMessage)

                _ ->
                    model |> UR.init

        SelectedTab tab ->
            let
                tabToString tab_ =
                    case tab_ of
                        WaitingVote ->
                            "waiting vote"

                        Analyzed ->
                            "analyzed"
            in
            { model | selectedTab = tab }
                |> UR.init
                |> UR.addBreadcrumb
                    { type_ = Log.InfoBreadcrumb
                    , category = msg
                    , message = "Selected claims tab"
                    , data = Dict.fromList [ ( "tab", tabToString tab |> Encode.string ) ]
                    , level = Log.Info
                    }

        OpenedFilterModal ->
            { model | showFiltersModal = True }
                |> UR.init

        ClosedFilterModal ->
            { model | showFiltersModal = False }
                |> UR.init

        ToggledSorting ->
            let
                newDirection =
                    case model.orderDirection of
                        Asc ->
                            Desc

                        Desc ->
                            Asc

                directionToString direction =
                    case direction of
                        Asc ->
                            "ASC"

                        Desc ->
                            "DESC"
            in
            { model | orderDirection = newDirection }
                |> UR.init
                |> UR.addBreadcrumb
                    { type_ = Log.QueryBreadcrumb
                    , category = msg
                    , message = "Toggled sorting"
                    , data = Dict.fromList [ ( "newSorting", directionToString newDirection |> Encode.string ) ]
                    , level = Log.Info
                    }

        SelectedStatusFilter statusFilter ->
            { model | editingStatusFilter = statusFilter }
                |> UR.init

        ClickedApplyFilters ->
            { model
                | currentStatusFilter = model.editingStatusFilter
                , showFiltersModal = False
            }
                |> UR.init

        ClickedAcceptCodeOfConduct ->
            model
                |> UR.init
                |> UR.addExt LoggedIn.ShowCodeOfConductModal


profileClaimQuery : LoggedIn.Model -> String -> Eos.Symbol -> LoggedIn.External Msg
profileClaimQuery loggedIn accountName symbol =
    LoggedIn.query loggedIn
        (Cambiatus.Query.user { account = accountName } (selectionSet loggedIn.shared.now symbol))
        ClaimsLoaded


selectionSet : Time.Posix -> Eos.Symbol -> SelectionSet ProfileClaims Cambiatus.Object.User
selectionSet now communityId =
    Profile.claims
        (\_ -> { communityId = Present (Eos.symbolToString communityId) })
        (Claim.selectionSet now)


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
        NoOp ->
            [ "NoOp" ]

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

        SelectedTab _ ->
            [ "SelectedTab" ]

        OpenedFilterModal ->
            [ "OpenedFilterModal" ]

        ClosedFilterModal ->
            [ "ClosedFilterModal" ]

        ToggledSorting ->
            [ "ToggledSorting" ]

        SelectedStatusFilter _ ->
            [ "SelectedStatusFilter" ]

        ClickedApplyFilters ->
            [ "ClickedApplyFilters" ]

        ClickedAcceptCodeOfConduct ->
            [ "ClickedAcceptCodeOfConduct" ]
