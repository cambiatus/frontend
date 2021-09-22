module Page.Dashboard exposing
    ( Model
    , Msg(..)
    , init
    , jsAddressToMsg
    , msgToString
    , receiveBroadcast
    , update
    , view
    )

import Api
import Api.Graphql
import Api.Relay
import Browser.Dom
import Cambiatus.Enum.Direction
import Cambiatus.Enum.TransferDirectionValue as TransferDirectionValue exposing (TransferDirectionValue)
import Cambiatus.InputObject
import Cambiatus.Query
import Cambiatus.Scalar
import Claim
import Community exposing (Balance)
import Date
import DatePicker
import Eos
import Eos.Account as Eos
import Eos.EosError as EosError
import Graphql.Http
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Html exposing (Html, a, button, div, h1, img, p, span, text)
import Html.Attributes exposing (class, classList, src, tabindex)
import Html.Events exposing (onClick)
import Http
import Icons
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import List.Extra as List
import Log
import Page
import Ports
import Profile
import Profile.Contact as Contact
import Profile.Summary
import RemoteData exposing (RemoteData)
import Route
import Select
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import Shop
import Simple.Fuzzy
import Task
import Time
import Transfer exposing (QueryTransfers, Transfer)
import UpdateResult as UR
import Url
import Utils
import View.Components
import View.Feedback as Feedback
import View.Form
import View.Form.Input as Input
import View.Form.Select as Select
import View.Modal as Modal



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    let
        model =
            initModel loggedIn.shared
    in
    ( model
    , Cmd.batch
        [ LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
        , LoggedIn.maybeInitWith (\_ -> CompletedLoadProfile) .profile loggedIn
        ]
    )



-- MODEL


type alias Model =
    { balance : RemoteData Http.Error (Maybe Balance)
    , analysis : GraphqlStatus (Maybe Claim.Paginated) (List ClaimStatus)
    , analysisFilter : Direction
    , profileSummaries : List Claim.ClaimProfileSummaries
    , lastSocket : String
    , transfers : GraphqlStatus (Maybe QueryTransfers) (List ( Transfer, Profile.Summary.Model ))
    , transfersFilters : TransfersFilters
    , transfersFiltersBeingEdited :
        { datePicker : DatePicker.DatePicker
        , otherAccountInput : String
        , otherAccountState : Select.State
        , otherAccountProfileSummary : Profile.Summary.Model
        , filters : TransfersFilters
        }
    , showTransferFiltersModal : Bool
    , contactModel : Contact.Model
    , showContactModal : Bool
    , inviteModalStatus : InviteModalStatus
    , claimModalStatus : Claim.ModalStatus
    , copied : Bool
    , isModalRequestingSponsorVisible : Bool
    }


initModel : Shared -> Model
initModel shared =
    { balance = RemoteData.NotAsked
    , analysis = LoadingGraphql Nothing
    , analysisFilter = initAnalysisFilter
    , profileSummaries = []
    , lastSocket = ""
    , transfers = LoadingGraphql Nothing
    , transfersFilters = initTransfersFilters
    , transfersFiltersBeingEdited =
        { datePicker = DatePicker.initFromDate (Date.fromPosix shared.timezone shared.now)
        , otherAccountInput = ""
        , otherAccountState = Select.newState "other-account-select"
        , otherAccountProfileSummary = Profile.Summary.init False
        , filters = initTransfersFilters
        }
    , showTransferFiltersModal = False
    , contactModel = Contact.initSingle
    , showContactModal = False
    , inviteModalStatus = InviteModalClosed
    , claimModalStatus = Claim.Closed
    , copied = False
    , isModalRequestingSponsorVisible = not shared.hasSeenSponsorModal
    }


initAnalysisFilter : Direction
initAnalysisFilter =
    DESC


initTransfersFilters : TransfersFilters
initTransfersFilters =
    { date = Nothing
    , direction = Nothing
    , otherAccount = Nothing
    }


type alias TransfersFilters =
    { date : Maybe Date.Date
    , direction : Maybe TransferDirectionValue
    , otherAccount : Maybe Profile.Minimal
    }


type GraphqlStatus err a
    = LoadingGraphql (Maybe a)
    | LoadedGraphql a (Maybe Api.Relay.PageInfo)
    | FailedGraphql (Graphql.Http.Error err)


type ClaimStatus
    = ClaimLoaded Claim.Model
    | ClaimLoading Claim.Model
    | ClaimVoted Claim.Model
    | ClaimVoteFailed Claim.Model


type InviteModalStatus
    = InviteModalClosed
    | InviteModalLoading
    | InviteModalFailed String
    | InviteModalLoaded String


type Direction
    = ASC
    | DESC



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view ({ shared, accountName } as loggedIn) model =
    let
        t =
            shared.translators.t

        isCommunityAdmin =
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    community.creator == accountName

                _ ->
                    False

        areObjectivesEnabled =
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    community.hasObjectives

                _ ->
                    False

        content =
            case ( model.balance, loggedIn.selectedCommunity ) of
                ( RemoteData.Loading, _ ) ->
                    Page.fullPageLoading shared

                ( RemoteData.NotAsked, _ ) ->
                    Page.fullPageLoading shared

                ( RemoteData.Failure e, _ ) ->
                    Page.fullPageError (t "dashboard.sorry") e

                ( RemoteData.Success (Just balance), RemoteData.Success community ) ->
                    div []
                        [ div [ class "container mx-auto px-4" ]
                            [ viewHeader loggedIn community isCommunityAdmin
                            , div
                                [ class "grid mb-10 md:grid-cols-2 md:gap-6" ]
                                [ div [ class "w-full" ]
                                    [ viewBalance loggedIn balance
                                    , div [ class "mt-6 flex space-x-6" ]
                                        [ viewMyClaimsCard loggedIn
                                        , viewMyOffersCard loggedIn
                                        ]
                                    ]
                                , viewTransfers loggedIn model False
                                ]
                            , if areObjectivesEnabled && List.any (\account -> account == loggedIn.accountName) community.validators then
                                viewAnalysisList loggedIn model

                              else
                                text ""
                            ]
                        , viewTransfers loggedIn model True
                        , viewInvitationModal loggedIn model
                        , addContactModal shared model
                        , viewModalRequestingSponsor shared community model
                        , viewTransferFilters loggedIn community.members model
                        ]

                ( RemoteData.Success _, _ ) ->
                    Page.fullPageNotFound (t "dashboard.sorry") ""
    in
    { title = t "menu.dashboard"
    , content = content
    }


viewHeader : LoggedIn.Model -> Community.Model -> Bool -> Html Msg
viewHeader loggedIn community isCommunityAdmin =
    div [ class "flex inline-block text-gray-900 font-light mt-6 mb-5 md:text-heading" ]
        [ div []
            [ text (loggedIn.shared.translators.t "menu.my_communities")
            , span [ class "text-indigo-500 font-medium" ]
                [ text community.name
                ]
            ]
        , if isCommunityAdmin then
            a
                [ Route.href Route.CommunitySettings
                , class "ml-auto"
                ]
                [ Icons.settings ]

          else
            text ""
        ]


viewNewTag : Shared -> Html msg
viewNewTag shared =
    p [ class "flex items-center bg-purple-100 text-white rounded-full py-0.5 px-2 text-caption uppercase" ]
        [ text <| shared.translators.t "contact_modal.new" ]


viewModalRequestingSponsor : Shared -> Community.Model -> Model -> Html Msg
viewModalRequestingSponsor shared community model =
    let
        text_ =
            shared.translators.t >> text
    in
    Modal.initWith
        { closeMsg = ClosedModalRequestingSponsor
        , isVisible = model.isModalRequestingSponsorVisible
        }
        |> Modal.withHeaderElement (viewNewTag shared)
        |> Modal.withBody
            [ div [ class "flex flex-col items-center h-full pb-6" ]
                [ h1 [ class "text-center text-heading font-bold" ]
                    [ text_ "sponsorship.dashboard_modal.title" ]
                , img [ class "mt-4", src "/images/sponsor-community.svg" ] []
                , div [ class "w-full mt-7 mx-auto space-y-6 md:w-5/6 lg:w-2/3" ]
                    [ p [ class "text-center" ]
                        [ text_ "sponsorship.dashboard_modal.subtitle" ]
                    , p [ class "text-center" ]
                        [ text_ "sponsorship.dashboard_modal.explanation" ]
                    , a
                        [ class "button button-primary w-full md:mt-8"
                        , Route.href Route.CommunitySponsor
                        ]
                        [ text (shared.translators.tr "sponsorship.dashboard_modal.sponsor" [ ( "community_name", community.name ) ]) ]
                    ]
                ]
            ]
        |> Modal.withSize Modal.Large
        |> Modal.toHtml


addContactModal : Shared -> Model -> Html Msg
addContactModal shared ({ contactModel } as model) =
    let
        text_ s =
            shared.translators.t s
                |> text

        form =
            Contact.view shared.translators contactModel
                |> Html.map GotContactMsg
    in
    Modal.initWith
        { closeMsg = ClosedAddContactModal
        , isVisible = model.showContactModal
        }
        |> Modal.withHeaderElement (viewNewTag shared)
        |> Modal.withBody
            [ p [ class "text-heading font-bold mt-2" ] [ text_ "contact_modal.title" ]
            , img [ class "mx-auto mt-10", src "/images/girl-with-phone.svg" ] []
            , form
            , p [ class "text-caption text-center uppercase my-4" ]
                [ text_ "contact_modal.footer" ]
            ]
        |> Modal.withSize Modal.FullScreen
        |> Modal.toHtml


viewInvitationModal : LoggedIn.Model -> Model -> Html Msg
viewInvitationModal { shared } model =
    let
        t =
            shared.translators.t

        text_ s =
            text (t s)

        protocol =
            case shared.url.protocol of
                Url.Http ->
                    "http://"

                Url.Https ->
                    "https://"

        url invitationId =
            let
                portStr =
                    case shared.url.port_ of
                        Just p ->
                            ":" ++ String.fromInt p

                        Nothing ->
                            ""
            in
            protocol ++ shared.url.host ++ portStr ++ "/invite/" ++ invitationId

        isInviteModalVisible =
            case model.inviteModalStatus of
                InviteModalClosed ->
                    False

                _ ->
                    True

        header =
            t "community.invite.title"

        body =
            case model.inviteModalStatus of
                InviteModalClosed ->
                    []

                InviteModalLoading ->
                    [ div [ class "spinner m-auto" ] [] ]

                InviteModalFailed err ->
                    [ p [ class "text-center text-red" ] [ text err ] ]

                InviteModalLoaded invitationId ->
                    [ div [ class "mt-3 input-label" ]
                        [ text_ "community.invite.label" ]
                    , p [ class "py-2 md:text-heading text-black" ]
                        [ text (url invitationId) ]
                    , Input.init
                        { label = ""
                        , id = "invitation-id"
                        , onInput = \_ -> NoOp
                        , disabled = False
                        , value = url invitationId
                        , placeholder = Nothing
                        , problems = Nothing
                        , translators = shared.translators
                        }
                        |> Input.withAttrs [ class "absolute opacity-0 left-[-9999em]" ]
                        |> Input.withContainerAttrs [ class "mb-0 overflow-hidden" ]
                        |> Input.toHtml
                    ]

        footer =
            case model.inviteModalStatus of
                InviteModalLoaded _ ->
                    [ button
                        [ classList
                            [ ( "button-primary", not model.copied )
                            , ( "button-success", model.copied )
                            ]
                        , class "button w-full md:w-48 select-all"
                        , onClick (CopyToClipboard "invitation-id")
                        ]
                        [ if model.copied then
                            text_ "community.invite.copied"

                          else
                            text_ "community.invite.copy"
                        ]
                    ]

                InviteModalFailed _ ->
                    [ button
                        [ class "button button-primary"
                        , onClick CloseInviteModal
                        ]
                        [ text_ "menu.close" ]
                    ]

                _ ->
                    []
    in
    Modal.initWith
        { closeMsg = CloseInviteModal
        , isVisible = isInviteModalVisible
        }
        |> Modal.withHeader header
        |> Modal.withBody body
        |> Modal.withFooter footer
        |> Modal.toHtml


viewAnalysisList : LoggedIn.Model -> Model -> Html Msg
viewAnalysisList loggedIn model =
    let
        text_ s =
            text <| loggedIn.shared.translators.t s

        isVoted : List ClaimStatus -> Bool
        isVoted claims =
            List.all
                (\c ->
                    case c of
                        ClaimVoted _ ->
                            True

                        _ ->
                            False
                )
                claims
    in
    case model.analysis of
        LoadingGraphql _ ->
            div [ class "md:mb-40 md:mt-10" ] [ Page.fullPageLoading loggedIn.shared ]

        LoadedGraphql claims _ ->
            div [ class "w-full flex mb-10 md:mb-40" ]
                [ div
                    [ class "w-full" ]
                    [ div [ class "flex justify-between text-gray-600 text-heading font-light flex mt-4 mb-4" ]
                        [ div [ class "flex flex-wrap mr-4" ]
                            [ div [ class "text-indigo-500 mr-2 font-medium" ]
                                [ text_ "dashboard.analysis.title.1"
                                ]
                            , text_ "dashboard.analysis.title.2"
                            ]
                        , div [ class "flex xs-max:flex-col xs-max:space-x-0 justify-between space-x-4" ]
                            [ button
                                [ class "w-full button button-secondary"
                                , onClick ToggleAnalysisSorting
                                ]
                                [ Icons.sortDirection ""
                                ]
                            , a
                                [ class "button button-secondary font-medium "
                                , Route.href Route.Analysis
                                ]
                                [ text_ "dashboard.analysis.all" ]
                            ]
                        ]
                    , if isVoted claims then
                        div [ class "flex flex-col w-full items-center justify-center px-3 py-12 my-2 rounded-lg bg-white" ]
                            [ img [ src "/images/not_found.svg", class "object-contain h-32 mb-3" ] []
                            , p [ class "flex text-body text-gray-600" ]
                                [ p [ class "font-bold" ] [ text_ "dashboard.analysis.empty.1" ]
                                , text_ "dashboard.analysis.empty.2"
                                ]
                            , p [ class "text-body text-gray-600" ] [ text_ "dashboard.analysis.empty.3" ]
                            ]

                      else
                        let
                            pendingClaims =
                                List.map3 (viewAnalysis loggedIn)
                                    model.profileSummaries
                                    (List.range 0 (List.length claims))
                                    claims
                        in
                        div [ class "flex flex-wrap -mx-2" ] <|
                            List.append pendingClaims
                                [ viewVoteConfirmationModal loggedIn model ]
                    ]
                ]

        FailedGraphql err ->
            div [ class "md:mb-40 md:mt-10" ] [ Page.fullPageGraphQLError "Failed load" err ]


viewVoteConfirmationModal : LoggedIn.Model -> Model -> Html Msg
viewVoteConfirmationModal loggedIn { claimModalStatus } =
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
    case claimModalStatus of
        Claim.VoteConfirmationModal claimId vote ->
            viewVoteModal claimId vote False

        Claim.Loading claimId vote ->
            viewVoteModal claimId vote True

        Claim.PhotoModal claim ->
            Claim.viewPhotoModal loggedIn claim
                |> Html.map (ClaimMsg 0)

        Claim.Closed ->
            text ""


viewAnalysis : LoggedIn.Model -> Claim.ClaimProfileSummaries -> Int -> ClaimStatus -> Html Msg
viewAnalysis loggedIn profileSummaries claimIndex claimStatus =
    case claimStatus of
        ClaimLoaded claim ->
            Claim.viewClaimCard loggedIn profileSummaries claim
                |> Html.map (ClaimMsg claimIndex)

        ClaimLoading _ ->
            div [ class "w-full md:w-1/2 lg:w-1/3 xl:w-1/4 px-2 mb-4" ]
                [ div [ class "rounded-lg bg-white h-56 my-2 pt-8" ]
                    [ Page.fullPageLoading loggedIn.shared ]
                ]

        ClaimVoted _ ->
            text ""

        ClaimVoteFailed claim ->
            Claim.viewClaimCard loggedIn profileSummaries claim
                |> Html.map (ClaimMsg claimIndex)


viewTransfers : LoggedIn.Model -> Model -> Bool -> Html Msg
viewTransfers loggedIn model isMobile =
    let
        t =
            loggedIn.shared.translators.t

        outerContainer children =
            if isMobile then
                div [ class "w-full bg-white md:hidden" ]
                    [ div [ class "container mx-auto" ]
                        children
                    ]

            else
                div [ class "w-full bg-white hidden md:flex md:flex-col md:rounded" ]
                    children
    in
    outerContainer
        [ div [ class "flex justify-between items-center p-4 pb-0" ]
            [ p [ class "text-heading" ]
                [ span [ class "text-gray-900 font-light" ] [ text <| t "transfer.transfers_latest" ]
                , text " "
                , span [ class "text-indigo-500 font-medium" ] [ text <| t "transfer.transfers" ]
                ]
            , button
                [ class "flex text-heading lowercase text-indigo-500 rounded ring-offset-2 focus:outline-none focus:ring"
                , onClick ClickedOpenTransferFilters
                ]
                [ text <| t "all_analysis.filter.title"
                , Icons.arrowDown "fill-current"
                ]
            ]
        , case model.transfers of
            LoadingGraphql Nothing ->
                Page.viewCardEmpty
                    [ div [ class "text-gray-900 text-sm" ]
                        [ text <| t "menu.loading" ]
                    ]

            FailedGraphql _ ->
                Page.viewCardEmpty
                    [ div [ class "text-gray-900 text-sm" ]
                        [ text (t "transfer.loading_error") ]
                    ]

            LoadedGraphql [] _ ->
                Page.viewCardEmpty
                    [ div [ class "text-gray-900 text-sm" ]
                        [ text (t "transfer.no_transfers_yet") ]
                    ]

            LoadingGraphql (Just transfers) ->
                viewTransferList loggedIn transfers Nothing { isLoading = True, isMobile = isMobile }

            LoadedGraphql transfers maybePageInfo ->
                viewTransferList loggedIn transfers maybePageInfo { isLoading = False, isMobile = isMobile }
        ]


viewTransferList :
    LoggedIn.Model
    -> List ( Transfer, Profile.Summary.Model )
    -> Maybe Api.Relay.PageInfo
    -> { isLoading : Bool, isMobile : Bool }
    -> Html Msg
viewTransferList loggedIn transfers maybePageInfo { isLoading, isMobile } =
    let
        addLoading transfers_ =
            if isLoading then
                transfers_
                    ++ [ View.Components.loadingLogoAnimated loggedIn.shared.translators "" ]

            else
                transfers_
    in
    View.Components.infiniteList
        { onRequestedItems =
            maybePageInfo
                |> Maybe.andThen
                    (\pageInfo ->
                        if pageInfo.hasNextPage then
                            Just RequestedMoreTransfers

                        else
                            Nothing
                    )
        , distanceToRequest = 1000
        , elementToTrack =
            if isMobile then
                View.Components.TrackWindow

            else
                View.Components.TrackSelf
        }
        [ class "pb-6 divide-y flex-grow w-full flex-basis-0 md:px-4" ]
        (transfers
            |> List.groupWhile
                (\( t1, _ ) ( t2, _ ) ->
                    Utils.areSameDay loggedIn.shared.timezone
                        (Utils.fromDateTime t1.blockTime)
                        (Utils.fromDateTime t2.blockTime)
                )
            |> List.map
                (\( ( t1, _ ) as first, rest ) ->
                    div []
                        [ div [ class "mt-4 mx-4" ]
                            [ View.Components.dateViewer
                                [ class "uppercase text-caption text-black tracking-wider" ]
                                identity
                                loggedIn.shared
                                (Utils.fromDateTime t1.blockTime)
                            ]
                        , div [ class "divide-y" ]
                            (List.map
                                (\( transfer, profileSummary ) ->
                                    Transfer.view loggedIn
                                        transfer
                                        profileSummary
                                        (GotTransferCardProfileSummaryMsg transfer.id)
                                        (ClickedTransferCard transfer.id)
                                        []
                                )
                                (first :: rest)
                            )
                        ]
                )
            |> addLoading
        )


datePickerSettings : Shared -> DatePicker.Settings
datePickerSettings shared =
    let
        defaultSettings =
            DatePicker.defaultSettings
    in
    { defaultSettings
        | changeYear = DatePicker.off
        , placeholder = shared.translators.t "payment_history.pick_date"
        , inputClassList = [ ( "input w-full", True ) ]
        , containerClassList = [ ( "relative-table w-full", True ) ]
        , dateFormatter = Date.format "E, d MMM y"
        , inputId = Just "transfers-filters-date-input"
    }


selectConfiguration : Shared -> Select.Config Msg Profile.Minimal
selectConfiguration shared =
    let
        toLabel =
            .account >> Eos.nameToString

        filter minChars query items =
            if String.length query < minChars then
                Nothing

            else
                items
                    |> Simple.Fuzzy.filter toLabel query
                    |> Just
    in
    Profile.selectConfig
        (Select.newConfig
            { onSelect = SelectedTransfersFiltersOtherAccount
            , toLabel = toLabel
            , filter = filter 2
            }
            |> Select.withMenuClass "max-h-44 overflow-y-auto !relative"
        )
        shared
        False


viewTransferFilters : LoggedIn.Model -> List Profile.Minimal -> Model -> Html Msg
viewTransferFilters ({ shared } as loggedIn) users model =
    let
        { t } =
            shared.translators

        directionText =
            case model.transfersFiltersBeingEdited.filters.direction of
                Nothing ->
                    "transfer.direction.other_user"

                Just TransferDirectionValue.Receiving ->
                    "transfer.direction.user_who_sent"

                Just TransferDirectionValue.Sending ->
                    "transfer.direction.user_who_received"
    in
    Modal.initWith
        { closeMsg = ClosedTransfersFilters
        , isVisible = model.showTransferFiltersModal
        }
        |> Modal.withHeader (t "all_analysis.filter.title")
        |> Modal.withBody
            [ span [ class "input-label" ] [ text (t "payment_history.pick_date") ]
            , div [ class "flex space-x-4" ]
                [ div [ class "relative w-full" ]
                    [ DatePicker.view model.transfersFiltersBeingEdited.filters.date
                        (datePickerSettings shared)
                        model.transfersFiltersBeingEdited.datePicker
                        |> Html.map TransfersFiltersDatePickerMsg
                    , img
                        [ class "absolute right-0 top-0 h-12 cursor-pointer"
                        , src "/icons/calendar.svg"
                        , tabindex -1
                        , onClick ClickedTransfersFiltersCalendar
                        ]
                        []
                    ]
                , button
                    [ class "h-12"
                    , onClick ClickedClearTransfersFiltersDate
                    ]
                    [ Icons.trash "" ]
                ]
            , Select.init
                { id = "direction-selector"
                , label = t "transfer.direction.title"
                , onInput = SelectedTransfersDirection
                , firstOption = { value = Nothing, label = t "transfer.direction.both" }
                , value = model.transfersFiltersBeingEdited.filters.direction
                , valueToString =
                    Maybe.map TransferDirectionValue.toString
                        >> Maybe.withDefault "BOTH"
                , disabled = False
                , problems = Nothing
                }
                |> Select.withOption
                    { value = Just TransferDirectionValue.Sending
                    , label = t "transfer.direction.sending"
                    }
                |> Select.withOption
                    { value = Just TransferDirectionValue.Receiving
                    , label = t "transfer.direction.receiving"
                    }
                |> Select.withContainerAttrs [ class "mt-10" ]
                |> Select.toHtml
            , View.Form.label "other-account-select" (t directionText)
            , model.transfersFiltersBeingEdited.filters.otherAccount
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
                |> Select.view (selectConfiguration shared)
                    model.transfersFiltersBeingEdited.otherAccountState
                    users
                |> Html.map TransfersFiltersOtherAccountSelectMsg
            , case model.transfersFiltersBeingEdited.filters.otherAccount of
                Nothing ->
                    text ""

                Just otherAccount ->
                    div [ class "flex mt-4 items-start" ]
                        [ div [ class "flex flex-col items-center" ]
                            [ model.transfersFiltersBeingEdited.otherAccountProfileSummary
                                |> Profile.Summary.withRelativeSelector ".modal-content"
                                |> Profile.Summary.withScrollSelector ".modal-body"
                                |> Profile.Summary.withPreventScrolling View.Components.PreventScrollAlways
                                |> Profile.Summary.view shared
                                    loggedIn.accountName
                                    otherAccount
                                |> Html.map GotTransfersFiltersProfileSummaryMsg
                            , button
                                [ class "mt-2"
                                , onClick ClickedClearTransfersFiltersUser
                                ]
                                [ Icons.trash "" ]
                            ]
                        ]
            , button
                [ class "button button-primary w-full mt-10"
                , onClick ClickedApplyTransfersFilters
                ]
                [ text (t "all_analysis.filter.apply") ]
            ]
        |> Modal.toHtml


viewBalance : LoggedIn.Model -> Balance -> Html Msg
viewBalance ({ shared } as loggedIn) balance =
    let
        text_ =
            text << shared.translators.t
    in
    div [ class "bg-white rounded p-4 md:p-6" ]
        [ p [ class "input-label" ] [ text_ "account.my_wallet.balances.current" ]
        , p [ class "text-indigo-500 mt-3" ]
            [ span [ class "font-bold text-3xl" ]
                [ text <| Eos.formatSymbolAmount balance.asset.symbol balance.asset.amount ]
            , text " "
            , span [] [ text <| Eos.symbolToSymbolCodeString balance.asset.symbol ]
            ]
        , a
            [ class "button button-primary w-full mt-6"
            , Route.href (Route.Transfer Nothing)
            ]
            [ text_ "dashboard.transfer" ]
        , button
            [ class "button button-secondary w-full mt-4"
            , onClick ClickedSupportUsButton
            ]
            [ text_ "community.index.support_us" ]
        , div [ class "flex flex-col divide-y divide-y-gray-500 mt-2 md:mt-6" ]
            [ a
                [ class "w-full flex items-center justify-between text-gray-900 py-5"
                , Route.href Route.Community
                ]
                [ text <| shared.translators.tr "dashboard.explore" [ ( "symbol", Eos.symbolToSymbolCodeString balance.asset.symbol ) ]
                , Icons.arrowDown "-rotate-90"
                ]
            , button
                [ class "w-full flex items-center justify-between text-gray-900 py-5"
                , onClick CreateInvite
                ]
                [ text_ "dashboard.invite"
                , Icons.arrowDown "-rotate-90"
                ]
            , a
                [ class "w-full flex items-center justify-between text-gray-900 py-5"
                , Route.href (Route.ProfileContributions loggedIn.accountName)
                ]
                [ text_ "dashboard.my_contributions"
                , Icons.arrowDown "-rotate-90"
                ]
            ]
        ]


viewMyClaimsCard : LoggedIn.Model -> Html Msg
viewMyClaimsCard loggedIn =
    let
        { t } =
            loggedIn.shared.translators
    in
    case RemoteData.map .hasObjectives loggedIn.selectedCommunity of
        RemoteData.Success True ->
            a
                [ class "w-full rounded bg-white px-6 py-10 flex flex-col justify-between hover:shadow"
                , Route.href (Route.ProfileClaims (Eos.nameToString loggedIn.accountName))
                ]
                [ div []
                    [ Icons.claims "w-8 h-8"
                    , p [ class "text-gray-600 mt-5" ]
                        [ text <| t "dashboard.my_claims.1"
                        , span [ class "font-bold" ] [ text <| t "dashboard.my_claims.2" ]
                        ]
                    ]
                , div [ class "button button-primary w-full mt-6 lg:mt-12" ]
                    [ text <| t "dashboard.my_claims.go" ]
                ]

        _ ->
            text ""


viewMyOffersCard : LoggedIn.Model -> Html Msg
viewMyOffersCard loggedIn =
    let
        { t } =
            loggedIn.shared.translators
    in
    case RemoteData.map .hasShop loggedIn.selectedCommunity of
        RemoteData.Success True ->
            a
                [ class "w-full rounded bg-white px-6 py-10 flex flex-col justify-between hover:shadow"
                , Route.href (Route.Shop Shop.UserSales)
                ]
                [ div []
                    [ Icons.shop "w-8 h-8 fill-current"
                    , p [ class "text-gray-600 mt-5" ]
                        [ text <| t "dashboard.my_offers.1"
                        , span [ class "font-bold" ] [ text <| t "dashboard.my_offers.2" ]
                        ]
                    ]
                , div [ class "button button-primary w-full mt-6 lg:mt-12" ]
                    [ text <| t "dashboard.my_offers.go" ]
                ]

        _ ->
            text ""



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


type Msg
    = NoOp
    | ClosedAuthModal
    | CompletedLoadCommunity Community.Model
    | CompletedLoadProfile
    | CompletedLoadBalance (Result Http.Error (Maybe Balance))
    | CompletedLoadUserTransfers (RemoteData (Graphql.Http.Error (Maybe QueryTransfers)) (Maybe QueryTransfers))
    | ClaimsLoaded (RemoteData (Graphql.Http.Error (Maybe Claim.Paginated)) (Maybe Claim.Paginated))
    | ClaimMsg Int Claim.Msg
    | VoteClaim Claim.ClaimId Bool
    | GotVoteResult Claim.ClaimId (Result (Maybe Value) String)
    | GotTransferCardProfileSummaryMsg Int Profile.Summary.Msg
    | RequestedMoreTransfers
    | ClickedOpenTransferFilters
    | ClosedTransfersFilters
    | SelectedTransfersDirection (Maybe TransferDirectionValue)
    | TransfersFiltersDatePickerMsg DatePicker.Msg
    | ClickedTransfersFiltersCalendar
    | ClickedClearTransfersFiltersDate
    | GotTransfersFiltersProfileSummaryMsg Profile.Summary.Msg
    | ClickedClearTransfersFiltersUser
    | TransfersFiltersOtherAccountSelectMsg (Select.Msg Profile.Minimal)
    | SelectedTransfersFiltersOtherAccount (Maybe Profile.Minimal)
    | ClickedApplyTransfersFilters
    | ClickedTransferCard Int
    | ClickedSupportUsButton
    | CreateInvite
    | GotContactMsg Contact.Msg
    | ClosedAddContactModal
    | CloseInviteModal
    | CompletedInviteCreation (Result Http.Error String)
    | CopyToClipboard String
    | CopiedToClipboard
    | ToggleAnalysisSorting
    | ClosedModalRequestingSponsor


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model ({ shared, accountName } as loggedIn) =
    case msg of
        NoOp ->
            UR.init model

        ClosedAuthModal ->
            { model | claimModalStatus = Claim.Closed }
                |> UR.init

        CompletedLoadCommunity community ->
            let
                markSponsorModalAsSeen =
                    UR.addExt (LoggedIn.UpdatedLoggedIn { loggedIn | shared = { shared | hasSeenSponsorModal = True } })
                        >> UR.addCmd (Ports.storeHasSeenSponsorModal True)
            in
            UR.init
                { model
                    | balance = RemoteData.Loading
                    , analysis = LoadingGraphql Nothing
                }
                |> UR.addCmd (fetchBalance shared accountName community)
                |> UR.addCmd (fetchAvailableAnalysis loggedIn Nothing model.analysisFilter community)
                |> UR.addCmd (fetchTransfers loggedIn community Nothing model)
                |> markSponsorModalAsSeen

        CompletedLoadProfile ->
            { model | showContactModal = shouldShowContactModal loggedIn model }
                |> UR.init

        CompletedLoadBalance (Ok balance) ->
            UR.init { model | balance = RemoteData.Success balance }

        CompletedLoadBalance (Err httpError) ->
            UR.init { model | balance = RemoteData.Failure httpError }
                |> UR.logHttpError msg
                    (Just loggedIn.accountName)
                    "Got an error when loading balance on the dashboard"
                    { moduleName = "Page.Dashboard", function = "update" }
                    [ Log.contextFromCommunity loggedIn.selectedCommunity ]
                    httpError

        ClaimsLoaded (RemoteData.Success claims) ->
            let
                wrappedClaims =
                    List.map ClaimLoaded (Claim.paginatedToList claims)

                initProfileSummaries cs =
                    List.map (unwrapClaimStatus >> Claim.initClaimProfileSummaries) cs
            in
            case model.analysis of
                LoadedGraphql existingClaims _ ->
                    { model
                        | analysis = LoadedGraphql (existingClaims ++ wrappedClaims) (Claim.paginatedPageInfo claims)
                        , profileSummaries = initProfileSummaries (existingClaims ++ wrappedClaims)
                    }
                        |> UR.init

                _ ->
                    { model
                        | analysis = LoadedGraphql wrappedClaims (Claim.paginatedPageInfo claims)
                        , profileSummaries = initProfileSummaries wrappedClaims
                    }
                        |> UR.init

        ClaimsLoaded (RemoteData.Failure err) ->
            { model | analysis = FailedGraphql err }
                |> UR.init
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when loading claims on the dashboard"
                    { moduleName = "Page.Dashboard", function = "update" }
                    [ Log.contextFromCommunity loggedIn.selectedCommunity ]
                    err

        ClaimsLoaded _ ->
            UR.init model

        CompletedLoadUserTransfers (RemoteData.Success maybeTransfers) ->
            let
                maybePageInfo : Maybe Api.Relay.PageInfo
                maybePageInfo =
                    maybeTransfers
                        |> Maybe.andThen .transfers
                        |> Maybe.map .pageInfo

                previousTransfers : List ( Transfer, Profile.Summary.Model )
                previousTransfers =
                    case model.transfers of
                        LoadedGraphql previousTransfers_ _ ->
                            previousTransfers_

                        LoadingGraphql (Just previousTransfers_) ->
                            previousTransfers_

                        _ ->
                            []
            in
            { model
                | transfers =
                    Transfer.getTransfers maybeTransfers
                        |> List.map (\transfer -> ( transfer, Profile.Summary.init False ))
                        |> (\transfers -> LoadedGraphql (previousTransfers ++ transfers) maybePageInfo)
            }
                |> UR.init

        CompletedLoadUserTransfers (RemoteData.Failure err) ->
            { model | transfers = FailedGraphql err }
                |> UR.init
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when trying to load user's transfers"
                    { moduleName = "Page.Dashboard", function = "update" }
                    [ Log.contextFromCommunity loggedIn.selectedCommunity ]
                    err

        CompletedLoadUserTransfers _ ->
            UR.init model

        ClaimMsg claimIndex m ->
            let
                updatedProfileSummaries =
                    case m of
                        Claim.GotExternalMsg subMsg ->
                            List.updateAt claimIndex (Claim.updateProfileSummaries subMsg) model.profileSummaries

                        _ ->
                            model.profileSummaries
            in
            { model | profileSummaries = updatedProfileSummaries }
                |> Claim.updateClaimModalStatus m
                |> UR.init

        VoteClaim claimId vote ->
            case model.analysis of
                LoadedGraphql claims pageInfo ->
                    let
                        newClaims =
                            setClaimStatus claims claimId ClaimLoading

                        newModel =
                            { model
                                | analysis = LoadedGraphql newClaims pageInfo
                                , claimModalStatus = Claim.Closed
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
            case model.analysis of
                LoadedGraphql claims pageInfo ->
                    let
                        maybeClaim : Maybe Claim.Model
                        maybeClaim =
                            findClaim claims claimId

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

                                cmd =
                                    case ( pageInfo, loggedIn.selectedCommunity ) of
                                        ( Just page, RemoteData.Success community ) ->
                                            if page.hasNextPage then
                                                fetchAvailableAnalysis loggedIn page.endCursor model.analysisFilter community

                                            else
                                                Cmd.none

                                        ( _, _ ) ->
                                            Cmd.none
                            in
                            { model
                                | analysis = LoadedGraphql (setClaimStatus claims claimId ClaimVoted) pageInfo
                            }
                                |> UR.init
                                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Success (message value))
                                |> UR.addCmd cmd

                        Nothing ->
                            model
                                |> UR.init

                _ ->
                    model |> UR.init

        GotVoteResult claimId (Err eosErrorString) ->
            let
                errorMessage =
                    EosError.parseClaimError loggedIn.shared.translators eosErrorString
            in
            case model.analysis of
                LoadedGraphql claims pageInfo ->
                    let
                        updateShowClaimModal profileSummary =
                            { profileSummary | showClaimModal = False }
                    in
                    { model
                        | analysis = LoadedGraphql (setClaimStatus claims claimId ClaimVoteFailed) pageInfo
                        , profileSummaries = List.map updateShowClaimModal model.profileSummaries
                    }
                        |> UR.init
                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure errorMessage)

                _ ->
                    model |> UR.init

        GotTransferCardProfileSummaryMsg transferId subMsg ->
            let
                updateTransfers transfers =
                    transfers
                        |> List.updateIf
                            (\( transfer, _ ) -> transfer.id == transferId)
                            (\( transfer, profileSummary ) ->
                                ( transfer, Profile.Summary.update subMsg profileSummary )
                            )
            in
            case model.transfers of
                LoadedGraphql transfers pageInfo ->
                    { model | transfers = LoadedGraphql (updateTransfers transfers) pageInfo }
                        |> UR.init

                LoadingGraphql maybeTransfers ->
                    { model
                        | transfers =
                            maybeTransfers
                                |> Maybe.map updateTransfers
                                |> LoadingGraphql
                    }
                        |> UR.init

                FailedGraphql _ ->
                    model
                        |> UR.init

        RequestedMoreTransfers ->
            case ( model.transfers, loggedIn.selectedCommunity ) of
                ( LoadedGraphql transfers maybePageInfo, RemoteData.Success community ) ->
                    let
                        maybeCursor : Maybe String
                        maybeCursor =
                            Maybe.andThen .endCursor maybePageInfo
                    in
                    { model | transfers = LoadingGraphql (Just transfers) }
                        |> UR.init
                        |> UR.addCmd (fetchTransfers loggedIn community maybeCursor model)

                _ ->
                    model
                        |> UR.init

        ClickedOpenTransferFilters ->
            { model | showTransferFiltersModal = True }
                |> UR.init

        ClosedTransfersFilters ->
            let
                oldFiltersBeingEdited =
                    model.transfersFiltersBeingEdited
            in
            { model
                | showTransferFiltersModal = False
                , transfersFiltersBeingEdited =
                    { oldFiltersBeingEdited
                        | otherAccountInput =
                            Maybe.map (.account >> Eos.nameToString) model.transfersFilters.otherAccount
                                |> Maybe.withDefault ""
                        , filters = model.transfersFilters
                    }
            }
                |> UR.init

        SelectedTransfersDirection maybeDirection ->
            let
                oldFiltersBeingEdited =
                    model.transfersFiltersBeingEdited

                oldFilters =
                    oldFiltersBeingEdited.filters
            in
            { model
                | transfersFiltersBeingEdited =
                    { oldFiltersBeingEdited
                        | filters = { oldFilters | direction = maybeDirection }
                    }
            }
                |> UR.init

        TransfersFiltersDatePickerMsg subMsg ->
            let
                ( newDatePicker, datePickerEvent ) =
                    DatePicker.update (datePickerSettings shared)
                        subMsg
                        model.transfersFiltersBeingEdited.datePicker

                oldFiltersBeingEdited =
                    model.transfersFiltersBeingEdited

                oldFilters =
                    oldFiltersBeingEdited.filters

                newDate =
                    case datePickerEvent of
                        DatePicker.Picked pickedDate ->
                            Just pickedDate

                        _ ->
                            oldFilters.date
            in
            { model
                | transfersFiltersBeingEdited =
                    { oldFiltersBeingEdited
                        | datePicker = newDatePicker
                        , filters = { oldFilters | date = newDate }
                    }
            }
                |> UR.init

        ClickedTransfersFiltersCalendar ->
            model
                |> UR.init
                |> UR.addCmd
                    (Browser.Dom.focus "transfers-filters-date-input"
                        |> Task.attempt (\_ -> NoOp)
                    )

        ClickedClearTransfersFiltersDate ->
            let
                oldFiltersBeingEdited =
                    model.transfersFiltersBeingEdited

                oldFilters =
                    oldFiltersBeingEdited.filters
            in
            { model
                | transfersFiltersBeingEdited =
                    { oldFiltersBeingEdited
                        | filters = { oldFilters | date = Nothing }
                    }
            }
                |> UR.init

        GotTransfersFiltersProfileSummaryMsg subMsg ->
            let
                oldFiltersBeingEdited =
                    model.transfersFiltersBeingEdited
            in
            { model
                | transfersFiltersBeingEdited =
                    { oldFiltersBeingEdited
                        | otherAccountProfileSummary =
                            Profile.Summary.update subMsg oldFiltersBeingEdited.otherAccountProfileSummary
                    }
            }
                |> UR.init

        ClickedClearTransfersFiltersUser ->
            let
                oldFiltersBeingEdited =
                    model.transfersFiltersBeingEdited

                oldFilters =
                    oldFiltersBeingEdited.filters
            in
            { model
                | transfersFiltersBeingEdited =
                    { oldFiltersBeingEdited
                        | filters = { oldFilters | otherAccount = Nothing }
                        , otherAccountInput = ""
                    }
            }
                |> UR.init

        TransfersFiltersOtherAccountSelectMsg subMsg ->
            let
                ( updated, cmd ) =
                    Select.update (selectConfiguration loggedIn.shared)
                        subMsg
                        model.transfersFiltersBeingEdited.otherAccountState

                oldTransfersFiltersBeingEdited =
                    model.transfersFiltersBeingEdited
            in
            { model
                | transfersFiltersBeingEdited =
                    { oldTransfersFiltersBeingEdited
                        | otherAccountState = updated
                    }
            }
                |> UR.init
                |> UR.addCmd cmd

        SelectedTransfersFiltersOtherAccount maybeMinimalProfile ->
            let
                oldTransfersFiltersBeingEdited =
                    model.transfersFiltersBeingEdited

                oldFilters =
                    oldTransfersFiltersBeingEdited.filters
            in
            { model
                | transfersFiltersBeingEdited =
                    { oldTransfersFiltersBeingEdited
                        | filters =
                            { oldFilters
                                | otherAccount = maybeMinimalProfile
                            }
                    }
            }
                |> UR.init

        ClickedApplyTransfersFilters ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    let
                        newModel =
                            { model
                                | transfersFilters = model.transfersFiltersBeingEdited.filters
                                , showTransferFiltersModal = False
                                , transfers = LoadingGraphql Nothing
                            }
                    in
                    newModel
                        |> UR.init
                        |> UR.addCmd (fetchTransfers loggedIn community Nothing newModel)

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Applied filters on transfer list, but community wasn't loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Dashboard", function = "update" }
                            []

        ClickedTransferCard transferId ->
            model
                |> UR.init
                |> UR.addCmd (Route.pushUrl loggedIn.shared.navKey (Route.ViewTransfer transferId))

        ClickedSupportUsButton ->
            { model | isModalRequestingSponsorVisible = True }
                |> UR.init

        CreateInvite ->
            case model.balance of
                RemoteData.Success (Just b) ->
                    UR.init
                        { model | inviteModalStatus = InviteModalLoading }
                        |> UR.addCmd
                            (CompletedInviteCreation
                                |> Api.communityInvite loggedIn.shared b.asset.symbol loggedIn.accountName
                            )

                _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Created invitation, but balance wasn't loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Dashboard", function = "update" }
                            []

        GotContactMsg subMsg ->
            case LoggedIn.profile loggedIn of
                Just userProfile ->
                    let
                        ( contactModel, cmd, contactResponse ) =
                            Contact.update subMsg
                                model.contactModel
                                loggedIn.shared
                                loggedIn.authToken
                                userProfile.contacts

                        addContactResponse model_ =
                            case contactResponse of
                                Contact.NotAsked ->
                                    model_
                                        |> UR.init

                                Contact.WithError errorMessage ->
                                    { model_ | showContactModal = False }
                                        |> UR.init
                                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure errorMessage)

                                Contact.WithContacts successMessage contacts _ ->
                                    let
                                        newProfile =
                                            { userProfile | contacts = contacts }
                                    in
                                    { model_ | showContactModal = False }
                                        |> UR.init
                                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Success successMessage)
                                        |> UR.addExt
                                            (LoggedIn.ProfileLoaded newProfile
                                                |> LoggedIn.ExternalBroadcast
                                            )
                    in
                    { model | contactModel = contactModel }
                        |> addContactResponse
                        |> UR.addCmd (Cmd.map GotContactMsg cmd)

                Nothing ->
                    model |> UR.init

        ClosedAddContactModal ->
            { model | showContactModal = False }
                |> UR.init

        CloseInviteModal ->
            UR.init
                { model
                    | inviteModalStatus = InviteModalClosed
                    , copied = False
                }

        CompletedInviteCreation (Ok invitationId) ->
            { model | inviteModalStatus = InviteModalLoaded invitationId }
                |> UR.init

        CompletedInviteCreation (Err httpError) ->
            UR.init
                { model | inviteModalStatus = InviteModalFailed (loggedIn.shared.translators.t "community.invite.failed") }
                |> UR.logHttpError msg
                    (Just loggedIn.accountName)
                    "Got an error when creating an invite"
                    { moduleName = "Page.Dashboard", function = "update" }
                    [ Log.contextFromCommunity loggedIn.selectedCommunity ]
                    httpError

        CopyToClipboard elementId ->
            model
                |> UR.init
                |> UR.addPort
                    { responseAddress = CopiedToClipboard
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "id", Encode.string elementId )
                            , ( "name", Encode.string "copyToClipboard" )
                            ]
                    }

        CopiedToClipboard ->
            { model | copied = True }
                |> UR.init

        ToggleAnalysisSorting ->
            let
                newModel =
                    { model
                        | analysisFilter =
                            case model.analysisFilter of
                                ASC ->
                                    DESC

                                DESC ->
                                    ASC
                        , analysis = LoadingGraphql Nothing
                    }

                fetchCmd =
                    case loggedIn.selectedCommunity of
                        RemoteData.Success community ->
                            fetchAvailableAnalysis loggedIn Nothing newModel.analysisFilter community

                        _ ->
                            Cmd.none
            in
            newModel
                |> UR.init
                |> UR.addCmd fetchCmd

        ClosedModalRequestingSponsor ->
            let
                newModel =
                    { model | isModalRequestingSponsorVisible = False }
            in
            { newModel | showContactModal = shouldShowContactModal loggedIn newModel }
                |> UR.init



-- HELPERS


shouldShowContactModal : LoggedIn.Model -> Model -> Bool
shouldShowContactModal loggedIn model =
    case loggedIn.profile of
        RemoteData.Success profile ->
            let
                addContactLimitDate =
                    -- 01/01/2022
                    1641006000000

                showContactModalFromDate =
                    addContactLimitDate - Time.posixToMillis loggedIn.shared.now > 0
            in
            showContactModalFromDate
                && List.isEmpty profile.contacts
                && not model.isModalRequestingSponsorVisible

        _ ->
            False


fetchBalance : Shared -> Eos.Name -> Community.Model -> Cmd Msg
fetchBalance shared accountName community =
    Api.getBalances shared
        accountName
        (Result.map
            (\balances ->
                let
                    maybeBalance =
                        List.find (.asset >> .symbol >> (==) community.symbol) balances
                in
                case maybeBalance of
                    Just b ->
                        Just b

                    Nothing ->
                        List.head balances
            )
            >> CompletedLoadBalance
        )


fetchTransfers : LoggedIn.Model -> Community.Model -> Maybe String -> Model -> Cmd Msg
fetchTransfers loggedIn community maybeCursor model =
    Api.Graphql.query loggedIn.shared
        (Just loggedIn.authToken)
        (Transfer.transfersUserQuery
            loggedIn.accountName
            (\args ->
                { args
                    | first = Present 10
                    , after = OptionalArgument.fromMaybe maybeCursor
                    , filter =
                        Present
                            { communityId = Present (Eos.symbolToString community.symbol)
                            , date =
                                model.transfersFilters.date
                                    |> Maybe.map (Date.toIsoString >> Cambiatus.Scalar.Date)
                                    |> OptionalArgument.fromMaybe
                            , direction =
                                case ( model.transfersFilters.direction, model.transfersFilters.otherAccount ) of
                                    ( Nothing, Nothing ) ->
                                        Absent

                                    _ ->
                                        Present
                                            { direction = OptionalArgument.fromMaybe model.transfersFilters.direction
                                            , otherAccount =
                                                model.transfersFilters.otherAccount
                                                    |> Maybe.map (.account >> Eos.nameToString)
                                                    |> OptionalArgument.fromMaybe
                                            }
                            }
                }
            )
        )
        CompletedLoadUserTransfers


fetchAvailableAnalysis : LoggedIn.Model -> Maybe String -> Direction -> Community.Model -> Cmd Msg
fetchAvailableAnalysis { shared, authToken } maybeCursor direction community =
    let
        arg =
            { communityId = Eos.symbolToString community.symbol
            }

        optionalArguments =
            \a ->
                { a
                    | first =
                        case maybeCursor of
                            Just _ ->
                                Present 1

                            Nothing ->
                                Present 4
                    , after =
                        case maybeCursor of
                            Nothing ->
                                Absent

                            Just "" ->
                                Absent

                            Just cursor ->
                                Present cursor
                    , filter =
                        (\claimsFilter ->
                            { claimsFilter
                                | direction =
                                    case direction of
                                        ASC ->
                                            Present Cambiatus.Enum.Direction.Asc

                                        DESC ->
                                            Present Cambiatus.Enum.Direction.Desc
                            }
                        )
                            |> Cambiatus.InputObject.buildClaimsFilter
                            |> Present
                }
    in
    Api.Graphql.query shared
        (Just authToken)
        (Cambiatus.Query.pendingClaims optionalArguments arg Claim.claimPaginatedSelectionSet)
        ClaimsLoaded


setClaimStatus : List ClaimStatus -> Claim.ClaimId -> (Claim.Model -> ClaimStatus) -> List ClaimStatus
setClaimStatus claims claimId status =
    claims
        |> List.map
            (\c ->
                case c of
                    ClaimLoaded c_ ->
                        if c_.id == claimId then
                            status c_

                        else
                            c

                    ClaimLoading c_ ->
                        if c_.id == claimId then
                            status c_

                        else
                            c

                    _ ->
                        c
            )


findClaim : List ClaimStatus -> Claim.ClaimId -> Maybe Claim.Model
findClaim claims claimId =
    claims
        |> List.map unwrapClaimStatus
        |> List.find (\c -> c.id == claimId)


unwrapClaimStatus : ClaimStatus -> Claim.Model
unwrapClaimStatus claimStatus =
    case claimStatus of
        ClaimLoaded claim ->
            claim

        ClaimLoading claim ->
            claim

        ClaimVoted claim ->
            claim

        ClaimVoteFailed claim ->
            claim


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityLoaded community ->
            Just (CompletedLoadCommunity community)

        LoggedIn.ProfileLoaded _ ->
            Just CompletedLoadProfile

        _ ->
            Nothing


jsAddressToMsg : List String -> Value -> Maybe Msg
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

        "CopiedToClipboard" :: _ ->
            Just CopiedToClipboard

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        ClosedAuthModal ->
            [ "ClosedAuthModal" ]

        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        CompletedLoadProfile ->
            [ "CompletedLoadProfile" ]

        CompletedLoadBalance result ->
            [ "CompletedLoadBalance", UR.resultToString result ]

        CompletedLoadUserTransfers result ->
            [ "CompletedLoadUserTransfers", UR.remoteDataToString result ]

        ClaimsLoaded result ->
            [ "ClaimsLoaded", UR.remoteDataToString result ]

        ClaimMsg _ _ ->
            [ "ClaimMsg" ]

        VoteClaim claimId _ ->
            [ "VoteClaim", String.fromInt claimId ]

        GotVoteResult _ result ->
            [ "GotVoteResult", UR.resultToString result ]

        GotTransferCardProfileSummaryMsg _ _ ->
            [ "GotTransferCardProfileSummaryMsg" ]

        RequestedMoreTransfers ->
            [ "RequestedMoreTransfers" ]

        ClickedOpenTransferFilters ->
            [ "ClickedOpenTransferFilters" ]

        ClosedTransfersFilters ->
            [ "ClosedTransfersFilters" ]

        SelectedTransfersDirection _ ->
            [ "SelectedTransfersDirection" ]

        TransfersFiltersDatePickerMsg _ ->
            [ "TransfersFiltersDatePickerMsg" ]

        ClickedTransfersFiltersCalendar ->
            [ "ClickedTransfersFiltersCalendar" ]

        ClickedClearTransfersFiltersDate ->
            [ "ClickedClearTransfersFiltersDate" ]

        GotTransfersFiltersProfileSummaryMsg subMsg ->
            "GotTransfersFiltersProfileSummaryMsg" :: Profile.Summary.msgToString subMsg

        ClickedClearTransfersFiltersUser ->
            [ "ClickedClearTransfersFiltersUser" ]

        TransfersFiltersOtherAccountSelectMsg _ ->
            [ "TransfersFiltersOtherAccountSelectMsg" ]

        SelectedTransfersFiltersOtherAccount _ ->
            [ "SelectedTransfersFiltersOtherAccount" ]

        ClickedApplyTransfersFilters ->
            [ "ClickedApplyTransfersFilters" ]

        ClickedTransferCard _ ->
            [ "ClickedTransferCard" ]

        ClickedSupportUsButton ->
            [ "ClickedSupportUsButton" ]

        CreateInvite ->
            [ "CreateInvite" ]

        GotContactMsg _ ->
            [ "GotContactMsg" ]

        ClosedAddContactModal ->
            [ "ClosedAddContactModal" ]

        CloseInviteModal ->
            [ "CloseInviteModal" ]

        CompletedInviteCreation _ ->
            [ "CompletedInviteCreation" ]

        CopyToClipboard _ ->
            [ "CopyToClipboard" ]

        CopiedToClipboard ->
            [ "CopiedToClipboard" ]

        ToggleAnalysisSorting ->
            [ "ToggleAnalysisSorting" ]

        ClosedModalRequestingSponsor ->
            [ "ClosedModalRequestingSponsor" ]
