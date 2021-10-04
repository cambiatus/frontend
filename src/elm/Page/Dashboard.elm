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
import Avatar
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
import Graphql.Http
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Html exposing (Html, a, br, button, div, h1, img, li, p, span, strong, text, ul)
import Html.Attributes exposing (class, classList, src, style, tabindex)
import Html.Events exposing (onClick)
import Http
import Icons
import Json.Decode exposing (Value)
import Json.Encode as Encode
import List.Extra as List
import Log
import Page
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
import Time
import Transfer exposing (QueryTransfers, Transfer)
import UpdateResult as UR
import Url
import View.Components
import View.Feedback as Feedback
import View.Form
import View.Form.Input as Input
import View.Form.Select as Select
import View.MarkdownEditor
import View.Modal as Modal



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( initModel loggedIn.shared
    , Cmd.batch
        [ LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
        , LoggedIn.maybeInitWith CompletedLoadProfile .profile loggedIn
        ]
    )



-- MODEL


type alias Model =
    { balance : RemoteData Http.Error (Maybe Balance)
    , analysis : GraphqlStatus (Maybe Claim.Paginated) { claims : List Claim.Model, count : Int }
    , analysisFilter : Direction
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
    }


initModel : Shared -> Model
initModel shared =
    { balance = RemoteData.NotAsked
    , analysis = LoadingGraphql Nothing
    , analysisFilter = initAnalysisFilter
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
    | FailedGraphql


type InviteModalStatus
    = InviteModalClosed
    | InviteModalLoading
    | InviteModalFailed String
    | InviteModalLoaded String


type Direction
    = DESC



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view ({ shared } as loggedIn) model =
    let
        t =
            shared.translators.t

        content =
            case ( model.balance, loggedIn.selectedCommunity ) of
                ( RemoteData.Loading, _ ) ->
                    Page.fullPageLoading shared

                ( RemoteData.NotAsked, _ ) ->
                    Page.fullPageLoading shared

                ( RemoteData.Failure e, _ ) ->
                    Page.fullPageError (t "dashboard.sorry") e

                ( RemoteData.Success (Just balance), RemoteData.Success community ) ->
                    let
                        isValidator =
                            List.any ((==) loggedIn.accountName) community.validators
                    in
                    div []
                        [ div [ class "container mx-auto my-8 px-4 lg:grid lg:grid-cols-3 lg:gap-7" ]
                            [ viewWelcomeCard loggedIn community balance
                            , if community.hasObjectives && isValidator then
                                viewActionsForAnalysisCard loggedIn model

                              else
                                text ""
                            , viewTimelineCard loggedIn model
                            ]
                        , viewInvitationModal loggedIn model
                        , addContactModal shared model
                        , viewTransferFilters loggedIn community.members model
                        ]

                ( RemoteData.Success _, _ ) ->
                    Page.fullPageNotFound (t "dashboard.sorry") ""
    in
    { title = t "menu.dashboard"
    , content = content
    }


addContactModal : Shared -> Model -> Html Msg
addContactModal shared ({ contactModel } as model) =
    let
        text_ s =
            shared.translators.t s
                |> text

        header =
            div [ class "mt-4" ]
                [ p [ class "inline bg-purple-100 text-white rounded-full py-0.5 px-2 text-sm uppercase" ]
                    [ text_ "contact_modal.new" ]
                , p [ class "text-lg font-bold mt-2" ]
                    [ text_ "contact_modal.title" ]
                ]

        form =
            Contact.view shared.translators contactModel
                |> Html.map GotContactMsg
    in
    Modal.initWith
        { closeMsg = ClosedAddContactModal
        , isVisible = model.showContactModal
        }
        |> Modal.withBody
            [ header
            , img [ class "mx-auto mt-10", src "/images/girl-with-phone.svg" ] []
            , form
            , p [ class "text-sm text-center uppercase my-4" ]
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
                    [ div [ class "mt-3 label" ]
                        [ text_ "community.invite.label" ]
                    , p [ class "py-2 md:text-lg text-black" ]
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
                        |> Input.withAttrs
                            [ class "absolute opacity-0 left-[-9999em]"
                            , tabindex -1
                            ]
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


viewTransferList :
    LoggedIn.Model
    -> List ( Transfer, Profile.Summary.Model )
    -> Maybe Api.Relay.PageInfo
    -> Bool
    -> Html Msg
viewTransferList loggedIn transfers maybePageInfo isLoading =
    let
        addLoading transfers_ =
            if isLoading then
                transfers_
                    ++ [ View.Components.loadingLogoAnimated loggedIn.shared.translators "mb-8" ]

            else
                transfers_

        infiniteList isMobile_ =
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
                , distanceToRequest = 800
                , elementToTrack =
                    if isMobile_ then
                        View.Components.TrackWindow

                    else
                        View.Components.TrackSelf
                }

        container attrs children =
            div []
                [ infiniteList True
                    (class "md:hidden" :: attrs)
                    children
                , infiniteList False
                    (class "hidden md:block overflow-y-auto"
                        :: style "height" "max(60vh, 400px)"
                        :: attrs
                    )
                    children
                ]
    in
    container
        [ class "divide-y divide-gray-100 w-full"
        , tabindex -1
        ]
        (transfers
            |> List.map
                (\( transfer, profileSummary ) ->
                    viewTransfer loggedIn transfer profileSummary
                )
            |> addLoading
        )


viewTransfer : LoggedIn.Model -> Transfer -> Profile.Summary.Model -> Html Msg
viewTransfer loggedIn transfer profileSummary =
    let
        { t } =
            loggedIn.shared.translators

        ( otherProfile, isFromUser ) =
            if transfer.from.account == loggedIn.accountName then
                ( transfer.to, True )

            else
                ( transfer.from, False )
    in
    button
        [ class "flex w-full px-6 py-4 focus-ring ring-inset focus-visible:rounded-sm hover:bg-gray-200 first:pt-6 last:pb-6"
        , onClick (ClickedTransferCard transfer.id)
        ]
        [ profileSummary
            |> Profile.Summary.withoutName
            |> Profile.Summary.withImageSize "w-8 h-8"
            |> Profile.Summary.view loggedIn.shared
                loggedIn.accountName
                otherProfile
            |> Html.map (GotTransferCardProfileSummaryMsg transfer.id)
        , div [ class "ml-4 text-left" ]
            [ p [ class "mb-1" ]
                [ if isFromUser then
                    text <| t "transfer.sent_to"

                  else
                    text <| t "transfer.received_from"
                , text " "
                , strong []
                    [ otherProfile.name
                        |> Maybe.withDefault (Eos.nameToString otherProfile.account)
                        |> text
                    ]
                ]
            , p [ class "text-green" ]
                [ strong []
                    [ { amount = transfer.value
                      , symbol = transfer.community.symbol
                      }
                        |> Eos.assetToString
                        |> text
                    ]
                ]
            ]
        ]


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
            [ span [ class "label" ] [ text (t "payment_history.pick_date") ]
            , div [ class "flex space-x-4" ]
                [ DatePicker.view model.transfersFiltersBeingEdited.filters.date
                    (datePickerSettings shared)
                    model.transfersFiltersBeingEdited.datePicker
                    |> Html.map TransfersFiltersDatePickerMsg
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
            , View.Form.label [] "other-account-select" (t directionText)
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


viewWelcomeCard : LoggedIn.Model -> Community.Model -> Balance -> Html Msg
viewWelcomeCard ({ shared } as loggedIn) community balance =
    let
        { t, tr } =
            shared.translators

        text_ =
            text << t

        listItem :
            (String -> Html Msg)
            -> String
            -> String
            -> (List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg)
            -> List (Html.Attribute Msg)
            -> Html Msg
        listItem icon iconSize description element attrs =
            li []
                [ element (class "py-2 flex items-center w-full group hover:text-orange-300 focus-visible:text-orange-300 focus-ring focus:ring-orange-300 focus:ring-opacity-50 focus:ring-offset-4 rounded-sm" :: attrs)
                    [ icon (iconSize ++ " mr-4 text-gray-500 fill-current group-hover:text-orange-300 group-focus-visible:text-orange-300")
                    , text description
                    , Icons.arrowDown "-rotate-90 ml-auto text-gray-900 fill-current group-hover:text-orange-300 group-focus-visible:text-orange-300"
                    ]
                ]
    in
    div
        [ class "relative"
        , classList [ ( "md:animate-fade-in-from-above-lg md:motion-reduce:animate-none", not loggedIn.hasSeenDashboard ) ]
        ]
        [ h1 [ class "text-gray-333 mb-4" ]
            [ text_ "menu.my_communities"
            , strong [] [ text community.name ]
            ]
        , div
            [ class "bg-white rounded flex flex-col pt-4 pb-2"
            ]
            [ div [ class "flex flex-col px-4 pb-6 border-b border-gray-100" ]
                [ span [ class "text-green text-xl font-bold text-center" ]
                    [ text (Eos.formatSymbolAmount balance.asset.symbol balance.asset.amount) ]
                , span [ class "text-gray-900 text-sm font-bold uppercase text-center" ]
                    [ text (tr "dashboard.your_balance" [ ( "symbol", Eos.symbolToSymbolCodeString community.symbol ) ]) ]
                , div [ class "flex space-x-4 mt-4" ]
                    [ a
                        [ class "button button-primary w-full"
                        , Route.href (Route.Transfer Nothing)
                        ]
                        [ text_ "dashboard.transfer" ]

                    -- TODO - Add "Support us" button
                    ]
                ]
            , ul [ class "px-4 pt-2 divide-y divide-y-gray-100" ]
                [ listItem Icons.cambiatusCoin
                    "w-5"
                    (tr "dashboard.how_to_earn" [ ( "symbol", Eos.symbolToSymbolCodeString community.symbol ) ])
                    a
                    [ Route.href Route.Community ]
                , listItem Icons.profile
                    "w-5 h-5"
                    (t "dashboard.invite")
                    button
                    [ onClick CreateInvite ]
                , if community.hasObjectives then
                    listItem Icons.flagWithoutBackground
                        "h-5"
                        (t "dashboard.my_claims")
                        a
                        [ Route.href (Route.ProfileClaims (Eos.nameToString loggedIn.accountName)) ]

                  else
                    text ""
                , if community.hasShop then
                    listItem Icons.shop
                        "w-5 h-5"
                        (t "dashboard.my_offers")
                        a
                        [ Route.href (Route.Shop Shop.UserSales) ]

                  else
                    text ""

                -- TODO - Add "My contributions" listItem
                ]
            ]
        , img
            [ class "absolute top-0 right-0"
            , src "/images/success-doggo.svg"
            ]
            []
        ]


viewActionsForAnalysisCard : LoggedIn.Model -> Model -> Html Msg
viewActionsForAnalysisCard loggedIn model =
    let
        { t, tr } =
            loggedIn.shared.translators

        text_ =
            text << t
    in
    div [ classList [ ( "md:animate-fade-in-from-above-lg md:animation-delay-150 md:motion-reduce:animate-none", not loggedIn.hasSeenDashboard ) ] ]
        [ h1 [ class "text-gray-333 mt-6 mb-4 lg:mt-0" ]
            [ strong [] [ text_ "dashboard.analysis.title.1" ]
            , text " "
            , text_ "dashboard.analysis.title.2"
            ]
        , div [ class "bg-white rounded py-6" ]
            [ case model.analysis of
                LoadingGraphql _ ->
                    View.Components.loadingLogoAnimated loggedIn.shared.translators "-mt-8"

                FailedGraphql ->
                    div [ class "px-6" ]
                        [ img [ class "w-2/3 mx-auto", src "/images/error.svg" ] []
                        , p [ class "text-center mt-4" ]
                            [ text_ "dashboard.analysis.error_fetching" ]
                        ]

                LoadedGraphql ({ claims } as analysis) _ ->
                    let
                        compareAvatars first second =
                            case ( Avatar.toMaybeString first, Avatar.toMaybeString second ) of
                                ( Just x, Just y ) ->
                                    -- Prioritize showing different avatars
                                    if x == y then
                                        LT

                                    else
                                        EQ

                                ( Just _, Nothing ) ->
                                    LT

                                ( Nothing, Just _ ) ->
                                    GT

                                ( Nothing, Nothing ) ->
                                    EQ
                    in
                    if analysis.count > 0 then
                        div [ class "w-2/3 mx-auto" ]
                            [ div [ class "flex justify-center space-x-2 mb-4" ]
                                (claims
                                    |> List.map (.claimer >> .avatar)
                                    |> List.sortWith compareAvatars
                                    |> List.take 5
                                    |> List.map (\avatar -> Avatar.view avatar "w-7 h-7")
                                )
                            , View.MarkdownEditor.viewReadOnly [ class "mb-4 text-center" ]
                                (tr "dashboard.analysis.count" [ ( "amount", String.fromInt analysis.count ) ])
                            , a
                                [ class "button button-primary w-full"
                                , Route.href Route.Analysis
                                ]
                                [ text_ "dashboard.analysis.analyze_now" ]
                            ]

                    else
                        div [ class "py-4" ]
                            [ img
                                [ class "mx-auto"
                                , src "/images/empty-analysis.svg"
                                ]
                                []
                            , p [ class "mt-5 text-center text-gray-333" ]
                                [ strong [] [ text_ "dashboard.analysis.empty.1" ]
                                , br [] []
                                , text_ "dashboard.analysis.empty.2"
                                ]
                            ]
            ]
        ]


viewTimelineCard : LoggedIn.Model -> Model -> Html Msg
viewTimelineCard loggedIn model =
    let
        translators =
            loggedIn.shared.translators

        text_ =
            text << translators.t
    in
    div [ classList [ ( "md:animate-fade-in-from-above-lg md:animation-delay-300 md:motion-reduce:animate-none", not loggedIn.hasSeenDashboard ) ] ]
        [ div [ class "flex justify-between items-center mt-6 mb-4 md:mb-1 lg:mt-0" ]
            [ h1 [ class "text-gray-333" ]
                [ text_ "transfer.transfers_latest"
                , text " "
                , strong [] [ text_ "transfer.transfers" ]
                ]
            , button
                [ class "button button-secondary w-auto h-auto pl-4 flex"
                , onClick ClickedOpenTransferFilters
                ]
                [ text_ "all_analysis.filter.title"
                , Icons.arrowDown "fill-current"
                ]
            ]
        , div [ class "bg-white rounded md:overflow-hidden" ]
            [ case model.transfers of
                LoadingGraphql Nothing ->
                    View.Components.loadingLogoAnimated translators "px-6 mb-8"

                FailedGraphql ->
                    p [ class "text-gray-900 text-sm py-20 px-6 text-center" ]
                        [ text_ "transfer.loading_error" ]

                LoadedGraphql [] _ ->
                    p [ class "text-gray-900 text-sm py-20 px-6 text-center" ]
                        [ text_ "transfer.no_transfers_yet" ]

                LoadingGraphql (Just transfers) ->
                    viewTransferList loggedIn transfers Nothing True

                LoadedGraphql transfers maybePageInfo ->
                    viewTransferList loggedIn transfers maybePageInfo False
            ]
        ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


type Msg
    = NoOp
    | CompletedLoadCommunity Community.Model
    | CompletedLoadProfile Profile.Model
    | CompletedLoadBalance (Result Http.Error (Maybe Balance))
    | CompletedLoadUserTransfers (RemoteData (Graphql.Http.Error (Maybe QueryTransfers)) (Maybe QueryTransfers))
    | ClaimsLoaded (RemoteData (Graphql.Http.Error (Maybe Claim.Paginated)) (Maybe Claim.Paginated))
    | GotTransferCardProfileSummaryMsg Int Profile.Summary.Msg
    | RequestedMoreTransfers
    | ClickedOpenTransferFilters
    | ClosedTransfersFilters
    | SelectedTransfersDirection (Maybe TransferDirectionValue)
    | TransfersFiltersDatePickerMsg DatePicker.Msg
    | ClickedClearTransfersFiltersDate
    | GotTransfersFiltersProfileSummaryMsg Profile.Summary.Msg
    | ClickedClearTransfersFiltersUser
    | TransfersFiltersOtherAccountSelectMsg (Select.Msg Profile.Minimal)
    | SelectedTransfersFiltersOtherAccount (Maybe Profile.Minimal)
    | ClickedApplyTransfersFilters
    | ClickedTransferCard Int
    | CreateInvite
    | GotContactMsg Contact.Msg
    | ClosedAddContactModal
    | CloseInviteModal
    | CompletedInviteCreation (Result Http.Error String)
    | CopyToClipboard String
    | CopiedToClipboard


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model ({ shared, accountName } as loggedIn) =
    case msg of
        NoOp ->
            UR.init model

        CompletedLoadCommunity community ->
            UR.init
                { model
                    | balance = RemoteData.Loading
                    , analysis = LoadingGraphql Nothing
                }
                |> UR.addCmd (fetchBalance shared accountName community)
                |> UR.addCmd (fetchAvailableAnalysis loggedIn Nothing model.analysisFilter community)
                |> UR.addCmd (fetchTransfers loggedIn community Nothing model)

        CompletedLoadProfile profile ->
            let
                addContactLimitDate =
                    -- 01/01/2022
                    1641006000000

                showContactModalFromDate =
                    addContactLimitDate - Time.posixToMillis shared.now > 0
            in
            { model | showContactModal = showContactModalFromDate && List.isEmpty profile.contacts }
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
                analysis =
                    { claims = Claim.paginatedToList claims
                    , count =
                        claims
                            |> Maybe.andThen .count
                            |> Maybe.withDefault 0
                    }
            in
            { model | analysis = LoadedGraphql analysis (Claim.paginatedPageInfo claims) }
                |> UR.init

        ClaimsLoaded (RemoteData.Failure err) ->
            { model | analysis = FailedGraphql }
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
            { model | transfers = FailedGraphql }
                |> UR.init
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when trying to load user's transfers"
                    { moduleName = "Page.Dashboard", function = "update" }
                    [ Log.contextFromCommunity loggedIn.selectedCommunity ]
                    err

        CompletedLoadUserTransfers _ ->
            UR.init model

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

                FailedGraphql ->
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



-- HELPERS


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
                                Present 5
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


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityLoaded community ->
            Just (CompletedLoadCommunity community)

        LoggedIn.ProfileLoaded profile ->
            Just (CompletedLoadProfile profile)

        _ ->
            Nothing


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "CopiedToClipboard" :: _ ->
            Just CopiedToClipboard

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        CompletedLoadProfile _ ->
            [ "CompletedLoadProfile" ]

        CompletedLoadBalance result ->
            [ "CompletedLoadBalance", UR.resultToString result ]

        CompletedLoadUserTransfers result ->
            [ "CompletedLoadUserTransfers", UR.remoteDataToString result ]

        ClaimsLoaded result ->
            [ "ClaimsLoaded", UR.remoteDataToString result ]

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
