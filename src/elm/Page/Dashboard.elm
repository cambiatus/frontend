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
import Api.Relay
import Avatar
import Browser.Dom
import Cambiatus.Enum.Direction
import Cambiatus.Enum.Permission as Permission
import Cambiatus.Enum.TransferDirectionValue as TransferDirectionValue exposing (TransferDirectionValue)
import Cambiatus.InputObject
import Cambiatus.Query
import Cambiatus.Scalar
import Claim
import Community exposing (Balance)
import Date
import Eos
import Eos.Account as Eos
import Form
import Form.DatePicker
import Form.Select
import Form.Text
import Form.UserPicker
import Graphql.Http
import Graphql.OptionalArgument as OptionalArgument exposing (OptionalArgument(..))
import Html exposing (Html, a, br, button, div, h1, h2, hr, img, li, p, span, strong, text, ul)
import Html.Attributes exposing (alt, class, classList, disabled, id, src, style, tabindex)
import Html.Events exposing (onClick)
import Http
import Icons
import Json.Decode exposing (Value)
import Json.Encode as Encode
import List.Extra as List
import Log
import Markdown
import Page
import Ports
import Profile
import Profile.Contact as Contact
import Profile.Summary
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import Task
import Time
import Transfer exposing (QueryTransfers, Transfer)
import UpdateResult as UR
import Url
import Utils
import View.Components
import View.Feedback as Feedback
import View.Modal as Modal



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    let
        model =
            initModel loggedIn.shared
    in
    ( model
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )



-- MODEL


type alias Model =
    { balance : RemoteData Http.Error (Maybe Balance)
    , analysis : GraphqlStatus (Maybe Claim.Paginated) { claims : List Claim.Model, count : Int }
    , lastSocket : String
    , transfers : GraphqlStatus (Maybe QueryTransfers) (List ( Transfer, Profile.Summary.Model ))
    , transfersFilters : TransfersFilters
    , showTransferFiltersModal : Bool
    , transferFiltersForm : Form.Model TransferFiltersFormInput
    , contactModel : Contact.Model
    , showContactModal : Bool
    , inviteModalStatus : InviteModalStatus
    , claimModalStatus : Claim.ModalStatus
    , copied : Bool
    , showModalRequestingSponsor : Bool
    }


hasModalsOpen : Model -> Bool
hasModalsOpen model =
    let
        isShowingInviteModal =
            case model.inviteModalStatus of
                InviteModalClosed ->
                    False

                _ ->
                    True

        isShowingClaimModal =
            case model.claimModalStatus of
                Claim.Closed ->
                    False

                _ ->
                    True
    in
    model.showTransferFiltersModal
        || model.showContactModal
        || model.showModalRequestingSponsor
        || isShowingInviteModal
        || isShowingClaimModal


initModel : Shared -> Model
initModel shared =
    { balance = RemoteData.NotAsked
    , analysis = LoadingGraphql Nothing
    , lastSocket = ""
    , transfers = LoadingGraphql Nothing
    , transfersFilters = initTransfersFilters
    , showTransferFiltersModal = False
    , transferFiltersForm =
        Form.init
            { date = Form.DatePicker.initModel (Date.fromPosix shared.timezone shared.now)
            , direction = Nothing
            , user = Form.UserPicker.initSingle { id = "transfer-filters-user-picker" }
            }
    , contactModel = Contact.initSingle
    , showContactModal = False
    , inviteModalStatus = InviteModalClosed
    , claimModalStatus = Claim.Closed
    , copied = False
    , showModalRequestingSponsor = False
    }


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
                            [ if not loggedIn.hasAcceptedCodeOfConduct then
                                LoggedIn.viewFrozenAccountCard shared.translators
                                    { onClick = ClickedAcceptCodeOfConduct
                                    , isHorizontal = False
                                    }
                                    [ class "mb-6 lg:mb-0 self-start" ]

                              else
                                text ""
                            , viewWelcomeCard loggedIn community balance
                            , if community.hasObjectives && isValidator && loggedIn.hasAcceptedCodeOfConduct then
                                viewActionsForAnalysisCard loggedIn model

                              else
                                text ""
                            , viewTimelineCard loggedIn (community.hasObjectives && isValidator) model
                            ]
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
        , isVisible = model.showModalRequestingSponsor
        }
        |> Modal.withHeaderElement (viewNewTag shared)
        |> Modal.withBody
            [ div [ class "flex flex-col items-center h-full" ]
                [ h1 [ class "text-center text-heading font-bold" ]
                    [ text_ "sponsorship.dashboard_modal.title" ]
                , img [ class "mt-4", src "/images/sponsor-community.svg" ] []
                , div [ class "w-full mt-7 mx-auto md:w-5/6 lg:w-2/3" ]
                    [ p [ class "text-center mb-6" ]
                        [ text_ "sponsorship.dashboard_modal.subtitle" ]
                    , p [ class "text-center mb-6" ]
                        [ text_ "sponsorship.dashboard_modal.explanation" ]
                    , a
                        [ class "button button-primary w-full md:mt-8 mb-6"
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

        header =
            div []
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
        |> Modal.withHeaderElement header
        |> Modal.withBody
            [ img [ class "mx-auto mt-10", src "/images/girl-with-phone.svg" ] []
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
                    , Form.Text.view
                        (Form.Text.init
                            { label = ""
                            , id = "invitation-id"
                            }
                            |> Form.Text.withExtraAttrs
                                [ class "absolute opacity-0 left-[-9999em]"
                                , tabindex -1
                                ]
                            |> Form.Text.withContainerAttrs [ class "mb-0 overflow-hidden" ]
                        )
                        { onChange = \_ -> NoOp
                        , onBlur = NoOp
                        , value = url invitationId
                        , error = text ""
                        , hasError = False
                        , translators = shared.translators
                        , isRequired = False
                        }
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
                        , id "copy-invite-button"
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
    -> Model
    -> List ( Transfer, Profile.Summary.Model )
    -> Maybe Api.Relay.PageInfo
    -> Bool
    -> Html Msg
viewTransferList loggedIn model transfers maybePageInfo isLoading =
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
                    (class "hidden md:block"
                        :: style "height" "max(60vh, 400px)"
                        :: attrs
                    )
                    children
                ]
    in
    container
        [ tabindex -1
        , class "w-full"
        , classList [ ( "md:overflow-y-hidden", hasModalsOpen model ) ]
        ]
        (transfers
            |> List.groupWhile
                (\( t1, _ ) ( t2, _ ) ->
                    Utils.areSameDay loggedIn.shared.timezone
                        (Utils.fromDateTime t1.blockTime)
                        (Utils.fromDateTime t2.blockTime)
                )
            |> List.map
                (\( ( t1, _ ) as first, rest ) ->
                    div [ class "pb-6 first:pt-4 last:pb-2" ]
                        (([ div [ class "uppercase text-sm font-bold px-6" ]
                                [ View.Components.dateViewer [ class "text-black" ]
                                    identity
                                    loggedIn.shared
                                    (Utils.fromDateTime t1.blockTime)
                                , text " "
                                , View.Components.dateViewer [ class "text-gray-333" ]
                                    (\translations ->
                                        { translations
                                            | today = Just "{{date}}"
                                            , yesterday = Just "{{date}}"
                                            , other = ""
                                        }
                                    )
                                    loggedIn.shared
                                    (Utils.fromDateTime t1.blockTime)
                                ]
                          ]
                            :: List.map
                                (\( transfer, profileSummary ) ->
                                    [ viewTransfer loggedIn transfer profileSummary
                                    , hr [ class "mx-6 border-gray-100" ] []
                                    ]
                                )
                                (first :: rest)
                         )
                            |> List.concat
                        )
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

        addRelativeSelector =
            if loggedIn.hasSeenDashboard then
                identity

            else
                Profile.Summary.withRelativeSelector "#transfer-list-container"
    in
    button
        [ class "flex w-full px-6 py-4 focus-ring ring-inset focus-visible:rounded-sm hover:bg-gray-200"
        , onClick (ClickedTransferCard transfer.id)
        ]
        [ profileSummary
            |> Profile.Summary.withoutName
            |> Profile.Summary.withImageSize "w-8 h-8"
            |> Profile.Summary.withPreventScrolling View.Components.PreventScrollAlways
            |> addRelativeSelector
            |> Profile.Summary.withScrollSelector "#transfer-list-container"
            |> Profile.Summary.view loggedIn.shared.translators
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
                        |> Eos.assetToString loggedIn.shared.translators
                        |> text
                    ]
                ]
            ]
        ]


type alias TransferFiltersFormInput =
    { date : Form.DatePicker.Model
    , direction : Maybe TransferDirectionValue
    , user : Form.UserPicker.SinglePickerModel
    }


transferFiltersForm : LoggedIn.Model -> List Profile.Minimal -> Form.Form msg TransferFiltersFormInput TransfersFilters
transferFiltersForm loggedIn users =
    let
        { t } =
            loggedIn.shared.translators
    in
    Form.succeed TransfersFilters
        |> Form.with
            (Form.DatePicker.init { label = t "payment_history.pick_date", id = "transfer-filters-date-picker" }
                |> Form.DatePicker.withContainerAttrs [ class "mb-10" ]
                |> Form.DatePicker.withAbsolutePositioning False
                |> Form.datePicker
                    { parser = Ok
                    , value = .date
                    , update = \date input -> { input | date = date }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Select.init
                { label = t "transfer.direction.title"
                , id = "transfer-filters-direction-select"
                , optionToString =
                    Maybe.map TransferDirectionValue.toString >> Maybe.withDefault "BOTH"
                }
                |> Form.Select.withOption Nothing (t "transfer.direction.both")
                |> Form.Select.withOption (Just TransferDirectionValue.Sending) (t "transfer.direction.sending")
                |> Form.Select.withOption (Just TransferDirectionValue.Receiving) (t "transfer.direction.receiving")
                |> Form.Select.withContainerAttrs [ class "mb-10" ]
                |> Form.select TransferDirectionValue.fromString
                    { parser = Ok
                    , value = .direction
                    , update = \direction input -> { input | direction = direction }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            ((\{ direction } ->
                let
                    directionText =
                        case direction of
                            Nothing ->
                                "transfer.direction.other_user"

                            Just TransferDirectionValue.Receiving ->
                                "transfer.direction.user_who_sent"

                            Just TransferDirectionValue.Sending ->
                                "transfer.direction.user_who_received"
                in
                Form.UserPicker.init { label = t directionText, currentUser = loggedIn.accountName, profiles = users }
                    |> Form.UserPicker.withMenuClass "max-h-44 overflow-y-auto !relative"
                    |> Form.UserPicker.withModalSelectors True
                    |> Form.userPicker
                        { parser = Ok
                        , value = .user
                        , update = \user input -> { input | user = user }
                        , externalError = always Nothing
                        }
             )
                |> Form.introspect
            )


viewTransferFilters : LoggedIn.Model -> List Profile.Minimal -> Model -> Html Msg
viewTransferFilters ({ shared } as loggedIn) users model =
    let
        { t } =
            shared.translators
    in
    Modal.initWith
        { closeMsg = ClosedTransfersFilters
        , isVisible = model.showTransferFiltersModal
        }
        |> Modal.withHeader (t "all_analysis.filter.title")
        |> Modal.withBody
            [ Form.view []
                shared.translators
                (\submitButton ->
                    [ submitButton [ class "button button-primary w-full" ]
                        [ text (t "all_analysis.filter.apply") ]
                    ]
                )
                (transferFiltersForm loggedIn users)
                model.transferFiltersForm
                { toMsg = GotTransfersFiltersFormMsg
                , onSubmit = SubmittedTransfersFiltersForm
                }
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
            -> Bool
            -> String
            -> String
            -> (List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg)
            -> List (Html.Attribute Msg)
            -> Html Msg
        listItem icon isDisabled iconSize description element attrs =
            li []
                [ element
                    (class "py-2 flex items-center w-full"
                        :: classList
                            [ ( "group hover:text-orange-300 focus-visible:text-orange-300 focus-ring focus:ring-orange-300 focus:ring-opacity-50 focus:ring-offset-4 rounded-sm", not isDisabled )
                            , ( "text-gray-900", isDisabled )
                            ]
                        :: attrs
                    )
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
        [ h1 [ class "text-gray-333 mb-4 md:mb-7" ]
            [ text_ "menu.my_communities"
            , strong [] [ text community.name ]
            ]
        , div
            [ class "bg-white rounded flex flex-col pt-4 pb-2"
            ]
            [ div [ class "flex flex-col px-4 pb-6 border-b border-gray-100" ]
                [ span [ class "text-green text-xl font-bold text-center" ]
                    [ text (Eos.formatSymbolAmount shared.translators balance.asset.symbol balance.asset.amount) ]
                , span [ class "text-gray-900 text-sm font-bold uppercase text-center" ]
                    [ text (tr "dashboard.your_balance" [ ( "symbol", Eos.symbolToSymbolCodeString community.symbol ) ]) ]
                , div [ class "flex space-x-4 mt-4" ]
                    [ View.Components.disablableLink
                        { isDisabled = not loggedIn.hasAcceptedCodeOfConduct }
                        [ class "button button-primary w-full"
                        , classList [ ( "button-disabled", not loggedIn.hasAcceptedCodeOfConduct ) ]
                        , Route.href (Route.Transfer Nothing)
                        ]
                        [ text_ "dashboard.transfer" ]
                    , case community.contributionConfiguration |> Maybe.andThen .paypalAccount of
                        Just _ ->
                            button
                                [ class "button button-secondary w-full"
                                , disabled (not loggedIn.hasAcceptedCodeOfConduct)
                                , onClick ClickedSupportUsButton
                                ]
                                [ text_ "community.index.support_us" ]

                        _ ->
                            text ""
                    ]
                ]
            , ul [ class "px-4 pt-2 divide-y divide-y-gray-100" ]
                [ if community.hasObjectives then
                    listItem Icons.cambiatusCoin
                        (not loggedIn.hasAcceptedCodeOfConduct)
                        "w-5"
                        (tr "dashboard.how_to_earn" [ ( "symbol", Eos.symbolToSymbolCodeString community.symbol ) ])
                        (\attrs ->
                            View.Components.disablableLink
                                { isDisabled = not loggedIn.hasAcceptedCodeOfConduct }
                                (Route.href (Route.CommunityObjectives Route.WithNoObjectiveSelected) :: attrs)
                        )
                        []

                  else
                    text ""
                , listItem Icons.profile
                    (not loggedIn.hasAcceptedCodeOfConduct)
                    "w-5 h-5"
                    (t "dashboard.invite")
                    button
                    [ onClick CreateInvite
                    , disabled (not loggedIn.hasAcceptedCodeOfConduct)
                    , classList [ ( "cursor-default", not loggedIn.hasAcceptedCodeOfConduct ) ]
                    ]
                , if community.hasObjectives then
                    listItem Icons.flagWithoutBackground
                        False
                        "h-5"
                        (t "dashboard.my_claims")
                        a
                        [ Route.href (Route.ProfileClaims (Eos.nameToString loggedIn.accountName)) ]

                  else
                    text ""
                , if community.hasShop then
                    listItem Icons.shop
                        False
                        "w-5 h-5"
                        (t "dashboard.my_offers")
                        a
                        [ Route.href (Route.Shop { owner = Just loggedIn.accountName, categories = [] }) ]

                  else
                    text ""
                , case loggedIn.contributionCount of
                    RemoteData.Success contributionCount ->
                        if contributionCount > 0 then
                            listItem Icons.heartStroke
                                False
                                "w-5 h-5"
                                (t "dashboard.my_contributions")
                                a
                                [ Route.href (Route.ProfileContributions loggedIn.accountName) ]

                        else
                            text ""

                    _ ->
                        text ""
                , listItem Icons.globe
                    False
                    "w-5 h-5"
                    (t "dashboard.about_community")
                    a
                    [ Route.href Route.CommunityAbout
                    ]
                ]
            ]
        , img
            [ class "absolute -top-2 md:top-0 right-2 md:right-4"
            , alt ""
            , src "/images/success-doggo.svg"
            ]
            []
        ]


viewActionsForAnalysisCard : LoggedIn.Model -> Model -> Html Msg
viewActionsForAnalysisCard loggedIn model =
    let
        { t } =
            loggedIn.shared.translators

        text_ =
            text << t
    in
    div [ classList [ ( "md:animate-fade-in-from-above-lg md:animation-delay-150 md:motion-reduce:animate-none", not loggedIn.hasSeenDashboard ) ] ]
        [ h2 [ class "text-gray-333 mt-6 mb-4 md:mb-7 lg:mt-0" ]
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
                            , Markdown.view [ class "mb-4 text-center" ]
                                (Markdown.fromTranslationWithReplacements loggedIn.shared.translators
                                    "dashboard.analysis.count"
                                    [ ( "amount", String.fromInt analysis.count ) ]
                                )
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


viewTimelineCard : LoggedIn.Model -> Bool -> Model -> Html Msg
viewTimelineCard loggedIn isValidator model =
    let
        translators =
            loggedIn.shared.translators

        text_ =
            text << translators.t
    in
    div
        [ classList
            [ ( "md:animate-fade-in-from-above-lg md:motion-reduce:animate-none", not loggedIn.hasSeenDashboard )
            , ( "md:animation-delay-300", not loggedIn.hasSeenDashboard && isValidator )
            , ( "md:animation-delay-150", not loggedIn.hasSeenDashboard && not isValidator )
            ]
        , id "transfer-list-container"
        ]
        [ div [ class "flex justify-between items-center mt-6 mb-4 lg:mt-0" ]
            [ h2 [ class "text-gray-333" ]
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
                    viewTransferList loggedIn model transfers Nothing True

                LoadedGraphql transfers maybePageInfo ->
                    viewTransferList loggedIn model transfers maybePageInfo False
            ]
        ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


type Msg
    = NoOp
    | CompletedLoadCommunity Community.Model
    | CompletedLoadBalance (Result Http.Error (Maybe Balance))
    | CompletedLoadUserTransfers (RemoteData (Graphql.Http.Error (Maybe QueryTransfers)) (Maybe QueryTransfers))
    | ClaimsLoaded (RemoteData (Graphql.Http.Error (Maybe Claim.Paginated)) (Maybe Claim.Paginated))
    | GotTransferCardProfileSummaryMsg Int Profile.Summary.Msg
    | RequestedMoreTransfers
    | ClickedOpenTransferFilters
    | ClosedTransfersFilters
    | GotTransfersFiltersFormMsg (Form.Msg TransferFiltersFormInput)
    | SubmittedTransfersFiltersForm TransfersFilters
    | ClickedTransferCard Int
    | ClickedSupportUsButton
    | CreateInvite
    | GotContactMsg Contact.Msg
    | ClosedAddContactModal
    | CloseInviteModal
    | CompletedInviteCreation (Result Http.Error String)
    | CopyToClipboard String
    | CopiedToClipboard
    | ClosedModalRequestingSponsor
    | ClickedAcceptCodeOfConduct


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model ({ shared, accountName } as loggedIn) =
    case msg of
        NoOp ->
            UR.init model

        CompletedLoadCommunity community ->
            let
                hasContributionConfiguration =
                    case community.contributionConfiguration |> Maybe.andThen .paypalAccount of
                        Just _ ->
                            True

                        Nothing ->
                            False

                markSponsorModalAsSeen =
                    if hasContributionConfiguration then
                        UR.addExt (LoggedIn.UpdatedLoggedIn { loggedIn | shared = { shared | hasSeenSponsorModal = True } })
                            >> UR.addCmd (Ports.storeHasSeenSponsorModal True)

                    else
                        identity

                showModalRequestingSponsor =
                    hasContributionConfiguration && not shared.hasSeenSponsorModal
            in
            UR.init
                { model
                    | balance = RemoteData.Loading
                    , analysis = LoadingGraphql Nothing
                    , showModalRequestingSponsor = showModalRequestingSponsor
                    , showContactModal = not showModalRequestingSponsor && shouldShowContactModal loggedIn model
                }
                |> UR.addCmd (fetchBalance shared accountName community)
                |> UR.addExt (fetchAvailableAnalysis loggedIn Nothing)
                |> UR.addExt (fetchTransfers loggedIn community Nothing model)
                |> markSponsorModalAsSeen

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
                        |> UR.addExt (fetchTransfers loggedIn community maybeCursor model)

                _ ->
                    model
                        |> UR.init

        ClickedOpenTransferFilters ->
            { model | showTransferFiltersModal = True }
                |> UR.init

        ClosedTransfersFilters ->
            { model | showTransferFiltersModal = False }
                |> UR.init

        GotTransfersFiltersFormMsg subMsg ->
            Form.update shared subMsg model.transferFiltersForm
                |> UR.fromChild (\newForm -> { model | transferFiltersForm = newForm })
                    GotTransfersFiltersFormMsg
                    LoggedIn.addFeedback
                    model

        SubmittedTransfersFiltersForm formOutput ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    let
                        newModel =
                            { model
                                | transfersFilters = formOutput
                                , showTransferFiltersModal = False
                                , transfers = LoadingGraphql Nothing
                            }
                    in
                    newModel
                        |> UR.init
                        |> UR.addExt
                            (fetchTransfers loggedIn
                                community
                                Nothing
                                newModel
                            )

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
            { model | showModalRequestingSponsor = True }
                |> UR.init

        CreateInvite ->
            case loggedIn.profile of
                RemoteData.Success profile ->
                    if LoggedIn.hasPermissions profile [ Permission.Invite ] then
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

                    else
                        model
                            |> UR.init
                            |> UR.addExt LoggedIn.ShowInsufficientPermissionsModal

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Tried creating invitation, but profile wasn't loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Dashboard", function = "update" }
                            []

        GotContactMsg subMsg ->
            case LoggedIn.profile loggedIn of
                Just userProfile ->
                    let
                        handleExtMsg extMsg =
                            case extMsg of
                                Contact.GotContacts successMessage contacts _ ->
                                    UR.mapModel (\model_ -> { model_ | showContactModal = False })
                                        >> UR.addExt (LoggedIn.ShowFeedback Feedback.Success successMessage)
                                        >> UR.addExt
                                            ({ userProfile | contacts = contacts }
                                                |> LoggedIn.ProfileLoaded
                                                |> LoggedIn.ExternalBroadcast
                                            )

                                Contact.GotContactsError errorMessage ->
                                    UR.mapModel (\model_ -> { model_ | showContactModal = False })
                                        >> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure errorMessage)

                                Contact.GotMutationRequest selectionSet responseMsg ->
                                    UR.addExt (LoggedIn.mutation loggedIn selectionSet (responseMsg >> GotContactMsg))
                    in
                    Contact.update subMsg
                        model.contactModel
                        loggedIn.shared.translators
                        userProfile.contacts
                        |> UR.fromChild (\newContactModel -> { model | contactModel = newContactModel })
                            GotContactMsg
                            handleExtMsg
                            model

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
                |> UR.addCmd
                    (Browser.Dom.focus "copy-invite-button"
                        |> Task.attempt (\_ -> NoOp)
                    )

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

        ClosedModalRequestingSponsor ->
            let
                newModel =
                    { model | showModalRequestingSponsor = False }
            in
            { newModel | showContactModal = shouldShowContactModal loggedIn newModel }
                |> UR.init

        ClickedAcceptCodeOfConduct ->
            model
                |> UR.init
                |> UR.addExt LoggedIn.ShowCodeOfConductModal



-- HELPERS


shouldShowContactModal : LoggedIn.Model -> Model -> Bool
shouldShowContactModal loggedIn model =
    case loggedIn.profile of
        RemoteData.Success profile ->
            let
                addContactLimitDate =
                    -- 28/01/2022
                    1643374799000

                showContactModalFromDate =
                    addContactLimitDate - Time.posixToMillis loggedIn.shared.now > 0
            in
            showContactModalFromDate
                && List.isEmpty profile.contacts
                && not model.showModalRequestingSponsor

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


fetchTransfers : LoggedIn.Model -> Community.Model -> Maybe String -> Model -> LoggedIn.External Msg
fetchTransfers loggedIn community maybeCursor model =
    LoggedIn.query loggedIn
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


fetchAvailableAnalysis : LoggedIn.Model -> Maybe String -> LoggedIn.External Msg
fetchAvailableAnalysis loggedIn maybeCursor =
    let
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
                                    Present Cambiatus.Enum.Direction.Asc
                            }
                        )
                            |> Cambiatus.InputObject.buildClaimsFilter
                            |> Present
                }
    in
    LoggedIn.query loggedIn
        (Cambiatus.Query.pendingClaims optionalArguments
            (Claim.claimPaginatedSelectionSet loggedIn.shared.now)
        )
        ClaimsLoaded


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityLoaded community ->
            Just (CompletedLoadCommunity community)

        _ ->
            Nothing


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr _ =
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

        GotTransfersFiltersFormMsg subMsg ->
            "GotTransfersFiltersFormMsg" :: Form.msgToString subMsg

        SubmittedTransfersFiltersForm _ ->
            [ "SubmittedTransfersFiltersForm" ]

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

        ClosedModalRequestingSponsor ->
            [ "ClosedModalRequestingSponsor" ]

        ClickedAcceptCodeOfConduct ->
            [ "ClickedAcceptCodeOfConduct" ]
