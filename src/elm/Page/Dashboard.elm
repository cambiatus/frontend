module Page.Dashboard exposing
    ( Model
    , Msg
    , init
    , jsAddressToMsg
    , msgToString
    , subscriptions
    , update
    , view
    )

import Api
import Api.Graphql
import Api.Relay
import Cambiatus.Enum.Direction
import Cambiatus.Query
import Claim
import Community exposing (Balance)
import Eos exposing (Symbol)
import Eos.Account as Eos
import Eos.EosError as EosError
import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Html exposing (Html, a, button, div, img, input, p, span, text)
import Html.Attributes exposing (class, classList, id, src, style, type_, value)
import Html.Events exposing (onClick)
import Http
import Icons
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import List.Extra as List
import Page
import Profile
import Profile.Contact as Contact
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..), FeatureStatus(..))
import Session.Shared exposing (Shared)
import Shop
import Task
import Time exposing (Posix)
import Transfer exposing (QueryTransfers, Transfer)
import UpdateResult as UR
import Url
import View.Feedback as Feedback
import View.Modal as Modal



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init ({ shared, accountName, selectedCommunity, authToken } as loggedIn) =
    ( initModel
    , Cmd.batch
        [ fetchBalance shared accountName
        , fetchTransfers shared accountName authToken
        , fetchCommunity shared selectedCommunity authToken
        , fetchAvailableAnalysis loggedIn Nothing initAnalysisFilter
        , Task.perform GotTime Time.now
        ]
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch []



-- MODEL


type alias Model =
    { date : Maybe Posix
    , community : GraphqlStatus (Maybe Community.DashboardInfo) Community.DashboardInfo
    , balance : Status Balance
    , analysis : GraphqlStatus (Maybe Claim.Paginated) (List ClaimStatus)
    , analysisFilter : Direction
    , lastSocket : String
    , transfers : GraphqlStatus (Maybe QueryTransfers) (List Transfer)
    , contactModel : Contact.Model
    , showContactModal : Bool
    , inviteModalStatus : InviteModalStatus
    , claimModalStatus : Claim.ModalStatus
    , copied : Bool
    }


initModel : Model
initModel =
    { date = Nothing
    , community = LoadingGraphql
    , balance = Loading
    , analysis = LoadingGraphql
    , analysisFilter = initAnalysisFilter
    , lastSocket = ""
    , transfers = LoadingGraphql
    , contactModel = Contact.initSingle
    , showContactModal = False
    , inviteModalStatus = InviteModalClosed
    , claimModalStatus = Claim.Closed
    , copied = False
    }


initAnalysisFilter : Direction
initAnalysisFilter =
    DESC


type Status a
    = Loading
    | Loaded a
    | NotFound
    | Failed Http.Error


type GraphqlStatus err a
    = LoadingGraphql
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
            case model.community of
                LoadedGraphql community _ ->
                    community.creator == accountName

                _ ->
                    False

        areObjectivesEnabled =
            case model.community of
                LoadedGraphql community _ ->
                    community.hasObjectives == True

                _ ->
                    False

        content =
            case ( model.balance, loggedIn.profile, model.community ) of
                ( Loading, _, _ ) ->
                    Page.fullPageLoading shared

                ( Failed e, _, _ ) ->
                    Page.fullPageError (t "dashboard.sorry") e

                ( Loaded balance, LoggedIn.Loaded profile, LoadedGraphql community _ ) ->
                    div [ class "container mx-auto px-4 mb-10" ]
                        [ viewHeader loggedIn community isCommunityAdmin
                        , viewBalance loggedIn model balance
                        , if areObjectivesEnabled && List.any (\account -> account == loggedIn.accountName) community.validators then
                            viewAnalysisList loggedIn model

                          else
                            text ""
                        , viewTransfers loggedIn model
                        , viewInvitationModal loggedIn model
                        , addContactModal shared profile model
                        ]

                ( _, _, _ ) ->
                    Page.fullPageNotFound (t "dashboard.sorry") ""
    in
    { title = t "menu.dashboard"
    , content = content
    }


viewHeader : LoggedIn.Model -> Community.DashboardInfo -> Bool -> Html Msg
viewHeader loggedIn community isCommunityAdmin =
    div [ class "flex inline-block text-gray-600 font-light mt-6 mb-5" ]
        [ div []
            [ text (loggedIn.shared.translators.t "menu.my_communities")
            , span [ class "text-indigo-500 font-medium" ]
                [ text community.name
                ]
            ]
        , if isCommunityAdmin then
            a
                [ Route.href (Route.CommunitySettings loggedIn.selectedCommunity)
                , class "ml-auto"
                ]
                [ Icons.settings ]

          else
            text ""
        ]


addContactModal : Shared -> Profile.Model -> Model -> Html Msg
addContactModal shared profile ({ contactModel } as model) =
    let
        addContactLimitDate =
            -- 01/01/2022
            1641006000000

        showContactModalFromDate =
            addContactLimitDate - Time.posixToMillis shared.now > 0

        text_ s =
            shared.translators.t s
                |> text

        header =
            div [ class "mt-4" ]
                [ p [ class "inline bg-purple-100 text-white rounded-full py-0.5 px-2 text-caption uppercase" ]
                    [ text_ "contact_modal.new" ]
                , p [ class "text-heading font-bold mt-2" ]
                    [ text_ "contact_modal.title" ]
                ]

        form =
            Contact.view shared.translators contactModel
                |> Html.map GotContactMsg
    in
    Modal.initWith
        { closeMsg = ClosedAddContactModal
        , isVisible = model.showContactModal && showContactModalFromDate && List.isEmpty profile.contacts
        }
        |> Modal.withBody
            [ header
            , img [ class "mx-auto mt-10", src "/images/girl-with-phone.svg" ] []
            , form
            , p [ class "text-caption text-center uppercase my-4" ]
                [ text_ "contact_modal.footer" ]
            ]
        |> Modal.withLarge True
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
                    , input
                        [ type_ "text"
                        , class "absolute opacity-0"
                        , style "left" "-9999em"
                        , id "invitation-id"
                        , value (url invitationId)
                        ]
                        []
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
        LoadingGraphql ->
            Page.fullPageLoading loggedIn.shared

        LoadedGraphql claims _ ->
            div [ class "w-full flex" ]
                [ div
                    [ class "w-full" ]
                    [ div [ class "flex justify-between text-gray-600 text-2xl font-light flex mt-4 mb-4" ]
                        [ div [ class "flex" ]
                            [ div [ class "text-indigo-500 mr-2 font-medium" ]
                                [ text_ "dashboard.analysis.title.1"
                                ]
                            , text_ "dashboard.analysis.title.2"
                            ]
                        , div [ class "flex justify-between space-x-4" ]
                            [ button
                                [ class "w-full button button-secondary"
                                , onClick ToggleAnalysisSorting
                                ]
                                [ Icons.sortDirection ""
                                ]
                            , a
                                [ class "button button-secondary font-medium "
                                , Route.href (Route.Analysis Nothing)
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
                                List.map (\c -> viewAnalysis loggedIn c model.date) claims
                        in
                        div [ class "flex flex-wrap -mx-2" ] <|
                            List.append pendingClaims
                                [ viewVoteConfirmationModal loggedIn model ]
                    ]
                ]

        FailedGraphql err ->
            div [] [ Page.fullPageGraphQLError "Failed load" err ]


viewVoteConfirmationModal : LoggedIn.Model -> Model -> Html Msg
viewVoteConfirmationModal loggedIn { claimModalStatus, date } =
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
    case claimModalStatus of
        Claim.VoteConfirmationModal claimId vote ->
            viewVoteModal claimId vote False

        Claim.Loading claimId vote ->
            viewVoteModal claimId vote True

        Claim.PhotoModal claim ->
            Claim.viewPhotoModal loggedIn claim date
                |> Html.map ClaimMsg

        Claim.Closed ->
            text ""


viewAnalysis : LoggedIn.Model -> ClaimStatus -> Maybe Time.Posix -> Html Msg
viewAnalysis loggedIn claimStatus now =
    case claimStatus of
        ClaimLoaded claim ->
            Claim.viewClaimCard loggedIn claim now Nothing
                |> Html.map ClaimMsg

        ClaimLoading _ ->
            div [ class "w-full md:w-1/2 lg:w-1/3 xl:w-1/4 px-2 mb-4" ]
                [ div [ class "rounded-lg bg-white h-56 my-2 pt-8" ]
                    [ Page.fullPageLoading loggedIn.shared ]
                ]

        ClaimVoted _ ->
            text ""

        ClaimVoteFailed claim ->
            Claim.viewClaimCard loggedIn claim now Nothing
                |> Html.map ClaimMsg


viewTransfers : LoggedIn.Model -> Model -> Html Msg
viewTransfers loggedIn model =
    let
        t =
            loggedIn.shared.translators.t
    in
    div [ class "mt-4" ]
        [ div [ class "text-2xl text-indigo-500 mr-2 font-medium mb-4" ]
            [ text <| t "transfer.last_title"
            ]
        , case model.transfers of
            LoadingGraphql ->
                Page.viewCardEmpty
                    [ div [ class "text-gray-900 text-sm" ]
                        [ text (t "menu.loading") ]
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

            LoadedGraphql transfers _ ->
                div [ class "rounded-lg bg-white" ]
                    (List.map (\transfer -> viewTransfer loggedIn transfer) transfers)
        ]


viewTransfer : LoggedIn.Model -> Transfer -> Html msg
viewTransfer ({ shared } as loggedIn) transfer =
    let
        isReceive =
            loggedIn.accountName == transfer.to.account

        amount =
            if isReceive then
                transfer.value

            else
                transfer.value * -1

        description =
            if isReceive then
                [ ( "user", Eos.nameToString transfer.from.account )
                , ( "amount", String.fromFloat transfer.value )
                ]
                    |> shared.translators.tr "notifications.transfer.receive"

            else
                [ ( "user", Eos.nameToString transfer.to.account )
                , ( "amount", String.fromFloat transfer.value )
                ]
                    |> shared.translators.tr "notifications.transfer.sent"
    in
    a
        [ class "flex items-start lg:items-center p-4 border-b last:border-b-0"
        , Route.href (Route.ViewTransfer transfer.id)
        ]
        [ div [ class "flex-col flex-grow-1 pl-4" ]
            [ p
                [ class "text-black text-sm leading-relaxed" ]
                [ text description ]
            , p
                [ class "text-gray-900 text-caption uppercase" ]
                [ text (Maybe.withDefault "" transfer.memo) ]
            ]
        , div [ class "flex flex-none pl-4" ]
            (viewAmount amount (Eos.symbolToSymbolCodeString transfer.symbol))
        ]


viewAmount : Float -> String -> List (Html msg)
viewAmount amount symbol =
    let
        amountText =
            FormatNumber.format usLocale amount

        color =
            if amount > 0 then
                "text-green"

            else
                "text-red"
    in
    [ div [ class "text-2xl", class color ] [ text amountText ]
    , div [ class "uppercase text-sm font-thin mt-3 ml-2 font-sans", class color ] [ text symbol ]
    ]


viewBalance : LoggedIn.Model -> Model -> Balance -> Html Msg
viewBalance ({ shared } as loggedIn) _ balance =
    let
        text_ =
            text << shared.translators.t

        symbolText =
            Eos.symbolToSymbolCodeString balance.asset.symbol

        balanceText =
            String.fromFloat balance.asset.amount ++ " "
    in
    div [ class "flex-wrap flex lg:space-x-3" ]
        [ div [ class "flex w-full lg:w-1/3 bg-white rounded h-64 p-4" ]
            [ div [ class "w-full" ]
                [ div [ class "input-label mb-2" ]
                    [ text_ "account.my_wallet.balances.current" ]
                , div [ class "flex items-center mb-4" ]
                    [ div [ class "text-indigo-500 font-bold text-3xl" ]
                        [ text balanceText ]
                    , div [ class "text-indigo-500 ml-2" ]
                        [ text symbolText ]
                    ]
                , a
                    [ class "button button-primary w-full font-medium mb-2"
                    , Route.href <| Route.Transfer loggedIn.selectedCommunity Nothing
                    ]
                    [ text_ "dashboard.transfer" ]
                , a
                    [ class "flex w-full items-center justify-between h-12 text-gray-600 border-b"
                    , Route.href <| Route.Community loggedIn.selectedCommunity
                    ]
                    [ text <| shared.translators.tr "dashboard.explore" [ ( "symbol", Eos.symbolToSymbolCodeString loggedIn.selectedCommunity ) ]
                    , Icons.arrowDown "rotate--90"
                    ]
                , button
                    [ class "flex w-full items-center justify-between h-12 text-gray-600"
                    , onClick CreateInvite
                    ]
                    [ text_ "dashboard.invite", Icons.arrowDown "rotate--90 text-gray-600" ]
                ]
            ]
        , div [ class "w-full lg:w-1/3 mt-4 lg:mt-0" ]
            [ viewQuickLinks loggedIn
            ]
        ]


viewQuickLinks : LoggedIn.Model -> Html Msg
viewQuickLinks ({ shared } as loggedIn) =
    let
        t =
            shared.translators.t
    in
    div [ class "flex-wrap flex" ]
        [ div [ class "w-1/2 lg:w-full" ]
            [ case loggedIn.hasObjectives of
                FeatureLoaded True ->
                    a
                        [ class "flex flex-wrap mr-2 px-4 py-6 rounded bg-white hover:shadow lg:flex-no-wrap lg:justify-between lg:items-center lg:mb-6 lg:mr-0"
                        , Route.href (Route.ProfileClaims (Eos.nameToString loggedIn.accountName))
                        ]
                        [ div []
                            [ div [ class "w-full mb-4" ] [ Icons.claims "w-8 h-8" ]
                            , p [ class "w-full h-12 lg:h-auto text-gray-600 mb-4 lg:mb-0" ]
                                [ text <| t "dashboard.my_claims.1"
                                , span [ class "font-bold" ] [ text <| t "dashboard.my_claims.2" ]
                                ]
                            ]
                        , div [ class "w-full lg:w-1/3 button button-primary" ] [ text <| t "dashboard.my_claims.go" ]
                        ]

                _ ->
                    text ""
            ]
        , div [ class "w-1/2 lg:w-full" ]
            [ case loggedIn.hasShop of
                FeatureLoaded True ->
                    a
                        [ class "flex flex-wrap ml-2 px-4 py-6 rounded bg-white hover:shadow lg:flex-no-wrap lg:justify-between lg:items-center lg:ml-0"
                        , Route.href (Route.Shop Shop.UserSales)
                        ]
                        [ div []
                            [ div [ class "w-full mb-4 lg:mb-2" ] [ Icons.shop "w-8 h-8 fill-current" ]
                            , p [ class "w-full h-12 lg:h-auto text-gray-600 mb-4 lg:mb-0" ]
                                [ text <| t "dashboard.my_offers.1"
                                , span [ class "font-bold" ] [ text <| t "dashboard.my_offers.2" ]
                                ]
                            ]
                        , div [ class "w-full lg:w-1/3 button button-primary" ] [ text <| t "dashboard.my_offers.go" ]
                        ]

                _ ->
                    text ""
            ]
        ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = GotTime Posix
    | CompletedLoadBalances (Result Http.Error (List Balance))
    | CompletedLoadUserTransfers (RemoteData (Graphql.Http.Error (Maybe QueryTransfers)) (Maybe QueryTransfers))
    | ClaimsLoaded (RemoteData (Graphql.Http.Error (Maybe Claim.Paginated)) (Maybe Claim.Paginated))
    | CommunityLoaded (RemoteData (Graphql.Http.Error (Maybe Community.DashboardInfo)) (Maybe Community.DashboardInfo))
    | ClaimMsg Claim.Msg
    | VoteClaim Claim.ClaimId Bool
    | GotVoteResult Claim.ClaimId (Result (Maybe Value) String)
    | CreateInvite
    | GotContactMsg Contact.Msg
    | ClosedAddContactModal
    | CloseInviteModal
    | CompletedInviteCreation (Result Http.Error String)
    | CopyToClipboard String
    | CopiedToClipboard
    | ToggleAnalysisSorting


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        GotTime date ->
            UR.init { model | date = Just date }

        CompletedLoadBalances (Ok balances) ->
            let
                findBalance balance =
                    balance.asset.symbol == loggedIn.selectedCommunity

                -- Try to find the balance, if not found default to the first balance
                statusBalance =
                    case List.find findBalance balances of
                        Just b ->
                            Loaded b

                        Nothing ->
                            case List.head balances of
                                Just b ->
                                    Loaded b

                                Nothing ->
                                    NotFound
            in
            UR.init
                { model
                    | balance = statusBalance
                    , showContactModal = True
                }

        CompletedLoadBalances (Err httpError) ->
            UR.init { model | balance = Failed httpError }
                |> UR.logHttpError msg httpError

        ClaimsLoaded (RemoteData.Success claims) ->
            let
                wrappedClaims =
                    List.map ClaimLoaded (Claim.paginatedToList claims)
            in
            case model.analysis of
                LoadedGraphql existingClaims _ ->
                    { model | analysis = LoadedGraphql (existingClaims ++ wrappedClaims) (Claim.paginatedPageInfo claims) }
                        |> UR.init

                _ ->
                    { model | analysis = LoadedGraphql wrappedClaims (Claim.paginatedPageInfo claims) }
                        |> UR.init

        ClaimsLoaded (RemoteData.Failure err) ->
            { model | analysis = FailedGraphql err }
                |> UR.init
                |> UR.logGraphqlError msg err

        ClaimsLoaded _ ->
            UR.init model

        CompletedLoadUserTransfers (RemoteData.Success maybeTransfers) ->
            { model | transfers = LoadedGraphql (Transfer.getTransfers maybeTransfers) Nothing }
                |> UR.init

        CompletedLoadUserTransfers (RemoteData.Failure err) ->
            { model | transfers = FailedGraphql err }
                |> UR.init
                |> UR.logGraphqlError msg err

        CompletedLoadUserTransfers _ ->
            UR.init model

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
                                    case pageInfo of
                                        Just page ->
                                            if page.hasNextPage then
                                                fetchAvailableAnalysis loggedIn page.endCursor model.analysisFilter

                                            else
                                                Cmd.none

                                        Nothing ->
                                            Cmd.none
                            in
                            { model
                                | analysis = LoadedGraphql (setClaimStatus claims claimId ClaimVoted) pageInfo
                            }
                                |> UR.init
                                |> UR.addExt (ShowFeedback Feedback.Success (message value))
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
                    { model | analysis = LoadedGraphql (setClaimStatus claims claimId ClaimVoteFailed) pageInfo }
                        |> UR.init
                        |> UR.addExt (ShowFeedback Feedback.Failure errorMessage)

                _ ->
                    model |> UR.init

        CommunityLoaded (RemoteData.Success community) ->
            case community of
                Just c ->
                    { model | community = LoadedGraphql c Nothing }
                        |> UR.init

                Nothing ->
                    model
                        |> UR.init

        CommunityLoaded (RemoteData.Failure err) ->
            { model | community = FailedGraphql err }
                |> UR.init
                |> UR.logGraphqlError msg err

        CommunityLoaded _ ->
            UR.init model

        CreateInvite ->
            case model.balance of
                Loaded b ->
                    UR.init
                        { model | inviteModalStatus = InviteModalLoading }
                        |> UR.addCmd
                            (CompletedInviteCreation
                                |> Api.communityInvite loggedIn.shared b.asset.symbol loggedIn.accountName
                            )

                _ ->
                    UR.init model

        GotContactMsg subMsg ->
            case LoggedIn.profile loggedIn of
                Just userProfile ->
                    let
                        ( contactModel, cmd, contactResponse ) =
                            Contact.update subMsg
                                model.contactModel
                                loggedIn.shared
                                loggedIn.authToken

                        addContactResponse model_ =
                            case contactResponse of
                                Contact.NotAsked ->
                                    model_
                                        |> UR.init

                                Contact.WithError errorMessage ->
                                    { model_ | showContactModal = False }
                                        |> UR.init
                                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure errorMessage)

                                Contact.WithContacts successMessage contacts ->
                                    let
                                        newProfile =
                                            { userProfile | contacts = contacts }
                                    in
                                    { model_ | showContactModal = False }
                                        |> UR.init
                                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Success successMessage)
                                        |> UR.addExt (LoggedIn.UpdatedLoggedIn { loggedIn | profile = LoggedIn.Loaded newProfile })
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
                |> UR.logHttpError msg httpError

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
                        , analysis = LoadingGraphql
                    }
            in
            newModel
                |> UR.init
                |> UR.addCmd
                    (fetchAvailableAnalysis loggedIn Nothing newModel.analysisFilter)



-- HELPERS


fetchBalance : Shared -> Eos.Name -> Cmd Msg
fetchBalance shared accountName =
    Api.getBalances shared accountName CompletedLoadBalances


fetchTransfers : Shared -> Eos.Name -> String -> Cmd Msg
fetchTransfers shared accountName authToken =
    Api.Graphql.query shared
        (Just authToken)
        (Transfer.transfersUserQuery
            accountName
            (\args ->
                { args | first = Present 10 }
            )
        )
        CompletedLoadUserTransfers


fetchAvailableAnalysis : LoggedIn.Model -> Maybe String -> Direction -> Cmd Msg
fetchAvailableAnalysis { shared, selectedCommunity, authToken } maybeCursor direction =
    let
        arg =
            { communityId = Eos.symbolToString selectedCommunity
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
                        Maybe.andThen
                            (\s ->
                                if String.isEmpty s then
                                    Nothing

                                else
                                    Just (Present s)
                            )
                            maybeCursor
                            |> Maybe.withDefault Absent
                    , filter =
                        Present
                            { direction =
                                case direction of
                                    ASC ->
                                        Present Cambiatus.Enum.Direction.Asc

                                    DESC ->
                                        Present Cambiatus.Enum.Direction.Desc
                            }
                }
    in
    Api.Graphql.query shared
        (Just authToken)
        (Cambiatus.Query.claimsAnalysis optionalArguments arg Claim.claimPaginatedSelectionSet)
        ClaimsLoaded


fetchCommunity : Shared -> Symbol -> String -> Cmd Msg
fetchCommunity shared selectedCommunity authToken =
    Api.Graphql.query shared
        (Just authToken)
        (Cambiatus.Query.community { symbol = Eos.symbolToString selectedCommunity } Community.dashboardSelectionSet)
        CommunityLoaded


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
        GotTime _ ->
            [ "GotTime" ]

        CompletedLoadBalances result ->
            [ "CompletedLoadBalances", UR.resultToString result ]

        CompletedLoadUserTransfers result ->
            [ "CompletedLoadUserTransfers", UR.remoteDataToString result ]

        ClaimsLoaded result ->
            [ "ClaimsLoaded", UR.remoteDataToString result ]

        ClaimMsg _ ->
            [ "ClaimMsg" ]

        VoteClaim claimId _ ->
            [ "VoteClaim", String.fromInt claimId ]

        GotVoteResult _ result ->
            [ "GotVoteResult", UR.resultToString result ]

        CommunityLoaded result ->
            [ "CommunityLoaded", UR.remoteDataToString result ]

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
