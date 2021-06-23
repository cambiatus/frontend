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
import Cambiatus.Enum.Direction
import Cambiatus.Query
import Claim
import Community exposing (Balance)
import Eos
import Eos.Account as Eos
import Eos.EosError as EosError
import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Html exposing (Html, a, button, div, img, p, span, text)
import Html.Attributes exposing (class, classList, src)
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
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import Shop
import Time
import Transfer exposing (QueryTransfers, Transfer)
import UpdateResult as UR
import Url
import View.Feedback as Feedback
import View.Form.Input as Input
import View.Modal as Modal



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init ({ shared, accountName, authToken } as loggedIn) =
    ( initModel
    , Cmd.batch
        [ fetchTransfers shared accountName authToken
        , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
        , LoggedIn.maybeInitWith CompletedLoadProfile .profile loggedIn
        ]
    )



-- MODEL


type alias Model =
    { balance : RemoteData Http.Error (Maybe Balance)
    , analysis : GraphqlStatus (Maybe Claim.Paginated) (List ClaimStatus)
    , analysisFilter : Direction
    , profileSummaries : List Claim.ClaimProfileSummaries
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
    { balance = RemoteData.NotAsked
    , analysis = LoadingGraphql
    , analysisFilter = initAnalysisFilter
    , profileSummaries = []
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
                    div [ class "container mx-auto px-4 mb-10" ]
                        [ viewHeader loggedIn community isCommunityAdmin
                        , viewBalance loggedIn model balance
                        , if areObjectivesEnabled && List.any (\account -> account == loggedIn.accountName) community.validators then
                            viewAnalysisList loggedIn model

                          else
                            text ""
                        , viewTransfers loggedIn model
                        , viewInvitationModal loggedIn model
                        , addContactModal shared model
                        ]

                ( RemoteData.Success _, _ ) ->
                    Page.fullPageNotFound (t "dashboard.sorry") ""
    in
    { title = t "menu.dashboard"
    , content = content
    }


viewHeader : LoggedIn.Model -> Community.Model -> Bool -> Html Msg
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
                [ Route.href Route.CommunitySettings
                , class "ml-auto"
                ]
                [ Icons.settings ]

          else
            text ""
        ]


addContactModal : Shared -> Model -> Html Msg
addContactModal shared ({ contactModel } as model) =
    let
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
        , isVisible = model.showContactModal
        }
        |> Modal.withBody
            [ header
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
        LoadingGraphql ->
            Page.fullPageLoading loggedIn.shared

        LoadedGraphql claims _ ->
            div [ class "w-full flex" ]
                [ div
                    [ class "w-full" ]
                    [ div [ class "flex justify-between text-gray-600 text-2xl font-light flex mt-4 mb-4" ]
                        [ div [ class "flex-wrap md:flex" ]
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
            div [] [ Page.fullPageGraphQLError "Failed load" err ]


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
        , Route.externalHref shared transfer.community (Route.ViewTransfer transfer.id)
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
            (viewAmount amount (Eos.symbolToSymbolCodeString transfer.community.symbol))
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
    , div [ class "uppercase text-sm font-extralight mt-3 ml-2 font-sans", class color ] [ text symbol ]
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
                (div [ class "input-label mb-2" ]
                    [ text_ "account.my_wallet.balances.current" ]
                    :: div [ class "flex items-center mb-4" ]
                        [ div [ class "text-indigo-500 font-bold text-3xl" ]
                            [ text balanceText ]
                        , div [ class "text-indigo-500 ml-2" ]
                            [ text symbolText ]
                        ]
                    :: (case loggedIn.selectedCommunity of
                            RemoteData.Success community ->
                                [ a
                                    [ class "button button-primary w-full font-medium mb-2"
                                    , Route.href <| Route.Transfer Nothing
                                    ]
                                    [ text_ "dashboard.transfer" ]
                                , a
                                    [ class "flex w-full items-center justify-between h-12 text-gray-600 border-b"
                                    , Route.href Route.Community
                                    ]
                                    [ text <| shared.translators.tr "dashboard.explore" [ ( "symbol", Eos.symbolToSymbolCodeString community.symbol ) ]
                                    , Icons.arrowDown "rotate--90"
                                    ]
                                ]

                            _ ->
                                []
                       )
                    ++ [ button
                            [ class "flex w-full items-center justify-between h-12 text-gray-600"
                            , onClick CreateInvite
                            ]
                            [ text_ "dashboard.invite", Icons.arrowDown "rotate--90 text-gray-600" ]
                       ]
                )
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
            [ case RemoteData.map .hasObjectives loggedIn.selectedCommunity of
                RemoteData.Success True ->
                    a
                        [ class "flex flex-wrap mr-2 px-4 py-6 rounded bg-white hover:shadow lg:flex-nowrap lg:justify-between lg:items-center lg:mb-6 lg:mr-0"
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
            [ case RemoteData.map .hasShop loggedIn.selectedCommunity of
                RemoteData.Success True ->
                    a
                        [ class "flex flex-wrap ml-2 px-4 py-6 rounded bg-white hover:shadow lg:flex-nowrap lg:justify-between lg:items-center lg:ml-0"
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
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


type Msg
    = NoOp
    | ClosedAuthModal
    | CompletedLoadCommunity Community.Model
    | CompletedLoadProfile Profile.Model
    | CompletedLoadBalance (Result Http.Error (Maybe Balance))
    | CompletedLoadUserTransfers (RemoteData (Graphql.Http.Error (Maybe QueryTransfers)) (Maybe QueryTransfers))
    | ClaimsLoaded (RemoteData (Graphql.Http.Error (Maybe Claim.Paginated)) (Maybe Claim.Paginated))
    | ClaimMsg Int Claim.Msg
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
update msg model ({ shared, accountName } as loggedIn) =
    case msg of
        NoOp ->
            UR.init model

        ClosedAuthModal ->
            { model | claimModalStatus = Claim.Closed }
                |> UR.init

        CompletedLoadCommunity community ->
            UR.init
                { model
                    | balance = RemoteData.Loading
                    , analysis = LoadingGraphql
                }
                |> UR.addCmd (fetchBalance shared accountName community)
                |> UR.addCmd (fetchAvailableAnalysis loggedIn Nothing model.analysisFilter community)

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
                |> UR.logHttpError msg httpError

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
                        |> UR.logImpossible msg [ "balanceNotLoaded" ]

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

        LoggedIn.ProfileLoaded profile ->
            Just (CompletedLoadProfile profile)

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

        CompletedLoadProfile _ ->
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
