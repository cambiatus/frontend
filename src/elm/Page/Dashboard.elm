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
import Cambiatus.Query
import Claim
import Community exposing (Balance)
import Eos exposing (Symbol)
import Eos.Account as Eos
import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Html exposing (Html, a, button, div, img, input, p, span, text)
import Html.Attributes exposing (class, classList, id, src, style, type_, value)
import Html.Events exposing (onClick)
import Http
import I18Next exposing (Delims(..))
import Icons
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import List.Extra as List
import Page
import Profile
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import Task
import Time exposing (Posix)
import Transfer exposing (QueryTransfers, Transfer)
import UpdateResult as UR
import Url
import View.Modal as Modal



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init ({ shared, accountName, selectedCommunity } as loggedIn) =
    ( initModel
    , Cmd.batch
        [ fetchBalance shared accountName
        , fetchTransfers shared accountName
        , fetchCommunity shared selectedCommunity
        , fetchAvailableAnalysis loggedIn Nothing
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
    , lastSocket : String
    , transfers : GraphqlStatus (Maybe QueryTransfers) (List Transfer)
    , inviteModalStatus : InviteModalStatus
    , voteModalStatus : VoteModalStatus
    , copied : Bool
    }


initModel : Model
initModel =
    { date = Nothing
    , community = LoadingGraphql
    , balance = Loading
    , analysis = LoadingGraphql
    , lastSocket = ""
    , transfers = LoadingGraphql
    , inviteModalStatus = InviteModalClosed
    , voteModalStatus = VoteModalClosed
    , copied = False
    }


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


type VoteModalStatus
    = VoteModalClosed
    | VoteModalOpened Int Bool


type InviteModalStatus
    = InviteModalClosed
    | InviteModalLoading
    | InviteModalFailed String
    | InviteModalLoaded String



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        t s =
            I18Next.t loggedIn.shared.translations s

        isCommunityAdmin =
            case model.community of
                LoadedGraphql community _ ->
                    community.creator == loggedIn.accountName

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
                    Page.fullPageLoading

                ( Failed e, _, _ ) ->
                    Page.fullPageError (t "dashboard.sorry") e

                ( Loaded balance, LoggedIn.Loaded profile, LoadedGraphql community _ ) ->
                    div [ class "container mx-auto px-4 mb-10" ]
                        [ div [ class "flex inline-block text-gray-600 font-light mt-6 mb-5" ]
                            [ div []
                                [ text (t "menu.my_communities")
                                , span [ class "text-indigo-500 font-medium" ]
                                    [ text community.name
                                    ]
                                ]
                            , if isCommunityAdmin then
                                a [ Route.href (Route.CommunitySettings loggedIn.selectedCommunity), class "ml-auto" ] [ Icons.settings ]

                              else
                                text ""
                            ]
                        , viewBalance loggedIn model balance
                        , if areObjectivesEnabled then
                            viewAnalysisList loggedIn profile model

                          else
                            text ""
                        , viewTransfers loggedIn model
                        , let
                            viewVoteModal claimId isApproving isLoading =
                                Claim.viewVoteClaimModal
                                    loggedIn.shared.translators
                                    { voteMsg = VoteClaim
                                    , closeMsg = CloseModal
                                    , claimId = claimId
                                    , isApproving = isApproving
                                    , isInProgress = isLoading
                                    }
                          in
                          case model.voteModalStatus of
                            VoteModalOpened claimId vote ->
                                viewVoteModal claimId vote False

                            VoteModalClosed ->
                                text ""
                        , viewInvitationModal loggedIn model
                        ]

                ( _, _, _ ) ->
                    Page.fullPageNotFound (t "dashboard.sorry") ""
    in
    { title = t "menu.dashboard"
    , content = content
    }


viewInvitationModal : LoggedIn.Model -> Model -> Html Msg
viewInvitationModal { shared } model =
    let
        t s =
            I18Next.t shared.translations s

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
                        , class "button w-full md:w-48"
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


viewAnalysisList : LoggedIn.Model -> Profile.Profile -> Model -> Html Msg
viewAnalysisList loggedIn profile model =
    let
        text_ s =
            text (I18Next.t loggedIn.shared.translations s)

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
            Page.fullPageLoading

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
                        , a
                            [ class "button button-secondary font-medium h-8 w-20"
                            , Route.href Route.Analysis
                            ]
                            [ text_ "dashboard.analysis.all" ]
                        ]
                    , if isVoted claims || profile.analysisCount < 0 then
                        div [ class "flex flex-col w-full items-center justify-center px-3 py-12 my-2 rounded-lg bg-white" ]
                            [ img [ src "/images/not_found.svg", class "object-contain h-32 mb-3" ] []
                            , p [ class "flex text-body text-gray" ]
                                [ p [ class "font-bold" ] [ text_ "dashboard.analysis.empty.1" ]
                                , text_ "dashboard.analysis.empty.2"
                                ]
                            , p [ class "text-body text-gray" ] [ text_ "dashboard.analysis.empty.3" ]
                            ]

                      else
                        div [ class "flex flex-wrap -mx-2" ]
                            (List.map (viewAnalysis loggedIn) claims)
                    ]
                ]

        FailedGraphql err ->
            div [] [ Page.fullPageGraphQLError "Failed load" err ]


viewAnalysis : LoggedIn.Model -> ClaimStatus -> Html Msg
viewAnalysis ({ shared, selectedCommunity } as loggedIn) claimStatus =
    case claimStatus of
        ClaimLoaded claim ->
            Claim.viewClaimCard loggedIn OpenModal claim

        ClaimLoading _ ->
            div [ class "w-full md:w-1/2 lg:w-1/3 xl:w-1/4 px-2 mb-4" ]
                [ div [ class "rounded-lg bg-white h-56 my-2" ] [ Page.fullPageLoading ]
                ]

        ClaimVoted _ ->
            text ""

        ClaimVoteFailed _ ->
            div [ class "text-red" ] [ text "failed" ]


viewTransfers : LoggedIn.Model -> Model -> Html Msg
viewTransfers loggedIn model =
    let
        t s =
            I18Next.t loggedIn.shared.translations s
    in
    div []
        [ Page.viewTitle (t "transfer.last_title")
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
                    |> I18Next.tr shared.translations I18Next.Curly "notifications.transfer.receive"

            else
                [ ( "user", Eos.nameToString transfer.to.account )
                , ( "amount", String.fromFloat transfer.value )
                ]
                    |> I18Next.tr shared.translations I18Next.Curly "notifications.transfer.sent"
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
viewBalance loggedIn _ balance =
    let
        text_ s =
            text (I18Next.t loggedIn.shared.translations s)

        symbolText =
            Eos.symbolToSymbolCodeString balance.asset.symbol

        balanceText =
            String.fromFloat balance.asset.amount ++ " "
    in
    div [ class "flex w-full lg:w-1/3 bg-white rounded h-64 p-4" ]
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
                [ class "flex w-full items-center justify-between h-12 text-gray border-b"
                , Route.href <| Route.Community loggedIn.selectedCommunity
                ]
                [ text_ "dashboard.explore", Icons.arrowDown "rotate--90" ]
            , button
                [ class "flex w-full items-center justify-between h-12 text-gray"
                , onClick CreateInvite
                ]
                [ text_ "dashboard.invite", Icons.arrowDown "rotate--90" ]
            ]
        ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = GotTime Posix
    | CompletedLoadBalances (Result Http.Error (List Balance))
    | CompletedLoadUserTransfers (Result (Graphql.Http.Error (Maybe QueryTransfers)) (Maybe QueryTransfers))
    | ClaimsLoaded (Result (Graphql.Http.Error (Maybe Claim.Paginated)) (Maybe Claim.Paginated))
    | CommunityLoaded (Result (Graphql.Http.Error (Maybe Community.DashboardInfo)) (Maybe Community.DashboardInfo))
    | OpenModal Int Bool
    | CloseModal
    | VoteClaim Int Bool
    | GotVoteResult Int (Result Value String)
    | CreateInvite
    | CloseInviteModal
    | CompletedInviteCreation (Result Http.Error String)
    | CopyToClipboard String
    | CopiedToClipboard


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
                }

        CompletedLoadBalances (Err httpError) ->
            UR.init { model | balance = Failed httpError }
                |> UR.logHttpError msg httpError

        ClaimsLoaded (Ok claims) ->
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

        ClaimsLoaded (Err err) ->
            { model | analysis = FailedGraphql err }
                |> UR.init
                |> UR.logGraphqlError msg err

        CompletedLoadUserTransfers (Ok maybeTransfers) ->
            { model | transfers = LoadedGraphql (Transfer.getTransfers maybeTransfers) Nothing }
                |> UR.init

        CompletedLoadUserTransfers (Err err) ->
            { model | transfers = FailedGraphql err }
                |> UR.init
                |> UR.logGraphqlError msg err

        OpenModal claimId vote ->
            { model | voteModalStatus = VoteModalOpened claimId vote }
                |> UR.init

        CloseModal ->
            { model | voteModalStatus = VoteModalClosed }
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
                                , voteModalStatus = VoteModalClosed
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
            case model.analysis of
                LoadedGraphql claims pageInfo ->
                    let
                        maybeClaim : Maybe Claim.Model
                        maybeClaim =
                            findClaim claims claimId

                        message val =
                            [ ( "value", val ) ]
                                |> I18Next.tr loggedIn.shared.translations I18Next.Curly "claim.reward"
                    in
                    case maybeClaim of
                        Just claim ->
                            let
                                value =
                                    String.fromFloat claim.action.verifierReward
                                        ++ " "
                                        ++ Eos.symbolToString claim.action.objective.community.symbol

                                cmd =
                                    case pageInfo of
                                        Just page ->
                                            if page.hasNextPage then
                                                fetchAvailableAnalysis loggedIn page.endCursor

                                            else
                                                Cmd.none

                                        Nothing ->
                                            Cmd.none
                            in
                            { model
                                | analysis = LoadedGraphql (setClaimStatus claims claimId ClaimVoted) pageInfo
                            }
                                |> UR.init
                                |> UR.addExt (ShowFeedback LoggedIn.Success (message value))
                                |> UR.addCmd cmd

                        Nothing ->
                            model
                                |> UR.init

                _ ->
                    model |> UR.init

        GotVoteResult claimId (Err _) ->
            case model.analysis of
                LoadedGraphql claims pageInfo ->
                    { model | analysis = LoadedGraphql (setClaimStatus claims claimId ClaimVoteFailed) pageInfo }
                        |> UR.init

                _ ->
                    model |> UR.init

        CommunityLoaded (Ok community) ->
            case community of
                Just c ->
                    { model | community = LoadedGraphql c Nothing }
                        |> UR.init

                Nothing ->
                    model
                        |> UR.init

        CommunityLoaded (Err err) ->
            { model | community = FailedGraphql err }
                |> UR.init
                |> UR.logGraphqlError msg err

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
                { model | inviteModalStatus = InviteModalFailed (I18Next.t loggedIn.shared.translations "community.invite.failed") }
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



-- HELPERS


fetchBalance : Shared -> Eos.Name -> Cmd Msg
fetchBalance shared accountName =
    Api.getBalances shared accountName CompletedLoadBalances


fetchTransfers : Shared -> Eos.Name -> Cmd Msg
fetchTransfers shared accountName =
    Api.Graphql.query shared
        (Transfer.transfersUserQuery
            accountName
            (\args ->
                { args | first = Present 10 }
            )
        )
        CompletedLoadUserTransfers


fetchAvailableAnalysis : LoggedIn.Model -> Maybe String -> Cmd Msg
fetchAvailableAnalysis { shared, accountName, selectedCommunity } maybeCursor =
    let
        arg =
            { input =
                { symbol = Eos.symbolToString selectedCommunity
                , account = Eos.nameToString accountName
                }
            }

        pagination =
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
                }
    in
    Api.Graphql.query
        shared
        (Cambiatus.Query.claimsAnalysis pagination arg Claim.claimPaginatedSelectionSet)
        ClaimsLoaded


fetchCommunity : Shared -> Symbol -> Cmd Msg
fetchCommunity shared selectedCommunity =
    Api.Graphql.query
        shared
        (Cambiatus.Query.community { symbol = Eos.symbolToString selectedCommunity } Community.dashboardSelectionSet)
        CommunityLoaded


setClaimStatus : List ClaimStatus -> Int -> (Claim.Model -> ClaimStatus) -> List ClaimStatus
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


findClaim : List ClaimStatus -> Int -> Maybe Claim.Model
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
                    , Decode.succeed (Err Encode.null)
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
    let
        resultToString ss r =
            case r of
                Ok _ ->
                    ss ++ [ "Ok" ]

                Err _ ->
                    ss ++ [ "Err" ]
    in
    case msg of
        GotTime _ ->
            [ "GotTime" ]

        CompletedLoadBalances result ->
            resultToString [ "CompletedLoadBalances" ] result

        CompletedLoadUserTransfers result ->
            resultToString [ "CompletedLoadUserTransfers" ] result

        ClaimsLoaded result ->
            resultToString [ "ClaimsLoaded" ] result

        OpenModal claimId _ ->
            [ "OpenConfirmationModal", String.fromInt claimId ]

        CloseModal ->
            [ "CloseModal" ]

        VoteClaim claimId _ ->
            [ "VoteClaim", String.fromInt claimId ]

        GotVoteResult _ result ->
            resultToString [ "GotVoteResult" ] result

        CommunityLoaded result ->
            resultToString [ "CommunityLoaded" ] result

        CreateInvite ->
            [ "CreateInvite" ]

        CloseInviteModal ->
            [ "CloseInviteModal" ]

        CompletedInviteCreation _ ->
            [ "CompletedInviteCreation" ]

        CopyToClipboard _ ->
            [ "CopyToClipboard" ]

        CopiedToClipboard ->
            [ "CopiedToClipboard" ]
