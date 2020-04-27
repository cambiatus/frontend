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
import Cambiatus.Query
import Cambiatus.Scalar exposing (DateTime(..))
import Claim
import Community exposing (Balance)
import Eos as Eos exposing (Symbol)
import Eos.Account as Eos
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Html exposing (Html, a, button, div, img, input, p, span, text)
import Html.Attributes exposing (class, classList, disabled, id, src, value)
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
import Strftime
import Task
import Time exposing (Posix)
import Transfer exposing (QueryTransfers, Transfer, userFilter)
import UpdateResult as UR
import Url
import Utils



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init { shared, accountName, selectedCommunity } =
    ( initModel
    , Cmd.batch
        [ fetchBalance shared accountName
        , fetchTransfers shared accountName
        , fetchCommunity shared selectedCommunity
        , fetchAvailableAnalysis shared selectedCommunity accountName
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
    , analysis : GraphqlStatus (List Claim.Model) (List ClaimStatus)
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
    | LoadedGraphql a
    | FailedGraphql (Graphql.Http.Error err)


type ClaimStatus
    = ClaimLoaded Claim.Model
    | ClaimLoading Claim.Model
    | ClaimVoted Claim.Model
    | ClaimVoteFailed Claim.Model


type VoteModalStatus
    = VoteModalClosed
    | VoteOpened Int Bool


type InviteModalStatus
    = InviteModalClosed
    | InviteModalLoading
    | InviteModalFailed String
    | InviteModalLoaded String



-- VIEW


view : LoggedIn.Model -> Model -> Html Msg
view loggedIn model =
    let
        t s =
            I18Next.t loggedIn.shared.translations s
    in
    case ( model.balance, loggedIn.profile ) of
        ( Loading, _ ) ->
            Page.fullPageLoading

        ( Failed e, _ ) ->
            Page.fullPageError (t "dashboard.sorry") e

        ( Loaded balance, LoggedIn.Loaded profile ) ->
            div [ class "container mx-auto px-4 mb-10" ]
                [ div [ class "inline-block text-gray-600 font-light mt-6 mb-4" ]
                    [ text (t "menu.my_communities")
                    , span [ class "text-indigo-500 font-medium" ]
                        [ text (profile.userName |> Maybe.withDefault (Eos.nameToString profile.account))
                        ]
                    ]
                , viewBalance loggedIn model balance
                , viewAnalysisList loggedIn profile model
                , viewTransfers loggedIn model
                , viewAnalysisModal loggedIn model
                , viewInvitationModal loggedIn model
                ]

        ( _, _ ) ->
            Page.fullPageNotFound (t "dashboard.sorry") ""


viewAnalysisModal : LoggedIn.Model -> Model -> Html Msg
viewAnalysisModal loggedIn model =
    case model.voteModalStatus of
        VoteOpened claimId vote ->
            let
                t s =
                    I18Next.t loggedIn.shared.translations s

                text_ s =
                    text (t s)
            in
            div [ class "modal container" ]
                [ div [ class "modal-bg", onClick CloseModal ] []
                , div [ class "modal-content" ]
                    [ div [ class "w-full" ]
                        [ p [ class "w-full font-bold text-heading text-2xl mb-4" ]
                            [ text_ "claim.modal.title" ]
                        , button
                            [ onClick CloseModal ]
                            [ Icons.close "absolute fill-current text-gray-400 top-0 right-0 mx-4 my-4"
                            ]
                        , p [ class "text-body w-full font-sans mb-10" ]
                            [ if vote then
                                text_ "claim.modal.message_approve"

                              else
                                text_ "claim.modal.message_disapprove"
                            ]
                        ]
                    , div [ class "modal-footer" ]
                        [ button [ class "modal-cancel", onClick CloseModal ]
                            [ text_ "claim.modal.secondary" ]
                        , button [ class "modal-accept", onClick (VoteClaim claimId vote) ]
                            [ if vote then
                                text_ "claim.modal.primary_approve"

                              else
                                text_ "claim.modal.primary_disapprove"
                            ]
                        ]
                    ]
                ]

        VoteModalClosed ->
            text ""


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
            protocol ++ shared.url.host ++ "/invite/" ++ invitationId
    in
    case model.inviteModalStatus of
        InviteModalClosed ->
            text ""

        _ ->
            div [ class "modal container" ]
                [ div [ class "modal-bg", onClick CloseInviteModal ] []
                , div [ class "modal-content" ]
                    [ div [ class "w-full" ]
                        [ p [ class "text-2xl font-medium mb-4" ]
                            [ text_ "community.invite.title" ]
                        , button [ onClick CloseInviteModal ]
                            [ Icons.close "absolute fill-current text-gray-400 top-0 right-0 mx-8 my-4" ]
                        , case model.inviteModalStatus of
                            InviteModalClosed ->
                                text ""

                            InviteModalLoading ->
                                div [ class "flex items-center justify-center" ]
                                    [ div [ class "spinner spinner--delay" ] [] ]

                            InviteModalFailed err ->
                                div []
                                    [ div [ class "flex items-center justify-center text-heading text-red" ]
                                        [ p [ class "text-sm text-red" ] [ text err ] ]
                                    , div [ class "w-full md:bg-gray-100 flex md:absolute rounded-b-lg md:inset-x-0 md:bottom-0 md:p-4 justify-center items-center" ]
                                        [ button
                                            [ class "button button-primary"
                                            , onClick CloseInviteModal
                                            ]
                                            [ text "OK" ]
                                        ]
                                    ]

                            InviteModalLoaded invitationId ->
                                div [ class "flex flex-wrap items-center mt-24 md:mt-0" ]
                                    [ div [ class "flex flex-col items-left w-full mb-4" ]
                                        [ span [ class "input-label" ]
                                            [ text_ "community.invite.label" ]
                                        , input
                                            [ class "text-menu border p-2 md:border-none md:text-heading outline-none text-black"
                                            , id "invitation-id"
                                            , value (url invitationId)
                                            , disabled True
                                            ]
                                            []
                                        ]
                                    , div [ class "w-full md:bg-gray-100 flex md:absolute rounded-b-lg md:inset-x-0 md:bottom-0 md:p-4 justify-center items-center" ]
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
                                    ]
                        ]
                    ]
                ]


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

        LoadedGraphql claims ->
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
                            [ class "button button-secondary h-8 w-20"
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
viewAnalysis ({ shared } as loggedIn) claimStatus =
    let
        text_ s =
            text (I18Next.t shared.translations s)

        date dateTime =
            Just dateTime
                |> Utils.posixDateTime
                |> Strftime.format "%d %b %Y" Time.utc
    in
    case claimStatus of
        ClaimLoaded claim ->
            div [ class "w-full sm:w-full md:w-1/2 lg:w-1/3 xl:w-1/4 px-2 mb-4" ]
                [ div
                    [ class " flex flex-col p-4 my-2 rounded-lg bg-white"
                    , id ("claim" ++ String.fromInt claim.id)
                    ]
                    [ div [ class "flex justify-start mb-8" ]
                        [ Profile.view shared loggedIn.accountName claim.claimer
                        ]
                    , div [ class "mb-6" ]
                        [ p [ class "text-body" ]
                            [ text claim.action.description ]
                        , p
                            [ class "text-gray-900 text-caption uppercase" ]
                            [ text <| date claim.createdAt ]
                        ]
                    , div [ class "flex" ]
                        [ button
                            [ class "flex-1 button button-secondary"
                            , onClick (OpenModal claim.id False)
                            ]
                            [ text_ "dashboard.reject" ]
                        , div [ class "w-4" ] []
                        , button
                            [ class "flex-1 button button-primary"
                            , onClick (OpenModal claim.id True)
                            ]
                            [ text_ "dashboard.verify" ]
                        ]
                    ]
                ]

        ClaimLoading _ ->
            div [ class "w-full sm:w-full md:w-1/2 lg:w-1/3 xl:w-1/4 px-2 mb-4" ]
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

            LoadedGraphql [] ->
                Page.viewCardEmpty
                    [ div [ class "text-gray-900 text-sm" ]
                        [ text (t "transfer.no_transfers_yet") ]
                    ]

            LoadedGraphql transfers ->
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
            (viewAmount amount (Eos.symbolToString transfer.symbol))
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
viewBalance loggedIn model balance =
    let
        text_ s =
            text (I18Next.t loggedIn.shared.translations s)

        symbolText =
            Eos.symbolToString balance.asset.symbol

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
                [ class "button button-primary w-full font-semibold mb-2"
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
    | ClaimsLoaded (Result (Graphql.Http.Error (List Claim.Model)) (List Claim.Model))
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

                statusBalance =
                    case List.find findBalance balances of
                        Just b ->
                            Loaded b

                        Nothing ->
                            NotFound
            in
            UR.init { model | balance = statusBalance }

        CompletedLoadBalances (Err httpError) ->
            UR.init { model | balance = Failed httpError }
                |> UR.logHttpError msg httpError

        ClaimsLoaded (Ok claims) ->
            let
                wrappedClaims =
                    List.map ClaimLoaded claims
            in
            { model | analysis = LoadedGraphql wrappedClaims }
                |> UR.init

        ClaimsLoaded (Err err) ->
            { model | analysis = FailedGraphql err }
                |> UR.init
                |> UR.logGraphqlError msg err

        CompletedLoadUserTransfers (Ok maybeTransfers) ->
            { model | transfers = LoadedGraphql (Transfer.getTransfers maybeTransfers) }
                |> UR.init

        CompletedLoadUserTransfers (Err err) ->
            { model | transfers = FailedGraphql err }
                |> UR.init
                |> UR.logGraphqlError msg err

        OpenModal claimId vote ->
            { model | voteModalStatus = VoteOpened claimId vote }
                |> UR.init

        CloseModal ->
            { model | voteModalStatus = VoteModalClosed }
                |> UR.init

        VoteClaim claimId vote ->
            case model.analysis of
                LoadedGraphql claims ->
                    let
                        newClaims =
                            setClaimStatus claims claimId ClaimLoading

                        newModel =
                            { model
                                | analysis = LoadedGraphql newClaims
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
                                        { actions =
                                            [ { accountName = "bes.cmm"
                                              , name = "verifyclaim"
                                              , authorization =
                                                    { actor = loggedIn.accountName
                                                    , permissionName = Eos.samplePermission
                                                    }
                                              , data = encodeVerification claimId loggedIn.accountName vote
                                              }
                                            ]
                                        }
                                }

                    else
                        UR.init newModel
                            |> UR.addExt (Just (VoteClaim claimId vote) |> LoggedIn.RequiredAuthentication)

                _ ->
                    model
                        |> UR.init

        GotVoteResult claimId (Ok _) ->
            case model.analysis of
                LoadedGraphql claims ->
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
                            in
                            { model
                                | analysis = LoadedGraphql (setClaimStatus claims claimId ClaimVoted)
                            }
                                |> UR.init
                                |> UR.addExt (ShowFeedback LoggedIn.Success (message value))

                        Nothing ->
                            model
                                |> UR.init

                _ ->
                    model |> UR.init

        GotVoteResult claimId (Err _) ->
            case model.analysis of
                LoadedGraphql claims ->
                    { model | analysis = LoadedGraphql (setClaimStatus claims claimId ClaimVoteFailed) }
                        |> UR.init

                _ ->
                    model |> UR.init

        CommunityLoaded (Ok community) ->
            case community of
                Just c ->
                    { model | community = LoadedGraphql c }
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
                { model | inviteModalStatus = InviteModalClosed }

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
        (Transfer.transfersQuery
            (userFilter accountName)
            (\args ->
                { args | first = Present 10 }
            )
        )
        CompletedLoadUserTransfers


fetchAvailableAnalysis : Shared -> Symbol -> Eos.Name -> Cmd Msg
fetchAvailableAnalysis shared communityId account =
    let
        arg =
            { claimer = Absent
            , symbol = Present (Eos.symbolToString communityId)
            , validator = Present (Eos.nameToString account)
            }
    in
    Api.Graphql.query
        shared
        (Cambiatus.Query.claims { input = arg } Claim.selectionSet)
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


encodeVerification : Int -> Eos.Name -> Bool -> Encode.Value
encodeVerification claimId validator vote =
    let
        encodedClaimId : Encode.Value
        encodedClaimId =
            Encode.int claimId

        encodedVerifier : Encode.Value
        encodedVerifier =
            Eos.encodeName validator

        encodedVote : Encode.Value
        encodedVote =
            vote
                |> Eos.boolToEosBool
                |> Eos.encodeEosBool
    in
    Encode.object
        [ ( "claim_id", encodedClaimId )
        , ( "verifier", encodedVerifier )
        , ( "vote", encodedVote )
        ]


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
