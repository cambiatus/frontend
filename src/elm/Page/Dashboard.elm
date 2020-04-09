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
import Html exposing (Html, a, button, div, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import I18Next exposing (Delims(..))
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import List.Extra as List
import Page
import Page.Dashboard.Balance as DashCommunity
import Profile
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import Task
import Time exposing (Posix)
import Transfer exposing (QueryTransfers, Transfer, userFilter)
import UpdateResult as UR



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init { shared, accountName, selectedCommunity } =
    ( initModel
    , Cmd.batch
        [ fetchBalance shared accountName
        , fetchTransfers shared accountName
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
    , communities : Status (List DashCommunity.Model)
    , analysis : GraphqlStatus (List Claim.Model) (List ClaimStatus)
    , lastSocket : String
    , transfers : GraphqlStatus (Maybe QueryTransfers) (List Transfer)
    }


initModel : Model
initModel =
    { date = Nothing
    , communities = Loading
    , analysis = LoadingGraphql
    , lastSocket = ""
    , transfers = LoadingGraphql
    }


type Status a
    = Loading
    | Loaded a
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



-- VIEW


view : LoggedIn.Model -> Model -> Html Msg
view loggedIn model =
    let
        t s =
            I18Next.t loggedIn.shared.translations s
    in
    case ( model.communities, loggedIn.profile ) of
        ( Loading, _ ) ->
            Page.fullPageLoading

        ( Failed e, _ ) ->
            Page.fullPageError (t "menu.my_communities") e

        ( Loaded communities, LoggedIn.Loaded profile ) ->
            div [ class "container mx-auto mb-10" ]
                [ div [ class "text-gray-600 text-2xl font-light flex mt-6 mb-4" ]
                    [ text (t "menu.my_communities")
                    , div [ class "text-indigo-500 ml-2 font-medium" ]
                        [ text (profile.userName |> Maybe.withDefault (Eos.nameToString profile.account))
                        ]
                    ]
                , viewInvitations loggedIn communities
                , viewBalances loggedIn communities
                , viewAnalysisList loggedIn model
                , viewSections loggedIn model
                ]

        ( _, _ ) ->
            Page.fullPageNotFound (t "menu.my_communities") ""


viewAnalysisList : LoggedIn.Model -> Model -> Html Msg
viewAnalysisList loggedIn model =
    let
        text_ s =
            text (I18Next.t loggedIn.shared.translations s)
    in
    case model.analysis of
        LoadingGraphql ->
            Page.fullPageLoading

        LoadedGraphql claims ->
            div [ class "w-full flex flex-wrap -mx-2" ]
                [ div
                    [ class "w-full sm:w-1/2 lg:w-1/3 xl:w-1/4 px-2 mb-6" ]
                    [ div [ class "text-gray-600 text-2xl font-light flex mt-6 mb-4" ]
                        [ div [ class "text-indigo-500 mr-2 font-medium" ]
                            [ text_ "dashboard.analysis.title.1"
                            ]
                        , text_ "dashboard.analysis.title.2"
                        ]
                    , div [] (List.map (viewAnalysis loggedIn) claims)
                    ]
                ]

        FailedGraphql err ->
            div [] [ Page.fullPageGraphQLError "Failed load" err ]


viewAnalysis : LoggedIn.Model -> ClaimStatus -> Html Msg
viewAnalysis ({ shared } as loggedIn) claimStatus =
    let
        text_ s =
            text (I18Next.t shared.translations s)
    in
    case claimStatus of
        ClaimLoaded claim ->
            a
                [ class "flex flex-col items-center justify-center px-3 pt-5 pb-2 my-2 rounded-lg hover:shadow-lg bg-white"
                , Route.href (Route.Claim loggedIn.selectedCommunity claim.action.objective.id claim.action.id claim.id)
                ]
                [ div []
                    [ Profile.view shared loggedIn.accountName claim.claimer
                    ]
                , div []
                    [ p [ class "text-body" ]
                        [ text claim.action.description ]
                    ]
                , div [ class "flex" ]
                    [ button
                        [ class "button button-secondary w-1/2"
                        , onClick (VoteClaim claim.id False)
                        ]
                        [ text_ "dashboard.reject" ]
                    , button
                        [ class "button button-primary w-1/2"
                        , onClick (VoteClaim claim.id True)
                        ]
                        [ text_ "dashboard.verify" ]
                    ]
                ]

        ClaimLoading _ ->
            div [ class "flex flex-col items-center justify-center px-3 pt-5 pb-2 my-2 rounded-lg hover:shadow-lg bg-white" ]
                [ Page.fullPageLoading
                ]

        ClaimVoted _ ->
            text ""

        ClaimVoteFailed _ ->
            div [ class "text-red" ] [ text "failed" ]


viewSections : LoggedIn.Model -> Model -> Html Msg
viewSections loggedIn model =
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



-- VIEW GRID


viewBalances : LoggedIn.Model -> List DashCommunity.Model -> Html Msg
viewBalances loggedIn communities =
    div [ class "flex flex-wrap -mx-2" ]
        (List.indexedMap
            (\i c ->
                DashCommunity.viewCard loggedIn c
                    |> Html.map (GotDashCommunityMsg i)
            )
            communities
        )


viewInvitations : LoggedIn.Model -> List DashCommunity.Model -> Html Msg
viewInvitations loggedIn balances =
    div []
        (List.indexedMap
            (\i b -> DashCommunity.viewInvitationModal loggedIn b |> Html.map (GotDashCommunityMsg i))
            balances
        )



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = GotTime Posix
    | CompletedLoadBalances (Result Http.Error (List Balance))
    | GotDashCommunityMsg Int DashCommunity.Msg
    | CompletedLoadUserTransfers (Result (Graphql.Http.Error (Maybe QueryTransfers)) (Maybe QueryTransfers))
    | ClaimsLoaded (Result (Graphql.Http.Error (List Claim.Model)) (List Claim.Model))
    | VoteClaim Int Bool
    | GotVoteResult Int (Result Value String)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        GotTime date ->
            UR.init { model | date = Just date }

        CompletedLoadBalances (Ok balances) ->
            let
                ( communities, commCmds ) =
                    List.indexedMap
                        (\i b ->
                            DashCommunity.init loggedIn b
                                |> Tuple.mapSecond
                                    (Cmd.map (GotDashCommunityMsg i))
                        )
                        (sortCambiatusFirst loggedIn balances)
                        |> List.unzip
            in
            UR.init { model | communities = Loaded communities }
                |> UR.addCmd (Cmd.batch commCmds)

        CompletedLoadBalances (Err httpError) ->
            UR.init { model | communities = Failed httpError }
                |> UR.logHttpError msg httpError

        GotDashCommunityMsg index subMsg ->
            case model.communities of
                Loaded communities ->
                    DashCommunity.update loggedIn subMsg
                        |> updateDashCommunity index communities model

                _ ->
                    UR.init model
                        |> UR.logImpossible msg []

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

        VoteClaim claimId vote ->
            case model.analysis of
                LoadedGraphql claims ->
                    let
                        newClaims =
                            setClaimStatus claims claimId ClaimLoading

                        newModel =
                            { model | analysis = LoadedGraphql newClaims }
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
                        newClaims =
                            setClaimStatus claims claimId ClaimVoted
                    in
                    { model | analysis = LoadedGraphql newClaims }
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


updateDashCommunity : Int -> List DashCommunity.Model -> Model -> (DashCommunity.Model -> DashCommunity.UpdateResult) -> UpdateResult
updateDashCommunity index communities model subUpdate =
    let
        ( updtUModel, updtCommunities ) =
            List.indexedFoldl
                (\i c ( m_, comms_ ) ->
                    if index == i then
                        subUpdate c
                            |> updateDashCommunityUpdateResult index m_
                            |> (\( x, y ) -> ( x, comms_ ++ [ y ] ))

                    else
                        ( m_, comms_ ++ [ c ] )
                )
                ( UR.init model, [] )
                communities
    in
    UR.mapModel
        (\m -> { m | communities = Loaded updtCommunities })
        updtUModel


updateDashCommunityUpdateResult : Int -> UpdateResult -> DashCommunity.UpdateResult -> ( UpdateResult, DashCommunity.Model )
updateDashCommunityUpdateResult index uResult commUResult =
    UR.map (\_ -> uResult.model)
        (GotDashCommunityMsg index)
        (\extMsg uResult_ ->
            UR.addExt
                (LoggedIn.mapExternal (GotDashCommunityMsg index) extMsg)
                uResult_
        )
        commUResult
        |> (\uR -> ( uR, commUResult.model ))



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

                    _ ->
                        c
            )


sortCambiatusFirst : LoggedIn.Model -> List Balance -> List Balance
sortCambiatusFirst _ balances =
    let
        bespiral : Maybe Balance
        bespiral =
            balances
                |> List.filter (\b -> b.asset.symbol == Eos.bespiralSymbol)
                |> List.head

        --|> Maybe.withDefault (Balance "" 0 False False)
        balancesWithoutSpiral : List Balance
        balancesWithoutSpiral =
            balances
                |> List.filter (\b -> b.asset.symbol /= Eos.bespiralSymbol)
    in
    Maybe.map (\b -> b :: balancesWithoutSpiral) bespiral
        |> Maybe.withDefault balancesWithoutSpiral


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
        "GotDashCommunityMsg" :: indexStr :: remainAddress ->
            let
                _ =
                    Debug.log "veio dash comunidade"
            in
            Maybe.map2
                GotDashCommunityMsg
                (String.toInt indexStr)
                (DashCommunity.jsAddressToMsg remainAddress val)

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

        GotDashCommunityMsg index subMsg ->
            "GotDashCommunityMsg"
                :: String.fromInt index
                :: DashCommunity.msgToString subMsg

        CompletedLoadUserTransfers result ->
            resultToString [ "CompletedLoadUserTransfers" ] result

        ClaimsLoaded result ->
            resultToString [ "ClaimsLoaded" ] result

        VoteClaim claimId _ ->
            [ "VoteClaim", String.fromInt claimId ]

        GotVoteResult _ result ->
            resultToString [ "GotVoteResult" ] result
