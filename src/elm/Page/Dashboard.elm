module Page.Dashboard exposing (Model, Msg, init, jsAddressToMsg, msgToString, subscriptions, update, view)

import Api
import Api.Graphql
import Bespiral.Object
import Bespiral.Object.Action as Action
import Bespiral.Object.Check as Check
import Bespiral.Object.Claim as Claim exposing (ChecksOptionalArguments)
import Bespiral.Object.Community as Community
import Bespiral.Object.Objective as Objective
import Bespiral.Query exposing (ClaimsRequiredArguments)
import Bespiral.Scalar exposing (DateTime(..))
import Community exposing (ActionVerification, ActionVerificationsResponse, Balance, ClaimResponse, Metadata, Transaction)
import Eos as Eos exposing (Symbol)
import Eos.Account as Eos
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (..)
import Html.Attributes exposing (class, src, value)
import Http
import I18Next exposing (Delims(..), t)
import Json.Decode exposing (Decoder, Value)
import List.Extra as List
import Page
import Page.Dashboard.Balance as DashCommunity
import Route
import Session.LoggedIn as LoggedIn exposing (External(..), ProfileStatus)
import Session.Shared as Shared exposing (Shared)
import Strftime
import Task
import Time exposing (Posix)
import Transfer exposing (QueryTransfers, Transfer, userFilter)
import UpdateResult as UR
import Utils
import View.Loading as Loading
import View.Tag as Tag



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init { shared, accountName } =
    ( initModel
    , Cmd.batch
        [ fetchVerifications shared accountName
        , fetchBalance shared accountName
        , fetchTransfers shared accountName
        , fetchClaims shared accountName
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
    , lastSocket : String
    , transfers : GraphqlStatus (Maybe QueryTransfers) (List Transfer)
    , verifications : GraphqlStatus ActionVerificationsResponse (List ActionVerification)
    , claims : GraphqlStatus ActionVerificationsResponse (List ActionVerification)
    }


initModel : Model
initModel =
    { date = Nothing
    , communities = Loading
    , lastSocket = ""
    , transfers = LoadingGraphql
    , verifications = LoadingGraphql
    , claims = LoadingGraphql
    }


type Status a
    = Loading
    | Loaded a
    | Failed Http.Error


type GraphqlStatus err a
    = LoadingGraphql
    | LoadedGraphql a
    | FailedGraphql (Graphql.Http.Error err)



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
            div [ class "container mx-auto" ]
                [ div [ class "text-gray-600 text-2xl font-light flex mt-6 mb-4" ]
                    [ text (t "menu.my_communities")
                    , div [ class "text-indigo-500 ml-2 font-medium" ]
                        [ text (profile.userName |> Maybe.withDefault (Eos.nameToString profile.account))
                        ]
                    ]
                , viewInvitations loggedIn communities
                , viewBalances loggedIn communities
                , viewVerifications loggedIn.shared model
                , viewSections loggedIn model
                ]

        ( _, _ ) ->
            Page.fullPageNotFound (t "menu.my_communities") ""


viewVerifications : Shared -> Model -> Html Msg
viewVerifications shared model =
    let
        t =
            I18Next.t shared.translations

        toView verifications =
            List.map
                (viewVerification shared)
                verifications
    in
    div
        []
        [ Page.viewTitle (t "dashboard.activities.title")
        , case model.verifications of
            LoadingGraphql ->
                viewNoVerification
                    [ Loading.view "text-gray-900" ]

            FailedGraphql err ->
                viewNoVerification
                    [ p
                        [ class "font-sans text-gray-900 text-sm" ]
                        [ text (Page.errorToString err) ]
                    ]

            LoadedGraphql verifications ->
                if List.isEmpty verifications then
                    viewNoVerification
                        [ p
                            [ class "font-sans text-gray-900 text-sm" ]
                            [ text (t "dashboard.activities.no_activities_yet") ]
                        ]

                else
                    div
                        [ class "rounded-lg bg-white mt-5" ]
                        (toView verifications)
        ]


viewVerification : Shared -> ActionVerification -> Html Msg
viewVerification shared verification =
    let
        maybeLogo =
            if String.isEmpty verification.logo then
                Nothing

            else
                Just (shared.endpoints.ipfs ++ "/" ++ verification.logo)

        description =
            verification.description

        date =
            Just verification.createdAt
                |> Utils.posixDateTime
                |> Strftime.format "%d %b %Y" Time.utc

        status =
            verification.status

        route =
            case verification.symbol of
                Just symbol ->
                    Route.VerifyClaim
                        symbol
                        verification.objectiveId
                        verification.actionId
                        verification.claimId

                Nothing ->
                    Route.ComingSoon
    in
    a
        [ class "border-b last:border-b-0 border-gray-500 flex items-start lg:items-center hover:bg-gray-100 first-hover:rounded-t-lg last-hover:rounded-b-lg p-4"
        , Route.href route
        ]
        [ div
            [ class "flex-none" ]
            [ case maybeLogo of
                Just logoUrl ->
                    img
                        [ class "w-10 h-10 object-scale-down"
                        , src logoUrl
                        ]
                        []

                Nothing ->
                    div
                        [ class "w-10 h-10 object-scale-down" ]
                        []
            ]
        , div
            [ class "flex-col flex-grow-1 pl-4" ]
            [ p
                [ class "font-sans text-black text-sm leading-relaxed" ]
                [ text description ]
            , p
                [ class "font-normal font-sans text-gray-900 text-caption uppercase" ]
                [ text date ]
            , div
                [ class "lg:hidden mt-4" ]
                [ Tag.view status shared.translations ]
            ]
        , div
            [ class "hidden lg:visible lg:flex lg:flex-none pl-4" ]
            [ Tag.view status shared.translations ]
        ]


viewNoVerification : List (Html Msg) -> Html Msg
viewNoVerification elements =
    div
        [ class "rounded-lg bg-white mt-5 p-4" ]
        [ div
            [ class "bg-white-smoke flex items-center justify-center p-8" ]
            elements
        ]


viewSections : LoggedIn.Model -> Model -> Html Msg
viewSections loggedIn model =
    let
        t s =
            I18Next.t loggedIn.shared.translations s

        viewAccountName accountName =
            Eos.nameToString accountName

        transferInfo from value to =
            [ ( "from", viewAccountName from )
            , ( "value", String.fromFloat value )
            , ( "to", viewAccountName to )
            ]
                |> I18Next.tr loggedIn.shared.translations I18Next.Curly "transfer.info"

        toView claims =
            List.map
                (viewVerification loggedIn.shared)
                claims
    in
    Page.viewMaxTwoColumn
        [ Page.viewTitle (t "community.actions.last_title")
        , case model.claims of
            LoadingGraphql ->
                viewNoVerification
                    [ Loading.view "text-gray-900" ]

            FailedGraphql err ->
                viewNoVerification
                    [ p
                        [ class "font-sans text-gray-900 text-sm" ]
                        [ text (Page.errorToString err) ]
                    ]

            LoadedGraphql claims ->
                if List.isEmpty claims then
                    viewNoVerification
                        [ p
                            [ class "font-sans text-gray-900 text-sm" ]
                            [ text (t "dashboard.activities.no_activities_yet") ]
                        ]

                else
                    div
                        [ class "rounded-lg bg-white " ]
                        (toView claims)
        ]
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
        , Route.href (Route.Transfer transfer.id)
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
                DashCommunity.viewCard loggedIn i c
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
    | CompletedLoadVerifications (Result (Graphql.Http.Error ActionVerificationsResponse) ActionVerificationsResponse)
    | CompletedLoadClaims (Result (Graphql.Http.Error ActionVerificationsResponse) ActionVerificationsResponse)


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
                        (sortBespiralFirst loggedIn balances)
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

        CompletedLoadUserTransfers (Ok maybeTransfers) ->
            { model | transfers = LoadedGraphql (Transfer.getTransfers maybeTransfers) }
                |> UR.init

        CompletedLoadUserTransfers (Err err) ->
            { model | transfers = FailedGraphql err }
                |> UR.init
                |> UR.logGraphqlError msg err

        CompletedLoadVerifications (Ok result) ->
            { model | verifications = LoadedGraphql (Community.toVerifications result) }
                |> UR.init

        CompletedLoadVerifications (Err err) ->
            { model | verifications = FailedGraphql err }
                |> UR.init
                |> UR.logGraphqlError msg err

        CompletedLoadClaims (Ok result) ->
            { model | claims = LoadedGraphql (Community.toVerifications result) }
                |> UR.init

        CompletedLoadClaims (Err err) ->
            { model | claims = FailedGraphql err }
                |> UR.init
                |> UR.logGraphqlError msg err


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


fetchClaims : Shared -> Eos.Name -> Cmd Msg
fetchClaims shared accountName =
    let
        claimer : String
        claimer =
            Eos.nameToString accountName

        selectionSet : SelectionSet ActionVerificationsResponse RootQuery
        selectionSet =
            verificationHistorySelectionSet False claimer
    in
    Api.Graphql.query
        shared
        selectionSet
        CompletedLoadClaims


fetchVerifications : Shared -> Eos.Name -> Cmd Msg
fetchVerifications shared accountName =
    let
        validator : String
        validator =
            Eos.nameToString accountName

        selectionSet : SelectionSet ActionVerificationsResponse RootQuery
        selectionSet =
            verificationHistorySelectionSet True validator
    in
    Api.Graphql.query
        shared
        selectionSet
        CompletedLoadVerifications


verificationHistorySelectionSet : Bool -> String -> SelectionSet ActionVerificationsResponse RootQuery
verificationHistorySelectionSet forValidator accName =
    let
        qInput : ClaimsRequiredArguments
        qInput =
            if forValidator then
                { input = { validator = Present accName, claimer = Absent, symbol = Absent }
                }

            else
                { input = { claimer = Present accName, validator = Absent, symbol = Absent }
                }

        selectionSet : SelectionSet ClaimResponse Bespiral.Object.Claim
        selectionSet =
            Community.claimSelectionSet accName
    in
    SelectionSet.succeed ActionVerificationsResponse
        |> with (Bespiral.Query.claims qInput selectionSet)


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


sortBespiralFirst : LoggedIn.Model -> List Balance -> List Balance
sortBespiralFirst loggedIn balances =
    let
        bespiral : Maybe Balance
        bespiral =
            balances
                |> List.filter (\b -> b.asset.symbol == Shared.bespiralSymbol loggedIn.shared)
                |> List.head

        --|> Maybe.withDefault (Balance "" 0 False False)
        balancesWithoutSpiral : List Balance
        balancesWithoutSpiral =
            balances
                |> List.filter (\b -> b.asset.symbol /= Shared.bespiralSymbol loggedIn.shared)
    in
    Maybe.map (\b -> b :: balancesWithoutSpiral) bespiral
        |> Maybe.withDefault balancesWithoutSpiral


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

        CompletedLoadVerifications result ->
            resultToString [ "CompletedLoadActivities" ] result

        CompletedLoadClaims result ->
            resultToString [ "CompletedLoadClaims" ] result


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "GotDashCommunityMsg" :: indexStr :: remainAddress ->
            Maybe.map2
                GotDashCommunityMsg
                (String.toInt indexStr)
                (DashCommunity.jsAddressToMsg remainAddress val)

        _ ->
            Nothing
