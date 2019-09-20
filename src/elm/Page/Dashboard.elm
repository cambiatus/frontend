module Page.Dashboard exposing (Model, Msg, init, jsAddressToMsg, msgToString, subscriptions, update, view)

import Activity exposing (ActivitiesResponse, Activity)
import Api
import Api.Graphql
import Community exposing (Balance, Metadata, Transaction)
import Eos as Eos
import Eos.Account as Eos
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Html exposing (..)
import Html.Attributes exposing (class, value)
import Http
import I18Next exposing (Delims(..), t)
import Json.Decode exposing (Decoder, Value)
import List.Extra as List
import Page
import Page.Dashboard.Community as DashCommunity
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared as Shared exposing (Shared)
import Strftime
import Task
import Time exposing (Posix)
import Transfer exposing (QueryTransfers, Transfer, userFilter)
import UpdateResult as UR
import Utils
import View.Icon as Icon
import View.Loading as Loading
import View.Tag as Tag



-- INIT


init : LoggedIn.Model -> ( Model, Cmd Msg )
init { shared, accountName } =
    ( initModel
    , Cmd.batch
        [ fetchActivities shared accountName
        , fetchBalance shared accountName
        , fetchTransfers shared accountName
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
    , activities : GraphqlStatus ActivitiesResponse (List Activity)
    }


initModel : Model
initModel =
    { date = Nothing
    , communities = Loading
    , lastSocket = ""
    , transfers = LoadingGraphql
    , activities = LoadingGraphql
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
    case model.communities of
        Loading ->
            Page.fullPageLoading

        Failed e ->
            Page.fullPageError (t "menu.my_communities") e

        Loaded communities ->
            Page.mainContentContainer
                [ Page.viewTitle (t "menu.my_communities")
                , Page.viewButtonNew (t "community.create_button") Route.NewCommunity
                , viewBalances loggedIn communities
                , viewActivities loggedIn.shared model
                , viewSections loggedIn model
                ]


viewActivities : Shared -> Model -> Html Msg
viewActivities shared model =
    let
        t =
            I18Next.t shared.translations

        toView activities =
            List.map
                (viewActivity shared.endpoints.ipfs)
                activities
    in
    div
        []
        [ Page.viewTitle (t "dashboard.activities.title")
        , case model.activities of
            LoadingGraphql ->
                viewNoActivity
                    [ Loading.view "text-nobel" ]

            FailedGraphql err ->
                viewNoActivity
                    [ p
                        [ class "font-sans text-nobel text-sm" ]
                        [ text (Page.errorToString err) ]
                    ]

            LoadedGraphql activities ->
                if List.isEmpty activities then
                    viewNoActivity
                        [ p
                            [ class "font-sans text-nobel text-sm" ]
                            [ text (t "dashboard.activities.no_activities_yet") ]
                        ]

                else
                    div
                        [ class "shadow-md rounded-lg bg-white mt-5" ]
                        (toView activities)
        ]


viewActivity : String -> Activity -> Html Msg
viewActivity url activity =
    let
        maybeHash =
            Just activity.communityLogo

        description =
            activity.actionDescription

        date =
            Just activity.claimCreatedAt
                |> Utils.posixDateTime
                |> Strftime.format "%d %b %Y" Time.utc

        status =
            activity.status
    in
    a
        [ class "border-b last:border-b-0 border-gainsboro flex items-start lg:items-center hover:bg-white-smoke first-hover:rounded-t-lg last-hover:rounded-b-lg p-4"
        , Route.href (Route.VerifyClaim "" "" "" "")
        ]
        [ div
            [ class "flex-none" ]
            [ Icon.smallView url maybeHash "" ]
        , div
            [ class "flex-col flex-grow-1 pl-4" ]
            [ p
                [ class "font-sans text-black text-sm leading-relaxed" ]
                [ text description ]
            , p
                [ class "font-normal font-sans text-nobel text-caption uppercase" ]
                [ text date ]
            , div
                [ class "lg:hidden mt-4" ]
                [ Tag.view status ]
            ]
        , div
            [ class "hidden lg:visible lg:flex lg:flex-none pl-4" ]
            [ Tag.view status ]
        ]


viewNoActivity : List (Html Msg) -> Html Msg
viewNoActivity elements =
    div
        [ class "shadow-md rounded-lg bg-white mt-5 p-4" ]
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
            , ( "value", value )
            , ( "to", viewAccountName to )
            ]
                |> I18Next.tr loggedIn.shared.translations I18Next.Curly "transfer.info"
    in
    Page.viewMaxTwoColumn
        [ Page.viewTitle (t "community.actions.last_title")
        , case model.transfers of
            LoadingGraphql ->
                Page.viewCardEmpty [ text (t "menu.loading") ]

            FailedGraphql _ ->
                Page.viewCardEmpty [ text (t "community.actions.loading_error") ]

            LoadedGraphql [] ->
                Page.viewCardEmpty [ text (t "community.actions.no_actions_yet") ]

            LoadedGraphql transfers ->
                Page.viewCardList
                    (List.map
                        (\transfer ->
                            ( [ text (transferInfo transfer.from transfer.value transfer.to)
                              , case transfer.memo of
                                    Nothing ->
                                        text ""

                                    Just mem ->
                                        p [ class "card__list-memo" ]
                                            [ text mem ]
                              ]
                            , Utils.posixDateTime (Just transfer.blockTime)
                            , model.date
                            )
                        )
                        transfers
                    )
        ]
        [ Page.viewTitle (t "transfer.last_title")
        , case model.transfers of
            LoadingGraphql ->
                Page.viewCardEmpty [ text (t "menu.loading") ]

            FailedGraphql _ ->
                Page.viewCardEmpty [ text (t "transfer.loading_error") ]

            LoadedGraphql [] ->
                Page.viewCardEmpty [ text (t "transfer.no_transfers_yet") ]

            LoadedGraphql transfers ->
                Page.viewCardList
                    (List.map
                        (\transfer ->
                            ( [ text (transferInfo transfer.from transfer.value transfer.to)
                              , case transfer.memo of
                                    Nothing ->
                                        text ""

                                    Just mem ->
                                        p [ class "card__list-memo" ]
                                            [ text mem ]
                              ]
                            , Utils.posixDateTime (Just transfer.blockTime)
                            , model.date
                            )
                        )
                        transfers
                    )
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



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = GotTime Posix
    | CompletedLoadBalances (Result Http.Error (List Balance))
    | GotDashCommunityMsg Int DashCommunity.Msg
    | CompletedLoadUserTransfers (Result (Graphql.Http.Error (Maybe QueryTransfers)) (Maybe QueryTransfers))
    | CompletedLoadActivities (Result (Graphql.Http.Error ActivitiesResponse) ActivitiesResponse)


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

        CompletedLoadActivities (Ok result) ->
            { model | activities = LoadedGraphql (Activity.mapToActivity result) }
                |> UR.init

        CompletedLoadActivities (Err err) ->
            { model | activities = FailedGraphql err }
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


fetchActivities : Shared -> Eos.Name -> Cmd Msg
fetchActivities shared accountName =
    let
        account =
            Eos.nameToString accountName

        selectionSet =
            Activity.activitiesSelectionSet account
    in
    Api.Graphql.query
        shared
        selectionSet
        CompletedLoadActivities


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

        CompletedLoadActivities result ->
            resultToString [ "CompletedLoadActivities" ] result


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
