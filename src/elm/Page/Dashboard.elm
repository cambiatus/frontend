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
import Community exposing (Balance, Metadata, Transaction)
import Eos as Eos exposing (Symbol)
import Eos.Account as Eos
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
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
        [ fetchVerifications shared accountName
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
    , verifications : GraphqlStatus VerificationHistoryResponse (List Verification)
    }


type alias Verification =
    { symbol : Maybe Symbol
    , logo : String
    , objectiveId : Int
    , actionId : Int
    , claimId : Int
    , description : String
    , createdAt : DateTime
    , status : Tag.TagStatus
    }


initModel : Model
initModel =
    let
        pending_verification =
            { symbol = Eos.symbolFromString "PUL"
            , logo = ""
            , objectiveId = 1
            , actionId = 1
            , claimId = 1
            , description = "Cevadis im ampola pa arma uma pindureta."
            , createdAt = DateTime "2019-09-20T16:00:00Z"
            , status = Tag.PENDING
            }

        disapproved_verification =
            { symbol = Eos.symbolFromString "PUL"
            , logo = ""
            , objectiveId = 1
            , actionId = 1
            , claimId = 2
            , description = "Delegadis gente finis, bibendum egestas augue arcu ut est."
            , createdAt = DateTime "2019-09-20T16:00:00Z"
            , status = Tag.DISAPPROVED
            }

        approved_verification =
            { symbol = Eos.symbolFromString "PUL"
            , logo = ""
            , objectiveId = 1
            , actionId = 1
            , claimId = 3
            , description = "Si num tem leite então bota uma pinga aí cumpadi!"
            , createdAt = DateTime "2019-09-20T16:00:00Z"
            , status = Tag.APPROVED
            }
    in
    { date = Nothing
    , communities = Loading
    , lastSocket = ""
    , transfers = LoadingGraphql
    , verifications =
        LoadedGraphql
            [ pending_verification
            , disapproved_verification
            , approved_verification
            ]
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
                , viewVerifications loggedIn.shared model
                , viewSections loggedIn model
                ]


viewVerifications : Shared -> Model -> Html Msg
viewVerifications shared model =
    let
        t =
            I18Next.t shared.translations

        toView verifications =
            List.map
                (viewVerification shared.endpoints.ipfs)
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
                        [ class "shadow-md rounded-lg bg-white mt-5" ]
                        (toView verifications)
        ]


viewVerification : String -> Verification -> Html Msg
viewVerification url verification =
    let
        maybeHash =
            Just verification.logo

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
                        (String.fromInt verification.objectiveId)
                        (String.fromInt verification.actionId)
                        (String.fromInt verification.claimId)

                Nothing ->
                    Route.ComingSoon
    in
    a
        [ class "border-b last:border-b-0 border-gray-500 flex items-start lg:items-center hover:bg-gray-100 first-hover:rounded-t-lg last-hover:rounded-b-lg p-4"
        , Route.href route
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
                [ class "font-normal font-sans text-gray-900 text-caption uppercase" ]
                [ text date ]
            , div
                [ class "lg:hidden mt-4" ]
                [ Tag.view status ]
            ]
        , div
            [ class "hidden lg:visible lg:flex lg:flex-none pl-4" ]
            [ Tag.view status ]
        ]


viewNoVerification : List (Html Msg) -> Html Msg
viewNoVerification elements =
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
    | CompletedLoadVerifications (Result (Graphql.Http.Error VerificationHistoryResponse) VerificationHistoryResponse)


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
            { model | verifications = LoadedGraphql (toVerifications result) }
                |> UR.init

        CompletedLoadVerifications (Err err) ->
            { model | verifications = FailedGraphql err }
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


type alias VerificationHistoryResponse =
    { claims : List ClaimResponse
    }


type alias ClaimResponse =
    { id : Int
    , createdAt : DateTime
    , checks : List CheckResponse
    , action : ActionResponse
    }


type alias CheckResponse =
    { isVerified : Bool
    }


type alias ActionResponse =
    { id : Int
    , description : String
    , objective : ObjectiveResponse
    }


type alias ObjectiveResponse =
    { id : Int
    , community : CommunityResponse
    }


type alias CommunityResponse =
    { symbol : String
    , logo : String
    }


fetchVerifications : Shared -> Eos.Name -> Cmd Msg
fetchVerifications shared accountName =
    let
        validator : String
        validator =
            Eos.nameToString accountName

        selectionSet : SelectionSet VerificationHistoryResponse RootQuery
        selectionSet =
            verificationHistorySelectionSet validator
    in
    Api.Graphql.query
        shared
        selectionSet
        CompletedLoadVerifications


verificationHistorySelectionSet : String -> SelectionSet VerificationHistoryResponse RootQuery
verificationHistorySelectionSet validator =
    let
        claimsInput : ClaimsRequiredArguments
        claimsInput =
            { input = { validator = validator }
            }

        selectionSet : SelectionSet ClaimResponse Bespiral.Object.Claim
        selectionSet =
            claimSelectionSet validator
    in
    SelectionSet.succeed VerificationHistoryResponse
        |> with (Bespiral.Query.claims claimsInput selectionSet)


claimSelectionSet : String -> SelectionSet ClaimResponse Bespiral.Object.Claim
claimSelectionSet validator =
    let
        checksArg : ChecksOptionalArguments -> ChecksOptionalArguments
        checksArg _ =
            { input = Present { validator = Present validator }
            }
    in
    SelectionSet.succeed ClaimResponse
        |> with Claim.id
        |> with Claim.createdAt
        |> with (Claim.checks checksArg checkSelectionSet)
        |> with (Claim.action actionSelectionSet)


checkSelectionSet : SelectionSet CheckResponse Bespiral.Object.Check
checkSelectionSet =
    SelectionSet.succeed CheckResponse
        |> with Check.isVerified


actionSelectionSet : SelectionSet ActionResponse Bespiral.Object.Action
actionSelectionSet =
    SelectionSet.succeed ActionResponse
        |> with Action.id
        |> with Action.description
        |> with (Action.objective objectiveSelectionSet)


objectiveSelectionSet : SelectionSet ObjectiveResponse Bespiral.Object.Objective
objectiveSelectionSet =
    SelectionSet.succeed ObjectiveResponse
        |> with Objective.id
        |> with (Objective.community communitySelectionSet)


communitySelectionSet : SelectionSet CommunityResponse Bespiral.Object.Community
communitySelectionSet =
    SelectionSet.succeed CommunityResponse
        |> with Community.symbol
        |> with Community.logo


toVerifications : VerificationHistoryResponse -> List Verification
toVerifications verificationHistoryResponse =
    let
        claimsResponse : List ClaimResponse
        claimsResponse =
            verificationHistoryResponse.claims

        toStatus : List CheckResponse -> Tag.TagStatus
        toStatus checks =
            case List.head checks of
                Just check ->
                    if check.isVerified == True then
                        Tag.APPROVED

                    else
                        Tag.DISAPPROVED

                Nothing ->
                    Tag.PENDING

        toVerification : ClaimResponse -> Verification
        toVerification claimResponse =
            { symbol = Eos.symbolFromString claimResponse.action.objective.community.symbol
            , logo = claimResponse.action.objective.community.logo
            , objectiveId = claimResponse.action.objective.id
            , actionId = claimResponse.action.id
            , claimId = claimResponse.id
            , description = claimResponse.action.description
            , createdAt = claimResponse.createdAt
            , status = toStatus claimResponse.checks
            }
    in
    List.map
        toVerification
        claimsResponse


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
