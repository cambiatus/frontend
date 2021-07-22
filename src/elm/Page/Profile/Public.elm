module Page.Profile.Public exposing (Model, Msg, Status, init, msgToString, receiveBroadcast, update, view)

import Api
import Api.Graphql
import Cambiatus.Object
import Cambiatus.Object.Claim
import Cambiatus.Object.Product
import Cambiatus.Object.TransferConnection
import Cambiatus.Object.User as User
import Cambiatus.Query
import Community
import Eos
import Eos.Account as Eos
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Http
import List.Extra as List
import Page
import Page.Profile exposing (ProfilePage(..), viewUserInfo)
import Profile
import RemoteData exposing (RemoteData)
import Session.LoggedIn as LoggedIn exposing (External(..))
import Time
import UpdateResult as UR


init : LoggedIn.Model -> String -> ( Model, Cmd Msg )
init loggedIn accountName =
    let
        profileQuery =
            Api.Graphql.query loggedIn.shared
                (Just loggedIn.authToken)
                (Profile.query (Eos.stringToName accountName))
                CompletedProfileLoad
    in
    ( initModel (Eos.stringToName accountName)
    , Cmd.batch
        [ profileQuery
        , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
        ]
    )


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = CompletedProfileLoad (RemoteData (Graphql.Http.Error (Maybe Profile.Model)) (Maybe Profile.Model))
    | CompletedLoadCommunity Community.Model
    | CompletedLoadBalance (Result Http.Error (Maybe Community.Balance))
    | CompletedLoadGraphqlInfo (RemoteData (Graphql.Http.Error (Maybe GraphqlInfo)) (Maybe GraphqlInfo))


type alias Model =
    { profileAccountName : Eos.Name
    , profileStatus : Status
    , blockchainInfo : RemoteData (QueryError Http.Error) Community.Balance
    , graphqlInfo : RemoteData (QueryError (Graphql.Http.Error (Maybe GraphqlInfo))) GraphqlInfo
    }


type alias GraphqlInfo =
    { totalTransfers : Int
    , totalClaims : Int
    , totalProducts : Int
    }


type Status
    = Loading
    | LoadingFailed (Graphql.Http.Error (Maybe Profile.Model))
    | Loaded Profile.Model
    | NotFound


type QueryError error
    = ResultNotFound
    | ResultWithError error


initModel : Eos.Name -> Model
initModel profileAccountName =
    { profileAccountName = profileAccountName
    , profileStatus = Loading
    , blockchainInfo = RemoteData.NotAsked
    , graphqlInfo = RemoteData.NotAsked
    }


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        t =
            loggedIn.shared.translators.t

        title =
            case model.profileStatus of
                Loaded profile ->
                    Maybe.withDefault "" profile.name

                _ ->
                    ""

        content =
            case ( model.profileStatus, model.blockchainInfo, model.graphqlInfo ) of
                ( Loaded profile, RemoteData.Success blockchainInfo, RemoteData.Success graphqlInfo ) ->
                    div [ class "flex-grow flex flex-col" ]
                        [ Page.viewHeader loggedIn (t "menu.profile")
                        , viewUserInfo loggedIn
                            profile
                            (Just
                                { totalBalance = blockchainInfo.asset
                                , totalTransfers = graphqlInfo.totalTransfers
                                , totalClaims = graphqlInfo.totalClaims
                                , totalProducts = graphqlInfo.totalProducts
                                , registrationDate = Time.millisToPosix 0
                                , lastTransaction = blockchainInfo.lastActivity
                                }
                            )
                            Public
                            Nothing
                        ]

                ( NotFound, _, _ ) ->
                    Page.fullPageNotFound (t "error.unknown") (t "error.pageNotFound")

                ( LoadingFailed err, _, _ ) ->
                    Page.fullPageGraphQLError (t "error.unknown") err

                ( _, RemoteData.Failure err, _ ) ->
                    case err of
                        ResultNotFound ->
                            Page.fullPageNotFound (t "error.unknown") (t "error.pageNotFound")

                        ResultWithError httpError ->
                            Page.fullPageError (t "error.unknown") httpError

                ( _, _, RemoteData.Failure err ) ->
                    case err of
                        ResultNotFound ->
                            Page.fullPageNotFound (t "error.unknown") (t "error.pageNotFound")

                        ResultWithError graphqlError ->
                            Page.fullPageGraphQLError (t "error.unknown") graphqlError

                _ ->
                    Page.fullPageLoading loggedIn.shared
    in
    { title = title
    , content = content
    }



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        CompletedProfileLoad (RemoteData.Success Nothing) ->
            { model | profileStatus = NotFound }
                |> UR.init

        CompletedProfileLoad (RemoteData.Success (Just profile)) ->
            { model | profileStatus = Loaded profile }
                |> UR.init

        CompletedProfileLoad (RemoteData.Failure err) ->
            { model | profileStatus = LoadingFailed err }
                |> UR.init
                |> UR.logGraphqlError msg err

        CompletedProfileLoad _ ->
            UR.init model

        CompletedLoadCommunity community ->
            model
                |> UR.init
                |> UR.addCmd
                    (Api.getBalances loggedIn.shared
                        model.profileAccountName
                        (\result ->
                            Result.map (List.find (\balance -> balance.asset.symbol == community.symbol))
                                result
                                |> CompletedLoadBalance
                        )
                    )
                |> UR.addCmd
                    (Api.Graphql.query loggedIn.shared
                        (Just loggedIn.authToken)
                        (graphqlInfoQuery model.profileAccountName community.symbol)
                        CompletedLoadGraphqlInfo
                    )

        CompletedLoadBalance (Ok (Just balance)) ->
            { model | blockchainInfo = RemoteData.Success balance }
                |> UR.init

        CompletedLoadBalance (Ok Nothing) ->
            { model | blockchainInfo = RemoteData.Failure ResultNotFound }
                |> UR.init

        CompletedLoadBalance (Err error) ->
            { model | blockchainInfo = RemoteData.Failure (ResultWithError error) }
                |> UR.init
                |> UR.logHttpError msg error

        CompletedLoadGraphqlInfo (RemoteData.Success (Just graphqlInfo)) ->
            { model | graphqlInfo = RemoteData.Success graphqlInfo }
                |> UR.init

        CompletedLoadGraphqlInfo (RemoteData.Success Nothing) ->
            { model | graphqlInfo = RemoteData.Failure ResultNotFound }
                |> UR.init

        CompletedLoadGraphqlInfo (RemoteData.Failure err) ->
            { model | graphqlInfo = RemoteData.Failure (ResultWithError err) }
                |> UR.init
                |> UR.logGraphqlError msg err

        CompletedLoadGraphqlInfo RemoteData.NotAsked ->
            model
                |> UR.init

        CompletedLoadGraphqlInfo RemoteData.Loading ->
            model
                |> UR.init



-- GRAPHQL


graphqlInfoSelectionSet : Eos.Symbol -> SelectionSet GraphqlInfo Cambiatus.Object.User
graphqlInfoSelectionSet communitySymbol =
    SelectionSet.succeed GraphqlInfo
        |> SelectionSet.with
            (User.transfers
                (\optionals ->
                    { optionals
                        | first = Present 0
                        , filter =
                            Present
                                { communityId =
                                    communitySymbol
                                        |> Eos.symbolToString
                                        |> Present
                                , date = Absent
                                , direction = Absent
                                }
                    }
                )
                transfersSelectionSet
                |> SelectionSet.map (Maybe.withDefault 0)
            )
        |> SelectionSet.with
            (User.claims identity claimSelectionSet
                |> SelectionSet.map List.length
            )
        |> SelectionSet.with
            (User.products productSelectionSet
                |> SelectionSet.map
                    (\products ->
                        products
                            |> List.filterMap identity
                            |> List.filter (\{ symbol } -> symbol == communitySymbol)
                            |> List.length
                    )
            )


graphqlInfoQuery : Eos.Name -> Eos.Symbol -> SelectionSet (Maybe GraphqlInfo) RootQuery
graphqlInfoQuery profileAccountName communitySymbol =
    Cambiatus.Query.user
        { account = Eos.nameToString profileAccountName }
        (graphqlInfoSelectionSet communitySymbol)


transfersSelectionSet : SelectionSet Int Cambiatus.Object.TransferConnection
transfersSelectionSet =
    SelectionSet.succeed identity
        |> SelectionSet.with
            (Cambiatus.Object.TransferConnection.count
                |> SelectionSet.map (Maybe.withDefault 0)
            )


claimSelectionSet : SelectionSet { id : Int } Cambiatus.Object.Claim
claimSelectionSet =
    SelectionSet.succeed (\id -> { id = id })
        |> SelectionSet.with Cambiatus.Object.Claim.id


productSelectionSet : SelectionSet { id : Int, symbol : Eos.Symbol } Cambiatus.Object.Product
productSelectionSet =
    SelectionSet.succeed (\id symbol -> { id = id, symbol = symbol })
        |> SelectionSet.with Cambiatus.Object.Product.id
        |> SelectionSet.with (Eos.symbolSelectionSet Cambiatus.Object.Product.communityId)



-- UTILS


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityLoaded community ->
            Just (CompletedLoadCommunity community)

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedProfileLoad r ->
            [ "CompletedProfileLoad", UR.remoteDataToString r ]

        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        CompletedLoadBalance r ->
            [ "CompletedLoadBalance", UR.resultToString r ]

        CompletedLoadGraphqlInfo r ->
            [ "CompletedLoadGraphqlInfo", UR.remoteDataToString r ]
