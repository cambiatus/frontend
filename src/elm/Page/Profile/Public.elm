module Page.Profile.Public exposing (Model, Msg, Status, init, msgToString, receiveBroadcast, update, view)

import Api
import Api.Graphql
import Community
import Eos
import Eos.Account as Eos
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html, div, text)
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
    | CompletedLoadGraphqlInfo (RemoteData (Graphql.Http.Error GraphqlInfo) GraphqlInfo)


type alias Model =
    { profileAccountName : Eos.Name
    , profileStatus : Status
    , blockchainInfo : RemoteData BalanceError Community.Balance
    , graphqlInfo : RemoteData (Graphql.Http.Error GraphqlInfo) GraphqlInfo
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


type BalanceError
    = BalanceNotFound
    | BalanceWithHttpError Http.Error


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
                            (text "")
                        ]

                ( NotFound, _, _ ) ->
                    Page.fullPageNotFound (t "error.unknown") (t "error.pageNotFound")

                ( LoadingFailed err, _, _ ) ->
                    Page.fullPageGraphQLError (t "error.unknown") err

                ( _, RemoteData.Failure err, _ ) ->
                    case err of
                        BalanceNotFound ->
                            Page.fullPageNotFound (t "error.unknown") (t "error.pageNotFound")

                        BalanceWithHttpError httpError ->
                            Page.fullPageError (t "error.unknown") httpError

                ( _, _, RemoteData.Failure err ) ->
                    Page.fullPageGraphQLError (t "error.unknown") err

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
                        graphqlInfoSelectionSet
                        CompletedLoadGraphqlInfo
                    )

        CompletedLoadBalance (Ok (Just balance)) ->
            { model | blockchainInfo = RemoteData.Success balance }
                |> UR.init

        CompletedLoadBalance (Ok Nothing) ->
            { model | blockchainInfo = RemoteData.Failure BalanceNotFound }
                |> UR.init

        CompletedLoadBalance (Err error) ->
            { model | blockchainInfo = RemoteData.Failure (BalanceWithHttpError error) }
                |> UR.init
                |> UR.logHttpError msg error

        CompletedLoadGraphqlInfo (RemoteData.Success graphqlInfo) ->
            { model | graphqlInfo = RemoteData.Success graphqlInfo }
                |> UR.init

        CompletedLoadGraphqlInfo (RemoteData.Failure err) ->
            { model | graphqlInfo = RemoteData.Failure err }
                |> UR.init
                |> UR.logGraphqlError msg err

        CompletedLoadGraphqlInfo RemoteData.NotAsked ->
            model
                |> UR.init

        CompletedLoadGraphqlInfo RemoteData.Loading ->
            model
                |> UR.init


graphqlInfoSelectionSet : SelectionSet GraphqlInfo RootQuery
graphqlInfoSelectionSet =
    -- TODO
    SelectionSet.succeed GraphqlInfo
        |> SelectionSet.hardcoded 0
        |> SelectionSet.hardcoded 0
        |> SelectionSet.hardcoded 0


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
