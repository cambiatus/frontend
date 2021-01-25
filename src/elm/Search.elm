module Search exposing (Model, Msg, init, isActive, subscriptions, update, viewForm, viewRecentQueries)

import Api.Graphql
import Cambiatus.Object
import Cambiatus.Object.Action
import Cambiatus.Object.Product
import Cambiatus.Object.SearchResult
import Cambiatus.Query
import Eos exposing (Symbol)
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, div, input, li, span, text, ul)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onBlur, onClick, onFocus, onInput, onSubmit)
import Icons
import Json.Decode as Decode exposing (Value, list, string)
import Json.Encode as Encode
import List.Extra as List
import Ports
import Session.Shared exposing (Shared)



-- MODEL


type alias Model =
    { state : State
    , recentQueries : List String
    , queryText : String
    , selectedCommunity : Symbol
    }


init : Symbol -> Model
init selectedCommunity =
    { state = Inactive
    , recentQueries = []
    , queryText = ""
    , selectedCommunity = selectedCommunity
    }



-- TYPES


type State
    = Inactive
    | Active String
    | ResultsShowed



-- GRAPHQL


sendSearchQuery : Symbol -> Shared -> String -> Cmd Msg
sendSearchQuery selectedCommunity shared queryString =
    let
        req =
            { communityId = Eos.symbolToString selectedCommunity }
    in
    Api.Graphql.query
        shared
        (Cambiatus.Query.search req (searchResultSelectionSet queryString))
        SearchResultsLoaded


type alias SearchResult =
    { products : List SearchProduct
    , actions : List SearchAction
    }


type alias SearchProduct =
    { title : String
    }


type alias SearchAction =
    { description : String
    }


searchResultSelectionSet : String -> SelectionSet SearchResult Cambiatus.Object.SearchResult
searchResultSelectionSet queryString =
    SelectionSet.succeed SearchResult
        |> with (Cambiatus.Object.SearchResult.products (\_ -> { query = Present queryString }) productsSelectionSet)
        |> with (Cambiatus.Object.SearchResult.actions (\_ -> { query = Present queryString }) actionsSelectionSet)


productsSelectionSet : SelectionSet SearchProduct Cambiatus.Object.Product
productsSelectionSet =
    SelectionSet.map SearchProduct Cambiatus.Object.Product.title


actionsSelectionSet : SelectionSet SearchAction Cambiatus.Object.Action
actionsSelectionSet =
    SelectionSet.map SearchAction Cambiatus.Object.Action.description



-- UPDATE


type Msg
    = StateChanged State
    | GotRecentSearches String
    | RecentSearchClicked String
    | SearchResultsLoaded (Result (Graphql.Http.Error SearchResult) SearchResult)
    | QuerySubmitted


update : Shared -> Model -> Msg -> ( Model, Cmd Msg )
update shared model msg =
    case msg of
        RecentSearchClicked q ->
            let
                _ =
                    Debug.log "q" q
            in
            update shared { model | queryText = q } QuerySubmitted

        SearchResultsLoaded res ->
            let
                _ =
                    Debug.log "CompletedLoadSearchResults" res
            in
            ( model, Cmd.none )

        GotRecentSearches queries ->
            case Decode.decodeString (list string) queries of
                Ok queryList ->
                    ( { model | recentQueries = queryList }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        StateChanged state ->
            let
                searchText =
                    case state of
                        Active q ->
                            q

                        _ ->
                            model.queryText
            in
            ( { model
                | state = state
                , queryText = searchText
              }
            , Cmd.none
            )

        QuerySubmitted ->
            let
                newRecentSearches : List String
                newRecentSearches =
                    (model.queryText :: model.recentQueries)
                        |> List.unique
                        |> List.take 3

                storeRecentSearches : Cmd msg
                storeRecentSearches =
                    newRecentSearches
                        |> Encode.list Encode.string
                        |> Encode.encode 0
                        |> Ports.storeRecentSearches

                selectedCommunity =
                    model.selectedCommunity
            in
            ( { model | recentQueries = newRecentSearches }
            , Cmd.batch
                [ storeRecentSearches
                , sendSearchQuery selectedCommunity shared model.queryText
                ]
            )



-- VIEW


viewForm : Model -> Html Msg
viewForm model =
    let
        iconColor =
            case model.state of
                Inactive ->
                    "fill-gray"

                _ ->
                    "fill-indigo"
    in
    div [ class "w-full px-4" ]
        [ Html.form
            [ class "w-full mt-2 flex"
            , onSubmit QuerySubmitted
            ]
            [ div [ class "relative w-full" ]
                [ input
                    [ type_ "search"
                    , class "w-full form-input rounded-full bg-gray-100 pl-10 m-0 block"
                    , placeholder "Find friends and communities"
                    , value model.queryText
                    , onFocus (StateChanged <| Active model.queryText)
                    , onInput (\q -> StateChanged (Active q))
                    ]
                    []
                , Icons.search <| "absolute top-0 left-0 mt-2 ml-2" ++ " " ++ iconColor
                ]
            , case model.state of
                Active _ ->
                    span
                        [ class "text-orange-300 leading-10 inline-block ml-3"
                        , onClick (StateChanged Inactive)
                        ]
                        [ text "cancel" ]

                _ ->
                    text ""
            ]
        ]


viewRecentQueries : Model -> Html Msg
viewRecentQueries model =
    let
        viewQuery q =
            li
                [ class "leading-10 hover:text-orange-500 cursor-pointer"
                , onClick (RecentSearchClicked q)
                ]
                [ Icons.clock "fill-gray inline-block align-middle mr-3"
                , span [ class "inline align-middle" ] [ text q ]
                ]
    in
    case model.state of
        Active _ ->
            div [ class "w-full left-0 p-4" ]
                [ span [ class "font-bold" ] [ text "Recently searched" ]
                , ul [ class "text-gray-900" ]
                    (List.map viewQuery model.recentQueries)
                ]

        _ ->
            text ""


isActive : Model -> Bool
isActive model =
    case model.state of
        Active _ ->
            True

        _ ->
            False



-- SUBSCRIPTION


subscriptions : Sub Msg
subscriptions =
    Ports.gotRecentSearches GotRecentSearches
