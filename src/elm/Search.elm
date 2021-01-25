module Search exposing (Model, Msg, init, subscriptions, update, view)

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
    { searchState : SearchState
    , recentSearches : List String
    , searchText : String
    , selectedCommunity : Symbol
    }


init : Symbol -> Model
init selectedCommunity =
    { searchState = Inactive
    , recentSearches = []
    , searchText = ""
    , selectedCommunity = selectedCommunity
    }



-- TYPES


type SearchState
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
    = StateChanged SearchState
    | GotRecentSearches String
    | RecentSearchClicked String
    | SearchResultsLoaded (Result (Graphql.Http.Error SearchResult) SearchResult)
    | QuerySubmitted


update : Shared -> Model -> Msg -> ( Model, Cmd Msg )
update shared model msg =
    case msg of
        RecentSearchClicked q ->
            update shared { model | searchText = q } QuerySubmitted

        SearchResultsLoaded res ->
            let
                _ =
                    Debug.log "CompletedLoadSearchResults" res
            in
            ( model, Cmd.none )

        GotRecentSearches queries ->
            case Decode.decodeString (list string) queries of
                Ok queryList ->
                    ( { model | recentSearches = queryList }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        StateChanged state ->
            let
                searchText =
                    case state of
                        Active q ->
                            q

                        _ ->
                            model.searchText
            in
            ( { model
                | searchState = state
                , searchText = searchText
              }
            , Cmd.none
            )

        QuerySubmitted ->
            let
                newRecentSearches : List String
                newRecentSearches =
                    (model.searchText :: model.recentSearches)
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
            ( { model | recentSearches = newRecentSearches }
            , Cmd.batch
                [ storeRecentSearches
                , sendSearchQuery selectedCommunity shared model.searchText
                ]
            )



-- VIEW


view : Model -> Html Msg
view model =
    let
        iconColor =
            case model.searchState of
                Inactive ->
                    "fill-gray"

                _ ->
                    "fill-indigo"
    in
    div [ class "w-full" ]
        [ Html.form
            [ class "w-full relative block mt-2"
            , onSubmit QuerySubmitted
            ]
            [ input
                [ type_ "search"
                , class "w-full form-input rounded-full bg-gray-100 pl-10 m-0 block"
                , placeholder "Find friends and communities"
                , value model.searchText
                , onFocus (StateChanged <| Active model.searchText)
                , onBlur (StateChanged Inactive)
                , onInput (\q -> StateChanged (Active q))
                ]
                []
            , Icons.search <| "absolute top-0 left-0 mt-2 ml-2" ++ " " ++ iconColor
            ]
        , viewRecentSearches model
        ]


viewRecentSearches : Model -> Html Msg
viewRecentSearches model =
    let
        viewQuery q =
            li [ class "leading-10", onClick (RecentSearchClicked q) ]
                [ Icons.clock "fill-gray inline-block align-middle mr-3"
                , span [ class "inline align-middle" ] [ text q ]
                ]
    in
    case model.searchState of
        Active _ ->
            div [ class "fixed bg-white w-full left-0 px-2 py-4 border-2" ]
                [ span [ class "font-bold" ] [ text "Recently searched" ]
                , ul []
                    (List.map viewQuery model.recentSearches)
                ]

        _ ->
            text ""



-- SUBSCRIPTION


subscriptions : Sub Msg
subscriptions =
    Ports.gotRecentSearches GotRecentSearches
