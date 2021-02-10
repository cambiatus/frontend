module Search exposing
    ( ActiveTab(..)
    , Model
    , Msg
    , State(..)
    , closeSearch
    , init
    , isActive
    , subscriptions
    , update
    , viewEmptyResults
    , viewForm
    , viewOffers
    , viewRecentQueries
    , viewResultsOverview
    , viewTabs
    )

import Action exposing (Action)
import Api.Graphql
import Cambiatus.Object
import Cambiatus.Object.Product
import Cambiatus.Object.SearchResult
import Cambiatus.Query
import Eos exposing (Symbol)
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, br, button, div, h3, img, input, li, p, span, strong, text, ul)
import Html.Attributes exposing (class, disabled, placeholder, src, type_, value)
import Html.Events exposing (onClick, onFocus, onInput, onSubmit)
import Icons
import Json.Decode as Decode exposing (list, string)
import Json.Encode as Encode
import List.Extra as List
import Ports
import Route exposing (Route)
import Session.Shared exposing (Shared)



-- MODEL


type alias Model =
    { state : State
    , currentQuery : String
    , recentQueries : List String
    , selectedCommunity : Symbol
    }


init : Symbol -> Model
init selectedCommunity =
    { state = Inactive
    , currentQuery = ""
    , recentQueries = []
    , selectedCommunity = selectedCommunity
    }



-- TYPES


type State
    = Inactive
    | Loading
    | RecentSearchesShowed
      -- | OverviewShowed
    | ResultsShowed (Maybe SearchResult) ActiveTab


type ActiveTab
    = OffersTab
    | ActionsTab
    | None


type alias SearchResult =
    { offers : List Offer
    , actions : List Action
    }


type alias Offer =
    { id : Int
    , title : String
    , price : Float
    , image : Maybe String
    }



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


searchResultSelectionSet : String -> SelectionSet SearchResult Cambiatus.Object.SearchResult
searchResultSelectionSet queryString =
    SelectionSet.succeed SearchResult
        |> with (Cambiatus.Object.SearchResult.products (\_ -> { query = Present queryString }) offersSelectionSet)
        |> with (Cambiatus.Object.SearchResult.actions (\_ -> { query = Present queryString }) Action.selectionSet)


offersSelectionSet : SelectionSet Offer Cambiatus.Object.Product
offersSelectionSet =
    SelectionSet.map4 Offer
        Cambiatus.Object.Product.id
        Cambiatus.Object.Product.title
        Cambiatus.Object.Product.price
        Cambiatus.Object.Product.image



-- UPDATE


type Msg
    = CancelClicked
    | InputFocused
    | GotRecentSearches String
    | RecentQueryClicked String
    | SearchResultsLoaded (Result (Graphql.Http.Error SearchResult) SearchResult)
    | QuerySubmitted
    | TabActivated ActiveTab
    | CurrentQueryChanged String
    | FoundItemClicked Route


closeSearch : Shared -> Model -> ( Model, Cmd Msg )
closeSearch shared model =
    update shared model CancelClicked


update : Shared -> Model -> Msg -> ( Model, Cmd Msg )
update shared model msg =
    case msg of
        CurrentQueryChanged q ->
            ( { model | currentQuery = q }, Cmd.none )

        FoundItemClicked route ->
            ( { model
                | state = Inactive
                , currentQuery = ""
              }
            , Route.replaceUrl shared.navKey route
            )

        RecentQueryClicked q ->
            update shared
                { model
                    | state = Loading
                    , currentQuery = q
                }
                QuerySubmitted

        TabActivated activeTab ->
            case model.state of
                ResultsShowed r _ ->
                    ( { model
                        | state = ResultsShowed r activeTab
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SearchResultsLoaded res ->
            case res of
                Ok searchResult ->
                    ( { model
                        | state = ResultsShowed (Just searchResult) OffersTab
                      }
                    , Cmd.none
                    )

                Err _ ->
                    -- TODO: Show the error message
                    ( model, Cmd.none )

        GotRecentSearches queries ->
            case Decode.decodeString (list string) queries of
                Ok queryList ->
                    ( { model | recentQueries = queryList }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        CancelClicked ->
            ( { model
                | state = Inactive
                , currentQuery = ""
              }
            , Cmd.none
            )

        InputFocused ->
            ( { model
                | state = RecentSearchesShowed
              }
            , Cmd.none
            )

        QuerySubmitted ->
            let
                newRecentSearches : List String
                newRecentSearches =
                    (model.currentQuery :: model.recentQueries)
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
            ( { model
                | recentQueries = newRecentSearches
                , state = Loading
              }
            , Cmd.batch
                [ storeRecentSearches
                , sendSearchQuery selectedCommunity shared model.currentQuery
                ]
            )



-- VIEW


viewForm : Model -> Html Msg
viewForm model =
    let
        isLoading =
            case model.state of
                Loading ->
                    True

                _ ->
                    False

        iconColor =
            case model.state of
                Inactive ->
                    "fill-gray"

                _ ->
                    "fill-indigo"

        viewCancel =
            case model.state of
                Inactive ->
                    text ""

                _ ->
                    span
                        [ class "text-orange-300 pl-3 leading-10 cursor-pointer"
                        , onClick CancelClicked
                        ]
                        [ text "cancel" ]
    in
    div [ class "w-full px-4" ]
        [ Html.form
            [ class "w-full mt-2 flex items-center"
            , onSubmit QuerySubmitted
            ]
            [ div [ class "relative w-full" ]
                [ input
                    [ type_ "search"
                    , disabled isLoading

                    --, minlength 3
                    --, required True
                    , class "w-full form-input rounded-full bg-gray-100 pl-10 m-0"
                    , placeholder "Find friends and communities"
                    , value model.currentQuery
                    , onFocus InputFocused
                    , onInput CurrentQueryChanged
                    ]
                    []
                , Icons.search <| "absolute top-0 left-0 mt-2 ml-2" ++ " " ++ iconColor
                ]
            , viewCancel
            ]
        ]


viewEmptyResults : String -> Html msg
viewEmptyResults queryText =
    div [ class "flex-grow bg-white text-center" ]
        [ h3 [ class "mt-20 text-xl font-bold" ]
            [ text <| "You searched for \"" ++ queryText ++ "\"" ]
        , div []
            [ img
                [ class "w-2/3 mx-auto md:w-64 mt-6 mb-8"
                , src "/images/not_found.svg"
                ]
                []
            , text "No results were found"
            ]
        ]


viewRecentQueries : List String -> Html Msg
viewRecentQueries recentQueries =
    let
        viewItem q =
            li
                [ class "leading-10 hover:text-orange-500 cursor-pointer"
                , onClick (RecentQueryClicked q)
                ]
                [ Icons.clock "fill-gray inline-block align-middle mr-3"
                , span [ class "inline align-middle" ] [ text q ]
                ]
    in
    div [ class "w-full p-4 bg-white" ]
        [ strong [] [ text "Recently searched" ]
        , ul [ class "text-gray-900" ]
            (List.map viewItem recentQueries)
        ]


viewTabs : SearchResult -> ActiveTab -> Html Msg
viewTabs results activeTab =
    -- TODO: Function may consume an `Active q r t` and render everything from it.
    let
        viewTab : ActiveTab -> String -> List a -> Msg -> Html Msg
        viewTab tabKind label foundItems clickMsg =
            let
                count =
                    List.length foundItems
            in
            li
                [ if activeTab == tabKind then
                    class "bg-orange-300 text-white"

                  else
                    class "bg-gray-100"
                , class "rounded-sm flex-1 text-center cursor-pointer"
                , if count > 0 then
                    onClick clickMsg

                  else
                    class "cursor-not-allowed text-gray-300"
                ]
                [ text <| label ++ String.fromInt count ]
    in
    ul [ class "space-x-2 flex items-stretch leading-10 p-4 pb-2 bg-white" ]
        [ viewTab OffersTab "Offers " results.offers (TabActivated OffersTab)
        , viewTab ActionsTab "Actions " results.actions (TabActivated ActionsTab)
        ]


viewResultsOverview : SearchResult -> Html Msg
viewResultsOverview { offers, actions } =
    let
        viewItem icon count singular plural showMsg =
            li [ class "py-4 flex items-center" ]
                [ div [ class "flex-grow flex items-center" ]
                    [ icon "w-8 h-8 fill-black mr-3"
                    , span []
                        [ text "We found "
                        , strong []
                            [ text (String.fromInt count)
                            , text " "
                            , text <|
                                if count == 1 then
                                    singular

                                else
                                    plural
                            ]
                        ]
                    ]
                , button
                    (class "button w-auto button-sm px-6"
                        :: (if count == 0 then
                                [ class "button-disabled" ]

                            else
                                [ class "button-primary"
                                , onClick showMsg
                                ]
                           )
                    )
                    [ text "Show" ]
                ]
    in
    div []
        [ strong [ class "block py-4" ] [ text "Here is what we found" ]
        , ul []
            [ viewItem Icons.shop (List.length offers) "offer" "offers" (TabActivated OffersTab)
            , viewItem Icons.flag (List.length actions) "action" "actions" (TabActivated ActionsTab)
            ]
        ]


viewOffers : Symbol -> List Offer -> Html Msg
viewOffers symbol offers =
    let
        viewOffer : Offer -> Html Msg
        viewOffer offer =
            li
                [ class "flex px-2 w-1/2 sm:w-1/3 md:w-1/4" ]
                [ div
                    [ class "rounded-md overflow-hidden bg-white flex-grow mb-4 pb-4 cursor-pointer hover:shadow"
                    , onClick (FoundItemClicked (Route.ViewSale (String.fromInt offer.id)))
                    ]
                    [ case offer.image of
                        Nothing ->
                            text ""

                        Just url ->
                            img [ src url ] []
                    , h3 [ class "p-3" ] [ text offer.title ]
                    , p [ class "px-3 leading-none" ]
                        [ span [ class "text-xl text-green font-medium" ] [ text <| String.fromFloat offer.price ]
                        , br [] []
                        , span [ class "text-gray-300 text-xs" ]
                            [ text <| Eos.symbolToSymbolCodeString symbol
                            ]
                        ]
                    ]
                ]
    in
    ul [ class "offers-list flex flex-wrap mt-6 mb-8 mx-2 justify-left" ]
        (List.map viewOffer offers)


isActive : Model -> Bool
isActive model =
    case model.state of
        Inactive ->
            False

        _ ->
            True



-- SUBSCRIPTION


subscriptions : Sub Msg
subscriptions =
    Ports.gotRecentSearches GotRecentSearches
