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
    , viewSearchBody
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
import Html.Attributes exposing (class, disabled, minlength, placeholder, required, src, type_, value)
import Html.Events exposing (onClick, onFocus, onInput, onSubmit)
import Icons
import Json.Decode as Decode exposing (list, string)
import Json.Encode as Encode
import List.Extra as List
import Ports
import RemoteData exposing (RemoteData)
import Route exposing (Route)
import Session.Shared exposing (Shared, Translators)
import Time exposing (Posix)
import View.Components



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
    | RecentSearchesShowed
    | ResultsShowed FoundData (Maybe ActiveTab)


type ActiveTab
    = OffersTab
    | ActionsTab


type alias SearchResults =
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


type alias FoundData =
    RemoteData (Graphql.Http.Error SearchResults) SearchResults


sendSearchQuery : Symbol -> Shared -> String -> Cmd Msg
sendSearchQuery selectedCommunity shared queryString =
    let
        req =
            { communityId = Eos.symbolToString selectedCommunity }
    in
    Api.Graphql.query
        shared
        (Cambiatus.Query.search req (searchResultSelectionSet queryString))
        (RemoteData.fromResult >> GotSearchResults)


searchResultSelectionSet : String -> SelectionSet SearchResults Cambiatus.Object.SearchResult
searchResultSelectionSet queryString =
    SelectionSet.succeed SearchResults
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
    | GotSearchResults FoundData
    | QuerySubmitted
    | TabActivated ActiveTab
    | CurrentQueryChanged String
    | FoundItemClicked Route


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
                    | state = ResultsShowed RemoteData.Loading Nothing
                    , currentQuery = q
                }
                QuerySubmitted

        TabActivated activeTab ->
            case model.state of
                ResultsShowed r _ ->
                    ( { model
                        | state = ResultsShowed r (Just activeTab)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        GotSearchResults res ->
            ( { model
                | state = ResultsShowed res Nothing
              }
            , Cmd.none
            )

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
                , state = ResultsShowed RemoteData.Loading Nothing
              }
            , Cmd.batch
                [ storeRecentSearches
                , sendSearchQuery selectedCommunity shared model.currentQuery
                ]
            )



-- VIEW


viewForm : Translators -> Model -> Html Msg
viewForm { t } model =
    let
        isLoading =
            case model.state of
                ResultsShowed RemoteData.Loading _ ->
                    True

                _ ->
                    False

        iconColor =
            case model.state of
                Inactive ->
                    "text-gray-400"

                _ ->
                    "text-indigo-500"

        viewCancel =
            case model.state of
                Inactive ->
                    text ""

                _ ->
                    span
                        [ class "text-orange-300 pl-3 leading-10 cursor-pointer lowercase"
                        , onClick CancelClicked
                        ]
                        [ text (t "menu.cancel") ]
    in
    Html.form
        [ class "flex items-center"
        , onSubmit QuerySubmitted
        ]
        [ div [ class "relative w-full" ]
            [ input
                [ type_ "search"
                , disabled isLoading
                , minlength 3
                , required True
                , class "w-full form-input rounded-full border-0 bg-gray-100 pl-10 m-0"
                , placeholder (t "menu.search.placeholder")
                , value model.currentQuery
                , onFocus InputFocused
                , onInput CurrentQueryChanged
                ]
                []
            , Icons.search <| "absolute top-0 left-0 mt-2 ml-2 fill-current" ++ " " ++ iconColor
            ]
        , viewCancel
        ]


viewSearchBody :
    Translators
    -> Symbol
    -> Maybe Posix
    -> (Msg -> parentMsg)
    -> (Action.Msg -> parentMsg)
    -> Model
    -> Html parentMsg
viewSearchBody translators selectedCommunity maybeToday searchToMsg actionToMsg searchModel =
    div [ class "container mx-auto flex flex-grow" ]
        [ case searchModel.state of
            ResultsShowed (RemoteData.Success { actions, offers }) activeTab ->
                case ( List.length actions, List.length offers ) of
                    ( 0, 0 ) ->
                        viewEmptyResults translators searchModel.currentQuery
                            |> Html.map searchToMsg

                    _ ->
                        let
                            results =
                                { actions = actions
                                , offers = offers
                                }
                        in
                        case activeTab of
                            Just OffersTab ->
                                div [ class "w-full" ]
                                    [ viewTabs translators results OffersTab
                                    , viewOffers selectedCommunity results.offers
                                    ]
                                    |> Html.map searchToMsg

                            Just ActionsTab ->
                                div [ class "w-full" ]
                                    [ viewTabs translators results ActionsTab
                                        |> Html.map searchToMsg
                                    , Action.viewSearchActions translators maybeToday results.actions
                                        |> Html.map actionToMsg
                                    ]

                            Nothing ->
                                div [ class "bg-white w-full p-4" ]
                                    [ viewResultsOverview translators results
                                    ]
                                    |> Html.map searchToMsg

            ResultsShowed RemoteData.Loading _ ->
                View.Components.loadingLogoAnimated translators

            _ ->
                viewRecentQueries translators searchModel.recentQueries
                    |> Html.map searchToMsg
        ]


viewEmptyResults : Translators -> String -> Html msg
viewEmptyResults { t } queryText =
    div [ class "flex-grow bg-white text-center" ]
        [ h3 [ class "mt-20 text-xl font-bold" ]
            [ text <| t "menu.search.notFoundLabel" ++ " \"" ++ queryText ++ "\"" ]
        , div []
            [ img
                [ class "w-2/3 mx-auto md:w-64 mt-6 mb-8"
                , src "/images/not_found.svg"
                ]
                []
            , text (t "menu.search.notFoundResults")
            ]
        ]


viewRecentQueries : Translators -> List String -> Html Msg
viewRecentQueries { t } recentQueries =
    let
        viewItem q =
            li
                [ class "leading-10 hover:text-orange-500 cursor-pointer"
                , onClick (RecentQueryClicked q)
                ]
                [ Icons.clock "inline-block align-middle mr-3 fill-current text-gray-900"
                , span [ class "inline align-middle" ] [ text q ]
                ]
    in
    div [ class "w-full p-4 bg-white" ]
        [ strong [] [ text (t "menu.search.recentlyHeader") ]
        , ul [ class "text-gray-900" ]
            (List.map viewItem recentQueries)
        ]


viewTabs : Translators -> SearchResults -> ActiveTab -> Html Msg
viewTabs { t } results activeTab =
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
                , class "rounded-sm flex-1 text-center cursor-pointer capitalize"
                , if count > 0 then
                    onClick clickMsg

                  else
                    class "cursor-not-allowed text-gray-300"
                ]
                [ text label
                , text " ("
                , text (String.fromInt count)
                , text ")"
                ]
    in
    ul [ class "space-x-2 flex items-stretch leading-10 p-4 pb-2 bg-white" ]
        [ viewTab OffersTab
            (t "menu.search.offers")
            results.offers
            (TabActivated OffersTab)
        , viewTab ActionsTab
            (t "menu.search.actions")
            results.actions
            (TabActivated ActionsTab)
        ]


viewResultsOverview : Translators -> SearchResults -> Html Msg
viewResultsOverview { t } { offers, actions } =
    let
        viewItem icon count singular plural showMsg =
            li [ class "py-4 flex items-center" ]
                [ div [ class "flex-grow flex items-center" ]
                    [ icon "w-6 h-6 text-black fill-current mr-3"
                    , span []
                        [ text <| t "menu.search.overviewFound"
                        , text " "
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
                    [ text (t "menu.search.overviewShow") ]
                ]
    in
    div []
        [ strong [ class "block pb-2" ]
            [ text (t "menu.search.overviewHeader") ]
        , ul []
            [ viewItem Icons.shop
                (List.length offers)
                (t "menu.search.offer")
                (t "menu.search.offers")
                (TabActivated OffersTab)
            , viewItem Icons.flag
                (List.length actions)
                (t "menu.search.action")
                (t "menu.search.actions")
                (TabActivated ActionsTab)
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



-- SUBSCRIPTION


subscriptions : Sub Msg
subscriptions =
    Ports.gotRecentSearches GotRecentSearches



-- HELPERS


isActive : Model -> Bool
isActive model =
    case model.state of
        Inactive ->
            False

        _ ->
            True


closeSearch : Shared -> Model -> ( Model, Cmd Msg )
closeSearch shared model =
    update shared model CancelClicked
