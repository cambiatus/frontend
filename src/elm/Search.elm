module Search exposing (Model, Msg, init, isActive, subscriptions, update, viewForm, viewRecentQueries, viewSearchBody)

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
import Html exposing (Html, button, div, h3, img, input, li, p, span, strong, text, ul)
import Html.Attributes exposing (class, placeholder, src, type_, value)
import Html.Events exposing (onClick, onFocus, onInput, onSubmit)
import Icons
import Json.Decode as Decode exposing (list, string)
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
    , found : Maybe SearchResult
    }


init : Symbol -> Model
init selectedCommunity =
    { state = Inactive
    , recentQueries = []
    , queryText = ""
    , selectedCommunity = selectedCommunity
    , found = Nothing
    }



-- TYPES


type State
    = Inactive
    | Active String
    | ResultsShowed
    | OffersShowed
    | ActionsShowed



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
    { offers : List SearchOffer
    , actions : List SearchAction
    }


type alias SearchOffer =
    { title : String
    , price : Float
    , image : Maybe String
    }


type alias SearchAction =
    { description : String
    }


searchResultSelectionSet : String -> SelectionSet SearchResult Cambiatus.Object.SearchResult
searchResultSelectionSet queryString =
    SelectionSet.succeed SearchResult
        |> with (Cambiatus.Object.SearchResult.products (\_ -> { query = Present queryString }) productsSelectionSet)
        |> with (Cambiatus.Object.SearchResult.actions (\_ -> { query = Present queryString }) actionsSelectionSet)


productsSelectionSet : SelectionSet SearchOffer Cambiatus.Object.Product
productsSelectionSet =
    SelectionSet.map3 SearchOffer
        Cambiatus.Object.Product.title
        Cambiatus.Object.Product.price
        Cambiatus.Object.Product.image


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
    | ShowOffersClicked
    | ShowActionsClicked


update : Shared -> Model -> Msg -> ( Model, Cmd Msg )
update shared model msg =
    case msg of
        RecentSearchClicked q ->
            update shared { model | queryText = q } QuerySubmitted

        ShowOffersClicked ->
            ( { model | state = OffersShowed }, Cmd.none )

        ShowActionsClicked ->
            ( { model | state = ActionsShowed }, Cmd.none )

        SearchResultsLoaded res ->
            case res of
                Ok searchResult ->
                    ( { model | found = Just searchResult }
                    , Cmd.none
                    )

                Err _ ->
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
            [ class "w-full mt-2 flex items-center"
            , onSubmit QuerySubmitted
            ]
            [ div [ class "relative w-full" ]
                [ input
                    [ type_ "search"
                    , class "w-full form-input rounded-full bg-gray-100 pl-10 m-0"
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
                        [ class "text-orange-300 ml-3"
                        , onClick (StateChanged Inactive)
                        ]
                        [ text "cancel" ]

                _ ->
                    text ""
            ]
        ]


viewSearchBody : Model -> Html Msg
viewSearchBody model =
    div [ class "container mx-auto p-4" ]
        [ case model.found of
            Just results ->
                viewResults model.state results

            Nothing ->
                viewRecentQueries model
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
                [ strong [] [ text "Recently searched" ]
                , ul [ class "text-gray-900" ]
                    (List.map viewQuery model.recentQueries)
                ]

        _ ->
            text ""


type ActiveTab
    = OffersTab
    | ActionsTab


viewTabs results activeTab =
    let
        viewTab tab label clickMsg =
            li
                [ onClick clickMsg
                , if activeTab == tab then
                    class "bg-orange-300 text-white"

                  else
                    class "bg-gray-100"
                , class "rounded-sm flex-1 text-center"
                ]
                [ text label ]
    in
    ul [ class "space-x-2 flex items-stretch leading-10" ]
        [ viewTab OffersTab ("Offers " ++ String.fromInt (List.length results.offers)) ShowOffersClicked
        , viewTab ActionsTab ("Actions " ++ String.fromInt (List.length results.actions)) ShowActionsClicked
        ]


viewOffers ({ offers, actions } as results) =
    let
        viewOffer : SearchOffer -> Html msg
        viewOffer offer =
            div [ class "border-2 rounded-lg overflow-hidden bg-white" ]
                [ case offer.image of
                    Nothing ->
                        text ""

                    Just url ->
                        img [ src url ] []
                , h3 [ class "px-2" ] [ text offer.title ]
                , p [ class "px-2" ] [ text <| String.fromFloat offer.price ]
                ]
    in
    div []
        [ viewTabs results OffersTab
        , div [ class "flex" ]
            (List.map viewOffer offers)
        ]


viewActions : SearchResult -> Html Msg
viewActions ({ actions, offers } as results) =
    let
        viewAction action =
            div [ class "border-2 rounded-lg overflow-hidden bg-white" ]
                [ div [] [ text action.description ]
                ]
    in
    div []
        [ viewTabs results ActionsTab
        , div [ class "flex" ]
            (List.map viewAction actions)
        ]


viewResults : State -> SearchResult -> Html Msg
viewResults state ({ actions, offers } as results) =
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
                    [ onClick showMsg
                    , class "button button-primary w-auto button-sm px-6"
                    ]
                    [ text "Show" ]
                ]
    in
    case state of
        OffersShowed ->
            viewOffers results

        ActionsShowed ->
            viewActions results

        _ ->
            div []
                [ strong [ class "block py-4" ] [ text "Here is what we found" ]
                , ul []
                    [ viewItem Icons.shop (List.length offers) "offer" "offers" ShowOffersClicked
                    , viewItem Icons.flag (List.length actions) "action" "actions" ShowActionsClicked
                    ]
                ]


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
