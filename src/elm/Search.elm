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
    , viewForm
    , viewSearchBody
    )

import Action exposing (Action)
import Api.Graphql
import Browser.Dom as Dom
import Cambiatus.Object
import Cambiatus.Object.Product
import Cambiatus.Object.SearchResult
import Cambiatus.Query
import Eos exposing (Symbol)
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, br, button, div, h3, img, li, p, span, strong, text, ul)
import Html.Attributes exposing (autocomplete, class, minlength, required, src)
import Html.Events exposing (onClick, onFocus, onSubmit)
import Icons
import Json.Decode as Decode exposing (list, string)
import Json.Encode as Encode
import List.Extra as List
import Ports
import RemoteData exposing (RemoteData)
import Route exposing (Route)
import Session.Shared exposing (Shared, Translators)
import Task
import Time exposing (Posix)
import View.Components
import View.Form.Input as Input



-- MODEL


type alias Model =
    { state : State
    , currentQuery : String
    , recentQueries : List String
    }


init : Model
init =
    { state = Inactive
    , currentQuery = ""
    , recentQueries = []
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
    , units : Int
    , trackStock : Bool
    }



-- GRAPHQL


type alias FoundData =
    RemoteData (Graphql.Http.Error SearchResults) SearchResults


sendSearchQuery : Symbol -> Shared -> String -> String -> Cmd Msg
sendSearchQuery selectedCommunity shared queryString authToken =
    let
        req =
            { communityId = Eos.symbolToString selectedCommunity }
    in
    Api.Graphql.query
        shared
        (Just authToken)
        (Cambiatus.Query.search req (searchResultSelectionSet queryString))
        GotSearchResults


searchResultSelectionSet : String -> SelectionSet SearchResults Cambiatus.Object.SearchResult
searchResultSelectionSet queryString =
    SelectionSet.succeed SearchResults
        |> with (Cambiatus.Object.SearchResult.products (\_ -> { query = Present queryString }) offersSelectionSet)
        |> with (Cambiatus.Object.SearchResult.actions (\_ -> { query = Present queryString }) Action.selectionSet)


offersSelectionSet : SelectionSet Offer Cambiatus.Object.Product
offersSelectionSet =
    SelectionSet.map6 Offer
        Cambiatus.Object.Product.id
        Cambiatus.Object.Product.title
        Cambiatus.Object.Product.price
        Cambiatus.Object.Product.image
        Cambiatus.Object.Product.units
        Cambiatus.Object.Product.trackStock



-- UPDATE


type Msg
    = NoOp
    | CancelClicked
    | InputFocused
    | GotRecentSearches String
    | RecentQueryClicked String
    | GotSearchResults FoundData
    | QuerySubmitted
    | TabActivated ActiveTab
    | CurrentQueryChanged String
    | ClearSearchIconClicked
    | FoundItemClicked Route


update : Shared -> String -> Symbol -> Model -> Msg -> ( Model, Cmd Msg )
update shared authToken symbol model msg =
    case msg of
        NoOp ->
            ( model, Cmd.none )

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
                authToken
                symbol
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
            ( closeSearch model
            , Cmd.none
            )

        ClearSearchIconClicked ->
            ( { model
                | currentQuery = ""
              }
            , Dom.focus "searchInput"
                |> Task.attempt (\_ -> NoOp)
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
            in
            ( { model
                | recentQueries = newRecentSearches
                , state = ResultsShowed RemoteData.Loading Nothing
              }
            , Cmd.batch
                [ storeRecentSearches
                , sendSearchQuery symbol shared model.currentQuery authToken
                ]
            )



-- VIEW


viewForm : Translators -> Model -> Html Msg
viewForm ({ t } as translators) model =
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

        viewClearSearchIcon =
            case model.state of
                Inactive ->
                    text ""

                _ ->
                    if String.isEmpty model.currentQuery then
                        text ""

                    else
                        span
                            [ class "cursor-pointer absolute right-0 mr-3 h-full flex items-center top-0"
                            , onClick ClearSearchIconClicked
                            ]
                            [ Icons.clearInput ""
                            ]

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
            [ Input.init
                { label = ""
                , id = "searchInput"
                , onInput = CurrentQueryChanged
                , disabled = isLoading
                , value = model.currentQuery
                , placeholder = Just (t "menu.search.placeholder")
                , problems = Nothing
                , translators = translators
                }
                |> Input.withContainerAttrs [ class "!m-0" ]
                |> Input.withAttrs
                    [ class "rounded-full bg-gray-100 border-0 pl-10 text-base h-10"
                    , onFocus InputFocused
                    , minlength 3
                    , required True
                    , autocomplete False
                    ]
                |> Input.withElements
                    [ viewClearSearchIcon
                    , Icons.search <| "absolute top-0 left-0 mt-2 ml-2 fill-current " ++ " " ++ iconColor
                    ]
                |> Input.toHtml
            ]
        , viewCancel
        ]


viewSearchBody :
    Translators
    -> Symbol
    -> Posix
    -> (Msg -> parentMsg)
    -> (Action.Msg -> parentMsg)
    -> Model
    -> Html parentMsg
viewSearchBody translators selectedCommunity today searchToMsg actionToMsg searchModel =
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
                                    , viewOffers translators selectedCommunity results.offers
                                    ]
                                    |> Html.map searchToMsg

                            Just ActionsTab ->
                                div [ class "w-full" ]
                                    [ viewTabs translators results ActionsTab
                                        |> Html.map searchToMsg
                                    , Action.viewSearchActions translators today results.actions
                                        |> Html.map actionToMsg
                                    ]

                            Nothing ->
                                div [ class "bg-white w-full p-4" ]
                                    [ viewResultsOverview translators results
                                    ]
                                    |> Html.map searchToMsg

            ResultsShowed RemoteData.Loading _ ->
                View.Components.loadingLogoAnimated translators ""

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


viewOffers : Translators -> Symbol -> List Offer -> Html Msg
viewOffers translators symbol offers =
    let
        viewOffer : Offer -> Html Msg
        viewOffer offer =
            let
                imageUrl =
                    case offer.image of
                        Nothing ->
                            "/icons/shop-placeholder1.svg"

                        Just "" ->
                            "/icons/shop-placeholder1.svg"

                        Just url ->
                            url
            in
            li
                [ class "flex px-2 w-1/2 sm:w-1/3 md:w-1/4" ]
                [ div
                    [ class "rounded-md overflow-hidden bg-white flex-grow mb-4 pb-4 cursor-pointer hover:shadow"
                    , onClick (FoundItemClicked (Route.ViewSale (String.fromInt offer.id)))
                    ]
                    [ img [ src imageUrl ] []
                    , h3 [ class "p-3" ] [ text offer.title ]
                    , if offer.units == 0 && offer.trackStock then
                        p [ class "px-3 leading-none text-xl text-red" ]
                            [ text (translators.t "shop.out_of_stock")
                            ]

                      else
                        p [ class "px-3 leading-none" ]
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


closeSearch : Model -> Model
closeSearch model =
    { model | state = Inactive, currentQuery = "" }
