module Search exposing
    ( ActiveTab(..)
    , ExternalMsg(..)
    , Model
    , Msg
    , State(..)
    , closeMsg
    , closeSearch
    , init
    , isActive
    , isOpenMsg
    , subscriptions
    , update
    , viewForm
    , viewSearchBody
    )

import Action exposing (Action)
import Auth
import Avatar
import Browser.Dom as Dom
import Cambiatus.Object
import Cambiatus.Object.SearchResult
import Cambiatus.Query
import Debouncer.Basic
import Eos exposing (Symbol)
import Eos.Account
import Form
import Form.Text
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html exposing (Html, a, br, button, div, h3, img, li, p, span, strong, text, ul)
import Html.Attributes exposing (alt, autocomplete, class, classList, disabled, minlength, src, tabindex, type_)
import Html.Attributes.Aria exposing (role)
import Html.Events exposing (onClick, onFocus)
import Icons
import Json.Decode as Decode exposing (list, string)
import Json.Encode as Encode
import List.Extra as List
import Ports
import Profile
import RemoteData exposing (RemoteData)
import Route
import Session.Shared exposing (Shared, Translators)
import Shop
import Task
import Time exposing (Posix)
import UpdateResult as UR
import Utils
import View.Components
import View.Feedback as Feedback



-- MODEL


type alias Model =
    { state : State
    , form : Form.Model FormInput
    , recentQueries : List String
    , debouncer : Debouncer.Basic.Debouncer Msg Msg
    }


init : Model
init =
    { state = Inactive
    , form = Form.init { query = "" }
    , recentQueries = []
    , debouncer =
        Debouncer.Basic.debounce (Debouncer.Basic.fromSeconds 0.3)
            |> Debouncer.Basic.toDebouncer
    }



-- TYPES


type State
    = Inactive
    | RecentSearchesShowed
    | ResultsShowed FoundData (Maybe ActiveTab)


type ActiveTab
    = OffersTab
    | ActionsTab
    | MembersTab


type alias SearchResults =
    { offers : List ProductInfo
    , actions : List Action
    , members : List Profile.Minimal
    }


type alias ProductInfo =
    { product : Shop.Product
    , currentVisibleImage : Maybe Shop.ImageId
    , previousVisibleImage : Maybe Shop.ImageId
    }



-- GRAPHQL


type alias FoundData =
    RemoteData (Graphql.Http.Error SearchResults) SearchResults


sendSearchQuery : String -> ExternalMsg
sendSearchQuery queryString =
    RequestQuery
        (Cambiatus.Query.search (searchResultSelectionSet queryString))
        (GotSearchResults { for = queryString })


searchResultSelectionSet : String -> SelectionSet SearchResults Cambiatus.Object.SearchResult
searchResultSelectionSet queryString =
    SelectionSet.succeed
        (\products actions members ->
            { offers =
                List.map
                    (\product ->
                        { product = product
                        , currentVisibleImage = Nothing
                        , previousVisibleImage = Nothing
                        }
                    )
                    products
            , actions = actions
            , members = members
            }
        )
        |> with (Cambiatus.Object.SearchResult.products (\_ -> { query = Present queryString }) Shop.productSelectionSet)
        |> with (Cambiatus.Object.SearchResult.actions (\_ -> { query = Present queryString }) Action.selectionSet)
        |> with
            (Cambiatus.Object.SearchResult.members
                (\_ ->
                    { filters =
                        Present
                            { orderDirection = Absent
                            , orderMembersBy = Absent
                            , searchMembersBy = Absent
                            , searchString = Present queryString
                            }
                    }
                )
                Profile.minimalSelectionSet
            )


storeRecentSearches : List String -> Cmd msg
storeRecentSearches recentSearches =
    recentSearches
        |> List.unique
        |> List.take maximumRecentSearches
        |> Encode.list Encode.string
        |> Encode.encode 0
        |> Ports.storeRecentSearches


maximumRecentSearches : Int
maximumRecentSearches =
    10



-- UPDATE


type Msg
    = NoOp
    | GotDebouncerMsg (Debouncer.Basic.Msg Msg)
    | CancelClicked
    | InputFocused
    | GotRecentSearches String
    | RecentQueryClicked String
    | GotSearchResults { for : String } FoundData
    | QuerySubmitted
    | TabActivated ActiveTab
    | GotFormMsg (Form.Msg FormInput)
    | RequestedNewResults String
    | ClearSearchIconClicked
    | FoundItemClicked
    | ClickedScrollToImage { containerId : String, imageId : String }
    | ImageStartedIntersecting Shop.Id Shop.ImageId
    | ImageStoppedIntersecting Shop.Id Shop.ImageId


type alias UpdateResult =
    UR.UpdateResult Model Msg ExternalMsg


type ExternalMsg
    = SetFeedback Feedback.Model
    | RequestQuery (SelectionSet SearchResults RootQuery) (FoundData -> Msg)


update : Shared -> Model -> Msg -> UpdateResult
update shared model msg =
    case msg of
        NoOp ->
            UR.init model

        GotDebouncerMsg subMsg ->
            let
                ( subModel, subCmd, emittedMsg ) =
                    Debouncer.Basic.update subMsg model.debouncer

                mappedCmd =
                    Cmd.map GotDebouncerMsg subCmd

                updatedModel =
                    { model | debouncer = subModel }
            in
            case emittedMsg of
                Just emitted ->
                    update shared updatedModel emitted
                        |> UR.addCmd mappedCmd

                Nothing ->
                    updatedModel
                        |> UR.init
                        |> UR.addCmd mappedCmd

        FoundItemClicked ->
            { model
                | state = Inactive
                , form = Form.updateValues (\form -> { form | query = "" }) model.form
            }
                |> UR.init

        RecentQueryClicked q ->
            update shared
                { model
                    | state = ResultsShowed RemoteData.Loading Nothing
                    , form = Form.updateValues (\form -> { form | query = q }) model.form
                }
                QuerySubmitted

        TabActivated activeTab ->
            case model.state of
                ResultsShowed r previousActiveTab ->
                    let
                        newRecentQueries =
                            (Form.getValue .query model.form :: model.recentQueries)
                                |> List.unique
                                |> List.take maximumRecentSearches
                    in
                    { model
                        | state = ResultsShowed r (Just activeTab)
                        , recentQueries = newRecentQueries
                    }
                        |> UR.init
                        |> UR.addCmd
                            (case previousActiveTab of
                                Nothing ->
                                    storeRecentSearches newRecentQueries

                                Just _ ->
                                    Cmd.none
                            )

                _ ->
                    UR.init model

        GotSearchResults { for } res ->
            { model
                | state =
                    if String.isEmpty (Form.getValue .query model.form) then
                        model.state

                    else if for == Form.getValue .query model.form then
                        ResultsShowed res Nothing

                    else
                        model.state
            }
                |> UR.init

        GotRecentSearches queries ->
            case Decode.decodeString (list string) queries of
                Ok queryList ->
                    { model | recentQueries = queryList }
                        |> UR.init

                Err _ ->
                    UR.init model

        CancelClicked ->
            closeSearch model
                |> UR.init

        GotFormMsg subMsg ->
            let
                oldQuery =
                    Form.getValue .query model.form

                updateResult =
                    Form.update shared subMsg model.form
                        |> UR.fromChild (\newForm -> { model | form = newForm })
                            GotFormMsg
                            (SetFeedback >> UR.addExt)
                            model

                newQuery =
                    Form.getValue .query updateResult.model.form

                actOnQueryChange =
                    if String.isEmpty newQuery then
                        UR.mapModel (\m -> { m | state = RecentSearchesShowed })

                    else if oldQuery /= newQuery then
                        UR.addMsg
                            (RequestedNewResults newQuery
                                |> Debouncer.Basic.provideInput
                                |> GotDebouncerMsg
                            )

                    else
                        identity
            in
            updateResult
                |> actOnQueryChange

        RequestedNewResults queryString ->
            UR.init model
                |> UR.addExt (sendSearchQuery queryString)

        ClearSearchIconClicked ->
            { model
                | form = Form.updateValues (\form -> { form | query = "" }) model.form
                , state = RecentSearchesShowed
            }
                |> UR.init
                |> UR.addCmd
                    (Dom.focus "searchInput"
                        |> Task.attempt (\_ -> NoOp)
                    )

        InputFocused ->
            { model
                | state =
                    case model.state of
                        Inactive ->
                            RecentSearchesShowed

                        _ ->
                            model.state
            }
                |> UR.init

        QuerySubmitted ->
            let
                currentQuery =
                    Form.getValue .query model.form

                newRecentSearches : List String
                newRecentSearches =
                    (currentQuery :: model.recentQueries)
                        |> List.unique
                        |> List.take maximumRecentSearches
            in
            { model
                | recentQueries = newRecentSearches
                , state = ResultsShowed RemoteData.Loading Nothing
            }
                |> UR.init
                |> UR.addCmd (storeRecentSearches newRecentSearches)
                |> UR.addExt (sendSearchQuery currentQuery)

        ClickedScrollToImage { containerId, imageId } ->
            model
                |> UR.init
                |> UR.addPort
                    { responseAddress = NoOp
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "smoothHorizontalScroll" )
                            , ( "containerId", Encode.string containerId )
                            , ( "targetId", Encode.string imageId )
                            ]
                    }

        ImageStartedIntersecting productId imageId ->
            case model.state of
                ResultsShowed (RemoteData.Success searchResults) activeTab ->
                    let
                        newOffers =
                            searchResults.offers
                                |> List.updateIf
                                    (\{ product } -> product.id == productId)
                                    (\productInfo ->
                                        { productInfo
                                            | currentVisibleImage = Just imageId
                                            , previousVisibleImage = productInfo.currentVisibleImage
                                        }
                                    )
                    in
                    { model
                        | state =
                            ResultsShowed
                                (RemoteData.Success { searchResults | offers = newOffers })
                                activeTab
                    }
                        |> UR.init

                _ ->
                    model
                        |> UR.init

        ImageStoppedIntersecting productId imageId ->
            case model.state of
                ResultsShowed (RemoteData.Success searchResults) activeTab ->
                    let
                        newOffers =
                            searchResults.offers
                                |> List.updateIf
                                    (\{ product } -> product.id == productId)
                                    (\productInfo ->
                                        if Just imageId == productInfo.currentVisibleImage then
                                            { productInfo
                                                | currentVisibleImage = productInfo.previousVisibleImage
                                                , previousVisibleImage = Nothing
                                            }

                                        else if Just imageId == productInfo.previousVisibleImage then
                                            { productInfo | previousVisibleImage = Nothing }

                                        else
                                            productInfo
                                    )
                    in
                    { model
                        | state =
                            ResultsShowed
                                (RemoteData.Success { searchResults | offers = newOffers })
                                activeTab
                    }
                        |> UR.init

                _ ->
                    model
                        |> UR.init



-- VIEW


type alias FormInput =
    { query : String }


type alias FormOutput =
    { query : String }


createForm : Translators -> Model -> Form.Form Msg FormInput FormOutput
createForm { t } model =
    let
        isLoading =
            case model.state of
                ResultsShowed RemoteData.Loading _ ->
                    True

                _ ->
                    False

        isSearchOpen =
            case model.state of
                Inactive ->
                    False

                _ ->
                    True

        iconColor =
            case model.state of
                Inactive ->
                    "text-gray-400"

                _ ->
                    "text-indigo-500"

        viewClearSearchIcon =
            if isSearchOpen && not (String.isEmpty (Form.getValue .query model.form)) then
                button
                    [ class "absolute right-3 flex items-center top-1/2 -translate-y-1/2 focus-ring focus-visible:ring-red focus-visible:ring-opacity-50 rounded-full group"
                    , onClick ClearSearchIconClicked
                    , type_ "button"
                    ]
                    [ Icons.clearInput "fill-current text-gray-400 hover:text-red group-focus:text-red"
                    ]

            else
                text ""
    in
    Form.succeed FormOutput
        |> Form.with
            (Form.Text.init
                { label = ""
                , id = "search-input"
                }
                |> Form.Text.withPlaceholder (t "menu.search.placeholder")
                |> Form.Text.withDisabled isLoading
                |> Form.Text.withContainerAttrs [ class "!m-0 w-full" ]
                |> Form.Text.withExtraAttrs
                    [ class "rounded-full bg-gray-100 border-0 pl-12 h-12"
                    , onFocus InputFocused
                    , minlength 3
                    , autocomplete False
                    ]
                |> Form.Text.withElements
                    [ viewClearSearchIcon
                    , Icons.search ("absolute top-0 mt-[10px] left-4 fill-current " ++ iconColor)
                    ]
                |> Form.textField
                    { parser = Ok
                    , value = .query
                    , update = \query input -> { input | query = query }
                    , externalError = always Nothing
                    }
            )
        |> Form.withNoOutput
            (Form.unsafeArbitrary
                (button
                    [ class "text-orange-300 ml-3 lowercase hover:underline focus:outline-none focus:underline"
                    , classList [ ( "hidden", not isSearchOpen ) ]
                    , onClick CancelClicked
                    , type_ "button"
                    ]
                    [ text (t "menu.cancel") ]
                )
            )


viewForm : List (Html.Attribute Msg) -> Translators -> Model -> Html Msg
viewForm attrs translators model =
    Form.viewWithoutSubmit (class "flex items-center" :: attrs)
        translators
        (\_ -> [])
        (createForm translators model)
        model.form
        { toMsg = GotFormMsg }


type alias LoggedIn loggedIn community =
    { loggedIn
        | accountName : Eos.Account.Name
        , shared : Shared
        , selectedCommunity : RemoteData (Graphql.Http.Error (Maybe (Community community))) (Community community)
        , auth : Auth.Model
        , profile : RemoteData (Graphql.Http.Error (Maybe Profile.Model)) Profile.Model
    }


type alias Community community =
    { community | symbol : Symbol }


viewSearchBody :
    LoggedIn loggedIn community
    -> Symbol
    -> Posix
    -> (Msg -> parentMsg)
    -> (Action.Msg -> parentMsg)
    -> Model
    -> Html parentMsg
viewSearchBody loggedIn selectedCommunity today searchToMsg actionToMsg searchModel =
    let
        translators =
            loggedIn.shared.translators
    in
    div [ class "container mx-auto flex flex-grow" ]
        [ case searchModel.state of
            ResultsShowed (RemoteData.Success results) activeTab ->
                if List.isEmpty results.actions && List.isEmpty results.offers && List.isEmpty results.members then
                    viewEmptyResults translators (Form.getValue .query searchModel.form)
                        |> Html.map searchToMsg

                else
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
                                , View.Components.masonryLayout
                                    [ View.Components.Sm ]
                                    { transitionWithParent = False }
                                    [ class "grid mt-4 lg:mt-6 marker-hidden gap-4 lg:gap-x-6 sm:grid-cols-2 lg:grid-cols-3"
                                    , role "list"
                                    ]
                                    (List.map
                                        (Action.viewCard loggedIn
                                            { containerAttrs = [ class "mb-4 lg:mb-6" ]
                                            , position = Nothing
                                            , toMsg = actionToMsg
                                            }
                                        )
                                        results.actions
                                    )
                                ]

                        Just MembersTab ->
                            div [ class "w-full" ]
                                [ viewTabs translators results MembersTab
                                , viewMembers translators results.members
                                ]
                                |> Html.map searchToMsg

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
            li []
                [ button
                    [ class "flex items-center w-full hover:text-orange-500 focus:text-orange-500 focus:outline-none"
                    , onClick (RecentQueryClicked q)
                    ]
                    [ Icons.clock "mr-3 fill-current"
                    , span [] [ text q ]
                    ]
                ]
    in
    div [ class "w-full p-4 bg-white" ]
        [ strong [] [ text (t "menu.search.recentlyHeader") ]
        , ul [ class "text-gray-900 mt-4 space-y-2" ]
            (List.map viewItem recentQueries)
        ]


viewTabs : Translators -> SearchResults -> ActiveTab -> Html Msg
viewTabs { tr } results activeTab =
    let
        viewTab : ActiveTab -> String -> List a -> Msg -> Html Msg
        viewTab tabKind label foundItems clickMsg =
            let
                count =
                    List.length foundItems
            in
            li [ class "w-full" ]
                [ button
                    [ class "rounded-sm text-center capitalize w-full py-3 focus-ring focus-visible:ring-gray-300"
                    , classList
                        [ ( "bg-orange-300 text-white font-bold", activeTab == tabKind )
                        , ( "bg-gray-100 hover:bg-gray-200 focus:bg-gray-200", activeTab /= tabKind )
                        , ( "cursor-not-allowed text-gray-300", count <= 0 )
                        ]
                    , if count > 0 then
                        onClick clickMsg

                      else
                        tabindex -1
                    , if activeTab == tabKind then
                        tabindex -1

                      else
                        class ""
                    ]
                    [ text <| tr label [ ( "count", String.fromInt count ) ] ]
                ]
    in
    ul [ class "space-x-2 flex items-stretch p-4 pb-2 bg-white" ]
        [ viewTab OffersTab
            "menu.search.offers_title"
            results.offers
            (TabActivated OffersTab)
        , viewTab ActionsTab
            "menu.search.actions_title"
            results.actions
            (TabActivated ActionsTab)
        , viewTab MembersTab
            "menu.search.members_title"
            results.members
            (TabActivated MembersTab)
        ]


viewResultsOverview : Translators -> SearchResults -> Html Msg
viewResultsOverview { t, tr } { offers, actions, members } =
    let
        viewItem icon count singular plural showMsg =
            li [ class "py-4 flex items-center" ]
                [ div [ class "flex-grow flex items-center" ]
                    [ icon "w-6 h-6 text-black fill-current mr-3"
                    , span []
                        [ text <| t "menu.search.overviewFound"
                        , text " "
                        , strong []
                            [ if count == 1 then
                                text <| t singular

                              else
                                text <| tr plural [ ( "count", String.fromInt count ) ]
                            ]
                        ]
                    ]
                , button
                    [ class "button w-auto button-sm px-6"
                    , classList
                        [ ( "button-disabled", count == 0 )
                        , ( "button-primary", count /= 0 )
                        ]
                    , disabled (count == 0)
                    , onClick showMsg
                    ]
                    [ text (t "menu.search.overviewShow") ]
                ]
    in
    div []
        [ strong [ class "block pb-2" ]
            [ text (t "menu.search.overviewHeader") ]
        , ul []
            [ viewItem Icons.shop
                (List.length offers)
                "menu.search.offer"
                "menu.search.offers"
                (TabActivated OffersTab)
            , viewItem Icons.flag
                (List.length actions)
                "menu.search.action"
                "menu.search.actions"
                (TabActivated ActionsTab)
            , viewItem Icons.accountCircle
                (List.length members)
                "menu.search.member"
                "menu.search.members"
                (TabActivated MembersTab)
            ]
        ]


viewOffers : Translators -> Symbol -> List ProductInfo -> Html Msg
viewOffers translators symbol offers =
    let
        viewOffer : ProductInfo -> Html Msg
        viewOffer productInfo =
            let
                images =
                    case productInfo.product.images of
                        [] ->
                            img
                                [ src "/icons/shop-placeholder1.svg"
                                , alt ""
                                , class "h-32"
                                ]
                                []

                        firstImage :: otherImages ->
                            Shop.viewImageCarrousel
                                translators
                                { containerAttrs = [ class "h-32" ]
                                , listAttrs = [ class "gap-x-4 rounded-t bg-gray-100" ]
                                , imageContainerAttrs = [ class "bg-white rounded-t" ]
                                , imageOverlayAttrs = []
                                , imageAttrs = [ class "w-full h-full" ]
                                }
                                { showArrows = False
                                , productId = Just productInfo.product.id
                                , onScrollToImage = ClickedScrollToImage
                                , currentIntersecting = productInfo.currentVisibleImage
                                , onStartedIntersecting = ImageStartedIntersecting productInfo.product.id
                                , onStoppedIntersecting = ImageStoppedIntersecting productInfo.product.id
                                }
                                ( firstImage, otherImages )
            in
            li
                [ class "flex px-2 w-1/2 sm:w-1/3 md:w-1/4" ]
                [ a
                    [ class "rounded-md overflow-hidden bg-white flex-grow mb-4 pb-4 cursor-pointer hover:shadow focus:shadow focus:outline-none focus:ring focus:ring-gray-400"
                    , onClick FoundItemClicked
                    , Route.href (Route.ViewSale productInfo.product.id)
                    ]
                    [ images
                    , h3 [ class "p-3" ] [ text productInfo.product.title ]
                    , if Shop.isOutOfStock productInfo.product then
                        p [ class "px-3 text-xl text-red" ]
                            [ text (translators.t "shop.out_of_stock")
                            ]

                      else
                        p [ class "px-3" ]
                            [ span [ class "text-xl text-green font-semibold" ] [ text <| String.fromFloat productInfo.product.price ]
                            , br [] []
                            , span [ class "text-gray-900 text-sm" ]
                                [ text <| Eos.symbolToSymbolCodeString symbol
                                ]
                            ]
                    ]
                ]
    in
    ul [ class "flex flex-wrap mt-6 mb-8 mx-2 justify-left" ]
        (List.map viewOffer offers)


viewMembers : Translators -> List Profile.Minimal -> Html Msg
viewMembers { t } members =
    let
        viewMember member =
            div [ class "bg-white rounded flex flex-col p-4 space-y-4 justify-between" ]
                [ div [ class "grid grid-cols-3 px-4" ]
                    [ Avatar.view member.avatar "h-20 w-20 my-auto"
                    , div [ class "flex flex-col col-span-2" ]
                        [ span [ class "text-lg font-bold capitalize" ] [ text (member.name |> Maybe.withDefault "") ]
                        , span [ class "text-sm text-gray-900" ] [ text (member.email |> Maybe.withDefault "") ]
                        , span [ class "text-sm text-gray-900" ] [ text (Eos.Account.nameToString member.account) ]
                        ]
                    ]
                , a
                    [ class "w-full button button-secondary cursor-pointer"
                    , onClick FoundItemClicked
                    , Route.href (Route.Profile member.account)
                    ]
                    [ text (t "all_analysis.more_details") ]
                ]
    in
    ul [ class "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4 mt-6 mb-8 mx-2" ]
        (List.map viewMember members)



-- SUBSCRIPTION


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Ports.gotRecentSearches GotRecentSearches
        , Utils.escSubscription CancelClicked
        ]



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
    { model
        | state = Inactive
        , form = Form.updateValues (\form -> { form | query = "" }) model.form
    }


closeMsg : Msg
closeMsg =
    CancelClicked


isOpenMsg : Msg -> Bool
isOpenMsg msg =
    case msg of
        InputFocused ->
            True

        _ ->
            False
