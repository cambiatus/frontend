module Page.Shop exposing
    ( Model
    , Msg
    , init
    , msgToString
    , receiveBroadcast
    , update
    , view
    )

import Api
import AssocList as Dict
import Cambiatus.Enum.Permission as Permission
import Community exposing (Balance)
import Eos
import Eos.Account
import Form
import Form.Checkbox
import Form.Radio
import Graphql.Http
import Html exposing (Html, a, br, button, div, h1, h2, img, li, p, span, text, ul)
import Html.Attributes exposing (alt, class, classList, src)
import Html.Attributes.Aria exposing (ariaLabel)
import Html.Events exposing (onClick)
import Http
import I18Next exposing (t)
import Icons
import Json.Encode as Encode
import List.Extra
import Markdown
import Page
import Profile.Summary
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn exposing (External)
import Session.Shared as Shared
import Shop exposing (Product)
import Shop.Category
import Translation
import Tree
import UpdateResult as UR
import Utils.Tree
import View.Components
import View.Modal as Modal



-- INIT


init : LoggedIn.Model -> { owner : Maybe Eos.Account.Name, categories : List Shop.Category.Id } -> UpdateResult
init loggedIn filter =
    initModel filter
        |> UR.init
        |> UR.addCmd (Api.getBalances loggedIn.shared loggedIn.accountName CompletedLoadBalances)
        |> UR.addExt
            (LoggedIn.query loggedIn
                (Shop.productsQuery { user = filter.owner, categories = filter.categories })
                CompletedSalesLoad
            )
        |> UR.addExt (LoggedIn.RequestedCommunityField Community.ShopCategoriesField)



-- MODEL


type alias Model =
    { cards : Status
    , balances : List Balance
    , currentFilter : { owner : Maybe Eos.Account.Name, categories : List Shop.Category.Id }
    , isFiltersModalOpen : Bool
    , filtersForm : Form.Model FiltersFormInput
    }


initModel : { owner : Maybe Eos.Account.Name, categories : List Shop.Category.Id } -> Model
initModel filter =
    { cards = Loading
    , balances = []
    , currentFilter = filter
    , isFiltersModalOpen = False
    , filtersForm =
        Form.init
            { owner = filter.owner

            -- Categories are filled in on CompletedLoadShopCategories
            , categories = Dict.empty
            }
    }


type alias FiltersFormInput =
    { owner : Maybe Eos.Account.Name
    , categories : CategoriesFormInput
    }


type alias FiltersFormOutput =
    { owner : Maybe Eos.Account.Name
    , categories : List Shop.Category.Id
    }


filtersForm : LoggedIn.Model -> RemoteData err (List Shop.Category.Tree) -> Form.Form msg FiltersFormInput FiltersFormOutput
filtersForm loggedIn allCategories =
    let
        loadingItem width =
            div [ class "flex items-center gap-x-2" ]
                [ div [ class "animate-skeleton-loading h-5 rounded w-5" ] []
                , div [ class "animate-skeleton-loading h-5 rounded-full w-5" ] []
                , div [ class "animate-skeleton-loading h-5 rounded-sm", class width ] []
                ]

        loadingForm =
            Form.arbitraryWith []
                (div [ class "flex flex-col gap-y-4" ]
                    [ loadingItem "w-1/2"
                    , loadingItem "w-2/3"
                    , loadingItem "w-1/3"
                    , loadingItem "w-1/2"
                    ]
                )
    in
    Form.succeed FiltersFormOutput
        |> Form.with (ownerForm loggedIn.shared.translators loggedIn.accountName)
        |> Form.withNoOutput
            (case allCategories of
                RemoteData.Success categories ->
                    if List.isEmpty categories then
                        Form.succeed []

                    else
                        Form.arbitrary
                            (p [ class "label" ]
                                [ text <| loggedIn.shared.translators.t "settings.shop.categories.title" ]
                            )

                _ ->
                    Form.arbitrary
                        (p [ class "label" ]
                            [ text <| loggedIn.shared.translators.t "settings.shop.categories.title" ]
                        )
            )
        |> Form.with
            (case allCategories of
                RemoteData.Success categories ->
                    if List.isEmpty categories then
                        Form.succeed []

                    else
                        categoriesForm loggedIn.shared.translators categories

                RemoteData.Loading ->
                    loadingForm

                RemoteData.NotAsked ->
                    loadingForm

                RemoteData.Failure _ ->
                    Form.arbitraryWith []
                        (p [ class "form-error" ]
                            [ text <| loggedIn.shared.translators.t "shop.filters.categories.error_fetching"
                            ]
                        )
            )


ownerForm : Translation.Translators -> Eos.Account.Name -> Form.Form msg { input | owner : Maybe Eos.Account.Name } (Maybe Eos.Account.Name)
ownerForm { t } currentUser =
    Form.Radio.init
        { label = t "shop.filters.offers.label"
        , id = "offers-radio"
        , optionToString =
            \maybeAccount ->
                case maybeAccount of
                    Nothing ->
                        ""

                    Just account ->
                        Eos.Account.nameToString account
        }
        |> Form.Radio.withOption Nothing (text <| t "shop.filters.offers.all")
        |> Form.Radio.withOption (Just currentUser) (text <| t "shop.filters.offers.mine")
        |> Form.Radio.withDirection Form.Radio.Vertical
        |> Form.Radio.withContainerAttrs [ class "mb-6" ]
        |> Form.radio
            (\account ->
                if String.isEmpty account then
                    Nothing

                else
                    Just (Eos.Account.stringToName account)
            )
            { parser = Ok
            , value = .owner
            , update = \owner values -> { values | owner = owner }
            , externalError = always Nothing
            }


type alias CategoriesFormInput =
    Dict.Dict Shop.Category.Model Bool


categoriesForm : Translation.Translators -> List Shop.Category.Tree -> Form.Form msg FiltersFormInput (List Shop.Category.Id)
categoriesForm { t } allCategories =
    let
        checkbox : Shop.Category.Model -> Form.Form msg CategoriesFormInput (Maybe Shop.Category.Id)
        checkbox category =
            Form.Checkbox.init
                { label =
                    span [ class "flex items-center gap-x-2" ]
                        [ case category.icon of
                            Nothing ->
                                text ""

                            Just icon ->
                                img [ class "w-5 h-5 rounded-full", alt "", src icon ] []
                        , text category.name
                        ]
                , id = "category-" ++ Shop.Category.idToString category.id
                }
                |> Form.Checkbox.withContainerAttrs [ class "flex" ]
                |> Form.checkbox
                    { parser =
                        \value ->
                            if value then
                                Ok (Just category.id)

                            else
                                Ok Nothing
                    , value = \input -> Dict.get category input |> Maybe.withDefault False
                    , update = Dict.insert category
                    , externalError = always Nothing
                    }

        treeToForm : Tree.Tree Shop.Category.Model -> Form.Form msg CategoriesFormInput (List Shop.Category.Id)
        treeToForm tree =
            Form.succeed
                (\label children ->
                    case label of
                        Nothing ->
                            children

                        Just head ->
                            head :: children
                )
                |> Form.withGroup []
                    (checkbox (Tree.label tree))
                    (if List.isEmpty (Tree.children tree) then
                        Form.succeed []

                     else
                        Tree.children tree
                            |> List.map treeToForm
                            |> Form.list [ class "ml-4 mt-4 flex flex-col gap-y-4" ]
                            |> Form.mapOutput List.concat
                    )
    in
    Form.succeed identity
        |> Form.with
            (allCategories
                |> List.map treeToForm
                |> Form.list [ class "flex flex-col gap-y-4" ]
                |> Form.mapOutput List.concat
                |> Form.mapValues
                    { value = .categories
                    , update = \newChild parent -> { parent | categories = newChild }
                    }
            )


type Status
    = Loading
    | Loaded (List Card)
    | LoadingFailed (Graphql.Http.Error (List Product))


type alias Card =
    { product : Product
    , profileSummary : Profile.Summary.Model
    , isAvailable : Bool
    , currentVisibleImage : Maybe Shop.ImageId
    , previousVisibleImage : Maybe Shop.ImageId
    }


cardFromSale : Product -> Card
cardFromSale p =
    { product = p
    , profileSummary = Profile.Summary.init False
    , isAvailable = not (Shop.isOutOfStock p)
    , currentVisibleImage = Nothing
    , previousVisibleImage = Nothing
    }



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        { t } =
            loggedIn.shared.translators

        selectedCommunityName =
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    community.name

                _ ->
                    ""

        title =
            selectedCommunityName
                ++ " "
                ++ t "shop.title"

        viewFrozenAccountCard =
            if not loggedIn.hasAcceptedCodeOfConduct then
                LoggedIn.viewFrozenAccountCard loggedIn.shared.translators
                    { onClick = ClickedAcceptCodeOfConduct
                    , isHorizontal = True
                    }
                    [ class "mx-auto shadow-lg mb-6" ]

            else
                text ""

        content community =
            case model.cards of
                Loading ->
                    div [ class "mt-6 mb-10" ]
                        [ div [ class "container mx-auto px-4" ]
                            [ viewFrozenAccountCard
                            , viewHeader loggedIn.shared.translators
                            , viewShopFilter loggedIn model
                            ]
                        , if List.isEmpty model.currentFilter.categories then
                            text ""

                          else
                            viewFilterTags community model
                        , if List.isEmpty model.currentFilter.categories then
                            text ""

                          else
                            viewFoundOffersAmount loggedIn.shared.translators community model
                        , viewFiltersModal loggedIn model
                        , div [ class "container mx-auto px-4" ]
                            [ Page.fullPageLoading loggedIn.shared ]
                        ]

                LoadingFailed e ->
                    Page.fullPageGraphQLError (t "shop.title") e

                Loaded cards ->
                    if List.isEmpty cards then
                        if model.currentFilter.owner == Nothing && List.isEmpty model.currentFilter.categories then
                            div [ class "container mx-auto px-4 mt-6" ]
                                [ viewFrozenAccountCard
                                , viewEmptyState loggedIn community.symbol model
                                ]

                        else
                            div [ class "mt-6" ]
                                [ div [ class "container mx-auto px-4" ]
                                    [ viewFrozenAccountCard
                                    , viewHeader loggedIn.shared.translators
                                    , viewShopFilter loggedIn model
                                    ]
                                , if List.isEmpty model.currentFilter.categories then
                                    text ""

                                  else
                                    viewFilterTags community model
                                , div [ class "container mx-auto px-4" ]
                                    [ viewEmptyStateWithFilters loggedIn.shared.translators ]
                                , viewFiltersModal loggedIn model
                                ]

                    else
                        div [ class "mt-6" ]
                            [ div [ class "container mx-auto px-4" ]
                                [ viewFrozenAccountCard
                                , viewHeader loggedIn.shared.translators
                                , viewShopFilter loggedIn model
                                ]
                            , if List.isEmpty model.currentFilter.categories then
                                text ""

                              else
                                viewFilterTags community model
                            , if List.isEmpty model.currentFilter.categories then
                                text ""

                              else
                                viewFoundOffersAmount loggedIn.shared.translators community model
                            , div [ class "container mx-auto px-4" ]
                                [ viewGrid loggedIn cards ]
                            , viewFiltersModal loggedIn model
                            ]
    in
    { title = title
    , content =
        case loggedIn.selectedCommunity of
            RemoteData.Success community ->
                if community.hasShop then
                    content community

                else
                    Page.fullPageNotFound
                        (t "error.pageNotFound")
                        (t "shop.disabled.description")

            RemoteData.Loading ->
                Page.fullPageLoading loggedIn.shared

            RemoteData.NotAsked ->
                Page.fullPageLoading loggedIn.shared

            RemoteData.Failure e ->
                Page.fullPageGraphQLError (t "community.error_loading") e
    }


viewHeader : Shared.Translators -> Html Msg
viewHeader { t } =
    h1
        [ class "font-bold text-lg"
        , ariaLabel <| t "shop.headline_no_emoji"
        ]
        [ text <| t "shop.headline"
        ]


viewShopFilter : LoggedIn.Model -> Model -> Html Msg
viewShopFilter loggedIn model =
    let
        { t } =
            loggedIn.shared.translators

        canSell =
            case loggedIn.profile of
                RemoteData.Success profile ->
                    LoggedIn.hasPermissions profile [ Permission.Sell ]

                _ ->
                    False

        numberOfFiltersApplied =
            List.length model.currentFilter.categories
    in
    div [ class "grid xs-max:grid-cols-1 grid-cols-2 md:flex mt-4 gap-4" ]
        [ View.Components.disablableLink
            { isDisabled = not loggedIn.hasAcceptedCodeOfConduct || not canSell
            }
            [ class "w-full md:w-40 button button-primary"
            , classList [ ( "button-disabled", not loggedIn.hasAcceptedCodeOfConduct ) ]
            , Route.href (Route.NewSale Route.SaleMainInformation)
            ]
            [ text <| t "shop.create_new_offer" ]
        , button
            [ class "w-full md:w-40 button button-secondary"
            , onClick ClickedOpenFiltersModal
            ]
            [ text <| t "shop.filters.title"
            , if numberOfFiltersApplied > 0 then
                span [ class "ml-2 bg-orange-300 rounded-full px-2 h-6 min-w-6 text-white flex items-center justify-center text-center" ]
                    [ text (String.fromInt numberOfFiltersApplied) ]

              else
                text ""
            ]
        ]


viewFilterTags : Community.Model -> Model -> Html msg
viewFilterTags community model =
    let
        getCategory categoryId =
            community.shopCategories
                |> RemoteData.toMaybe
                |> Maybe.andThen (Utils.Tree.findInForest (\category -> category.id == categoryId))

        viewAppliedFilter categoryId =
            case getCategory categoryId of
                Nothing ->
                    text ""

                Just category ->
                    div [ class "bg-white text-orange-300 rounded-sm flex items-center justify-center font-bold p-2 gap-4 flex-shrink-0 mr-4 sm:last:mr-0" ]
                        [ text category.name
                        , a
                            [ class "focus-ring focus:ring-offset-4 rounded-sm"
                            , Route.href
                                (Route.Shop
                                    { owner = model.currentFilter.owner
                                    , categories = List.filter (\id -> id /= categoryId) model.currentFilter.categories
                                    }
                                )
                            ]
                            [ Icons.close "fill-current w-3.5" ]
                        ]
    in
    div [ class "container mx-auto pl-4 mt-4 sm:pr-4" ]
        [ div [ class "flex overflow-auto focus-ring rounded-sm" ]
            (List.map viewAppliedFilter model.currentFilter.categories)
        ]


viewFoundOffersAmount : Translation.Translators -> Community.Model -> Model -> Html Msg
viewFoundOffersAmount translators community model =
    div [ class "bg-white w-full py-4 mt-4" ]
        [ div [ class "container mx-auto flex w-full justify-between px-4" ]
            [ case model.cards of
                Loaded cards ->
                    if List.length cards == 1 then
                        Markdown.fromTranslation translators "shop.filters.found_single_offer"
                            |> Markdown.view []

                    else
                        Markdown.fromTranslationWithReplacements translators
                            "shop.filters.found_offers"
                            [ ( "count", String.fromInt (List.length cards) ) ]
                            |> Markdown.view []

                _ ->
                    div [ class "w-44 rounded-md animate-skeleton-loading" ] []
            , a
                [ class "text-orange-300 hover:underline flex-shrink-0 focus-ring rounded-sm focus:ring-offset-2"
                , Route.href (Route.Shop { owner = model.currentFilter.owner, categories = [] })
                ]
                [ text <| translators.t "shop.empty.clear_filters" ]
            ]
        ]


viewFiltersModal : LoggedIn.Model -> Model -> Html Msg
viewFiltersModal loggedIn model =
    let
        categories =
            Community.getField loggedIn.selectedCommunity .shopCategories
                |> RemoteData.map Tuple.second
    in
    Modal.initWith
        { closeMsg = ClosedFiltersModal
        , isVisible = model.isFiltersModalOpen
        }
        |> Modal.withHeader (loggedIn.shared.translators.t "shop.filters.title")
        |> Modal.withBody
            [ Form.viewWithoutSubmit [ class "mt-4" ]
                loggedIn.shared.translators
                (\_ -> [])
                (filtersForm loggedIn categories)
                model.filtersForm
                { toMsg = GotFiltersFormMsg }
            ]
        |> Modal.withFooter
            [ button
                [ class "button button-primary w-full"
                , onClick
                    (Form.parse (filtersForm loggedIn categories)
                        model.filtersForm
                        { onError = GotFiltersFormMsg
                        , onSuccess = SubmittedFiltersForm
                        }
                    )
                ]
                [ text <| loggedIn.shared.translators.t "shop.filters.apply" ]
            ]
        |> Modal.withSize Modal.Large
        |> Modal.toHtml



-- VIEW GRID


viewEmptyState : LoggedIn.Model -> Eos.Symbol -> Model -> Html Msg
viewEmptyState loggedIn communitySymbol model =
    let
        { t, tr } =
            loggedIn.shared.translators

        title =
            case model.currentFilter.owner of
                Just userName ->
                    if userName == loggedIn.accountName then
                        text <| t "shop.empty.user_title"

                    else
                        text <| t "shop.empty.all_title"

                Nothing ->
                    text <| t "shop.empty.all_title"

        description =
            case model.currentFilter.owner of
                Just userName ->
                    if userName == loggedIn.accountName then
                        [ text <| tr "shop.empty.you_can_offer" [ ( "symbol", Eos.symbolToSymbolCodeString communitySymbol ) ]
                        ]

                    else
                        [ text <| tr "shop.empty.user_is_not_selling" [ ( "user", Eos.Account.nameToString userName ) ]
                        , br [] []
                        , br [] []
                        , text <| t "shop.empty.offer_something"
                        ]

                Nothing ->
                    [ text <| t "shop.empty.no_one_is_selling"
                    , br [] []
                    , br [] []
                    , text <| t "shop.empty.offer_something"
                    ]
    in
    div [ class "flex flex-col items-center justify-center my-10" ]
        [ img
            [ src "/images/seller_confused.svg"
            , alt ""
            ]
            []
        , p [ class "font-bold text-black mt-4 text-center" ] [ title ]
        , p [ class "text-black text-center mt-4" ] description
        , a
            [ class "button button-primary mt-6 md:px-6 w-full md:w-max"
            , Route.href (Route.NewSale Route.SaleMainInformation)
            ]
            [ text <| t "shop.empty.create_new" ]
        ]


viewEmptyStateWithFilters : Translation.Translators -> Html msg
viewEmptyStateWithFilters { t } =
    div [ class "flex flex-col items-center mt-16 mb-10" ]
        [ img [ alt "", src "/images/not_found.svg", class "w-1/2 md:w-40" ] []
        , p [ class "mt-6 text-gray-900" ] [ text <| t "shop.empty.no_results_found" ]
        , a
            [ class "button button-secondary mt-2"
            , Route.href (Route.Shop { owner = Nothing, categories = [] })
            ]
            [ text <| t "shop.empty.clear_filters" ]
        ]


viewGrid : LoggedIn.Model -> List Card -> Html Msg
viewGrid loggedIn cards =
    let
        outOfStockCards =
            cards
                |> List.filter (.isAvailable >> not)

        availableCards =
            cards
                |> List.filter .isAvailable
    in
    div [ class "mt-6 mb-10" ]
        [ ul [ class "grid gap-4 xs-max:grid-cols-1 grid-cols-2 md:grid-cols-3 lg:grid-cols-4 xl:grid-cols-5" ]
            (List.indexedMap
                (viewCard loggedIn)
                (availableCards ++ outOfStockCards)
            )
        ]


viewCard : LoggedIn.Model -> Int -> Card -> Html Msg
viewCard loggedIn index card =
    let
        ({ t, tr } as translators) =
            loggedIn.shared.translators

        images =
            case card.product.images of
                [] ->
                    img
                        [ class "h-32 rounded-t object-cover"
                        , alt ""
                        , src
                            ("/icons/shop-placeholder"
                                ++ (index
                                        |> modBy 3
                                        |> String.fromInt
                                   )
                                ++ ".svg"
                            )
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
                        , productId = Just card.product.id
                        , onScrollToImage = ClickedScrollToImage
                        , currentIntersecting = card.currentVisibleImage
                        , onStartedIntersecting = ImageStartedIntersecting card.product.id
                        , onStoppedIntersecting = ImageStoppedIntersecting card.product.id
                        }
                        ( firstImage, otherImages )

        isFree =
            card.product.price == 0
    in
    li [ class "rounded bg-white" ]
        [ a
            [ class "h-full flex flex-col hover:shadow-md transition-shadow duration-300"
            , Html.Attributes.title card.product.title
            , Route.href (Route.ViewSale card.product.id)
            ]
            [ images
            , div [ class "p-4 flex flex-col flex-grow" ]
                [ h2 [ class "line-clamp-3 text-black" ] [ text card.product.title ]
                , p [ class "font-bold text-gray-900 text-sm uppercase mb-auto line-clamp-2 mt-1" ]
                    [ if loggedIn.accountName == card.product.creatorId then
                        text <| t "shop.by_you"

                      else
                        text <|
                            tr "shop.by_user"
                                [ ( "user"
                                  , card.product.creator.name
                                        |> Maybe.withDefault (Eos.Account.nameToString card.product.creator.account)
                                  )
                                ]
                    ]
                , div [ class "font-bold flex flex-col mt-4" ]
                    [ span
                        [ class "text-lg"
                        , classList
                            [ ( "text-green", card.isAvailable )
                            , ( "text-gray-900", not card.isAvailable )
                            , ( "lowercase", isFree )
                            ]
                        ]
                        [ if isFree then
                            text <| t "shop.free"

                          else
                            text <|
                                Eos.formatSymbolAmount translators
                                    card.product.symbol
                                    card.product.price
                        ]
                    , span
                        [ classList
                            [ ( "text-sm text-gray-333 uppercase", card.isAvailable )
                            , ( "text-red font-normal lowercase", not card.isAvailable )
                            ]
                        ]
                        [ if not card.isAvailable then
                            text <| t "shop.sold_out"

                          else if isFree then
                            text <| t "shop.enjoy"

                          else
                            text <| Eos.symbolToSymbolCodeString card.product.symbol
                        ]
                    ]
                ]
            ]
        ]



--- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = NoOp
    | CompletedLoadingShopCategories (List Shop.Category.Tree)
    | CompletedSalesLoad (RemoteData (Graphql.Http.Error (List Product)) (List Product))
    | CompletedLoadBalances (Result Http.Error (List Balance))
    | ClickedAcceptCodeOfConduct
    | ClickedScrollToImage { containerId : String, imageId : String }
    | ImageStartedIntersecting Shop.Id Shop.ImageId
    | ImageStoppedIntersecting Shop.Id Shop.ImageId
    | ClickedOpenFiltersModal
    | ClosedFiltersModal
    | GotFiltersFormMsg (Form.Msg FiltersFormInput)
    | SubmittedFiltersForm FiltersFormOutput


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        NoOp ->
            UR.init model

        CompletedLoadingShopCategories categories ->
            { model
                | filtersForm =
                    Form.updateValues
                        (\values ->
                            { values
                                | categories =
                                    model.currentFilter.categories
                                        |> List.filterMap
                                            (\categoryId ->
                                                categories
                                                    |> Utils.Tree.findInForest (\category -> category.id == categoryId)
                                                    |> Maybe.map (\category -> ( category, True ))
                                            )
                                        |> Dict.fromList
                            }
                        )
                        model.filtersForm
            }
                |> UR.init

        CompletedSalesLoad (RemoteData.Success sales) ->
            UR.init { model | cards = Loaded (List.map cardFromSale sales) }

        CompletedSalesLoad (RemoteData.Failure err) ->
            UR.init { model | cards = LoadingFailed err }
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when loading sales from shop"
                    { moduleName = "Page.Shop", function = "update" }
                    []
                    err

        CompletedSalesLoad _ ->
            UR.init model

        CompletedLoadBalances res ->
            case res of
                Ok bals ->
                    { model | balances = bals }
                        |> UR.init

                Err _ ->
                    model
                        |> UR.init

        ClickedAcceptCodeOfConduct ->
            model
                |> UR.init
                |> UR.addExt LoggedIn.ShowCodeOfConductModal

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

        ImageStartedIntersecting cardId imageId ->
            case model.cards of
                Loaded cards ->
                    let
                        newCards =
                            List.Extra.updateIf
                                (\card -> card.product.id == cardId)
                                (\card ->
                                    { card
                                        | currentVisibleImage = Just imageId
                                        , previousVisibleImage = card.currentVisibleImage
                                    }
                                )
                                cards
                    in
                    { model | cards = Loaded newCards }
                        |> UR.init

                _ ->
                    UR.init model

        ImageStoppedIntersecting cardId imageId ->
            case model.cards of
                Loaded cards ->
                    let
                        newCards : List Card
                        newCards =
                            List.Extra.updateIf
                                (\card -> card.product.id == cardId)
                                (\card ->
                                    if Just imageId == card.currentVisibleImage then
                                        { card
                                            | currentVisibleImage = card.previousVisibleImage
                                            , previousVisibleImage = Nothing
                                        }

                                    else if Just imageId == card.previousVisibleImage then
                                        { card | previousVisibleImage = Nothing }

                                    else
                                        card
                                )
                                cards
                    in
                    { model | cards = Loaded newCards }
                        |> UR.init

                _ ->
                    UR.init model

        ClickedOpenFiltersModal ->
            { model | isFiltersModalOpen = True }
                |> UR.init

        ClosedFiltersModal ->
            { model | isFiltersModalOpen = False }
                |> UR.init

        GotFiltersFormMsg subMsg ->
            Form.update loggedIn.shared subMsg model.filtersForm
                |> UR.fromChild (\newForm -> { model | filtersForm = newForm })
                    GotFiltersFormMsg
                    LoggedIn.addFeedback
                    model

        SubmittedFiltersForm formOutput ->
            { model | isFiltersModalOpen = False }
                |> UR.init
                |> UR.addCmd (Route.pushUrl loggedIn.shared.navKey (Route.Shop formOutput))


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityFieldLoaded _ (Community.ShopCategories categories) ->
            Just (CompletedLoadingShopCategories categories)

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        CompletedLoadingShopCategories _ ->
            [ "CompletedLoadingShopCategories" ]

        CompletedSalesLoad r ->
            [ "CompletedSalesLoad", UR.remoteDataToString r ]

        CompletedLoadBalances _ ->
            [ "CompletedLoadBalances" ]

        ClickedAcceptCodeOfConduct ->
            [ "ClickedAcceptCodeOfConduct" ]

        ClickedScrollToImage _ ->
            [ "ClickedScrollToImage" ]

        ImageStartedIntersecting _ _ ->
            [ "ImageStartedIntersecting" ]

        ImageStoppedIntersecting _ _ ->
            [ "ImageStoppedIntersecting" ]

        ClickedOpenFiltersModal ->
            [ "ClickedOpenFiltersModal" ]

        ClosedFiltersModal ->
            [ "ClosedFiltersModal" ]

        GotFiltersFormMsg subMsg ->
            "GotFiltersFormMsg" :: Form.msgToString subMsg

        SubmittedFiltersForm _ ->
            [ "SubmittedFiltersForm" ]
