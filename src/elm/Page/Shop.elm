module Page.Shop exposing
    ( Model
    , Msg
    , init
    , msgToString
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
import Html.Attributes exposing (alt, class, classList, src, type_)
import Html.Attributes.Aria exposing (ariaLabel)
import Html.Events exposing (onClick)
import Http
import I18Next exposing (t)
import Json.Encode as Encode
import List.Extra
import Page exposing (Session(..))
import Profile.EditKycForm exposing (Msg(..))
import Profile.Summary
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared as Shared
import Shop exposing (Filter, Product)
import Shop.Category
import Translation
import Tree
import UpdateResult as UR
import Utils.Tree
import View.Components
import View.Modal as Modal



-- INIT


init : LoggedIn.Model -> Filter -> UpdateResult
init loggedIn filter =
    initModel filter
        |> UR.init
        |> UR.addCmd (Api.getBalances loggedIn.shared loggedIn.accountName CompletedLoadBalances)
        |> UR.addExt
            (LoggedIn.query loggedIn
                (Shop.productsQuery filter loggedIn.accountName)
                CompletedSalesLoad
            )
        |> UR.addExt (LoggedIn.RequestedCommunityField Community.ShopCategoriesField)



-- MODEL


type alias Model =
    { cards : Status
    , balances : List Balance
    , filter : Filter
    , isFiltersModalOpen : Bool
    , filtersForm : Form.Model FiltersFormInput
    }


initModel : Filter -> Model
initModel filter =
    { cards = Loading
    , balances = []
    , filter = filter
    , isFiltersModalOpen = False
    , filtersForm =
        Form.init
            { owner = Nothing
            , areCategoriesExpanded = False
            , categories = Dict.empty
            }
    }


type alias FiltersFormInput =
    { owner : Maybe Eos.Account.Name
    , areCategoriesExpanded : Bool
    , categories : CategoriesFormInput
    }


type alias FiltersFormOutput =
    { owner : Maybe Eos.Account.Name
    , categories : List Shop.Category.Id
    }


filtersForm : LoggedIn.Model -> RemoteData err (List Shop.Category.Tree) -> Form.Form msg FiltersFormInput FiltersFormOutput
filtersForm loggedIn allCategories =
    Form.succeed FiltersFormOutput
        |> Form.with (ownerForm loggedIn.accountName)
        |> Form.with
            (case allCategories of
                RemoteData.Success categories ->
                    if List.isEmpty categories then
                        Form.succeed []

                    else
                        categoriesForm categories

                RemoteData.Loading ->
                    -- TODO - Display loading
                    Form.succeed []

                RemoteData.NotAsked ->
                    -- TODO - Display loading
                    Form.succeed []

                RemoteData.Failure _ ->
                    -- TODO - Display error
                    Form.succeed []
            )


ownerForm : Eos.Account.Name -> Form.Form msg { input | owner : Maybe Eos.Account.Name } (Maybe Eos.Account.Name)
ownerForm currentUser =
    Form.Radio.init
        { -- TODO - I18N
          label = "Offers"
        , id = "offers-radio"
        , optionToString =
            \maybeAccount ->
                case maybeAccount of
                    Nothing ->
                        ""

                    Just account ->
                        Eos.Account.nameToString account
        }
        -- TODO - I18N
        |> Form.Radio.withOption Nothing (text "All offers")
        -- TODO - I18N
        |> Form.Radio.withOption (Just currentUser) (text "My offers")
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


categoriesForm : List Shop.Category.Tree -> Form.Form msg FiltersFormInput (List Shop.Category.Id)
categoriesForm allCategories =
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
        -- TODO - I18N
        |> Form.withNoOutput (Form.arbitrary (p [ class "label" ] [ text "Categories" ]))
        |> Form.with
            (Form.introspect
                (\{ areCategoriesExpanded } ->
                    allCategories
                        |> (if areCategoriesExpanded then
                                identity

                            else
                                Utils.Tree.takeFirst 4
                           )
                        |> List.map treeToForm
                        |> Form.list [ class "flex flex-col gap-y-4" ]
                        |> Form.mapOutput List.concat
                        |> Form.mapValues
                            { value = .categories
                            , update = \newChild parent -> { parent | categories = newChild }
                            }
                )
            )
        |> Form.withNoOutput
            (Form.introspect
                (\{ areCategoriesExpanded } ->
                    if areCategoriesExpanded then
                        Form.succeed ()

                    else
                        Form.arbitrary
                            (button
                                [ class "button button-secondary w-full mt-6"
                                , type_ "button"
                                , onClick (\values -> { values | areCategoriesExpanded = True })
                                ]
                                -- TODO - I18N
                                [ text "Show all categories" ]
                            )
                )
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

        content symbol =
            case model.cards of
                Loading ->
                    div [ class "container mx-auto px-4 mt-6 mb-10" ]
                        [ viewFrozenAccountCard
                        , viewHeader loggedIn.shared.translators
                        , viewShopFilter loggedIn model
                        , viewFiltersModal loggedIn model
                        , Page.fullPageLoading loggedIn.shared
                        ]

                LoadingFailed e ->
                    Page.fullPageGraphQLError (t "shop.title") e

                Loaded cards ->
                    div [ class "container mx-auto px-4 mt-6" ]
                        (if List.isEmpty cards && model.filter == Shop.All then
                            [ viewFrozenAccountCard
                            , viewEmptyState loggedIn.shared.translators symbol model
                            ]

                         else if List.isEmpty cards && model.filter == Shop.UserSales then
                            [ viewFrozenAccountCard
                            , viewHeader loggedIn.shared.translators
                            , viewShopFilter loggedIn model
                            , viewFiltersModal loggedIn model
                            , viewEmptyState loggedIn.shared.translators symbol model
                            ]

                         else
                            [ viewFrozenAccountCard
                            , viewHeader loggedIn.shared.translators
                            , viewShopFilter loggedIn model
                            , viewFiltersModal loggedIn model
                            , viewGrid loggedIn cards
                            ]
                        )
    in
    { title = title
    , content =
        case loggedIn.selectedCommunity of
            RemoteData.Success community ->
                if community.hasShop then
                    content community.symbol

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
            [ -- TODO - I18N
              text "Filters"
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
        -- TODO - I18N
        |> Modal.withHeader "Filters"
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
                [ -- TODO - I18N
                  text "Apply"
                ]
            ]
        |> Modal.withSize Modal.Large
        |> Modal.toHtml



-- VIEW GRID


viewEmptyState : Translation.Translators -> Eos.Symbol -> Model -> Html Msg
viewEmptyState { t, tr } communitySymbol model =
    let
        title =
            case model.filter of
                Shop.UserSales ->
                    text <| t "shop.empty.user_title"

                Shop.All ->
                    text <| t "shop.empty.all_title"

        description =
            case model.filter of
                Shop.UserSales ->
                    [ text <| tr "shop.empty.you_can_offer" [ ( "symbol", Eos.symbolToSymbolCodeString communitySymbol ) ]
                    ]

                Shop.All ->
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
            { model
                | isFiltersModalOpen = False
                , filtersForm = Form.updateValues (\values -> { values | areCategoriesExpanded = False }) model.filtersForm
            }
                |> UR.init

        GotFiltersFormMsg subMsg ->
            Form.update loggedIn.shared subMsg model.filtersForm
                |> UR.fromChild (\newForm -> { model | filtersForm = newForm })
                    GotFiltersFormMsg
                    LoggedIn.addFeedback
                    model

        SubmittedFiltersForm formOutput ->
            Debug.todo ""


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

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
