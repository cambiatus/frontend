module Page.Shop exposing
    ( Model
    , Msg
    , init
    , msgToString
    , update
    , view
    )

import Api
import Cambiatus.Enum.Permission as Permission
import Community exposing (Balance)
import Eos
import Eos.Account
import Graphql.Http
import Html exposing (Html, a, br, div, h1, h2, img, li, p, span, text, ul)
import Html.Attributes exposing (alt, class, classList, src)
import Html.Attributes.Aria exposing (ariaLabel)
import Http
import I18Next exposing (t)
import Json.Encode as Encode
import List.Extra
import Page exposing (Session(..))
import Profile.Summary
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared as Shared
import Shop exposing (Filter, Product)
import Translation
import UpdateResult as UR
import View.Components



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



-- MODEL


type alias Model =
    { cards : Status
    , balances : List Balance
    , filter : Filter
    }


initModel : Filter -> Model
initModel filter =
    { cards = Loading
    , balances = []
    , filter = filter
    }


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
                            , viewEmptyState loggedIn.shared.translators symbol model
                            ]

                         else
                            [ viewFrozenAccountCard
                            , viewHeader loggedIn.shared.translators
                            , viewShopFilter loggedIn model
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

        newFilter =
            case model.filter of
                Shop.All ->
                    Shop.UserSales

                Shop.UserSales ->
                    Shop.All

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
        , a
            [ class "w-full md:w-40 button button-secondary"
            , Route.href (Route.Shop newFilter)
            ]
            [ case model.filter of
                Shop.UserSales ->
                    text <| t "shop.see_all"

                Shop.All ->
                    text <| t "shop.see_mine"
            ]
        ]



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
