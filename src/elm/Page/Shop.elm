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
import Community exposing (Balance)
import Eos
import Eos.Account
import Graphql.Http
import Html exposing (Html, a, div, h1, h2, img, li, p, span, text, ul)
import Html.Attributes exposing (alt, class, classList, src)
import Html.Attributes.Aria exposing (ariaLabel)
import Http
import I18Next exposing (t)
import Page exposing (Session(..))
import Profile.Summary
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared as Shared
import Shop exposing (Filter, Product)
import UpdateResult as UR



-- INIT


init : LoggedIn.Model -> Filter -> ( Model, Cmd Msg )
init loggedIn filter =
    let
        model =
            initModel filter
    in
    ( model
    , Cmd.batch
        [ LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
        , Api.getBalances loggedIn.shared loggedIn.accountName CompletedLoadBalances
        ]
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
    , form : SaleTransferForm
    , profileSummary : Profile.Summary.Model
    , isAvailable : Bool
    }


cardFromSale : Product -> Card
cardFromSale p =
    { product = p
    , form = initSaleFrom
    , profileSummary = Profile.Summary.init False
    , isAvailable = p.units > 0 || not p.trackStock
    }


type alias SaleTransferForm =
    { unit : String
    , unitValidation : Validation
    , memo : String
    , memoValidation : Validation
    }


initSaleFrom : SaleTransferForm
initSaleFrom =
    { unit = ""
    , unitValidation = Valid
    , memo = ""
    , memoValidation = Valid
    }


type Validation
    = Valid



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

        content =
            case model.cards of
                Loading ->
                    div [ class "container mx-auto px-4 mt-6 mb-10" ]
                        [ viewHeader loggedIn.shared.translators
                        , viewShopFilter loggedIn.shared.translators model
                        , Page.fullPageLoading loggedIn.shared
                        ]

                LoadingFailed e ->
                    Page.fullPageGraphQLError (t "shop.title") e

                Loaded cards ->
                    div [ class "container mx-auto px-4 mt-6" ]
                        [ viewHeader loggedIn.shared.translators
                        , viewShopFilter loggedIn.shared.translators model
                        , viewGrid loggedIn cards
                        ]
    in
    { title = title
    , content =
        case RemoteData.map .hasShop loggedIn.selectedCommunity of
            RemoteData.Success True ->
                content

            RemoteData.Success False ->
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


viewShopFilter : Shared.Translators -> Model -> Html Msg
viewShopFilter { t } model =
    let
        newFilter =
            case model.filter of
                Shop.All ->
                    Shop.UserSales

                Shop.UserSales ->
                    Shop.All
    in
    div [ class "flex mt-4 gap-4 flex-wrap" ]
        [ a
            [ class "button button-primary"
            , Route.href Route.NewSale
            ]
            [ text <| t "shop.create_new_offer" ]
        , a
            [ class "button button-secondary"
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

        image =
            Maybe.withDefault
                ("/icons/shop-placeholder"
                    ++ (index
                            |> modBy 3
                            |> String.fromInt
                       )
                    ++ ".svg"
                )
                card.product.image

        isFree =
            card.product.price == 0
    in
    li [ class "rounded bg-white" ]
        [ a
            [ class "h-full flex flex-col hover:shadow-md transition-shadow duration-300"
            , Html.Attributes.title card.product.title
            , Route.href (Route.ViewSale card.product.id)
            ]
            [ img [ src image, alt "", class "rounded-t h-32 object-cover" ] []
            , div [ class "p-4 flex flex-col flex-grow" ]
                [ h2 [ class "line-clamp-3 text-black" ] [ text card.product.title ]
                , p [ class "font-bold text-gray-900 text-sm uppercase mb-auto line-clamp-2" ]
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
    = CompletedSalesLoad (RemoteData (Graphql.Http.Error (List Product)) (List Product))
    | CompletedLoadCommunity Community.Model
    | CompletedLoadBalances (Result Http.Error (List Balance))


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
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

        CompletedLoadCommunity community ->
            UR.init model
                |> UR.addExt
                    (LoggedIn.query loggedIn
                        (Shop.productsQuery model.filter loggedIn.accountName community.symbol)
                        CompletedSalesLoad
                    )

        CompletedLoadBalances res ->
            case res of
                Ok bals ->
                    { model | balances = bals }
                        |> UR.init

                Err _ ->
                    model
                        |> UR.init


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityLoaded community ->
            Just (CompletedLoadCommunity community)

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedSalesLoad r ->
            [ "CompletedSalesLoad", UR.remoteDataToString r ]

        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        CompletedLoadBalances _ ->
            [ "CompletedLoadBalances" ]
