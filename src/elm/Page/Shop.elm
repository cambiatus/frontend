module Page.Shop exposing
    ( Model
    , Msg
    , init
    , jsAddressToMsg
    , msgToString
    , receiveBroadcast
    , update
    , view
    )

import Api
import Api.Graphql
import Array
import Community exposing (Balance)
import Eos
import Graphql.Http
import Html exposing (Html, a, button, div, img, p, text)
import Html.Attributes exposing (class, classList, src, value)
import Html.Events exposing (on, onClick)
import Html.Lazy as Lazy
import Http
import I18Next exposing (t)
import Json.Decode exposing (Value)
import List.Extra as LE
import Page exposing (Session(..))
import Profile exposing (viewProfileNameTag)
import Profile.Summary
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
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
                    div []
                        [ Lazy.lazy viewHeader loggedIn
                        , div [ class "container mx-auto px-4" ]
                            [ Page.fullPageLoading loggedIn.shared ]
                        ]

                LoadingFailed e ->
                    Page.fullPageGraphQLError (t "shop.title") e

                Loaded cards ->
                    div []
                        [ Lazy.lazy viewHeader loggedIn
                        , div [ class "container mx-auto justify-center px-4" ]
                            [ viewShopFilter loggedIn model.filter
                            , Lazy.lazy3 viewGrid loggedIn cards model
                            ]
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


viewHeader : LoggedIn.Model -> Html Msg
viewHeader loggedIn =
    let
        t =
            loggedIn.shared.translators.t
    in
    div [ class "w-full flex flex-wrap relative bg-indigo-500 p-4 lg:mx-auto lg:py-12" ]
        [ div [ class "flex w-full container mx-auto" ]
            [ div [ class "w-1/2" ]
                [ p [ class "text-white w-full text-xl font-medium mb-4 mx-8 text-xs font-light mb-2 uppercase" ]
                    [ text (t "shop.title") ]
                , p [ class "hidden lg:visible lg:flex text-white text-3xl mx-8 mb-4 font-medium" ]
                    [ text (t "shop.subtitle") ]
                , p [ class "hidden lg:visible lg:flex text-white mx-8 font-light text-sm" ]
                    [ text (t "shop.description") ]
                , a
                    [ Route.href Route.NewSale
                    , class "button button-primary button-sm w-full lg:w-64 lg:mx-8 lg:mt-6 lg:button-medium font-medium"
                    ]
                    [ text (t "shop.create_offer") ]
                ]
            , div [ class "hidden lg:visible lg:flex w-1/2 justify-center absolute right-0 bottom-0" ]
                [ img [ src "/images/shop.svg" ] []
                ]
            ]
        ]


viewShopFilter : LoggedIn.Model -> Filter -> Html Msg
viewShopFilter loggedIn filter =
    let
        t =
            loggedIn.shared.translators.t

        buttonClass =
            "w-1/2 lg:w-56 border border-purple-500 first:rounded-l last:rounded-r px-12 py-2 text-sm font-light text-gray"
    in
    div [ class "flex my-8 lg:my-16 lg:mx-auto lg:w-1/2 justify-center" ]
        [ button
            [ class buttonClass
            , classList [ ( "bg-purple-500 text-white", filter == Shop.All ) ]
            , value (t "shop.all_offers")
            , onClick (ClickedFilter Shop.All)
            ]
            [ text (t "shop.all_offers") ]
        , button
            [ class buttonClass
            , classList [ ( "bg-purple-500 text-white", filter == Shop.UserSales ) ]
            , value (t "shop.my_offers")
            , onClick (ClickedFilter Shop.UserSales)
            ]
            [ text (t "shop.my_offers") ]
        ]



-- VIEW GRID


viewGrid : LoggedIn.Model -> List Card -> Model -> Html Msg
viewGrid loggedIn cards model =
    let
        outOfStockCards =
            cards
                |> List.filter (.isAvailable >> not)

        availableCards =
            cards
                |> List.filter .isAvailable
    in
    div []
        [ div [ class "flex flex-wrap -mx-2" ]
            (List.indexedMap (viewCard model loggedIn) availableCards)
        , if List.length outOfStockCards > 0 then
            div []
                [ p [ class "ml-2 w-full border-b-2 pb-2 border-gray-300 mb-4 text-2xl capitalize" ]
                    [ text <| loggedIn.shared.translators.t "shop.out_of_stock" ]
                , div [ class "flex flex-wrap -mx-2" ]
                    (List.indexedMap (viewCard model loggedIn) outOfStockCards)
                ]

          else
            text ""
        ]


viewCard : Model -> LoggedIn.Model -> Int -> Card -> Html Msg
viewCard model ({ shared } as loggedIn) index card =
    let
        image =
            Maybe.withDefault "" card.product.image

        maybeBal =
            LE.find (\bal -> bal.asset.symbol == card.product.symbol) model.balances

        symbolBalance =
            case maybeBal of
                Just b ->
                    b.asset.amount

                Nothing ->
                    0.0

        currBalance =
            String.fromFloat symbolBalance ++ " " ++ Eos.symbolToSymbolCodeString card.product.symbol

        tr rId replaces =
            shared.translators.tr rId replaces

        title =
            if String.length card.product.title > 17 then
                String.slice 0 17 card.product.title ++ " ..."

            else
                card.product.title
    in
    a
        [ class "w-full md:w-1/2 lg:w-1/3 xl:w-1/4 px-2 mb-6"
        , Route.href (Route.ViewSale (String.fromInt card.product.id))
        ]
        [ div [ class "md:hidden rounded-lg bg-white h-32 flex" ]
            [ div [ class "w-1/4" ]
                [ img
                    [ class "rounded-l-lg object-cover h-32 w-full"
                    , src image
                    , on "error" (Json.Decode.succeed (OnImageError index))
                    ]
                    []
                ]
            , div [ class "px-4 pb-2 flex flex-wrap" ]
                [ p [ class "font-medium pt-2 w-full" ] [ text card.product.title ]
                , viewProfileNameTag shared loggedIn.accountName card.product.creator
                , div [ class "h-16 w-full flex flex-wrap items-end" ]
                    [ if card.product.units == 0 && card.product.trackStock then
                        div [ class "w-full" ]
                            [ p [ class "text-3xl text-red" ]
                                [ text (shared.translators.t "shop.out_of_stock")
                                ]
                            ]

                      else
                        div [ class "flex flex-none w-full items-center" ]
                            [ p [ class "text-green text-2xl font-medium" ] [ text (String.fromFloat card.product.price) ]
                            , div [ class "uppercase text-xs ml-2 font-extralight font-sans text-green" ] [ text (Eos.symbolToSymbolCodeString card.product.symbol) ]
                            ]
                    , div [ class "w-full h-4" ]
                        [ div [ class "bg-gray-100 absolute uppercase text-xs px-2" ]
                            [ text (tr "account.my_wallet.your_current_balance" [ ( "balance", currBalance ) ]) ]
                        ]
                    ]
                ]
            ]
        , div
            [ class "hidden md:visible md:flex md:flex-wrap rounded-lg hover:shadow-lg bg-white"
            ]
            [ div [ class "w-full relative bg-gray-500 rounded-t-lg" ]
                [ img [ class "w-full h-48 object-cover rounded-t-lg", src image ] []
                , div [ class "absolute right-1 bottom-1 " ]
                    [ Profile.Summary.view loggedIn.shared loggedIn.accountName card.product.creator card.profileSummary
                        |> Html.map (GotProfileSummaryMsg index card.isAvailable)
                    ]
                ]
            , div [ class "w-full px-6 pt-4" ]
                [ p [ class "text-xl" ] [ text title ]
                ]
            , if card.product.units == 0 && card.product.trackStock then
                div [ class "flex flex-none w-full px-6 pb-2" ]
                    [ p [ class "text-3xl text-red" ]
                        [ text (loggedIn.shared.translators.t "shop.out_of_stock")
                        ]
                    ]

              else
                div [ class "flex flex-none w-full px-6 pb-2" ]
                    [ p [ class "text-green text-3xl" ] [ text (String.fromFloat card.product.price) ]
                    , div [ class "uppercase text-xs font-extralight mt-3 ml-2 font-sans text-green" ] [ text (Eos.symbolToSymbolCodeString card.product.symbol) ]
                    ]
            , div [ class "px-6 pb-6" ]
                [ div [ class "bg-gray-200 flex items-center justify-left text-xs px-4" ]
                    [ text (tr "account.my_wallet.your_current_balance" [ ( "balance", currBalance ) ]) ]
                ]
            ]
        ]



--- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = CompletedSalesLoad (RemoteData (Graphql.Http.Error (List Product)) (List Product))
    | CompletedLoadCommunity Community.Model
    | ClickedFilter Filter
    | TransferSuccess Int
    | CompletedLoadBalances (Result Http.Error (List Balance))
    | OnImageError Int
    | GotProfileSummaryMsg Int Bool Profile.Summary.Msg


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        CompletedSalesLoad (RemoteData.Success sales) ->
            UR.init { model | cards = Loaded (List.map cardFromSale sales) }

        CompletedSalesLoad (RemoteData.Failure err) ->
            UR.init { model | cards = LoadingFailed err }
                |> UR.logGraphqlError msg err

        CompletedSalesLoad _ ->
            UR.init model

        CompletedLoadCommunity community ->
            UR.init model
                |> UR.addCmd
                    (Api.Graphql.query loggedIn.shared
                        (Just loggedIn.authToken)
                        (Shop.productsQuery model.filter loggedIn.accountName community.symbol)
                        CompletedSalesLoad
                    )

        TransferSuccess index ->
            updateCard msg index (\card -> ( card, [] )) (UR.init model)

        ClickedFilter filter ->
            let
                navKey =
                    loggedIn.shared.navKey

                route =
                    Route.Shop filter
            in
            UR.init model
                |> UR.addCmd (Route.pushUrl navKey route)

        CompletedLoadBalances res ->
            case res of
                Ok bals ->
                    { model | balances = bals }
                        |> UR.init

                Err _ ->
                    model
                        |> UR.init

        OnImageError index ->
            case model.cards of
                Loaded cards ->
                    let
                        cardArray =
                            Array.fromList cards
                    in
                    case cards |> Array.fromList |> Array.get index of
                        Just card ->
                            let
                                oldSale =
                                    card.product

                                icon =
                                    "/icons/shop-placeholder" ++ (index |> modBy 3 |> String.fromInt) ++ ".svg"

                                newSale =
                                    { oldSale | image = Just icon }

                                newCard =
                                    { card | product = newSale }

                                newList =
                                    Array.set index newCard cardArray
                            in
                            { model | cards = Loaded (Array.toList newList) } |> UR.init

                        Nothing ->
                            UR.logImpossible msg [ "cardOutOfIndex" ] (UR.init model)

                _ ->
                    model |> UR.init

        GotProfileSummaryMsg index isAvailable subMsg ->
            case model.cards of
                Loaded cards ->
                    let
                        targetCard =
                            cards
                                |> List.filter (\card -> isAvailable == card.isAvailable)
                                |> LE.getAt index

                        updatedCards =
                            case targetCard of
                                Just card ->
                                    let
                                        updatedSummary =
                                            Profile.Summary.update subMsg card.profileSummary
                                    in
                                    LE.updateIf (.product >> .id >> (==) card.product.id)
                                        (\c -> { c | profileSummary = updatedSummary })
                                        cards

                                Nothing ->
                                    cards
                    in
                    { model | cards = Loaded updatedCards }
                        |> UR.init

                _ ->
                    UR.init model


updateCard : Msg -> Int -> (Card -> ( Card, List (UpdateResult -> UpdateResult) )) -> UpdateResult -> UpdateResult
updateCard msg cardIndex transform ({ model } as uResult) =
    case model.cards of
        Loaded cards ->
            let
                head =
                    List.take cardIndex cards

                tail =
                    List.drop cardIndex cards
            in
            case tail of
                x :: xs ->
                    let
                        ( newX, xCmds ) =
                            transform x
                    in
                    { model | cards = Loaded (head ++ newX :: xs) }
                        |> UR.setModel uResult
                        |> (\uR -> List.foldl (\fn uR_ -> fn uR_) uR xCmds)

                _ ->
                    UR.logImpossible msg [ "cardOutOfIndex" ] uResult

        _ ->
            UR.logImpossible msg [] uResult


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr _ =
    case addr of
        "TransferSuccess" :: [ index ] ->
            String.toInt index
                |> Maybe.map TransferSuccess

        _ ->
            Nothing


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

        TransferSuccess index ->
            [ "TransferSuccess", String.fromInt index ]

        ClickedFilter _ ->
            [ "ClickedFilter" ]

        CompletedLoadBalances _ ->
            [ "CompletedLoadBalances" ]

        OnImageError _ ->
            [ "OnImageError" ]

        GotProfileSummaryMsg _ _ subMsg ->
            "GotProfileSummaryMsg" :: Profile.Summary.msgToString subMsg
