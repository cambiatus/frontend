module Page.Shop exposing
    ( Model
    , Msg
    , init
    , jsAddressToMsg
    , msgToString
    , subscriptions
    , update
    , view
    )

import Api
import Api.Graphql
import Browser exposing (Document)
import Browser.Dom as Dom
import Community exposing (Balance)
import Eos as Eos
import Eos.Account as Eos
import Graphql.Http
import Html exposing (Html, a, button, div, img, p, text)
import Html.Attributes exposing (class, classList, src, value)
import Html.Events exposing (onClick)
import Html.Lazy as Lazy
import Http
import I18Next exposing (t)
import Icons
import Json.Decode exposing (Value)
import Json.Encode as Encode
import List.Extra as LE
import Page exposing (Session(..))
import Profile exposing (viewProfileNameTag)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Shop exposing (Filter, Sale)
import Task
import Time exposing (Posix)
import Transfer
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
        [ Api.Graphql.query loggedIn.shared
            (Shop.salesQuery filter loggedIn.accountName loggedIn.selectedCommunity)
            CompletedSalesLoad
        , Api.getBalances loggedIn.shared loggedIn.accountName CompletedLoadBalances
        , Task.perform GotTime Time.now
        , Dom.focus "main-content"
            |> Task.attempt (\_ -> Ignored)
        ]
    )



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Sub.none



-- MODEL


type alias Model =
    { date : Maybe Posix
    , cards : Status
    , balances : List Balance
    , filter : Filter
    }


initModel : Filter -> Model
initModel filter =
    { date = Nothing
    , cards = Loading
    , balances = []
    , filter = filter
    }


type Status
    = Loading
    | Loaded (List Card)
    | LoadingFailed (Graphql.Http.Error (List Sale))


type alias Card =
    { sale : Sale
    , rate : Maybe Int
    , form : SaleTransferForm
    }


cardFromSale : Sale -> Card
cardFromSale sale =
    { sale = sale
    , rate = Nothing
    , form = initSaleFrom
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
    | Invalid ValidationError


type ValidationError
    = UnitsEmpty
    | UnitTooLow
    | UnitTooHigh
    | UnitNotOnlyNumbers
    | MemoEmpty
    | MemoTooLong



-- VIEW


view : LoggedIn.Model -> Model -> Document Msg
view loggedIn model =
    let
        selectedCommunityName =
            case loggedIn.profile of
                LoggedIn.Loaded profile ->
                    let
                        selectedCommunity =
                            profile.communities
                                |> List.filter (\p -> p.id == loggedIn.selectedCommunity)
                                |> List.head
                    in
                    case selectedCommunity of
                        Just c ->
                            c.name

                        Nothing ->
                            Eos.symbolToString loggedIn.selectedCommunity

                _ ->
                    ""

        pageTitle =
            selectedCommunityName
                ++ " "
                ++ t loggedIn.shared.translations "shop.title"

        body : Html Msg
        body =
            case model.cards of
                Loading ->
                    div []
                        [ Lazy.lazy viewHeader loggedIn
                        , div [ class "container mx-auto px-4" ]
                            [ Page.fullPageLoading ]
                        ]

                LoadingFailed e ->
                    Page.fullPageGraphQLError (t loggedIn.shared.translations "shop.title") e

                Loaded cards ->
                    div []
                        [ Lazy.lazy viewHeader loggedIn
                        , div [ class "container mx-auto justify-center px-4" ]
                            [ viewShopFilter loggedIn model.filter
                            , Lazy.lazy3 viewGrid loggedIn cards model
                            ]
                        ]
    in
    Document pageTitle [ body ]


viewHeader : LoggedIn.Model -> Html Msg
viewHeader loggedIn =
    div [ class "w-full flex flex-wrap relative bg-indigo-500 p-4 lg:container lg:mx-auto lg:py-12" ]
        [ div [ class "flex w-full container mx-auto" ]
            [ div [ class "w-1/2" ]
                [ p [ class "text-white w-full text-xl font-medium mb-4 mx-8 text-xs font-light mb-2 uppercase" ]
                    [ text (t loggedIn.shared.translations "shop.title") ]
                , p [ class "hidden lg:visible lg:flex text-white text-3xl mx-8 mb-4 font-medium" ]
                    [ text (t loggedIn.shared.translations "shop.subtitle") ]
                , p [ class "hidden lg:visible lg:flex text-white mx-8 font-light text-sm" ]
                    [ text (t loggedIn.shared.translations "shop.description") ]
                , a
                    [ Route.href Route.NewSale
                    , class "button button-primary button-sm w-full lg:w-64 lg:mx-8 lg:mt-6 lg:button-medium font-medium"
                    ]
                    [ text (t loggedIn.shared.translations "shop.create_offer") ]
                ]
            , div [ class "hidden lg:visible lg:flex w-1/2 justify-center absolute right-0 bottom-0" ]
                [ img [ src "/images/shop.svg" ] []
                ]
            ]
        ]


viewShopFilter : LoggedIn.Model -> Filter -> Html Msg
viewShopFilter loggedIn filter =
    let
        buttonClass =
            "w-1/2 lg:w-56 border border-purple-500 first:rounded-l last:rounded-r px-12 py-2 text-sm font-light text-gray"
    in
    div [ class "flex my-8 lg:my-16 lg:mx-auto lg:w-1/2 justify-center" ]
        [ button
            [ class buttonClass
            , classList [ ( "bg-purple-500 text-white", filter == Shop.All ) ]
            , value (t loggedIn.shared.translations "shop.all_offers")
            , onClick (ClickedFilter Shop.All)
            ]
            [ text (t loggedIn.shared.translations "shop.all_offers") ]
        , button
            [ class buttonClass
            , classList [ ( "bg-purple-500 text-white", filter == Shop.UserSales ) ]
            , value (t loggedIn.shared.translations "shop.my_offers")
            , onClick (ClickedFilter Shop.UserSales)
            ]
            [ text (t loggedIn.shared.translations "shop.my_offers") ]
        ]



-- VIEW GRID


viewGrid : LoggedIn.Model -> List Card -> Model -> Html Msg
viewGrid loggedIn cards model =
    let
        v_ viewFn card =
            viewFn model loggedIn card
    in
    div [ class "flex flex-wrap -mx-2" ]
        (List.map
            (v_ viewCard)
            cards
        )


viewCard : Model -> LoggedIn.Model -> Card -> Html Msg
viewCard model ({ shared } as loggedIn) card =
    let
        image =
            Maybe.withDefault "" card.sale.image

        imageUrl =
            shared.endpoints.ipfs ++ "/" ++ image

        maybeBal =
            LE.find (\bal -> bal.asset.symbol == card.sale.symbol) model.balances

        symbolBalance =
            case maybeBal of
                Just b ->
                    b.asset.amount

                Nothing ->
                    0.0

        currBalance =
            String.fromFloat symbolBalance ++ " " ++ Eos.symbolToString card.sale.symbol

        tr r_id replaces =
            I18Next.tr shared.translations I18Next.Curly r_id replaces

        title =
            if String.length card.sale.title > 17 then
                String.slice 0 17 card.sale.title ++ " ..."

            else
                card.sale.title
    in
    a
        [ class "w-full md:w-1/2 lg:w-1/3 xl:w-1/4 px-2 mb-6"
        , Route.href (Route.ViewSale (String.fromInt card.sale.id))
        ]
        [ div [ class "md:hidden rounded-lg bg-white h-32 flex" ]
            [ div [ class "w-1/4" ]
                [ img [ class "rounded-l-lg object-cover h-32 w-full", src imageUrl ] []
                ]
            , div [ class "px-4 pb-2 flex flex-wrap" ]
                [ p [ class "font-medium pt-2 w-full" ] [ text card.sale.title ]
                , viewProfileNameTag loggedIn.accountName card.sale.creator shared.translations
                , div [ class "h-16 w-full flex flex-wrap items-end" ]
                    [ if card.sale.units == 0 && card.sale.trackStock then
                        div [ class "w-full" ]
                            [ p [ class "text-3xl text-red" ]
                                [ text (t shared.translations "shop.out_of_stock")
                                ]
                            ]

                      else
                        div [ class "flex flex-none w-full items-center" ]
                            [ p [ class "text-green text-2xl font-medium" ] [ text (String.fromFloat card.sale.price) ]
                            , div [ class "uppercase text-xs ml-2 font-thin font-sans text-green" ] [ text (Eos.symbolToString card.sale.symbol) ]
                            ]
                    , div [ class "w-full h-4" ]
                        [ div [ class "bg-gray-100 absolute uppercase text-xs px-2" ]
                            [ text (tr "account.my_wallet.your_current_balance" [ ( "balance", currBalance ) ]) ]
                        ]
                    ]
                ]
            ]
        , div
            [ class "hidden md:visible md:flex md:flex-wrap rounded-lg hover:shadow-lg bg-white overflow-hidden"
            ]
            [ div [ class "w-full relative bg-gray-500" ]
                [ img [ class "w-full h-48 object-cover", src imageUrl ] []
                , div [ class "absolute right-1 bottom-1 " ]
                    [ Profile.view shared loggedIn.accountName card.sale.creator ]
                ]
            , div [ class "w-full px-6 pt-4" ]
                [ p [ class "text-xl" ] [ text title ]
                ]
            , div [ class "flex flex-none items-center pt-3 px-6 pb-4" ]
                [ Icons.thumbUp "text-indigo-500"
                , p [ class "pl-2 pr-6 text-sm" ] [ text "0" ]
                , Icons.thumbDown ""
                , p [ class "pl-2 pr-6 text-sm" ] [ text "0" ]
                ]
            , if card.sale.units == 0 && card.sale.trackStock then
                div [ class "border-t border-gray-300 flex flex-none w-full px-6 pb-2" ]
                    [ p [ class "text-3xl text-red" ]
                        [ text (t loggedIn.shared.translations "shop.out_of_stock")
                        ]
                    ]

              else
                div [ class "border-t border-gray-300 flex flex-none w-full px-6 pb-2" ]
                    [ p [ class "text-green text-3xl" ] [ text (String.fromFloat card.sale.price) ]
                    , div [ class "uppercase text-xs font-thin mt-3 ml-2 font-sans text-green" ] [ text (Eos.symbolToString card.sale.symbol) ]
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
    = Ignored
    | GotTime Posix
    | CompletedSalesLoad (Result (Graphql.Http.Error (List Sale)) (List Sale))
    | ClickedSendTransfer Card Int
    | ClickedMessages Int Eos.Name
    | ClickedFilter Filter
    | TransferSuccess Int
    | CompletedLoadBalances (Result Http.Error (List Balance))


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        Ignored ->
            UR.init model

        GotTime date ->
            UR.init { model | date = Just date }

        CompletedSalesLoad (Ok sales) ->
            UR.init { model | cards = Loaded (List.map cardFromSale sales) }

        CompletedSalesLoad (Err err) ->
            UR.init { model | cards = LoadingFailed err }
                |> UR.logGraphqlError msg err

        ClickedSendTransfer card cardIndex ->
            let
                newForm =
                    validateForm card.sale card.form
            in
            if isFormValid newForm then
                if LoggedIn.isAuth loggedIn then
                    let
                        authorization =
                            { actor = loggedIn.accountName
                            , permissionName = Eos.samplePermission
                            }

                        wantedUnits =
                            case String.toInt card.form.unit of
                                Just quantityInt ->
                                    quantityInt

                                Nothing ->
                                    1

                        tAmount =
                            card.sale.price * toFloat wantedUnits

                        quantity =
                            { amount = tAmount
                            , symbol = card.sale.symbol
                            }

                        from =
                            loggedIn.accountName

                        to =
                            card.sale.creatorId
                    in
                    UR.init model
                        |> UR.addPort
                            { responseAddress = TransferSuccess cardIndex
                            , responseData = Encode.null
                            , data =
                                Eos.encodeTransaction
                                    [ { accountName = "bes.token"
                                      , name = "transfer"
                                      , authorization = authorization
                                      , data =
                                            { from = from
                                            , to = to
                                            , value = quantity
                                            , memo = card.form.memo
                                            }
                                                |> Transfer.encodeEosActionData
                                      }
                                    , { accountName = "bes.cmm"
                                      , name = "transfersale"
                                      , authorization = authorization
                                      , data =
                                            { id = card.sale.id
                                            , from = from
                                            , to = to
                                            , quantity = quantity
                                            , units = wantedUnits
                                            }
                                                |> Shop.encodeTransferSale
                                      }
                                    ]
                            }

                else
                    UR.init model
                        |> UR.addExt (Just (ClickedSendTransfer card cardIndex) |> RequiredAuthentication)

            else
                UR.init model

        TransferSuccess index ->
            updateCard msg index (\card -> ( card, [] )) (UR.init model)

        ClickedMessages cardIndex creatorId ->
            UR.init model
                |> UR.addPort
                    { responseAddress = ClickedMessages cardIndex creatorId
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "openChat" )
                            , ( "username", Encode.string (Eos.nameToString creatorId) )
                            ]
                    }

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


validateForm : Sale -> SaleTransferForm -> SaleTransferForm
validateForm sale form =
    let
        unitValidation : Validation
        unitValidation =
            if form.unit == "" then
                Invalid UnitsEmpty

            else
                case String.toInt form.unit of
                    Just units ->
                        if units > sale.units then
                            Invalid UnitTooHigh

                        else if units <= 0 then
                            Invalid UnitTooLow

                        else
                            Valid

                    Nothing ->
                        Invalid UnitNotOnlyNumbers

        memoValidation =
            if form.memo == "" then
                Invalid MemoEmpty

            else if String.length form.memo > 256 then
                Invalid MemoTooLong

            else
                Valid
    in
    { form
        | unitValidation = unitValidation
        , memoValidation = memoValidation
    }


isFormValid : SaleTransferForm -> Bool
isFormValid form =
    form.unitValidation == Valid && form.memoValidation == Valid


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr _ =
    case addr of
        "TransferSuccess" :: [ index ] ->
            case String.toInt index of
                Just x ->
                    Just (TransferSuccess x)

                Nothing ->
                    Nothing

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        Ignored ->
            [ "Ignored" ]

        GotTime _ ->
            [ "GotTime" ]

        CompletedSalesLoad r ->
            [ "CompletedSalesLoad", UR.resultToString r ]

        ClickedSendTransfer _ _ ->
            [ "ClickedSendTransfer" ]

        TransferSuccess index ->
            [ "TransferSuccess", String.fromInt index ]

        ClickedMessages _ _ ->
            [ "ClickedMessages" ]

        ClickedFilter _ ->
            [ "ClickedFilter" ]

        CompletedLoadBalances _ ->
            [ "CompletedLoadBalances" ]
