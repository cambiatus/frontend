module Page.Shop exposing (Model, Msg, init, jsAddressToMsg, msgToString, subscriptions, update, view)

import Account
import Api
import Api.Graphql
import Asset.Icon as Icon
import Avatar
import Browser.Dom as Dom
import Community exposing (Balance)
import Eos as Eos exposing (Symbol)
import Eos.Account as Eos
import Graphql.Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, onSubmit, targetValue)
import Html.Lazy as Lazy
import Http
import I18Next exposing (t)
import Icons
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import List.Extra as LE
import Log
import Page exposing (Session(..), viewMenuFilter, viewMenuFilterButton, viewMenuFilterTabButton, viewMenuTab)
import Route exposing (Route)
import Session.Guest as Guest
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import Shop exposing (Filter, Sale, decodeTargetValueToFilter)
import Task
import Time exposing (Posix)
import Transfer exposing (Transfer)
import UpdateResult as UR
import User exposing (viewNameTag)



-- INIT


init : LoggedIn.Model -> Filter -> ( Model, Cmd Msg )
init loggedIn filter =
    let
        model =
            initModel loggedIn filter
    in
    ( model
    , Cmd.batch
        [ Api.Graphql.query loggedIn.shared
            (Shop.salesQuery filter loggedIn.accountName)
            CompletedSalesLoad
        , Api.getBalances loggedIn.shared loggedIn.accountName CompletedLoadBalances
        , Task.perform GotTime Time.now
        , Dom.focus "main-content"
            |> Task.attempt (\_ -> Ignored)
        ]
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


type alias Model =
    { date : Maybe Posix
    , cards : Status
    , balances : List Balance
    , filter : Filter
    }


initModel : LoggedIn.Model -> Filter -> Model
initModel loggedIn filter =
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
    , state : CardState
    , rate : Maybe Int
    , form : SaleTransferForm
    }


cardFromSale : Sale -> Card
cardFromSale sale =
    { sale = sale
    , state = ViewingCard
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


type CardState
    = ViewingCard



-- VIEW


view : LoggedIn.Model -> Model -> Html Msg
view loggedIn model =
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
                , div [ class "justify-center px-4" ]
                    [ viewShopFilter loggedIn model.filter
                    , Lazy.lazy3 viewGrid loggedIn cards model
                    ]
                ]


viewHeader : LoggedIn.Model -> Html Msg
viewHeader loggedIn =
    div [ class "w-full flex flex-wrap relative bg-indigo-500 p-4 lg:container lg:mx-auto lg:py-12" ]
        [ div [ class "w-full lg:w-1/2" ]
            [ p [ class "text-white w-full text-xl font-medium mb-4 lg:mx-8 lg:text-xs lg:font-light lg:mb-2 lg:uppercase" ]
                [ text (t loggedIn.shared.translations "shop.title") ]
            , p [ class "hidden lg:visible lg:flex text-white text-3xl lg:mx-8 lg:mb-4 font-medium" ]
                [ text (t loggedIn.shared.translations "shop.subtitle") ]
            , p [ class "hidden lg:visible lg:flex text-white lg:mx-8 font-light text-sm" ]
                [ text (t loggedIn.shared.translations "shop.description") ]
            , a
                [ Route.href Route.NewSale
                , class "button button-primary button-small w-full lg:w-64 lg:mx-8 lg:mt-6 lg:button-medium font-medium"
                ]
                [ text (t loggedIn.shared.translations "shop.create_offer") ]
            ]
        , div [ class "hidden lg:visible lg:flex lg:absolute lg:w-1/2 lg:right-0 lg:bottom-0" ]
            [ img [ src "/images/shop.svg" ] []
            ]
        ]


viewShopFilter : LoggedIn.Model -> Filter -> Html Msg
viewShopFilter loggedIn filter =
    let
        translations =
            ( t loggedIn.shared.translations "shop.all_offers"
            , t loggedIn.shared.translations "shop.my_offers"
            )

        decoder =
            decodeTargetValueToFilter translations

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
        v_ viewFn index card =
            viewFn model loggedIn index card
    in
    div [ class "flex flex-wrap -mx-2" ]
        (List.indexedMap
            (\index card ->
                case card.state of
                    ViewingCard ->
                        v_ viewCard index card
            )
            cards
        )


viewCard : Model -> LoggedIn.Model -> Int -> Card -> Html Msg
viewCard model ({ shared } as loggedIn) index card =
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

        creatorId =
            Eos.nameToString card.sale.creatorId
    in
    a
        [ class "w-full sm:w-full md:w-1/2 lg:w-1/3 xl:w-1/4 px-2 mb-6"
        , Route.href (Route.ViewSale (String.fromInt card.sale.id))
        ]
        [ div [ class "md:hidden rounded-lg bg-white h-32 flex" ]
            [ div [ class "w-1/4" ]
                [ img [ class "rounded-l-lg object-cover h-32 w-full", src imageUrl ] []
                ]
            , div [ class "px-4 pb-2 flex flex-wrap" ]
                [ p [ class "font-medium pt-2 w-full" ] [ text card.sale.title ]
                , viewNameTag loggedIn.accountName card.sale.creator shared.translations
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
                , div [ class "absolute right-1 bottom-1 " ] [ User.view shared.endpoints.ipfs loggedIn.accountName shared.translations card.sale.creator ]
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


viewHeaderBackground : Session -> Card -> Html Msg
viewHeaderBackground session card =
    let
        ipfsUrl =
            getIpfsUrl session

        shared =
            case session of
                LoggedIn a ->
                    a.shared

                Guest a ->
                    a.shared

        tr r_id replaces =
            I18Next.tr shared.translations I18Next.Curly r_id replaces
    in
    div
        [ class "shop__background"
        , style "background-image"
            ("url("
                ++ Maybe.withDefault "/temp/44884525495_2e5c792dd2_z.jpg" (Maybe.map (\img -> ipfsUrl ++ "/" ++ img) card.sale.image)
                ++ ")"
            )
        ]
        [ span
            [ class "sale__more__details"
            ]
            [ text "MORE DETAILS" ]
        ]


getIpfsUrl : Session -> String
getIpfsUrl session =
    case session of
        Guest s ->
            s.shared.endpoints.ipfs

        LoggedIn s ->
            s.shared.endpoints.ipfs


getValidationMessage : Validation -> String
getValidationMessage validation =
    case validation of
        Valid ->
            ""

        Invalid error ->
            case error of
                UnitsEmpty ->
                    "Unit cannot be empty"

                UnitTooLow ->
                    "Unit is too low, must be at least 1"

                UnitTooHigh ->
                    "Not enough units available"

                UnitNotOnlyNumbers ->
                    "Only numbers are allowed"

                MemoEmpty ->
                    "Memo cannot be empty"

                MemoTooLong ->
                    "Memo is too long, max is 256 characters"



--- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = Ignored
    | GotTime Posix
    | CompletedSalesLoad (Result (Graphql.Http.Error (List Sale)) (List Sale))
    | ClickedSendTransfer Card Int
    | ClickedCancelTransfer Int
    | ClickedMessages Int Eos.Name
    | ClickedCloseMessages Int
    | ClickedFilter Filter
    | EnteredMemo Int String
    | EnteredQuantity Int String
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
                case LoggedIn.isAuth loggedIn of
                    True ->
                        let
                            authorization =
                                { actor = loggedIn.accountName
                                , permissionName = Eos.samplePermission
                                }

                            wantedUnits =
                                case String.toInt card.form.unit of
                                    Just quantityInt ->
                                        quantityInt

                                    -- TODO sort sales without units
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
                                        { actions =
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
                                }

                    False ->
                        UR.init model
                            |> UR.addExt (Just (ClickedSendTransfer card cardIndex) |> RequiredAuthentication)

            else
                UR.init model

        TransferSuccess index ->
            updateCard msg index (\card -> ( { card | state = ViewingCard }, [] )) (UR.init model)
                |> UR.addExt UpdateBalances

        ClickedCancelTransfer cardIndex ->
            UR.init model
                |> updateCardState msg cardIndex ViewingCard

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

        ClickedCloseMessages cardIndex ->
            UR.init model
                |> updateCardState msg cardIndex ViewingCard

        ClickedFilter filter ->
            let
                navKey =
                    loggedIn.shared.navKey

                route =
                    Route.Shop filter
            in
            UR.init model
                |> UR.addCmd (Route.pushUrl navKey route)

        EnteredQuantity cardIndex quantity ->
            updateCard msg
                cardIndex
                (\card ->
                    let
                        form_ =
                            card.form

                        newForm =
                            { form_ | unit = quantity }
                                |> validateForm card.sale
                    in
                    ( { card
                        | form = newForm
                      }
                    , []
                    )
                )
                (UR.init model)

        EnteredMemo cardIndex s ->
            updateCard msg
                cardIndex
                (\card ->
                    let
                        form_ =
                            card.form

                        newForm =
                            { form_ | memo = s }
                                |> validateForm card.sale
                    in
                    ( { card | form = newForm }, [] )
                )
                (UR.init model)

        CompletedLoadBalances res ->
            case res of
                Ok bals ->
                    { model | balances = bals }
                        |> UR.init

                Err err ->
                    model
                        |> UR.init


updateCardState : Msg -> Int -> CardState -> UpdateResult -> UpdateResult
updateCardState msg cardIndex newState uResult =
    updateCard msg cardIndex (\card -> ( { card | state = newState }, [] )) uResult


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
jsAddressToMsg addr val =
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

        ClickedCancelTransfer _ ->
            [ "ClickedCancelTransfer" ]

        ClickedMessages _ _ ->
            [ "ClickedMessages" ]

        ClickedCloseMessages _ ->
            [ "ClickedCloseMessages" ]

        ClickedFilter _ ->
            [ "ClickedFilter" ]

        EnteredQuantity _ _ ->
            [ "EnteredQuantity" ]

        EnteredMemo _ _ ->
            [ "EnteredMemo" ]

        CompletedLoadBalances _ ->
            [ "CompletedLoadBalances" ]
