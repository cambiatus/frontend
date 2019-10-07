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
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import List.Extra as LE
import Log
import Page exposing (Session(..), viewMenuFilter, viewMenuFilterButton, viewMenuFilterDropdown, viewMenuFilterDropdownOption)
import Route exposing (Route)
import Session.Guest as Guest
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import Shop exposing (Filter, Sale, decodeTargetValueToFilter)
import Task
import Time exposing (Posix)
import Transfer exposing (Transfer)
import UpdateResult as UR



-- INIT


init : Session -> Maybe Filter -> ( Model, Cmd Msg )
init session maybeFilter =
    case session of
        Page.Guest guest ->
            initGuest guest maybeFilter

        Page.LoggedIn loggedIn ->
            initLoggedIn loggedIn maybeFilter


initGuest : Guest.Model -> Maybe Filter -> ( Model, Cmd Msg )
initGuest guest maybeFilter =
    let
        model =
            initGuestModel guest

        name =
            Eos.nameQueryUrlParser "bespiral"
    in
    ( model
    , Cmd.batch
        [ Api.Graphql.query guest.shared
            (Shop.salesQuery (Maybe.withDefault Shop.All maybeFilter) name)
            CompletedSalesLoad
        , Task.perform GotTime Time.now
        , Dom.focus "main-content"
            |> Task.attempt (\_ -> Ignored)
        ]
    )


initLoggedIn : LoggedIn.Model -> Maybe Filter -> ( Model, Cmd Msg )
initLoggedIn loggedIn maybeFilter =
    let
        model =
            initLoggedInModel loggedIn
    in
    ( model
    , Cmd.batch
        [ Api.Graphql.query loggedIn.shared
            (Shop.salesQuery (Maybe.withDefault Shop.All maybeFilter) loggedIn.accountName)
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
    }


initGuestModel : Guest.Model -> Model
initGuestModel guest =
    { date = Nothing
    , cards = Loading
    , balances = []
    }


initLoggedInModel : LoggedIn.Model -> Model
initLoggedInModel loggedIn =
    { date = Nothing
    , cards = Loading
    , balances = []
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


view : Session -> Maybe Filter -> Model -> Html Msg
view session maybeFilter model =
    let
        shared =
            Page.toShared session
    in
    case model.cards of
        Loading ->
            Page.mainContentContainer
                [ Lazy.lazy viewHeader session
                , Page.fullPageLoading
                ]

        LoadingFailed e ->
            Page.fullPageGraphQLError (t shared.translations "shop.title") e

        Loaded cards ->
            Page.mainContentContainer
                [ Lazy.lazy viewHeader session
                , viewShopFilter session maybeFilter
                , viewGrid session cards model
                ]


viewHeader : Session -> Html Msg
viewHeader session =
    let
        shared =
            Page.toShared session
    in
    div []
        [ Page.viewTitle (t shared.translations "shop.title")
        , case session of
            Page.LoggedIn _ ->
                Page.viewButtonNew
                    (t shared.translations "shop.create_offer")
                    Route.NewSale

            _ ->
                text ""
        ]


viewShopFilter : Session -> Maybe Filter -> Html Msg
viewShopFilter session maybeFilter =
    let
        shared =
            Page.toShared session

        translations =
            ( t shared.translations "shop.my_communities_offers"
            , t shared.translations "shop.all_offers"
            , t shared.translations "shop.my_offers"
            )
    in
    case session of
        Page.LoggedIn loggedIn ->
            div [ class "shop-filter" ]
                [ viewShopFilterButtons
                    translations
                    ( Route.Shop (Just Shop.MyCommunities)
                    , Route.Shop (Just Shop.All)
                    , Route.Shop (Just Shop.UserSales)
                    )
                    maybeFilter
                    loggedIn
                , viewShopFilterDropdown
                    translations
                    maybeFilter
                    loggedIn
                ]

        _ ->
            text ""



-- VIEW GRID


viewGrid : Session -> List Card -> Model -> Html Msg
viewGrid session cards model =
    div [ class "flex flex-wrap -mx-2" ]
        (List.indexedMap
            (\index card ->
                let
                    v_ viewFn =
                        viewFn model session index card
                in
                case card.state of
                    ViewingCard ->
                        v_ viewCard
            )
            cards
        )


viewCard : Model -> Session -> Int -> Card -> Html Msg
viewCard model session index card =
    let
        account =
            case session of
                LoggedIn m ->
                    Eos.nameToString m.accountName

                _ ->
                    ""

        shared =
            Page.toShared session

        text_ str =
            text (t shared.translations str)
    in
    viewCardWithHeader
        model
        session
        card
        [ div [ class "sale__info" ]
            [ div [ class "sale__rating" ]
                [ p [ class "sale__rating__title" ]
                    [ text_ "shop.rate_sale" ]
                , div [ class "sale__rating__icons" ]
                    [ div [ class "sale__like" ]
                        [ Icon.like "sale__like__icon"
                        , span [ class "sale__like__text" ] [ text "0" ]
                        ]
                    , div [ class "sale__dislike" ]
                        [ Icon.dislike "sale__dislike__icon"
                        , span [ class "sale__like__text" ] [ text "0" ]
                        ]
                    ]
                ]
            , if card.sale.trackStock then
                div [ class "sale__quantity" ]
                    [ p [ class "sale__quantity__title" ]
                        [ text_ "shop.units_available" ]
                    , p [ class "sale__quantity__text" ]
                        [ text (String.fromInt card.sale.units) ]
                    ]

              else
                text ""
            ]
        , if Eos.nameToString card.sale.creatorId == account then
            text ""

          else if card.sale.units <= 0 && card.sale.trackStock then
            div [ class "sale__out__of__stock" ]
                [ p [] [ text_ "shop.out_of_stock" ] ]

          else
            div [ class "card__button-row" ]
                [ button
                    [ class "btn btn--primary"
                    ]
                    [ text_ "shop.transfer.submit" ]
                , button
                    [ class "btn btn--primary"
                    , onClick (ClickedMessages index card.sale.creatorId)
                    ]
                    [ text_ "shop.ask" ]
                ]
        ]


viewCardWithHeader : Model -> Session -> Card -> List (Html Msg) -> Html Msg
viewCardWithHeader model session card content =
    a
        [ class "w-full sm:w-1/2 lg:w-1/2 xl:w-1/3 py-2 px-2 "
        , Route.href (Route.ViewSale (String.fromInt card.sale.id))
        ]
        [ div
            [ classList
                [ ( "card", True )
                , ( "card--shop", True )
                ]
            ]
            ([ div [ class "card__header__mobile" ]
                [ viewHeaderBackground session card
                , viewHeaderAvatarTitle model session card
                ]
             ]
                ++ content
            )
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
        , style "background-image" ("url(" ++ Maybe.withDefault "/temp/44884525495_2e5c792dd2_z.jpg" (Maybe.map (\img -> ipfsUrl ++ "/" ++ img) card.sale.image) ++ ")")
        ]
        [ span
            [ class "sale__more__details"
            ]
            [ text "MORE DETAILS" ]
        ]


viewHeaderAvatarTitle : Model -> Session -> Card -> Html Msg
viewHeaderAvatarTitle model session { sale } =
    let
        ipfsUrl =
            getIpfsUrl session

        saleSymbol =
            Eos.symbolToString sale.symbol

        balances =
            model.balances

        maybeBal =
            LE.find (\bal -> bal.asset.symbol == sale.symbol) balances

        symbolBalance =
            case maybeBal of
                Just b ->
                    b.asset.amount

                Nothing ->
                    0.0

        balanceString =
            let
                currBalance =
                    String.fromFloat symbolBalance ++ " " ++ saleSymbol
            in
            currBalance

        shared =
            case session of
                LoggedIn a ->
                    a.shared

                Guest a ->
                    a.shared

        tr r_id replaces =
            I18Next.tr shared.translations I18Next.Curly r_id replaces
    in
    div [ class "shop__header" ]
        [ Avatar.view ipfsUrl sale.creator.avatar "shop__avatar"
        , div [ class "shop__title-text" ]
            [ h3 [ class "shop__title" ] [ text sale.title ]
            , div [ class "shop__sale__price" ]
                [ p [ class "sale__amount" ] [ text sale.price ]
                , p [ class "sale__symbol" ] [ text saleSymbol ]
                ]
            , p [ class "shop__balance" ]
                [ text (tr "account.my_wallet.your_current_balance" [ ( "balance", balanceString ) ]) ]
            ]
        ]


viewShopFilterButtons : ( String, String, String ) -> ( Route, Route, Route ) -> Maybe Filter -> LoggedIn.Model -> Html Msg
viewShopFilterButtons translations routes maybeFilter loggedIn =
    let
        ( communities, all, user ) =
            translations

        ( rCommunities, rAll, rUser ) =
            routes

        buttons =
            [ viewMenuFilterButton
                (maybeFilter == Just Shop.MyCommunities)
                communities
                rCommunities
            , viewMenuFilterButton
                (maybeFilter == Just Shop.All)
                all
                rAll
            , viewMenuFilterButton
                (maybeFilter == Just Shop.UserSales)
                user
                rUser
            ]
    in
    viewMenuFilter buttons


viewShopFilterDropdown : ( String, String, String ) -> Maybe Filter -> LoggedIn.Model -> Html Msg
viewShopFilterDropdown translations maybeFilter loggedIn =
    let
        ( communities, all, user ) =
            translations

        decoder =
            decodeTargetValueToFilter translations

        options =
            [ viewMenuFilterDropdownOption
                (maybeFilter == Just Shop.MyCommunities)
                communities
            , viewMenuFilterDropdownOption
                (maybeFilter == Just Shop.All)
                all
            , viewMenuFilterDropdownOption
                (maybeFilter == Just Shop.UserSales)
                user
            ]
    in
    viewMenuFilterDropdown ClickedFilter decoder options


viewCardTransfer : Bool -> Model -> Session -> Int -> Card -> Html Msg
viewCardTransfer isDisabled model session cardIndex card =
    let
        shared =
            Page.toShared session

        text_ str =
            text (t shared.translations str)

        plchdr str =
            placeholder (t shared.translations str)

        quantInputId =
            "shop-quantity-input-" ++ String.fromInt cardIndex

        memoInputId =
            "shop-memo-input-" ++ String.fromInt cardIndex
    in
    Html.form
        [ classList
            [ ( "card", True )
            , ( "card--shop", True )
            , ( "card--shop--transfer", True )
            ]
        , onSubmit (ClickedSendTransfer card cardIndex)
        ]
        [ viewHeaderBackground session card
        , viewHeaderAvatarTitle model session card
        , div [ class "form-field" ]
            [ label [ for quantInputId ] [ text_ "shop.units_label" ]
            , input
                [ id quantInputId
                , class "input input--quantity"
                , type_ "number"
                , Html.Attributes.pattern "[0-9]*"
                , value card.form.unit
                , onInput (EnteredQuantity cardIndex)
                , Html.Attributes.min "0"
                , Html.Attributes.max (String.fromInt card.sale.units)
                , required True
                , disabled isDisabled
                , plchdr "account.my_wallet.transfer.amount_placeholder"
                ]
                []
            , if card.form.unitValidation == Valid then
                text ""

              else
                span [ class "field-error" ]
                    [ text (getValidationMessage card.form.unitValidation) ]
            ]
        , div [ class "form-field" ]
            [ label [ for memoInputId ] [ text_ "account.my_wallet.transfer.memo" ]
            , input
                [ id memoInputId
                , class "input"
                , type_ "text"
                , plchdr "account.my_wallet.transfer.memo_placeholder"
                , value card.form.memo
                , onInput (EnteredMemo cardIndex)
                , required True
                , disabled isDisabled
                ]
                []
            , if card.form.memoValidation == Valid then
                text ""

              else
                span [ class "field-error" ]
                    [ text (getValidationMessage card.form.memoValidation) ]
            ]
        , div [ class "card__button-row" ]
            [ button
                [ class "btn btn--outline btn--primary"
                , onClick (ClickedCancelTransfer cardIndex)
                , disabled isDisabled
                , type_ "button"
                ]
                [ text_ "menu.cancel" ]
            , button
                [ class "btn btn--primary"
                , disabled isDisabled
                ]
                [ text_ "account.my_wallet.transfer.submit" ]
            ]
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
            let
                sales_ =
                    List.map
                        Shop.salePriceToInT
                        sales
            in
            UR.init { model | cards = Loaded (List.map cardFromSale sales_) }

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

                            price =
                                case String.toFloat card.sale.price of
                                    Just priceInt ->
                                        priceInt

                                    Nothing ->
                                        1.0

                            wantedUnits =
                                case String.toInt card.form.unit of
                                    Just quantityInt ->
                                        quantityInt

                                    -- TODO sort sales without units
                                    Nothing ->
                                        1

                            tAmount =
                                price * toFloat wantedUnits

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
                    Route.Shop (Just filter)
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
