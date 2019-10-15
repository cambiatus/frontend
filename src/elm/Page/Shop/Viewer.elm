module Page.Shop.Viewer exposing (Model, Msg, init, msgToString, subscriptions, update, view)

import Api
import Api.Graphql
import Asset.Icon as Icon
import Avatar
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Eos as Eos exposing (Symbol)
import Eos.Account as Eos
import Graphql.Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, onSubmit, targetValue)
import Html.Lazy as Lazy
import I18Next exposing (Translations, t)
import Icons
import Json.Encode as Encode
import List.Extra as LE
import Page exposing (Session(..))
import Route
import Session.Guest as Guest
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import Shop exposing (Sale)
import Transfer
import UpdateResult as UR



-- INIT


init : LoggedIn.Model -> String -> ( Model, Cmd Msg )
init { shared } saleId =
    let
        currentStatus =
            initStatus saleId

        model =
            { status = currentStatus
            , viewing = ViewingCard
            , form = initForm shared.translations
            }
    in
    ( model
    , Cmd.batch
        [ initCmd shared currentStatus ]
    )


initStatus : String -> Status
initStatus saleId =
    case String.toInt saleId of
        Just sId ->
            LoadingSale sId

        Nothing ->
            InvalidId saleId


initCmd : Shared -> Status -> Cmd Msg
initCmd shared status =
    case status of
        LoadingSale id ->
            Api.Graphql.query shared
                (Shop.saleQuery id)
                CompletedSaleLoad

        _ ->
            Cmd.none



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MODEL


type alias Model =
    { status : Status
    , viewing : ViewState
    , form : Form
    }


type alias Form =
    { price : String
    , unitValidation : Validation
    , memoValidation : Validation
    , units : String
    , memo : String
    }


initForm : Translations -> Form
initForm translations =
    { price = ""
    , units = ""
    , memo = t translations "shop.transfer.default_memo"
    , unitValidation = Valid
    , memoValidation = Valid
    }


type ViewState
    = ViewingCard
    | EditingTransfer


type Status
    = LoadingSale Int
    | InvalidId String
    | LoadingFailed (Graphql.Http.Error (Maybe Sale))
    | LoadedSale (Maybe Sale)


type FormError
    = UnitEmpty
    | UnitTooLow
    | UnitTooHigh
    | MemoEmpty
    | MemoTooLong
    | UnitNotOnlyNumbers


type Validation
    = Valid
    | Invalid FormError



-- Msg


type Msg
    = CompletedSaleLoad (Result (Graphql.Http.Error (Maybe Sale)) (Maybe Sale))
    | ClickedAsk Sale
    | ClickedBuy Sale
    | ClickedEdit Sale
    | ClickedQuestions Sale
    | ClickedTransfer Sale
    | GoBack
    | EnteredUnit String
    | EnteredMemo String


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model user =
    case msg of
        CompletedSaleLoad (Ok maybeSale) ->
            let
                mSale =
                    Maybe.map
                        Shop.salePriceToInT
                        maybeSale
            in
            { model | status = LoadedSale mSale }
                |> UR.init

        CompletedSaleLoad (Err err) ->
            { model | status = LoadingFailed err }
                |> UR.init
                |> UR.logGraphqlError msg err

        GoBack ->
            model
                |> UR.init
                |> UR.addCmd
                    (Nav.back user.shared.navKey 1)

        ClickedAsk sale ->
            model
                |> UR.init
                |> UR.addPort
                    { responseAddress = ClickedAsk sale
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "openChat" )
                            , ( "username", Encode.string (Eos.nameToString sale.creatorId) )
                            ]
                    }

        ClickedQuestions sale ->
            model
                |> UR.init
                |> UR.addPort
                    { responseAddress = ClickedQuestions sale
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "name", Encode.string "openChat" )
                            , ( "username", Encode.string (Eos.nameToString sale.creatorId) )
                            ]
                    }

        ClickedEdit sale ->
            let
                idString =
                    String.fromInt sale.id
            in
            model
                |> UR.init
                |> UR.addCmd
                    (Route.replaceUrl user.shared.navKey (Route.EditSale idString))

        ClickedBuy sale ->
            { model | viewing = EditingTransfer }
                |> UR.init

        ClickedTransfer sale ->
            let
                validatedForm =
                    validateForm sale model.form
            in
            if isFormValid validatedForm then
                case LoggedIn.isAuth user of
                    True ->
                        let
                            authorization =
                                { actor = user.accountName
                                , permissionName = Eos.samplePermission
                                }

                            price =
                                case String.toFloat sale.price of
                                    Just pF ->
                                        pF

                                    Nothing ->
                                        1

                            requiredUnits =
                                case String.toInt model.form.units of
                                    Just rU ->
                                        rU

                                    Nothing ->
                                        1

                            value =
                                { amount = price * toFloat requiredUnits
                                , symbol = sale.symbol
                                }

                            unitPrice =
                                { amount = price
                                , symbol = sale.symbol
                                }
                        in
                        model
                            |> UR.init
                            |> UR.addPort
                                { responseAddress = ClickedTransfer sale
                                , responseData = Encode.null
                                , data =
                                    Eos.encodeTransaction
                                        { actions =
                                            [ { accountName = "bes.token"
                                              , name = "transfer"
                                              , authorization = authorization
                                              , data =
                                                    { from = user.accountName
                                                    , to = sale.creatorId
                                                    , value = value
                                                    , memo = model.form.memo
                                                    }
                                                        |> Transfer.encodeEosActionData
                                              }
                                            , { accountName = "bes.cmm"
                                              , name = "transfersale"
                                              , authorization = authorization
                                              , data =
                                                    { id = sale.id
                                                    , from = user.accountName
                                                    , to = sale.creatorId
                                                    , quantity = unitPrice
                                                    , units = requiredUnits
                                                    }
                                                        |> Shop.encodeTransferSale
                                              }
                                            ]
                                        }
                                }
                            |> UR.addCmd (Route.replaceUrl user.shared.navKey (Route.Shop (Just Shop.MyCommunities)))

                    False ->
                        model
                            |> UR.init
                            |> UR.addExt (Just (ClickedTransfer sale) |> RequiredAuthentication)

            else
                { model | form = validatedForm }
                    |> UR.init

        EnteredUnit u ->
            case model.status of
                LoadedSale (Just saleItem) ->
                    let
                        salePrice =
                            case String.toFloat saleItem.price of
                                Just price ->
                                    price

                                -- Impossible State
                                Nothing ->
                                    0

                        newPrice =
                            case String.toFloat u of
                                Just uF ->
                                    String.fromFloat (uF * salePrice)

                                Nothing ->
                                    "Invalid Units"

                        currentForm =
                            model.form

                        newForm =
                            { currentForm | units = u, price = newPrice }
                    in
                    { model | form = newForm }
                        |> UR.init

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg []

        EnteredMemo m ->
            let
                currentForm =
                    model.form

                newForm =
                    { currentForm | memo = m }
            in
            { model | form = newForm }
                |> UR.init



-- VIEW


type alias Card =
    { sale : Sale
    , rate : Maybe Int
    }


cardFromSale : Sale -> Card
cardFromSale sale =
    { sale = sale
    , rate = Nothing
    }


view : Session -> Model -> Html Msg
view session model =
    let
        shared =
            Page.toShared session
    in
    case model.status of
        LoadingSale id ->
            div []
                [ Lazy.lazy viewHeader session
                , Page.fullPageLoading
                ]

        InvalidId invalidId ->
            div [ class "container mx-auto px-4" ]
                [ Lazy.lazy viewHeader session
                , div []
                    [ text (invalidId ++ " is not a valid Sale Id") ]
                ]

        LoadingFailed e ->
            Page.fullPageGraphQLError (t shared.translations "shop.title") e

        LoadedSale maybeSale ->
            case maybeSale of
                Just sale ->
                    let
                        cardData =
                            cardFromSale sale
                    in
                    div [ class "" ]
                        [ Lazy.lazy viewHeader session
                        , viewCard session cardData model
                        ]

                Nothing ->
                    div [ class "container mx-auto px-4" ]
                        [ div []
                            [ text "Could not load the sale" ]
                        ]


viewHeader : Session -> Html msg
viewHeader session =
    let
        shared =
            Page.toShared session
    in
    -- Page.viewTitle (t shared.translations "shop.title")
    div [ class "h-16 w-full bg-indigo-500 mb-4 flex px-4" ]
        [ a
            [ class "items-center flex"
            , Route.href (Route.Shop (Just Shop.MyCommunities))
            ]
            [ Icons.back ""
            , p [ class "text-white text-sm ml-2" ] [ text (t shared.translations "dashboard.back") ]
            ]
        ]


viewCard : Session -> Card -> Model -> Html Msg
viewCard session card model =
    let
        account =
            case session of
                LoggedIn data ->
                    Eos.nameToString data.accountName

                _ ->
                    ""

        shared =
            Page.toShared session

        text_ str =
            text (t shared.translations str)
    in
    viewCardWithHeader session
        card
        [ div [ class "sale__info" ]
            [ div [ class "large__sale__rating" ] []
            , if card.sale.trackStock then
                div [ class "large__sale__quantity" ]
                    [ p [ class "sale__quantity__title" ]
                        [ text_ "shop.units_available" ]
                    , p [ class "sale__quantity__text" ]
                        [ text (String.fromInt card.sale.units) ]
                    ]

              else
                text ""
            ]
        , if model.viewing == ViewingCard then
            div [ class "large__card__description" ]
                [ p [] [ text card.sale.description ] ]

          else
            viewTransferForm session card Dict.empty model
        , if Eos.nameToString card.sale.creatorId == account then
            div [ class "card__button-row" ]
                [ button
                    [ class "btn btn--primary"
                    , onClick (ClickedQuestions card.sale)
                    ]
                    [ text "See Questions" ]
                , button
                    [ class "btn btn--primary"
                    , onClick (ClickedEdit card.sale)
                    ]
                    [ text "Edit" ]
                ]

          else if card.sale.units <= 0 && card.sale.trackStock == True then
            div [ class "sale__out__of__stock" ]
                [ p [] [ text_ "shop.out_of_stock" ] ]

          else if model.viewing == EditingTransfer then
            div [ class "card__button-row" ]
                [ button
                    [ class "btn btn--primary"
                    , onClick (ClickedTransfer card.sale)
                    ]
                    [ text_ "shop.transfer.submit" ]
                ]

          else
            div [ class "card__button-row" ]
                [ button
                    [ class "btn btn--primary"
                    , onClick (ClickedBuy card.sale)
                    ]
                    [ text_ "shop.buy" ]

                -- , button
                --     [ class "btn btn--primary"
                --     , onClick (ClickedAsk card.sale)
                --     ]
                --     [ text_ "shop.ask" ]
                ]
        ]


viewCardWithHeader : Session -> Card -> List (Html Msg) -> Html Msg
viewCardWithHeader session card content =
    div [ class "large__card__container" ]
        [ div
            [ class "large__card" ]
            ([ viewHeaderBackground session card
             , viewHeaderAvatarTitle session card
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
        []


viewHeaderAvatarTitle : Session -> Card -> Html Msg
viewHeaderAvatarTitle session { sale } =
    let
        ipfsUrl =
            getIpfsUrl session

        saleSymbol =
            Eos.symbolToString sale.symbol

        balances =
            case session of
                LoggedIn sesh ->
                    sesh.balances

                Guest sesh ->
                    []

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

        ( shared, account ) =
            case session of
                LoggedIn a ->
                    ( a.shared, Eos.nameToString a.accountName )

                Guest a ->
                    ( a.shared, "" )

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
            , if Eos.nameToString sale.creatorId == account then
                text ""

              else
                p [ class "shop__balance" ]
                    [ text (tr "account.my_wallet.your_current_balance" [ ( "balance", balanceString ) ]) ]
            ]
        ]


viewTransferForm : Session -> Card -> Dict String FormError -> Model -> Html Msg
viewTransferForm session card errors model =
    let
        accountName =
            Eos.nameToString card.sale.creatorId

        form =
            model.form

        shared =
            Page.toShared session

        t =
            I18Next.t shared.translations

        saleSymbol =
            Eos.symbolToString card.sale.symbol

        balances =
            case session of
                LoggedIn sesh ->
                    sesh.balances

                Guest sesh ->
                    []

        maybeBal =
            LE.find (\bal -> bal.asset.symbol == card.sale.symbol) balances

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

        tr r_id replaces =
            I18Next.tr shared.translations I18Next.Curly r_id replaces
    in
    div [ class "large__card__transfer" ]
        [ div [ class "large__card__account" ]
            [ p [ class "large__card__label" ] [ text "User" ]
            , p [ class "large__card__name" ] [ text accountName ]
            ]
        , div [ class "large__card__quant" ]
            [ formField
                [ label [ for fieldId.units ]
                    [ text (t "shop.transfer.units_label") ]
                , input
                    [ class "input"
                    , type_ "number"
                    , id fieldId.units
                    , value form.units
                    , onInput EnteredUnit
                    , required True
                    , Html.Attributes.min "0"
                    ]
                    []
                , if form.unitValidation == Valid then
                    text ""

                  else
                    span [ class "field-error" ]
                        [ text (getValidationMessage form.unitValidation) ]
                ]
            , formField
                [ label [ for fieldId.price ]
                    [ text (t "shop.transfer.quantity_label" ++ " (" ++ saleSymbol ++ ")") ]
                , input
                    [ class "input"
                    , type_ "number"
                    , id fieldId.price
                    , value form.price
                    , required True
                    , disabled True
                    , Html.Attributes.min "0"
                    ]
                    []
                ]
            ]
        , p [ class "large__card__balance" ]
            [ text (tr "account.my_wallet.your_current_balance" [ ( "balance", balanceString ) ]) ]
        , div []
            [ formField
                [ label [ for fieldId.units ]
                    [ text (t "shop.transfer.memo_label") ]
                , textarea
                    [ class "input"
                    , id fieldId.memo
                    , value form.memo
                    , onInput EnteredMemo
                    , required True
                    , placeholder (t "shop.transfer.default_memo")
                    , Html.Attributes.min "0"
                    ]
                    []
                , if form.memoValidation == Valid then
                    text ""

                  else
                    span [ class "field-error" ]
                        [ text (getValidationMessage form.memoValidation) ]
                ]
            ]
        ]



-- VIEW HELPERS


getValidationMessage : Validation -> String
getValidationMessage validation =
    case validation of
        Valid ->
            ""

        Invalid error ->
            case error of
                UnitEmpty ->
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


formField : List (Html msg) -> Html msg
formField =
    div [ class "form-field" ]


fieldSuffix : String -> String
fieldSuffix s =
    "shop-editor-" ++ s


fieldId :
    { price : String
    , units : String
    , memo : String
    }
fieldId =
    { price = fieldSuffix "price"
    , memo = fieldSuffix "memo"
    , units = fieldSuffix "units"
    }


validateForm : Sale -> Form -> Form
validateForm sale form =
    let
        unitValidation : Validation
        unitValidation =
            if form.units == "" then
                Invalid UnitEmpty

            else
                case String.toInt form.units of
                    Just units ->
                        if units > sale.units && sale.trackStock then
                            Invalid UnitTooHigh

                        else if units <= 0 && sale.trackStock then
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


isFormValid : Form -> Bool
isFormValid form =
    form.unitValidation == Valid && form.memoValidation == Valid



-- UTILS


getIpfsUrl : Session -> String
getIpfsUrl session =
    case session of
        Guest s ->
            s.shared.endpoints.ipfs

        LoggedIn s ->
            s.shared.endpoints.ipfs


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedSaleLoad r ->
            "CompletedSaleLoad" :: []

        ClickedAsk _ ->
            [ "ClickedAsk" ]

        ClickedBuy _ ->
            [ "ClickedBuy" ]

        ClickedEdit _ ->
            [ "ClickedEdit" ]

        ClickedQuestions _ ->
            [ "ClickedQuestions" ]

        ClickedTransfer _ ->
            [ "ClickedTransfer" ]

        GoBack ->
            [ "GoBack" ]

        EnteredUnit u ->
            "EnteredUnit" :: [ u ]

        EnteredMemo m ->
            "EnteredMemo" :: [ m ]
