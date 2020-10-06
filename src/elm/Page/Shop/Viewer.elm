module Page.Shop.Viewer exposing (Model, Msg, init, msgToString, subscriptions, update, view)

import Api
import Api.Graphql
import Avatar
import Community exposing (Balance)
import Eos
import Eos.Account as Eos
import Graphql.Http
import Html exposing (Html, a, button, div, img, input, label, p, span, text, textarea)
import Html.Attributes exposing (class, disabled, for, id, placeholder, required, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import I18Next exposing (Translations, t)
import Json.Encode as Encode
import List.Extra as LE
import Page exposing (Session(..))
import Profile
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import Shop exposing (Sale)
import Transfer
import UpdateResult as UR



-- INIT


init : LoggedIn.Model -> String -> ( Model, Cmd Msg )
init { shared, accountName } saleId =
    let
        currentStatus =
            initStatus saleId

        model =
            { status = currentStatus
            , viewing = ViewingCard
            , form = initForm shared.translations
            , balances = []
            }
    in
    ( model
    , Cmd.batch
        [ initCmd shared currentStatus
        , Api.getBalances shared accountName CompletedLoadBalances
        ]
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
    , balances : List Balance
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
    | CompletedLoadBalances (Result Http.Error (List Balance))
    | ClickedBuy Sale
    | ClickedEdit Sale
    | ClickedQuestions Sale
    | ClickedTransfer Sale
    | EnteredUnit String
    | EnteredMemo String


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model user =
    case msg of
        CompletedSaleLoad (Ok maybeSale) ->
            { model | status = LoadedSale maybeSale }
                |> UR.init

        CompletedSaleLoad (Err err) ->
            { model | status = LoadingFailed err }
                |> UR.init
                |> UR.logGraphqlError msg err

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

        ClickedBuy _ ->
            { model | viewing = EditingTransfer }
                |> UR.init

        ClickedTransfer sale ->
            let
                validatedForm =
                    validateForm sale model.form
            in
            if isFormValid validatedForm then
                if LoggedIn.isAuth user then
                    let
                        authorization =
                            { actor = user.accountName
                            , permissionName = Eos.samplePermission
                            }

                        requiredUnits =
                            case String.toInt model.form.units of
                                Just rU ->
                                    rU

                                Nothing ->
                                    1

                        value =
                            { amount = sale.price * toFloat requiredUnits
                            , symbol = sale.symbol
                            }

                        unitPrice =
                            { amount = sale.price
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
                                    [ { accountName = user.shared.contracts.token
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
                                    , { accountName = user.shared.contracts.community
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
                        |> UR.addCmd (Route.replaceUrl user.shared.navKey (Route.Shop Shop.All))
                        |> UR.addExt (ShowFeedback LoggedIn.Success (user.shared.translators.t "transfer_result.transfer_success"))

                else
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
                        newPrice =
                            case String.toFloat u of
                                Just uF ->
                                    String.fromFloat (uF * saleItem.price)

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

        CompletedLoadBalances res ->
            case res of
                Ok bals ->
                    { model | balances = bals }
                        |> UR.init

                Err _ ->
                    model
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


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        shopTitle =
            t loggedIn.shared.translations "shop.title"

        title =
            case model.status of
                LoadedSale maybeSale ->
                    case maybeSale of
                        Just sale ->
                            sale.title ++ " - " ++ shopTitle

                        Nothing ->
                            shopTitle

                _ ->
                    shopTitle

        content =
            case model.status of
                LoadingSale _ ->
                    div []
                        [ Page.viewHeader loggedIn "" (Route.Shop Shop.All)
                        , Page.fullPageLoading
                        ]

                InvalidId invalidId ->
                    div [ class "container mx-auto px-4" ]
                        [ Page.viewHeader loggedIn "" (Route.Shop Shop.All)
                        , div []
                            [ text (invalidId ++ " is not a valid Sale Id") ]
                        ]

                LoadingFailed e ->
                    Page.fullPageGraphQLError (t loggedIn.shared.translations "shop.title") e

                LoadedSale maybeSale ->
                    case maybeSale of
                        Just sale ->
                            let
                                cardData =
                                    cardFromSale sale
                            in
                            div []
                                [ Page.viewHeader loggedIn cardData.sale.title (Route.Shop Shop.All)
                                , div [ class "container mx-auto" ] [ viewCard loggedIn cardData model ]
                                ]

                        Nothing ->
                            div [ class "container mx-auto px-4" ]
                                [ div []
                                    [ text "Could not load the sale" ]
                                ]
    in
    { title = title
    , content = content
    }


viewCard : LoggedIn.Model -> Card -> Model -> Html Msg
viewCard ({ shared } as loggedIn) card model =
    let
        cmmBalance =
            LE.find (\bal -> bal.asset.symbol == card.sale.symbol) model.balances

        balance =
            case cmmBalance of
                Just b ->
                    b.asset.amount

                Nothing ->
                    0.0

        currBalance =
            String.fromFloat balance ++ " " ++ Eos.symbolToString card.sale.symbol

        text_ str =
            text (t shared.translations str)

        tr r_id replaces =
            I18Next.tr shared.translations I18Next.Curly r_id replaces
    in
    div [ class "flex flex-wrap" ]
        [ div [ class "w-full md:w-1/2 p-4 flex justify-center" ]
            [ img
                [ src (Maybe.withDefault "" card.sale.image)
                , class "object-scale-down w-full h-64"
                ]
                []
            ]
        , div [ class "w-full md:w-1/2 flex flex-wrap bg-white p-4" ]
            [ div [ class "font-medium text-3xl w-full" ] [ text card.sale.title ]
            , div [ class "text-gray w-full md:text-sm" ] [ text card.sale.description ]
            , div [ class "w-full flex items-center text-sm" ]
                [ div [ class "mr-4" ] [ Avatar.view card.sale.creator.avatar "h-10 w-10" ]
                , text_ "shop.sold_by"
                , a
                    [ class "font-bold ml-1"
                    , Route.href (Route.PublicProfile <| Eos.nameToString card.sale.creator.account)
                    ]
                    [ Profile.viewProfileName loggedIn.accountName card.sale.creator shared.translations ]
                ]
            , div [ class "flex flex-wrap w-full justify-between items-center" ]
                [ div [ class "" ]
                    [ div [ class "flex items-center" ]
                        [ div [ class "text-2xl text-green font-medium" ] [ text (String.fromFloat card.sale.price) ]
                        , div [ class "uppercase text-sm font-thin ml-2 text-green" ] [ text (Eos.symbolToString card.sale.symbol) ]
                        ]
                    , div [ class "flex" ]
                        [ div [ class "bg-gray-100 uppercase text-xs px-2" ]
                            [ text (tr "account.my_wallet.your_current_balance" [ ( "balance", currBalance ) ]) ]
                        ]
                    ]
                , div [ class "mt-6 md:mt-0 w-full sm:w-40" ]
                    [ if card.sale.creatorId == loggedIn.accountName then
                        div [ class "flex md:justify-end" ]
                            [ button
                                [ class "button button-primary w-full px-4"
                                , onClick (ClickedEdit card.sale)
                                ]
                                [ text_ "shop.edit" ]
                            ]

                      else if card.sale.units <= 0 && card.sale.trackStock == True then
                        div [ class "flex -mx-2 md:justify-end" ]
                            [ button
                                [ disabled True
                                , class "button button-disabled mx-auto"
                                ]
                                [ text_ "shop.out_of_stock" ]
                            ]

                      else if model.viewing == EditingTransfer then
                        div [ class "flex md:justify-end" ]
                            [ button
                                [ class "button button-primary"
                                , onClick (ClickedTransfer card.sale)
                                ]
                                [ text_ "shop.transfer.submit" ]
                            ]

                      else
                        div [ class "flex -mx-2 md:justify-end" ]
                            [ button
                                [ class "button button-primary w-full sm:w-40 mx-auto"
                                , onClick (ClickedBuy card.sale)
                                ]
                                [ text_ "shop.buy" ]
                            ]
                    ]
                ]
            , div
                [ class "w-full flex" ]
                [ if model.viewing == ViewingCard then
                    div []
                        []

                  else
                    viewTransferForm loggedIn card model
                ]
            ]
        ]


viewTransferForm : LoggedIn.Model -> Card -> Model -> Html Msg
viewTransferForm { shared } card model =
    let
        accountName =
            Eos.nameToString card.sale.creatorId

        form =
            model.form

        t =
            I18Next.t shared.translations

        saleSymbol =
            Eos.symbolToString card.sale.symbol

        maybeBal =
            LE.find (\bal -> bal.asset.symbol == card.sale.symbol) model.balances

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


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedSaleLoad _ ->
            [ "CompletedSaleLoad" ]

        ClickedBuy _ ->
            [ "ClickedBuy" ]

        ClickedEdit _ ->
            [ "ClickedEdit" ]

        ClickedQuestions _ ->
            [ "ClickedQuestions" ]

        ClickedTransfer _ ->
            [ "ClickedTransfer" ]

        EnteredUnit u ->
            [ "EnteredUnit", u ]

        EnteredMemo m ->
            [ "EnteredMemo", m ]

        CompletedLoadBalances _ ->
            [ "CompletedLoadBalances" ]
