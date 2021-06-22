module Page.Shop.Viewer exposing
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
import Avatar
import Community exposing (Balance)
import Eos
import Eos.Account as Eos
import Eos.EosError as EosError
import Graphql.Http
import Html exposing (Html, a, button, div, img, p, text)
import Html.Attributes exposing (autocomplete, class, disabled, id, required, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import List.Extra as LE
import Page exposing (Session(..))
import Profile
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared, Translators)
import Shop exposing (Product)
import Transfer
import UpdateResult as UR
import View.Feedback as Feedback
import View.Form.Input as Input



-- INIT


init : LoggedIn.Model -> String -> ( Model, Cmd Msg )
init { shared, accountName, authToken } saleId =
    let
        currentStatus =
            initStatus saleId

        model =
            { status = currentStatus
            , viewing = ViewingCard
            , form = initForm shared.translators
            , balances = []
            }
    in
    ( model
    , Cmd.batch
        [ initCmd shared currentStatus authToken
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


initCmd : Shared -> Status -> String -> Cmd Msg
initCmd shared status authToken =
    case status of
        LoadingSale id ->
            Api.Graphql.query shared
                (Just authToken)
                (Shop.productQuery id)
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


initForm : Translators -> Form
initForm { t } =
    { price = ""
    , units = ""
    , memo = t "shop.transfer.default_memo"
    , unitValidation = Valid
    , memoValidation = Valid
    }


type ViewState
    = ViewingCard
    | EditingTransfer


type Status
    = LoadingSale Int
    | InvalidId String
    | LoadingFailed (Graphql.Http.Error (Maybe Product))
    | LoadedSale (Maybe Product)


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
    = Ignored
    | ClosedAuthModal
    | CompletedSaleLoad (RemoteData (Graphql.Http.Error (Maybe Product)) (Maybe Product))
    | CompletedLoadBalances (Result Http.Error (List Balance))
    | ClickedBuy
    | ClickedEdit Product
    | ClickedTransfer Product
    | EnteredUnit String
    | EnteredMemo String
    | GotTransferResult (Result (Maybe Value) String)


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    let
        { t } =
            loggedIn.shared.translators
    in
    case msg of
        Ignored ->
            UR.init model

        ClosedAuthModal ->
            UR.init model

        CompletedSaleLoad (RemoteData.Success maybeSale) ->
            { model | status = LoadedSale maybeSale }
                |> UR.init

        GotTransferResult (Ok _) ->
            case model.status of
                LoadedSale _ ->
                    model
                        |> UR.init
                        |> UR.addExt
                            (ShowFeedback Feedback.Success (t "shop.transfer.success"))
                        |> UR.addCmd
                            (Route.replaceUrl loggedIn.shared.navKey (Route.Shop Shop.All))

                _ ->
                    model
                        |> UR.init

        GotTransferResult (Err eosErrorString) ->
            let
                errorMessage =
                    EosError.parseTransferError loggedIn.shared.translators eosErrorString
            in
            case model.status of
                LoadedSale _ ->
                    model
                        |> UR.init
                        |> UR.addExt
                            (LoggedIn.ShowFeedback Feedback.Failure errorMessage)

                _ ->
                    model
                        |> UR.init

        CompletedSaleLoad (RemoteData.Failure err) ->
            { model | status = LoadingFailed err }
                |> UR.init
                |> UR.logGraphqlError msg err

        CompletedSaleLoad _ ->
            UR.init model

        ClickedEdit sale ->
            let
                idString =
                    String.fromInt sale.id
            in
            model
                |> UR.init
                |> UR.addCmd
                    (Route.replaceUrl loggedIn.shared.navKey (Route.EditSale idString))

        ClickedBuy ->
            { model | viewing = EditingTransfer }
                |> UR.init

        ClickedTransfer sale ->
            let
                validatedForm =
                    validateForm sale model.form
            in
            if isFormValid validatedForm then
                let
                    authorization =
                        { actor = loggedIn.accountName
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
                                [ { accountName = loggedIn.shared.contracts.token
                                  , name = "transfer"
                                  , authorization = authorization
                                  , data =
                                        { from = loggedIn.accountName
                                        , to = sale.creatorId
                                        , value = value
                                        , memo = model.form.memo
                                        }
                                            |> Transfer.encodeEosActionData
                                  }
                                , { accountName = loggedIn.shared.contracts.community
                                  , name = "transfersale"
                                  , authorization = authorization
                                  , data =
                                        { id = sale.id
                                        , from = loggedIn.accountName
                                        , to = sale.creatorId
                                        , quantity = unitPrice
                                        , units = requiredUnits
                                        }
                                            |> Shop.encodeTransferSale
                                  }
                                ]
                        }
                    |> LoggedIn.withAuthentication loggedIn
                        model
                        { successMsg = msg, errorMsg = ClosedAuthModal }

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
    { product : Product
    , rate : Maybe Int
    }


cardFromSale : Product -> Card
cardFromSale sale =
    { product = sale
    , rate = Nothing
    }


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        { t } =
            loggedIn.shared.translators

        shopTitle =
            t "shop.title"

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
                        [ Page.viewHeader loggedIn ""
                        , Page.fullPageLoading loggedIn.shared
                        ]

                InvalidId invalidId ->
                    div [ class "container mx-auto px-4" ]
                        [ Page.viewHeader loggedIn ""
                        , div []
                            [ text (invalidId ++ " is not a valid Sale Id") ]
                        ]

                LoadingFailed e ->
                    Page.fullPageGraphQLError (t "shop.title") e

                LoadedSale maybeSale ->
                    case maybeSale of
                        Just sale ->
                            let
                                cardData =
                                    cardFromSale sale
                            in
                            div []
                                [ Page.viewHeader loggedIn cardData.product.title
                                , div [ class "container mx-auto" ] [ viewCard loggedIn cardData model ]
                                ]

                        Nothing ->
                            div [ class "container mx-auto px-4" ]
                                [ div []
                                    [ text "Could not load the sale" ]
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


viewCard : LoggedIn.Model -> Card -> Model -> Html Msg
viewCard ({ shared } as loggedIn) card model =
    let
        cmmBalance =
            LE.find (\bal -> bal.asset.symbol == card.product.symbol) model.balances

        balance =
            case cmmBalance of
                Just b ->
                    b.asset.amount

                Nothing ->
                    0.0

        currBalance =
            String.fromFloat balance ++ " " ++ Eos.symbolToSymbolCodeString card.product.symbol

        text_ str =
            text (shared.translators.t str)

        tr rId replaces =
            shared.translators.tr rId replaces
    in
    div [ class "flex flex-wrap" ]
        [ div [ class "w-full md:w-1/2 p-4 flex justify-center" ]
            [ img
                [ src (Maybe.withDefault "" card.product.image)
                , class "object-scale-down w-full h-64"
                ]
                []
            ]
        , div [ class "w-full md:w-1/2 flex flex-wrap bg-white p-4" ]
            [ div [ class "font-medium text-3xl w-full" ] [ text card.product.title ]
            , div [ class "text-gray w-full md:text-sm" ] [ text card.product.description ]
            , div [ class "w-full flex items-center text-sm" ]
                [ div [ class "mr-4" ] [ Avatar.view card.product.creator.avatar "h-10 w-10" ]
                , text_ "shop.sold_by"
                , a
                    [ class "font-bold ml-1"
                    , Route.href (Route.ProfilePublic <| Eos.nameToString card.product.creator.account)
                    ]
                    [ Profile.viewProfileName shared loggedIn.accountName card.product.creator ]
                ]
            , div [ class "flex flex-wrap w-full justify-between items-center" ]
                [ div [ class "" ]
                    [ div [ class "flex items-center" ]
                        [ div [ class "text-2xl text-green font-medium" ]
                            [ text (String.fromFloat card.product.price) ]
                        , div [ class "uppercase text-sm font-extralight ml-2 text-green" ]
                            [ text (Eos.symbolToSymbolCodeString card.product.symbol) ]
                        ]
                    , div [ class "flex" ]
                        [ div [ class "bg-gray-100 uppercase text-xs px-2" ]
                            [ text (tr "account.my_wallet.your_current_balance" [ ( "balance", currBalance ) ]) ]
                        ]
                    ]
                , div [ class "mt-6 md:mt-0 w-full sm:w-40" ]
                    [ if card.product.creatorId == loggedIn.accountName then
                        div [ class "flex md:justify-end" ]
                            [ button
                                [ class "button button-primary w-full px-4"
                                , onClick (ClickedEdit card.product)
                                ]
                                [ text_ "shop.edit" ]
                            ]

                      else if card.product.units <= 0 && card.product.trackStock then
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
                                , onClick (ClickedTransfer card.product)
                                ]
                                [ text_ "shop.transfer.submit" ]
                            ]

                      else
                        div [ class "flex -mx-2 md:justify-end" ]
                            [ button
                                [ class "button button-primary w-full sm:w-40 mx-auto"
                                , onClick ClickedBuy
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
            Eos.nameToString card.product.creatorId

        form =
            model.form

        t =
            shared.translators.t

        saleSymbol =
            Eos.symbolToSymbolCodeString card.product.symbol

        maybeBal =
            LE.find (\bal -> bal.asset.symbol == card.product.symbol) model.balances

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

        tr rId replaces =
            shared.translators.tr rId replaces
    in
    div []
        [ div []
            [ p [] [ text "User" ]
            , p [] [ text accountName ]
            ]
        , div []
            [ Input.init
                { label = t "shop.transfer.units_label"
                , id = fieldId.units
                , onInput = EnteredUnit
                , disabled = False
                , value = form.units
                , placeholder = Nothing
                , problems = getError form.unitValidation
                , translators = shared.translators
                }
                |> Input.asNumeric
                |> Input.withType Input.Number
                |> Input.withAttrs
                    [ autocomplete False
                    , required True
                    , Html.Attributes.min "0"
                    ]
                |> Input.toHtml
            , Input.init
                { label = t "shop.transfer.quantity_label"
                , id = fieldId.price
                , onInput = \_ -> Ignored
                , disabled = True
                , value = form.price
                , placeholder = Nothing
                , problems = Nothing
                , translators = shared.translators
                }
                |> Input.withAttrs [ required True, Html.Attributes.min "0" ]
                |> Input.withCurrency card.product.symbol
                |> Input.toHtml
            ]
        , p []
            [ text (tr "account.my_wallet.your_current_balance" [ ( "balance", balanceString ) ]) ]
        , Input.init
            { label = t "shop.transfer.memo_label"
            , id = fieldId.memo
            , onInput = EnteredMemo
            , disabled = False
            , value = form.memo
            , placeholder = Just (t "shop.transfer.default_memo")
            , problems = getError form.memoValidation
            , translators = shared.translators
            }
            |> Input.withInputType Input.TextArea
            |> Input.withAttrs [ required True, Html.Attributes.min "0" ]
            |> Input.toHtml
        ]



-- VIEW HELPERS


getError : Validation -> Maybe (List String)
getError validation =
    case validation of
        Valid ->
            Nothing

        Invalid error ->
            let
                translationString =
                    case error of
                        UnitEmpty ->
                            "shop.transfer.errors.unitEmpty"

                        UnitTooLow ->
                            "shop.transfer.errors.unitTooLow"

                        UnitTooHigh ->
                            "shop.transfer.errors.unitTooHigh"

                        UnitNotOnlyNumbers ->
                            "shop.transfer.errors.unitNotOnlyNumbers"

                        MemoEmpty ->
                            "shop.transfer.errors.memoEmpty"

                        MemoTooLong ->
                            "shop.transfer.errors.memoTooLong"
            in
            Just [ translationString ]


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


validateForm : Product -> Form -> Form
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
        Ignored ->
            [ "Ignored" ]

        ClosedAuthModal ->
            [ "ClosedAuthModal" ]

        CompletedSaleLoad _ ->
            [ "CompletedSaleLoad" ]

        GotTransferResult _ ->
            [ "GotTransferResult" ]

        ClickedBuy ->
            [ "ClickedBuy" ]

        ClickedEdit _ ->
            [ "ClickedEdit" ]

        ClickedTransfer _ ->
            [ "ClickedTransfer" ]

        EnteredUnit u ->
            [ "EnteredUnit", u ]

        EnteredMemo m ->
            [ "EnteredMemo", m ]

        CompletedLoadBalances _ ->
            [ "CompletedLoadBalances" ]


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        [ "ClickedTransfer" ] ->
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.field "transactionId" Decode.string
                        |> Decode.map Ok
                    , Decode.field "error" (Decode.nullable Decode.value)
                        |> Decode.map Err
                    ]
                )
                val
                |> Result.map (Just << GotTransferResult)
                |> Result.withDefault Nothing

        _ ->
            Nothing
