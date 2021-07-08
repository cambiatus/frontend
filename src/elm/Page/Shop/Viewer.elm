module Page.Shop.Viewer exposing
    ( Model
    , Msg
    , init
    , jsAddressToMsg
    , msgToString
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
import Html.Attributes exposing (autocomplete, class, disabled, required, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import List.Extra as LE
import Page exposing (Session(..))
import Profile
import RemoteData exposing (RemoteData)
import Route
import Session.Guest as Guest
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Translators)
import Shop exposing (Product, ProductPreview)
import Transfer
import UpdateResult as UR
import View.Feedback as Feedback
import View.Form.Input as Input



-- INIT


init : Session -> Int -> ( Model, Cmd Msg )
init session saleId =
    case session of
        Page.LoggedIn { shared, authToken, accountName } ->
            ( AsLoggedIn
                { status = RemoteData.Loading
                , viewing = ViewingCard
                , form = initForm (Page.toShared session |> .translators)
                , balances = []
                }
            , Cmd.batch
                [ Api.Graphql.query shared
                    (Just authToken)
                    (Shop.productQuery saleId)
                    CompletedSaleLoad
                , Api.getBalances shared accountName CompletedLoadBalances
                ]
                |> Cmd.map AsLoggedInMsg
            )

        Page.Guest guest ->
            ( AsGuest { productPreview = RemoteData.Loading }
            , Api.Graphql.query guest.shared
                Nothing
                (Shop.productPreviewQuery saleId)
                CompletedSalePreviewLoad
                |> Cmd.map AsGuestMsg
            )



-- MODEL


type Model
    = AsGuest GuestModel
    | AsLoggedIn LoggedInModel


type alias GuestModel =
    { productPreview : RemoteData (Graphql.Http.Error (Maybe Shop.ProductPreview)) Shop.ProductPreview
    }


type alias LoggedInModel =
    { status : RemoteData (Graphql.Http.Error (Maybe Product)) Product
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
    = AsGuestMsg GuestMsg
    | AsLoggedInMsg LoggedInMsg


type GuestMsg
    = CompletedSalePreviewLoad (RemoteData (Graphql.Http.Error (Maybe ProductPreview)) (Maybe ProductPreview))


type LoggedInMsg
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
    UR.UpdateResult Model Msg (Page.External Msg)


type alias GuestUpdateResult =
    UR.UpdateResult GuestModel GuestMsg Guest.External


type alias LoggedInUpdateResult =
    UR.UpdateResult LoggedInModel LoggedInMsg (LoggedIn.External LoggedInMsg)


update : Msg -> Model -> Session -> UpdateResult
update msg model session =
    case ( msg, model, session ) of
        ( AsGuestMsg subMsg, AsGuest subModel, Page.Guest guest ) ->
            updateAsGuest subMsg subModel guest
                |> UR.map AsGuest AsGuestMsg (Page.GuestExternal >> UR.addExt)

        ( AsLoggedInMsg subMsg, AsLoggedIn subModel, Page.LoggedIn loggedIn ) ->
            updateAsLoggedIn subMsg subModel loggedIn
                |> UR.map AsLoggedIn
                    AsLoggedInMsg
                    (LoggedIn.mapExternal AsLoggedInMsg >> Page.LoggedInExternal >> UR.addExt)

        _ ->
            model
                |> UR.init
                |> UR.logImpossible msg [ "InvalidMsg" ]


updateAsGuest : GuestMsg -> GuestModel -> Guest.Model -> GuestUpdateResult
updateAsGuest msg model _ =
    case msg of
        CompletedSalePreviewLoad (RemoteData.Success (Just productPreview)) ->
            { model | productPreview = RemoteData.Success productPreview }
                |> UR.init

        CompletedSalePreviewLoad (RemoteData.Success Nothing) ->
            model
                |> UR.init
                |> UR.logImpossible msg [ "NoSaleFound" ]

        CompletedSalePreviewLoad (RemoteData.Failure err) ->
            { model | productPreview = RemoteData.Failure err }
                |> UR.init
                |> UR.logGraphqlError msg err

        _ ->
            model
                |> UR.init


updateAsLoggedIn : LoggedInMsg -> LoggedInModel -> LoggedIn.Model -> LoggedInUpdateResult
updateAsLoggedIn msg model loggedIn =
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
            case maybeSale of
                Nothing ->
                    model
                        |> UR.init
                        -- If there isn't a sale with the given id, the backend
                        -- returns an error
                        |> UR.logImpossible msg [ "NoSaleFound" ]

                Just sale ->
                    { model | status = RemoteData.Success sale }
                        |> UR.init

        GotTransferResult (Ok _) ->
            case model.status of
                RemoteData.Success _ ->
                    model
                        |> UR.init
                        |> UR.addExt
                            (LoggedIn.ShowFeedback Feedback.Success (t "shop.transfer.success"))
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
            model
                |> UR.init
                |> UR.addExt
                    (LoggedIn.ShowFeedback Feedback.Failure errorMessage)

        CompletedSaleLoad (RemoteData.Failure err) ->
            { model | status = RemoteData.Failure err }
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
                RemoteData.Success saleItem ->
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


view : Page.Session -> Model -> { title : String, content : Html Msg }
view session model =
    let
        { t } =
            (Page.toShared session).translators

        shopTitle =
            t "shop.title"

        title =
            case model of
                AsLoggedIn { status } ->
                    case status of
                        RemoteData.Success sale ->
                            sale.title ++ " - " ++ shopTitle

                        _ ->
                            shopTitle

                AsGuest { productPreview } ->
                    case productPreview of
                        RemoteData.Success sale ->
                            sale.title ++ " - " ++ shopTitle

                        _ ->
                            shopTitle

        content =
            case ( model, session ) of
                ( AsGuest _, Page.Guest _ ) ->
                    div [] [ text "Test" ]
                        |> Html.map AsGuestMsg

                ( AsLoggedIn model_, Page.LoggedIn loggedIn ) ->
                    case RemoteData.map .hasShop loggedIn.selectedCommunity of
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

                        RemoteData.Success True ->
                            case model_.status of
                                RemoteData.Loading ->
                                    div []
                                        [ Page.viewHeader loggedIn ""
                                        , Page.fullPageLoading loggedIn.shared
                                        ]

                                RemoteData.NotAsked ->
                                    div []
                                        [ Page.viewHeader loggedIn ""
                                        , Page.fullPageLoading loggedIn.shared
                                        ]

                                RemoteData.Failure e ->
                                    Page.fullPageGraphQLError (t "shop.title") e

                                RemoteData.Success sale ->
                                    let
                                        cardData =
                                            cardFromSale sale
                                    in
                                    div []
                                        [ Page.viewHeader loggedIn cardData.product.title
                                        , div [ class "container mx-auto" ] [ viewCard loggedIn cardData model_ ]
                                        , if model_.viewing == ViewingCard then
                                            text ""

                                          else
                                            viewTransferForm loggedIn cardData model_
                                        ]
                                        |> Html.map AsLoggedInMsg

                _ ->
                    Page.fullPageError (t "shop.title") Http.Timeout
    in
    { title = title
    , content = content
    }


viewProductImg : Maybe String -> Html msg
viewProductImg maybeImgUrl =
    let
        imageUrl =
            case maybeImgUrl of
                Nothing ->
                    "/icons/shop-placeholder0.svg"

                Just "" ->
                    "/icons/shop-placeholder0.svg"

                Just imgUrl ->
                    imgUrl
    in
    div [ class "w-full md:w-1/2 p-4 flex justify-center" ]
        [ img
            [ src imageUrl
            , class "object-contain w-full h-64"
            ]
            []
        ]


viewCard : LoggedIn.Model -> Card -> LoggedInModel -> Html LoggedInMsg
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
        [ viewProductImg card.product.image
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


viewTransferForm : LoggedIn.Model -> Card -> LoggedInModel -> Html LoggedInMsg
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
        AsGuestMsg subMsg ->
            guestMsgToString subMsg

        AsLoggedInMsg subMsg ->
            loggedInMsgToString subMsg


guestMsgToString : GuestMsg -> List String
guestMsgToString msg =
    case msg of
        CompletedSalePreviewLoad r ->
            [ "CompletedSalePreviewLoad", UR.remoteDataToString r ]


loggedInMsgToString : LoggedInMsg -> List String
loggedInMsgToString msg =
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
                |> Result.map (Just << AsLoggedInMsg << GotTransferResult)
                |> Result.withDefault Nothing

        _ ->
            Nothing
