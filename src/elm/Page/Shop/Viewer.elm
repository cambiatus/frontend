module Page.Shop.Viewer exposing
    ( LoggedInModel
    , LoggedInMsg
    , Model
    , Msg(..)
    , init
    , jsAddressToMsg
    , msgToString
    , receiveLoggedInBroadcast
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
import Session.Shared exposing (Shared)
import Shop exposing (Product, ProductPreview)
import Transfer
import UpdateResult as UR
import View.Feedback as Feedback
import View.Form.Input as Input
import View.MarkdownEditor



-- INIT


init : Session -> Int -> ( Model, Cmd Msg )
init session saleId =
    case session of
        Page.LoggedIn { shared, authToken, accountName } ->
            ( AsLoggedIn
                { status = RemoteData.Loading
                , viewing = ViewingCard
                , form = initForm
                , hasChangedDefaultMemo = False
                , balances = []
                , isBuyButtonDisabled = False
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
            ( AsGuest { saleId = saleId, productPreview = RemoteData.Loading }
            , Api.Graphql.query guest.shared
                Nothing
                (Shop.productPreviewQuery saleId)
                (CompletedSalePreviewLoad >> AsGuestMsg)
            )



-- MODEL


type Model
    = AsGuest GuestModel
    | AsLoggedIn LoggedInModel


type alias GuestModel =
    { saleId : Int
    , productPreview : RemoteData (Graphql.Http.Error ProductPreview) ProductPreview
    }


type alias LoggedInModel =
    { status : RemoteData (Graphql.Http.Error (Maybe Product)) Product
    , viewing : ViewState
    , form : Form
    , hasChangedDefaultMemo : Bool
    , balances : List Balance
    , isBuyButtonDisabled : Bool
    }


type alias Form =
    { price : String
    , unitValidation : Validation
    , units : String
    , memo : View.MarkdownEditor.Model
    }


defaultMemoKey : String
defaultMemoKey =
    "shop.transfer.default_memo"


initForm : Form
initForm =
    { price = ""
    , units = ""
    , memo =
        View.MarkdownEditor.init "memo-editor"
            |> View.MarkdownEditor.setContents defaultMemoKey
    , unitValidation = Valid
    }


type ViewState
    = ViewingCard
    | EditingTransfer


type FormError
    = UnitEmpty
    | UnitTooLow
    | UnitTooHigh
    | UnitNotOnlyNumbers


type Validation
    = Valid
    | Invalid FormError



-- Msg


type Msg
    = AsGuestMsg GuestMsg
    | AsLoggedInMsg LoggedInMsg


type GuestMsg
    = CompletedSalePreviewLoad (RemoteData (Graphql.Http.Error ProductPreview) ProductPreview)


type LoggedInMsg
    = Ignored
    | ClosedAuthModal
    | CompletedSaleLoad (RemoteData (Graphql.Http.Error (Maybe Product)) (Maybe Product))
    | CompletedLoadBalances (Result Http.Error (List Balance))
    | ClickedBuy
    | ClickedEdit Product
    | ClickedTransfer Product
    | EnteredUnit String
    | GotMemoEditorMsg View.MarkdownEditor.Msg
    | GotTransferResult (Result (Maybe Value) String)
    | CompletedLoadTranslations


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
                |> UR.logIncompatibleMsg msg
                    (Page.maybeAccountName session)
                    { moduleName = "Page.Shop.Viewer", function = "update" }
                    []


updateAsGuest : GuestMsg -> GuestModel -> Guest.Model -> GuestUpdateResult
updateAsGuest msg model _ =
    case msg of
        CompletedSalePreviewLoad (RemoteData.Success productPreview) ->
            { model | productPreview = RemoteData.Success productPreview }
                |> UR.init

        CompletedSalePreviewLoad (RemoteData.Failure err) ->
            { model | productPreview = RemoteData.Failure err }
                |> UR.init
                |> UR.logGraphqlError msg
                    Nothing
                    "Got an error when loading sale preview"
                    { moduleName = "Page.Shop.Viewer", function = "updateAsGuest" }
                    []
                    err

        CompletedSalePreviewLoad RemoteData.NotAsked ->
            model
                |> UR.init

        CompletedSalePreviewLoad RemoteData.Loading ->
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
                        |> UR.logImpossible msg
                            "Couldn't find the sale"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Shop.Viewer", function = "updateAsLoggedIn" }
                            []

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
                    { model | isBuyButtonDisabled = False }
                        |> UR.init

        GotTransferResult (Err eosErrorString) ->
            let
                errorMessage =
                    EosError.parseTransferError loggedIn.shared.translators eosErrorString
            in
            { model | isBuyButtonDisabled = False }
                |> UR.init
                |> UR.addExt
                    (LoggedIn.ShowFeedback Feedback.Failure errorMessage)

        CompletedSaleLoad (RemoteData.Failure err) ->
            { model | status = RemoteData.Failure err }
                |> UR.init
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when trying to load shop sale"
                    { moduleName = "Page.Shop.Viewer", function = "updateAsLoggedIn" }
                    []
                    err

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
                { model | isBuyButtonDisabled = True }
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
                                        , memo = model.form.memo.contents
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
                        |> UR.logImpossible msg
                            "Entered available units, but sale is not loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Shop.Viewer", function = "updateAsLoggedIn" }
                            []

        GotMemoEditorMsg subMsg ->
            let
                oldForm =
                    model.form

                ( memo, memoCmd ) =
                    View.MarkdownEditor.update subMsg oldForm.memo
            in
            { model
                | form = { oldForm | memo = memo }
                , hasChangedDefaultMemo =
                    ((String.trim memo.contents == defaultMemoKey)
                        || (String.trim memo.contents == t defaultMemoKey)
                    )
                        |> not
            }
                |> UR.init
                |> UR.addCmd (Cmd.map GotMemoEditorMsg memoCmd)

        CompletedLoadBalances res ->
            case res of
                Ok bals ->
                    { model | balances = bals }
                        |> UR.init

                Err _ ->
                    model
                        |> UR.init

        CompletedLoadTranslations ->
            if not model.hasChangedDefaultMemo then
                let
                    oldForm =
                        model.form

                    ( memo, memoCmd ) =
                        View.MarkdownEditor.forceSetContents (t defaultMemoKey)
                            oldForm.memo
                in
                { model | form = { oldForm | memo = memo } }
                    |> UR.init
                    |> UR.addCmd (Cmd.map GotMemoEditorMsg memoCmd)

            else
                model
                    |> UR.init



-- VIEW


view : Session -> Model -> { title : String, content : Html Msg }
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

        contentContainer children =
            div [ class "container mx-auto h-full flex items-center" ]
                [ div [ class "flex flex-wrap" ]
                    children
                ]

        cardContainer =
            div [ class "w-full md:w-1/2 flex flex-wrap bg-white p-4" ]

        content =
            case ( model, session ) of
                ( AsGuest model_, Page.Guest guest ) ->
                    case model_.productPreview of
                        RemoteData.Success sale ->
                            contentContainer
                                [ viewProductImg sale.image
                                , cardContainer
                                    (viewCard guest.shared
                                        Nothing
                                        sale
                                        (viewGuestButton guest.shared sale)
                                        Nothing
                                    )
                                ]

                        RemoteData.Failure err ->
                            Page.fullPageGraphQLError (t "shop.title") err

                        RemoteData.Loading ->
                            Page.fullPageLoading guest.shared

                        RemoteData.NotAsked ->
                            Page.fullPageLoading guest.shared

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
                                        maybeBalance =
                                            LE.find (\bal -> bal.asset.symbol == sale.symbol) model_.balances
                                                |> Maybe.map .asset

                                        card =
                                            viewCard
                                                loggedIn.shared
                                                (Just loggedIn.accountName)
                                                sale
                                                (viewLoggedInButton loggedIn model_ sale)
                                                maybeBalance

                                        transferForm =
                                            if model_.viewing == ViewingCard then
                                                []

                                            else
                                                [ viewTransferForm loggedIn sale model_ ]
                                    in
                                    div []
                                        [ Page.viewHeader loggedIn sale.title
                                        , contentContainer
                                            [ viewProductImg sale.image
                                            , cardContainer
                                                ([ card
                                                 , transferForm
                                                 ]
                                                    |> List.concat
                                                )
                                            ]
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


viewCard :
    Shared
    -> Maybe Eos.Name
    ->
        { product
            | title : String
            , description : String
            , symbol : Eos.Symbol
            , price : Float
            , creator : Shop.ShopProfile
        }
    -> Html msg
    -> Maybe Eos.Asset
    -> List (Html msg)
viewCard shared maybeCurrentName sale buttonView maybeAsset =
    let
        text_ str =
            text (shared.translators.t str)

        currentName =
            maybeCurrentName
                |> Maybe.withDefault (Eos.stringToName "")
    in
    [ div [ class "font-medium text-3xl w-full" ] [ text sale.title ]
    , View.MarkdownEditor.viewReadOnly [ class "text-gray w-full md:text-sm" ]
        sale.description
    , div [ class "w-full flex items-center text-sm mt-4" ]
        [ div [ class "mr-4" ] [ Avatar.view sale.creator.avatar "h-10 w-10" ]
        , text_ "shop.sold_by"
        , a
            [ class "font-bold ml-1"
            , Route.href (Route.Profile sale.creator.account)
            ]
            [ Profile.viewProfileName shared currentName sale.creator ]
        ]
    , div [ class "flex flex-wrap w-full justify-between items-center" ]
        [ div []
            [ div [ class "flex items-center" ]
                [ div [ class "text-2xl text-green font-medium" ]
                    [ text (String.fromFloat sale.price) ]
                , div [ class "uppercase text-sm font-extralight ml-2 text-green" ]
                    [ text (Eos.symbolToSymbolCodeString sale.symbol) ]
                ]
            , case maybeAsset of
                Nothing ->
                    text ""

                Just asset ->
                    div [ class "flex" ]
                        [ div [ class "bg-gray-100 uppercase text-sm px-2" ]
                            [ text
                                (shared.translators.tr
                                    "account.my_wallet.your_current_balance"
                                    [ ( "balance", Eos.assetToString asset ) ]
                                )
                            ]
                        ]
            ]
        , div [ class "mt-6 md:mt-0 w-full sm:w-40" ]
            [ buttonView ]
        ]
    ]


viewGuestButton : Shared -> ProductPreview -> Html msg
viewGuestButton { translators } sale =
    a
        [ Route.href
            (Route.ViewSale sale.id
                |> Just
                |> Route.Join
            )
        , class "button button-primary"
        ]
        [ text <| translators.t "shop.buy" ]


viewLoggedInButton : LoggedIn.Model -> LoggedInModel -> Product -> Html LoggedInMsg
viewLoggedInButton loggedIn model sale =
    let
        text_ =
            text << loggedIn.shared.translators.t
    in
    div [ class "mt-6 md:mt-0 w-full sm:w-40" ]
        [ if sale.creator.account == loggedIn.accountName then
            div [ class "flex md:justify-end" ]
                [ button
                    [ class "button button-primary w-full px-4"
                    , onClick (ClickedEdit sale)
                    ]
                    [ text_ "shop.edit" ]
                ]

          else if sale.units <= 0 && sale.trackStock then
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
                    , onClick (ClickedTransfer sale)
                    , disabled model.isBuyButtonDisabled
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


viewTransferForm : LoggedIn.Model -> Product -> LoggedInModel -> Html LoggedInMsg
viewTransferForm { shared } sale model =
    let
        accountName =
            Eos.nameToString sale.creator.account

        form =
            model.form

        t =
            shared.translators.t

        saleSymbol =
            Eos.symbolToSymbolCodeString sale.symbol

        maybeBal =
            LE.find (\bal -> bal.asset.symbol == sale.symbol) model.balances

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
                |> Input.withCurrency sale.symbol
                |> Input.toHtml
            ]
        , p []
            [ text (tr "account.my_wallet.your_current_balance" [ ( "balance", balanceString ) ]) ]
        , View.MarkdownEditor.view
            { translators = shared.translators
            , placeholder = Just (t defaultMemoKey)
            , label = t "shop.transfer.memo_label"
            , problem = Nothing
            , disabled = False
            }
            []
            form.memo
            |> Html.map GotMemoEditorMsg
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
    in
    { form
        | unitValidation = unitValidation
    }


isFormValid : Form -> Bool
isFormValid form =
    form.unitValidation == Valid



-- UTILS


receiveLoggedInBroadcast : LoggedIn.BroadcastMsg -> Maybe LoggedInMsg
receiveLoggedInBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.TranslationsLoaded ->
            Just CompletedLoadTranslations

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        AsGuestMsg subMsg ->
            "AsGuestMsg" :: guestMsgToString subMsg

        AsLoggedInMsg subMsg ->
            "AsLoggedInMsg" :: loggedInMsgToString subMsg


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

        GotMemoEditorMsg subMsg ->
            "GotMemoEditorMsg" :: View.MarkdownEditor.msgToString subMsg

        CompletedLoadBalances _ ->
            [ "CompletedLoadBalances" ]

        CompletedLoadTranslations ->
            [ "CompletedLoadTranslations" ]


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "AsLoggedInMsg" :: rAddress ->
            Maybe.map AsLoggedInMsg
                (jsAddressToLoggedInMsg rAddress val)

        _ ->
            Nothing


jsAddressToLoggedInMsg : List String -> Value -> Maybe LoggedInMsg
jsAddressToLoggedInMsg addr val =
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
