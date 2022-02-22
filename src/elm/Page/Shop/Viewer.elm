module Page.Shop.Viewer exposing
    ( LoggedInModel
    , LoggedInMsg
    , Model
    , Msg(..)
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
import Form
import Form.RichText
import Form.Text
import Form.Validate
import Graphql.Http
import Html exposing (Html, a, button, div, img, text)
import Html.Attributes exposing (autocomplete, class, disabled, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import List.Extra as LE
import Markdown exposing (Markdown)
import Page exposing (Session(..))
import Profile
import RemoteData exposing (RemoteData)
import Route
import Session.Guest as Guest
import Session.LoggedIn as LoggedIn
import Session.Shared as Shared exposing (Shared)
import Shop exposing (Product, ProductPreview)
import Transfer
import UpdateResult as UR
import View.Feedback as Feedback



-- INIT


init : Session -> Int -> UpdateResult
init session saleId =
    case session of
        Page.LoggedIn ({ shared, accountName } as loggedIn) ->
            let
                model =
                    AsLoggedIn
                        { status = RemoteData.Loading
                        , viewing = ViewingCard
                        , form2 =
                            Form.init
                                { units = "0"
                                , memo = Form.RichText.initModel "memo-input" Nothing
                                }
                        , hasChangedDefaultMemo = False
                        , balances = []
                        , isBuyButtonDisabled = False
                        }
            in
            model
                |> UR.init
                |> UR.addExt
                    (LoggedIn.query loggedIn
                        (Shop.productQuery saleId)
                        (CompletedSaleLoad >> AsLoggedInMsg)
                    )
                |> UR.addCmd (Api.getBalances shared accountName (CompletedLoadBalances >> AsLoggedInMsg))
                |> UR.map identity identity (Page.LoggedInExternal >> UR.addExt)

        Page.Guest guest ->
            AsGuest { saleId = saleId, productPreview = RemoteData.Loading }
                |> UR.init
                |> UR.addCmd
                    (Api.Graphql.query guest.shared
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
    , form2 : Form.Model FormInput
    , hasChangedDefaultMemo : Bool
    , balances : List Balance
    , isBuyButtonDisabled : Bool
    }


defaultMemoKey : String
defaultMemoKey =
    "shop.transfer.default_memo"


type ViewState
    = ViewingCard
    | EditingTransfer



-- Msg


type Msg
    = AsGuestMsg GuestMsg
    | AsLoggedInMsg LoggedInMsg


type GuestMsg
    = CompletedSalePreviewLoad (RemoteData (Graphql.Http.Error ProductPreview) ProductPreview)


type LoggedInMsg
    = ClosedAuthModal
    | CompletedSaleLoad (RemoteData (Graphql.Http.Error (Maybe Product)) (Maybe Product))
    | CompletedLoadBalances (Result Http.Error (List Balance))
    | ClickedBuy
    | ClickedEdit Product
    | ClickedTransfer Product FormOutput
    | GotFormMsg (Form.Msg FormInput)
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

        ClickedTransfer sale formOutput ->
            let
                authorization =
                    { actor = loggedIn.accountName
                    , permissionName = Eos.samplePermission
                    }

                value =
                    { amount = sale.price * toFloat formOutput.units
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
                    { responseAddress = ClickedTransfer sale formOutput
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
                                    , memo = formOutput.memo
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
                                    , units = formOutput.units
                                    }
                                        |> Shop.encodeTransferSale
                              }
                            ]
                    }
                |> LoggedIn.withPrivateKey loggedIn
                    []
                    model
                    { successMsg = msg, errorMsg = ClosedAuthModal }

        GotFormMsg subMsg ->
            Form.update loggedIn.shared subMsg model.form2
                |> UR.fromChild (\newForm -> { model | form2 = newForm })
                    GotFormMsg
                    LoggedIn.addFeedback
                    model

        CompletedLoadBalances res ->
            case res of
                Ok bals ->
                    { model | balances = bals }
                        |> UR.init

                Err _ ->
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
            , description : Markdown
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
    [ div [ class "font-semibold text-3xl w-full" ] [ text sale.title ]
    , Markdown.view [ class "text-gray w-full md:text-sm" ] sale.description
    , div [ class "w-full flex items-center text-sm mt-4" ]
        [ div [ class "mr-4" ] [ Avatar.view sale.creator.avatar "h-10 w-10" ]
        , text_ "shop.sold_by"
        , a
            [ class "font-bold ml-1"
            , Route.href (Route.Profile sale.creator.account)
            ]
            [ Profile.viewProfileName shared.translators currentName sale.creator ]
        ]
    , div [ class "flex flex-wrap w-full justify-between items-center" ]
        [ div []
            [ div [ class "flex items-center" ]
                [ div [ class "text-2xl text-green font-semibold" ]
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
                                    [ ( "balance", Eos.assetToString shared.translators asset ) ]
                                )
                            ]
                        ]
            ]
        , div [ class "mt-2 w-full sm:w-40 mb-10" ]
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
                    , onClick
                        (Form.parse (createForm loggedIn.shared.translators sale)
                            model.form2
                            { onError = GotFormMsg
                            , onSuccess = ClickedTransfer sale
                            }
                        )
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


type alias FormInput =
    { units : String
    , memo : Form.RichText.Model
    }


type alias FormOutput =
    { units : Int
    , memo : Markdown
    }


createForm : Shared.Translators -> Product -> Form.Form msg FormInput FormOutput
createForm ({ t } as translators) product =
    Form.succeed FormOutput
        |> Form.with
            (Form.Text.init
                { label = t "shop.transfer.units_label"
                , id = "units-input"
                }
                |> Form.Text.asNumeric
                |> Form.Text.withType Form.Text.Number
                |> Form.Text.withExtraAttrs
                    [ autocomplete False
                    , Html.Attributes.min "0"
                    ]
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.int
                            >> Form.Validate.intGreaterThanOrEqualTo 1
                            >> Form.Validate.withCustomError (\translators_ -> translators_.t "shop.transfer.errors.unitTooLow")
                            >> (if product.trackStock then
                                    Form.Validate.intLowerThanOrEqualTo product.units
                                        >> Form.Validate.withCustomError (\translators_ -> translators_.t "shop.transfer.errors.unitTooHigh")

                                else
                                    identity
                               )
                            >> Form.Validate.validate translators
                    , value = .units
                    , update = \units input -> { input | units = units }
                    , externalError = always Nothing
                    }
            )
        |> Form.withNoOutput
            (Form.Text.init
                { label = t "shop.transfer.quantity_label"
                , id = "value-input"
                }
                |> Form.Text.withDisabled True
                |> Form.Text.withCurrency product.symbol
                |> Form.textField
                    { parser = Ok
                    , value =
                        \{ units } ->
                            case String.toInt units of
                                Nothing ->
                                    "0"

                                Just unitsValue ->
                                    String.fromFloat (toFloat unitsValue * product.price)
                    , update = \_ input -> input
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.RichText.init { label = t "shop.transfer.memo_label" }
                |> Form.RichText.withPlaceholder (t defaultMemoKey)
                |> Form.RichText.withContainerAttrs [ class "mb-10" ]
                |> Form.richText
                    { parser = Ok
                    , value = .memo
                    , update = \memo input -> { input | memo = memo }
                    , externalError = always Nothing
                    }
            )


viewTransferForm : LoggedIn.Model -> Product -> LoggedInModel -> Html LoggedInMsg
viewTransferForm { shared } sale model =
    Form.viewWithoutSubmit []
        shared.translators
        (\_ -> [])
        (createForm shared.translators sale)
        model.form2
        { toMsg = GotFormMsg }



-- UTILS


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

        ClickedTransfer _ _ ->
            [ "ClickedTransfer" ]

        GotFormMsg subMsg ->
            "GotFormMsg" :: Form.msgToString subMsg

        CompletedLoadBalances _ ->
            [ "CompletedLoadBalances" ]


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
