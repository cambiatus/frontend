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
import Cambiatus.Enum.Permission as Permission
import Community exposing (Balance)
import Eos
import Eos.Account as Eos
import Eos.EosError as EosError
import Form
import Form.RichText
import Form.Text
import Form.Validate
import Graphql.Http
import Html exposing (Html, a, button, div, h2, h3, img, span, text)
import Html.Attributes exposing (alt, autocomplete, class, classList, disabled, href, src, type_)
import Html.Attributes.Aria exposing (ariaHidden, ariaLabel)
import Html.Events exposing (onClick)
import Http
import Icons
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode
import List.Extra as LE
import Markdown exposing (Markdown)
import Page exposing (Session(..))
import Profile
import Profile.Contact
import RemoteData exposing (RemoteData)
import Route
import Session.Guest as Guest
import Session.LoggedIn as LoggedIn
import Session.Shared as Shared
import Shop exposing (Product, ProductPreview)
import Transfer
import UpdateResult as UR
import View.Components
import View.Feedback as Feedback



-- INIT


init : Session -> Shop.Id -> UpdateResult
init session saleId =
    case session of
        Page.LoggedIn ({ shared, accountName } as loggedIn) ->
            let
                model =
                    AsLoggedIn
                        { status = RemoteData.Loading
                        , form =
                            Form.init
                                { units = "1"
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
            AsGuest
                { saleId = saleId
                , productPreview = RemoteData.Loading
                , form =
                    Form.init
                        { units = "1"
                        , memo = Form.RichText.initModel "memo-input" Nothing
                        }
                }
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
    { saleId : Shop.Id
    , productPreview : RemoteData (Graphql.Http.Error ProductPreview) ProductPreview
    , form : Form.Model FormInput
    }


type alias LoggedInModel =
    { status : RemoteData (Graphql.Http.Error (Maybe Product)) Product
    , form : Form.Model FormInput
    , hasChangedDefaultMemo : Bool
    , balances : List Balance
    , isBuyButtonDisabled : Bool
    }



-- Msg


type Msg
    = AsGuestMsg GuestMsg
    | AsLoggedInMsg LoggedInMsg


type GuestMsg
    = CompletedSalePreviewLoad (RemoteData (Graphql.Http.Error ProductPreview) ProductPreview)
    | GotFormInteractionMsgAsGuest FormInteractionMsg
    | GotFormMsgAsGuest (Form.Msg FormInput)


type LoggedInMsg
    = ClosedAuthModal
    | CompletedSaleLoad (RemoteData (Graphql.Http.Error (Maybe Product)) (Maybe Product))
    | CompletedLoadBalances (Result Http.Error (List Balance))
    | ClickedTransfer Product FormOutput
    | GotFormMsg (Form.Msg FormInput)
    | GotTransferResult (Result (Maybe Value) String)
    | GotFormInteractionMsg FormInteractionMsg


type FormInteractionMsg
    = ClickedDecrementUnits
    | ClickedIncrementUnits


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
updateAsGuest msg model guest =
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

        GotFormInteractionMsgAsGuest subMsg ->
            { model | form = updateFormInteraction subMsg { maxUnits = Nothing } model.form }
                |> UR.init

        GotFormMsgAsGuest subMsg ->
            Form.update guest.shared subMsg model.form
                |> UR.fromChild (\newForm -> { model | form = newForm })
                    GotFormMsgAsGuest
                    (Guest.SetFeedback >> UR.addExt)
                    model


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
                    [ Permission.Order ]
                    model
                    { successMsg = msg, errorMsg = ClosedAuthModal }

        GotFormMsg subMsg ->
            Form.update loggedIn.shared subMsg model.form
                |> UR.fromChild (\newForm -> { model | form = newForm })
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

        GotFormInteractionMsg subMsg ->
            { model
                | form =
                    updateFormInteraction subMsg
                        { maxUnits =
                            case model.status of
                                RemoteData.Success product ->
                                    Shop.getAvailableUnits product

                                _ ->
                                    Nothing
                        }
                        model.form
            }
                |> UR.init


updateFormInteraction : FormInteractionMsg -> { maxUnits : Maybe Int } -> Form.Model FormInput -> Form.Model FormInput
updateFormInteraction msg { maxUnits } model =
    Form.updateValues
        (\values ->
            { values
                | units =
                    case String.toInt values.units of
                        Nothing ->
                            values.units

                        Just units ->
                            let
                                newUnits =
                                    case msg of
                                        ClickedIncrementUnits ->
                                            case maxUnits of
                                                Nothing ->
                                                    units + 1

                                                Just maximum ->
                                                    min maximum (units + 1)

                                        ClickedDecrementUnits ->
                                            max 1 (units - 1)
                            in
                            String.fromInt newUnits
            }
        )
        model



-- VIEW


view : Session -> Model -> { title : String, content : Html Msg }
view session model =
    let
        ({ t } as translators) =
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

        viewContent :
            { product
                | image : Maybe String
                , title : String
                , description : Markdown
                , creator : Profile.Minimal
            }
            -> Html msg
            -> Html msg
        viewContent sale formView =
            let
                isGuest =
                    case session of
                        Page.Guest _ ->
                            True

                        Page.LoggedIn _ ->
                            False

                isCreator =
                    case session of
                        Page.Guest _ ->
                            False

                        Page.LoggedIn loggedIn ->
                            loggedIn.accountName == sale.creator.account
            in
            div [ class "flex flex-grow items-center relative bg-white md:bg-gray-100" ]
                [ div [ class "absolute bg-white top-0 bottom-0 left-0 right-1/2 hidden md:block" ] []
                , div [ class "container mx-auto px-4 my-4 md:my-10 md:isolate grid md:grid-cols-2" ]
                    [ div [ class "mb-6 md:mb-0 md:w-2/3 md:mx-auto" ]
                        [ viewProductImg translators sale.image
                        , h2 [ class "font-bold text-lg text-black mt-4", ariaHidden True ] [ text sale.title ]
                        , Markdown.view [ class "mt-2 mb-6 text-gray-333" ] sale.description
                        , if isCreator then
                            text ""

                          else
                            viewContactTheSeller translators { isGuest = isGuest } sale.creator
                        ]
                    , div [ class "bg-gray-100 px-4 pt-6 pb-4 w-full rounded-lg md:p-0 md:w-2/3 md:mx-auto md:place-self-start" ]
                        [ formView
                        ]
                    ]
                ]

        content =
            case ( model, session ) of
                ( AsGuest model_, Page.Guest guest ) ->
                    case model_.productPreview of
                        RemoteData.Success sale ->
                            viewContent sale
                                (Form.viewWithoutSubmit []
                                    guest.shared.translators
                                    (\_ ->
                                        [ a
                                            [ Route.ViewSale sale.id
                                                |> Just
                                                |> Route.Join
                                                |> Route.href
                                            , class "button button-primary w-full"
                                            ]
                                            [ text <| t "shop.buy" ]
                                        ]
                                    )
                                    (createForm guest.shared.translators
                                        { stockTracking = Shop.NoTracking
                                        , price = sale.price
                                        , symbol = sale.symbol
                                        }
                                        Nothing
                                        { isDisabled = False }
                                        GotFormInteractionMsgAsGuest
                                    )
                                    model_.form
                                    { toMsg = GotFormMsgAsGuest
                                    }
                                )
                                |> Html.map AsGuestMsg

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

                                        isOwner =
                                            sale.creator.account == loggedIn.accountName

                                        isOutOfStock =
                                            Shop.isOutOfStock sale

                                        isDisabled =
                                            isOwner || isOutOfStock
                                    in
                                    div [ class "flex-grow flex flex-col" ]
                                        [ Page.viewHeader loggedIn sale.title
                                        , viewContent sale
                                            (Form.view []
                                                loggedIn.shared.translators
                                                (\submitButton ->
                                                    [ if isOwner then
                                                        View.Components.disablableLink
                                                            { isDisabled = not loggedIn.hasAcceptedCodeOfConduct }
                                                            [ class "button button-primary w-full"
                                                            , classList [ ( "button-disabled", not loggedIn.hasAcceptedCodeOfConduct ) ]
                                                            , Route.href (Route.EditSale sale.id)
                                                            ]
                                                            [ text <| t "shop.edit" ]

                                                      else
                                                        submitButton
                                                            [ class "button button-primary w-full"
                                                            , disabled (isOutOfStock || not loggedIn.hasAcceptedCodeOfConduct)
                                                            ]
                                                            [ if isOutOfStock then
                                                                text <| t "shop.sold_out"

                                                              else
                                                                text <| t "shop.buy"
                                                            ]
                                                    ]
                                                )
                                                (createForm loggedIn.shared.translators
                                                    sale
                                                    maybeBalance
                                                    { isDisabled = isDisabled }
                                                    GotFormInteractionMsg
                                                )
                                                (Form.withDisabled isDisabled model_.form)
                                                { toMsg = GotFormMsg
                                                , onSubmit = ClickedTransfer sale
                                                }
                                            )
                                        ]
                                        |> Html.map AsLoggedInMsg

                _ ->
                    Page.fullPageError (t "shop.title") Http.Timeout
    in
    { title = title
    , content = content
    }


viewProductImg : Shared.Translators -> Maybe String -> Html msg
viewProductImg { t } maybeImgUrl =
    let
        defaultView =
            div [ class "flex flex-col items-center gap-2" ]
                [ Icons.image ""
                , span [ class "font-bold uppercase text-black text-sm" ]
                    [ text <| t "shop.no_image" ]
                ]

        image =
            case maybeImgUrl of
                Nothing ->
                    defaultView

                Just "" ->
                    defaultView

                Just imgUrl ->
                    img
                        [ src imgUrl
                        , class "object-cover object-center rounded max-h-68 max-w-full"
                        , alt ""
                        ]
                        []
    in
    div [ class "bg-gray-100 w-full rounded grid place-items-center h-68" ]
        [ image ]


viewContactTheSeller : Shared.Translators -> { isGuest : Bool } -> Profile.Minimal -> Html msg
viewContactTheSeller ({ t, tr } as translators) { isGuest } profile =
    let
        makeHref originalRoute =
            if isGuest then
                originalRoute
                    |> Just
                    |> Route.Join
                    |> Route.href

            else
                originalRoute
                    |> Route.href

        name =
            profile.name
                |> Maybe.withDefault (Eos.nameToString profile.account)
    in
    div []
        [ div [ class "flex items-center" ]
            [ a
                [ makeHref (Route.Profile profile.account)
                , ariaHidden True
                ]
                [ Avatar.view profile.avatar "w-14 h-14" ]
            , div [ class "ml-4 flex flex-col text-gray-333" ]
                [ h3 [ class "font-bold lowercase" ] [ text <| t "shop.transfer.contact_seller" ]
                , a
                    [ makeHref (Route.Profile profile.account)
                    , class "hover:underline"
                    , ariaLabel <| tr "shop.transfer.visit_seller" [ ( "seller", name ) ]
                    ]
                    [ text name ]
                ]
            ]
        , div [ class "ml-18 mt-2 flex flex-wrap gap-4" ]
            (profile.contacts
                |> List.map (Profile.Contact.circularLinkWithGrayBg translators "")
                |> (\contacts ->
                        case profile.email of
                            Nothing ->
                                contacts

                            Just email ->
                                contacts
                                    ++ [ a
                                            [ href ("mailto:" ++ email)
                                            , class "w-10 h-10 flex-shrink-0 bg-gray-100 rounded-full flex items-center justify-center hover:opacity-70"
                                            , ariaLabel <| t "contact_form.reach_out.email"
                                            ]
                                            [ Icons.mail "" ]
                                       ]
                   )
            )
        ]


type alias FormInput =
    { units : String
    , memo : Form.RichText.Model
    }


type alias FormOutput =
    { units : Int
    , memo : Markdown
    }


createForm :
    Shared.Translators
    ->
        { product
            | stockTracking : Shop.StockTracking
            , price : Float
            , symbol : Eos.Symbol
        }
    -> Maybe Balance
    -> { isDisabled : Bool }
    -> (FormInteractionMsg -> msg)
    -> Form.Form msg FormInput FormOutput
createForm ({ t, tr } as translators) product maybeBalance { isDisabled } toFormInteractionMsg =
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
                    , class "text-center"
                    ]
                |> Form.Text.withContainerAttrs [ class "mb-6" ]
                |> Form.Text.withElements
                    [ button
                        [ class "absolute top-1 bottom-1 left-1 px-4 rounded focus-ring text-orange-300"
                        , classList
                            [ ( "bg-white hover:text-orange-300/70", not isDisabled )
                            , ( "bg-gray-500 cursor-default", isDisabled )
                            ]
                        , type_ "button"
                        , disabled isDisabled
                        , onClick (toFormInteractionMsg ClickedDecrementUnits)
                        , ariaLabel <| t "shop.subtract_unit"
                        ]
                        [ Icons.minus "fill-current" ]
                    , button
                        [ class "absolute top-1 bottom-1 right-1 px-4 rounded focus-ring text-orange-300"
                        , classList
                            [ ( "bg-white hover:text-orange-300/70", not isDisabled )
                            , ( "bg-gray-500 cursor-default", isDisabled )
                            ]
                        , type_ "button"
                        , disabled isDisabled
                        , onClick (toFormInteractionMsg ClickedIncrementUnits)
                        , ariaLabel <| t "shop.add_unit"
                        ]
                        [ Icons.plus "fill-current" ]
                    ]
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.int
                            >> Form.Validate.intGreaterThanOrEqualTo 1
                            >> Form.Validate.withCustomError (\translators_ -> translators_.t "shop.transfer.errors.unitTooLow")
                            >> (case product.stockTracking of
                                    Shop.NoTracking ->
                                        identity

                                    Shop.UnitTracking { availableUnits } ->
                                        Form.Validate.intLowerThanOrEqualTo availableUnits
                                            >> Form.Validate.withCustomError (\translators_ -> translators_.t "shop.transfer.errors.unitTooHigh")
                               )
                            >> Form.Validate.validate translators
                    , value = .units
                    , update = \units input -> { input | units = units }
                    , externalError = always Nothing
                    }
            )
        |> Form.withNoOutput
            (Form.introspect
                (\values ->
                    div
                        [ class "bg-green pt-4 mb-6 rounded-sm flex flex-col text-white font-bold text-center"
                        ]
                        [ span
                            [ class "text-xl px-4"
                            , ariaHidden True
                            ]
                            [ case String.toInt values.units of
                                Nothing ->
                                    text "0"

                                Just unitsValue ->
                                    (toFloat unitsValue * product.price)
                                        |> String.fromFloat
                                        |> text
                            ]
                        , case String.toInt values.units of
                            Nothing ->
                                text ""

                            Just unitsValue ->
                                span [ class "sr-only" ]
                                    [ text <|
                                        tr "shop.total_price"
                                            [ ( "amount"
                                              , toFloat unitsValue
                                                    * product.price
                                                    |> String.fromFloat
                                              )
                                            , ( "symbol", Eos.symbolToSymbolCodeString product.symbol )
                                            ]
                                    ]
                        , span [ class "px-4 mb-4", ariaHidden True ]
                            [ text <|
                                tr "shop.transfer.price"
                                    [ ( "symbol", Eos.symbolToSymbolCodeString product.symbol ) ]
                            ]
                        , case maybeBalance of
                            Nothing ->
                                text ""

                            Just balance ->
                                span [ class "text-sm py-3 px-4 bg-black bg-opacity-20 rounded-b-sm uppercase" ]
                                    [ text <|
                                        tr "shop.transfer.balance"
                                            [ ( "asset"
                                              , Eos.assetToString translators
                                                    balance.asset
                                              )
                                            ]
                                    ]
                        ]
                        |> Form.arbitrary
                )
            )
        |> Form.with
            (Form.RichText.init { label = t "shop.transfer.memo_label" }
                |> Form.RichText.withContainerAttrs [ class "mb-6" ]
                |> Form.richText
                    { parser = Ok
                    , value = .memo
                    , update = \memo input -> { input | memo = memo }
                    , externalError = always Nothing
                    }
            )



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

        GotFormInteractionMsgAsGuest subMsg ->
            "GotFormInteractionMsgAsGuest" :: formInteractionMsgToString subMsg

        GotFormMsgAsGuest subMsg ->
            "GotFormMsgAsGuest" :: Form.msgToString subMsg


loggedInMsgToString : LoggedInMsg -> List String
loggedInMsgToString msg =
    case msg of
        ClosedAuthModal ->
            [ "ClosedAuthModal" ]

        CompletedSaleLoad _ ->
            [ "CompletedSaleLoad" ]

        GotTransferResult _ ->
            [ "GotTransferResult" ]

        ClickedTransfer _ _ ->
            [ "ClickedTransfer" ]

        GotFormMsg subMsg ->
            "GotFormMsg" :: Form.msgToString subMsg

        CompletedLoadBalances _ ->
            [ "CompletedLoadBalances" ]

        GotFormInteractionMsg subMsg ->
            "GotFormInteractionMsg" :: formInteractionMsgToString subMsg


formInteractionMsgToString : FormInteractionMsg -> List String
formInteractionMsgToString msg =
    case msg of
        ClickedDecrementUnits ->
            [ "ClickedDecrementUnits" ]

        ClickedIncrementUnits ->
            [ "ClickedIncrementUnits" ]


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
