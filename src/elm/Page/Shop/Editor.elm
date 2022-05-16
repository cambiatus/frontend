module Page.Shop.Editor exposing
    ( Model
    , Msg(..)
    , getCurrentStep
    , initCreate
    , initUpdate
    , maybeGoBackOneStep
    , msgToString
    , update
    , view
    )

import Cambiatus.Enum.Permission as Permission
import Eos
import Form
import Form.File
import Form.RichText
import Form.Text
import Form.Toggle
import Form.Validate
import Graphql.Http
import Html exposing (Html, a, button, div, h2, hr, p, span, text)
import Html.Attributes exposing (class, classList, disabled, maxlength, type_)
import Html.Attributes.Aria exposing (ariaLabel)
import Html.Events exposing (onClick)
import Icons
import List.Extra
import Markdown exposing (Markdown)
import Page
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import Shop exposing (Product)
import Translation
import UpdateResult as UR
import View.Feedback as Feedback



-- INIT


initCreate : LoggedIn.Model -> ( Model, Cmd Msg )
initCreate _ =
    ( EditingCreate initFormData, Cmd.none )


initUpdate : Shop.Id -> Route.EditSaleStep -> LoggedIn.Model -> UpdateResult
initUpdate productId step loggedIn =
    LoadingSaleUpdate step
        |> UR.init
        |> UR.addExt
            (LoggedIn.query loggedIn
                (Shop.productQuery productId)
                CompletedSaleLoad
            )



-- MODEL


type alias Model =
    Status


type
    Status
    -- Create
    = EditingCreate FormData
    | Creating FormData
      -- Update
    | LoadingSaleUpdate Route.EditSaleStep
    | EditingUpdate Product FormData
    | Saving Product FormData
      -- Errors
    | LoadSaleFailed (Graphql.Http.Error (Maybe Product))


type alias FormData =
    { mainInformation : Form.Model MainInformationFormInput
    , images : Form.Model ImagesFormInput
    , priceAndInventory : Form.Model PriceAndInventoryFormInput
    , currentStep : Step
    }


type Step
    = MainInformation
    | Images MainInformationFormOutput
    | PriceAndInventory MainInformationFormOutput ImagesFormOutput


type alias MainInformationFormInput =
    { title : String
    , description : Form.RichText.Model
    }


type alias MainInformationFormOutput =
    { title : String, description : Markdown }


mainInformationForm : Translation.Translators -> Form.Form msg MainInformationFormInput MainInformationFormOutput
mainInformationForm ({ t } as translators) =
    Form.succeed MainInformationFormOutput
        |> Form.with
            (Form.Text.init
                { label = t "shop.steps.main_information.name_label"
                , id = "product-title-input"
                }
                |> Form.Text.withExtraAttrs [ maxlength 255 ]
                |> Form.Text.withPlaceholder (t "shop.steps.main_information.name_placeholder")
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.stringShorterThan 255
                            >> Form.Validate.stringLongerThan 3
                            >> Form.Validate.validate translators
                    , value = .title
                    , update = \newTitle values -> { values | title = newTitle }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.RichText.init
                { label = t "shop.steps.main_information.description_label"
                }
                |> Form.RichText.withPlaceholder (t "shop.steps.main_information.description_placeholder")
                |> Form.richText
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.markdownLongerThan 10
                            >> Form.Validate.validate translators
                    , value = .description
                    , update = \newDescription values -> { values | description = newDescription }
                    , externalError = always Nothing
                    }
            )


type alias ImagesFormInput =
    List Form.File.Model


type alias ImagesFormOutput =
    List String


imagesForm : Translation.Translators -> Form.Form Msg ImagesFormInput ImagesFormOutput
imagesForm translators =
    Form.succeed (List.filterMap identity)
        |> Form.withNoOutput
            (Form.arbitrary
                (p [ class "mb-4" ] [ text <| translators.t "shop.steps.images.guidance" ])
            )
        |> Form.with
            (Form.introspect
                (\images ->
                    List.indexedMap
                        (\index image ->
                            Form.succeed (\imageOutput _ -> imageOutput)
                                |> Form.withGroup [ class "relative" ]
                                    (Form.File.init
                                        { label = ""
                                        , id = "product-image-input-" ++ String.fromInt index
                                        }
                                        |> Form.File.withAttrs
                                            [ class "w-24 h-24 rounded bg-gray-100 flex items-center justify-center"
                                            ]
                                        |> Form.File.withVariant Form.File.SimplePlus
                                        |> Form.File.withContainerAttrs [ classList [ ( "animate-bounce-in", Form.File.isEmpty image ) ] ]
                                        |> Form.file
                                            { translators = translators
                                            , value = \_ -> image
                                            , update = \newImage _ -> newImage
                                            , externalError = always Nothing
                                            }
                                        |> Form.mapValues
                                            { value = \_ -> image
                                            , update = \newImage -> List.Extra.setAt index newImage
                                            }
                                        |> Form.optional
                                    )
                                    (Form.arbitraryWith ()
                                        (div
                                            [ class "absolute top-0 right-0"
                                            , classList [ ( "hidden", Form.File.isEmpty image ) ]
                                            ]
                                            [ button
                                                [ class "bg-white rounded-full -translate-y-1/2 ml-1/2"
                                                , onClick (\values -> values |> List.Extra.removeAt index)
                                                , type_ "button"
                                                ]
                                                [ Icons.circularClose "w-6 h-6"
                                                ]
                                            ]
                                        )
                                    )
                        )
                        images
                        |> Form.list [ class "flex flex-wrap gap-6" ]
                )
            )


type alias PriceAndInventoryFormInput =
    { price : String
    , unitsInStock : String
    , trackUnits : Bool
    }


type alias PriceAndInventoryFormOutput =
    { price : Eos.Asset
    , unitTracking : Shop.StockTracking
    }


priceAndInventoryForm : Translation.Translators -> { isDisabled : Bool } -> Eos.Symbol -> Form.Form Msg PriceAndInventoryFormInput PriceAndInventoryFormOutput
priceAndInventoryForm translators isDisabled symbol =
    Form.succeed PriceAndInventoryFormOutput
        |> Form.withGroup [ class "grid grid-cols-2 gap-8" ]
            (Form.Text.init
                { label = translators.t "shop.steps.price_and_inventory.price_label"
                , id = "product-price-input"
                }
                |> Form.Text.withCurrency symbol
                |> Form.Text.withExtraAttrs [ Html.Attributes.min "0" ]
                |> Form.Text.withContainerAttrs [ class "bg-gray-100 rounded-sm p-4 mb-0 self-start" ]
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.maskedFloat translators
                            >> Form.Validate.floatGreaterThan 0
                            >> Form.Validate.map (\price -> { amount = price, symbol = symbol })
                            >> Form.Validate.validate translators
                    , value = .price
                    , update = \newPrice values -> { values | price = newPrice }
                    , externalError = always Nothing
                    }
            )
            (stockTrackingForm translators isDisabled)


stockTrackingForm : Translation.Translators -> { isDisabled : Bool } -> Form.Form Msg { input | unitsInStock : String, trackUnits : Bool } Shop.StockTracking
stockTrackingForm translators { isDisabled } =
    Form.succeed
        (\trackStock availableUnits ->
            if trackStock then
                Shop.UnitTracking { availableUnits = availableUnits }

            else
                Shop.NoTracking
        )
        |> Form.withGroup [ class "bg-gray-100 rounded-sm p-4" ]
            (Form.Toggle.init
                { label =
                    div [ class "text-gray-333 text-base mb-4" ]
                        [ span [ class "font-bold" ] [ text <| translators.t "shop.steps.price_and_inventory.inventory_management" ]
                        , p [ class "mt-2" ] [ text <| translators.t "shop.steps.price_and_inventory.inventory_management_description" ]
                        ]
                , id = "product-track-units-toggle"
                }
                |> Form.Toggle.withContainerAttrs [ class "flex flex-col" ]
                |> Form.Toggle.withToggleContainerAttrs [ class "ml-0 pl-0" ]
                |> Form.Toggle.withToggleSide (Form.Toggle.Right { invert = True })
                |> Form.toggle
                    { parser = Ok
                    , value = .trackUnits
                    , update = \newTrackUnits values -> { values | trackUnits = newTrackUnits }
                    , externalError = always Nothing
                    }
            )
            (Form.introspect
                (\{ trackUnits } ->
                    if trackUnits then
                        Form.Text.init
                            { label = translators.t "shop.steps.price_and_inventory.quantity_label"
                            , id = "product-quantity-input"
                            }
                            |> Form.Text.withPlaceholder "0"
                            |> Form.Text.asNumeric
                            |> Form.Text.withType Form.Text.Number
                            |> Form.Text.withContainerAttrs [ class "mb-0 mt-10" ]
                            |> Form.Text.withExtraAttrs
                                [ Html.Attributes.min "0"
                                , class "text-center"
                                ]
                            |> Form.Text.withElements
                                [ button
                                    [ class "absolute top-1 bottom-1 left-1 px-4 rounded focus-ring text-orange-300 hover:text-orange-300/70"
                                    , classList
                                        [ ( "bg-white", not isDisabled )
                                        , ( "bg-gray-500", isDisabled )
                                        ]
                                    , type_ "button"
                                    , ariaLabel <| translators.t "shop.subtract_unit"
                                    , onClick ClickedDecrementStockUnits
                                    ]
                                    [ Icons.minus "fill-current"
                                    ]
                                , button
                                    [ class "absolute top-1 bottom-1 right-1 px-4 rounded focus-ring text-orange-300 hover:text-orange-300/70"
                                    , classList
                                        [ ( "bg-white", not isDisabled )
                                        , ( "bg-gray-500", isDisabled )
                                        ]
                                    , type_ "button"
                                    , ariaLabel <| translators.t "shop.add_unit"
                                    , onClick ClickedIncrementStockUnits
                                    ]
                                    [ Icons.plus "fill-current" ]
                                ]
                            |> Form.textField
                                { parser =
                                    Form.Validate.succeed
                                        >> Form.Validate.int
                                        >> Form.Validate.intGreaterThanOrEqualTo 0
                                        >> Form.Validate.validate translators
                                , value = .unitsInStock
                                , update = \newUnitsInStock values -> { values | unitsInStock = newUnitsInStock }
                                , externalError = always Nothing
                                }

                    else
                        Form.succeed 0
                )
            )


initFormData : FormData
initFormData =
    { mainInformation =
        Form.init
            { title = ""
            , description = Form.RichText.initModel "product-description-editor" Nothing
            }
    , images = Form.init [ Form.File.initModel Nothing ]
    , priceAndInventory =
        Form.init
            { price = "0"
            , unitsInStock = "0"
            , trackUnits = False
            }
    , currentStep = MainInformation
    }


initEditingFormData : Product -> Route.EditSaleStep -> FormData
initEditingFormData product step =
    { mainInformation =
        Form.init
            { title = product.title
            , description = Form.RichText.initModel "product-description-editor" (Just product.description)
            }
    , images =
        product.images
            |> List.map (Just >> Form.File.initModel)
            |> (\images -> images ++ [ Form.File.initModel Nothing ])
            |> Form.init
    , priceAndInventory =
        Form.init
            { price = String.fromFloat product.price
            , unitsInStock =
                case product.stockTracking of
                    Shop.NoTracking ->
                        String.fromInt 0

                    Shop.UnitTracking { availableUnits } ->
                        String.fromInt availableUnits
            , trackUnits = Shop.hasUnitTracking product
            }
    , currentStep =
        case step of
            Route.SaleMainInformation ->
                MainInformation

            Route.SaleImages ->
                Images { title = product.title, description = product.description }

            Route.SalePriceAndInventory ->
                PriceAndInventory
                    { title = product.title, description = product.description }
                    product.images
    }



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        shared =
            loggedIn.shared

        t =
            shared.translators.t

        isEdit =
            case model of
                EditingUpdate _ _ ->
                    True

                Saving _ _ ->
                    True

                _ ->
                    False

        title =
            if isEdit then
                t "shop.edit_offer"

            else
                t "shop.create_offer"

        content =
            case model of
                LoadingSaleUpdate _ ->
                    Page.fullPageLoading shared

                LoadSaleFailed error ->
                    Page.fullPageGraphQLError (t "shop.title") error

                EditingCreate formData ->
                    viewForm loggedIn { isEdit = False, isDisabled = False } formData

                Creating formData ->
                    viewForm loggedIn { isEdit = False, isDisabled = True } formData

                EditingUpdate _ formData ->
                    viewForm loggedIn { isEdit = True, isDisabled = False } formData

                Saving _ formData ->
                    viewForm loggedIn { isEdit = True, isDisabled = True } formData
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
                Page.fullPageLoading shared

            RemoteData.NotAsked ->
                Page.fullPageLoading shared

            RemoteData.Failure e ->
                Page.fullPageGraphQLError (t "community.error_loading") e
    }


viewForm :
    LoggedIn.Model
    -> { isEdit : Bool, isDisabled : Bool }
    -> FormData
    -> Html Msg
viewForm ({ shared } as loggedIn) { isEdit, isDisabled } formData =
    let
        { t, tr } =
            shared.translators

        ( actionText, pageTitle ) =
            if isEdit then
                ( t "menu.save", t "shop.edit_offer" )

            else
                ( t "menu.create", t "shop.create_offer" )

        viewForm_ :
            Form.Form Msg input output
            ->
                Form.Model
                    input
            -> { submitText : String, isSubmitDisabled : Bool }
            -> (Form.Msg input -> FormMsg)
            -> (output -> Msg)
            -> Html Msg
        viewForm_ formFn formModel { submitText, isSubmitDisabled } toFormMsg onSubmitMsg =
            Form.view [ class "container mx-auto px-4 flex-grow flex flex-col lg:max-w-none lg:mx-0 lg:px-6" ]
                shared.translators
                (\submitButton ->
                    [ div [ class "mt-auto" ]
                        [ div [ class "mt-10 flex gap-x-4" ]
                            [ div
                                [ class "grid grid-cols-2 w-full gap-6 lg:w-1/2 lg:mx-auto"
                                , classList [ ( "grid-cols-3", isEdit ) ]
                                ]
                                [ a
                                    [ class "button button-secondary w-full"
                                    , Route.href (Route.Shop Shop.All)
                                    ]
                                    [ text <| t "menu.cancel" ]
                                , submitButton
                                    [ class "button button-primary w-full"
                                    , disabled (isDisabled || isSubmitDisabled)
                                    ]
                                    [ text submitText ]
                                ]
                            ]
                        ]
                    ]
                )
                formFn
                (Form.withDisabled isDisabled formModel)
                { toMsg = toFormMsg >> GotFormMsg
                , onSubmit = onSubmitMsg
                }

        ( stepNumber, stepName ) =
            case formData.currentStep of
                MainInformation ->
                    ( 1, t "shop.steps.main_information.title" )

                Images _ ->
                    ( 2, t "shop.steps.images.title" )

                PriceAndInventory _ _ ->
                    ( 3, t "shop.steps.price_and_inventory.title" )
    in
    div [ class "flex flex-col flex-grow" ]
        [ Page.viewHeader loggedIn pageTitle
        , div [ class "lg:container lg:mx-auto lg:px-4 lg:mt-6 lg:mb-20" ]
            [ div [ class "bg-white pt-4 pb-8 flex-grow flex flex-col min-h-150 lg:w-2/3 lg:mx-auto lg:rounded lg:shadow-lg lg:animate-fade-in-from-above-lg lg:motion-reduce:animate-none" ]
                [ div [ class "container mx-auto px-4 lg:max-w-none lg:mx-0 lg:px-6" ]
                    [ h2 [ class "font-bold text-black mb-2" ]
                        [ text <|
                            tr "shop.steps.index"
                                [ ( "current", String.fromInt stepNumber )
                                , ( "total", String.fromInt 3 )
                                ]
                        ]
                    , text stepName
                    ]
                , hr [ class "mt-4 mb-6 border-gray-500 lg:mx-4 lg:mb-10" ] []
                , case formData.currentStep of
                    MainInformation ->
                        viewForm_ (mainInformationForm shared.translators)
                            formData.mainInformation
                            { submitText = t "shop.steps.continue"
                            , isSubmitDisabled = False
                            }
                            MainInformationMsg
                            SubmittedMainInformation

                    Images _ ->
                        viewForm_ (imagesForm shared.translators)
                            formData.images
                            { submitText = t "shop.steps.continue"
                            , isSubmitDisabled =
                                Form.getValue identity formData.images
                                    |> List.any Form.File.isLoading
                            }
                            ImagesMsg
                            SubmittedImages

                    PriceAndInventory _ _ ->
                        case loggedIn.selectedCommunity of
                            RemoteData.Success community ->
                                viewForm_ (priceAndInventoryForm shared.translators { isDisabled = isDisabled } community.symbol)
                                    formData.priceAndInventory
                                    { submitText = actionText
                                    , isSubmitDisabled = False
                                    }
                                    PriceAndInventoryMsg
                                    SubmittedPriceAndInventory

                            RemoteData.Failure err ->
                                Page.fullPageGraphQLError pageTitle err

                            _ ->
                                Page.fullPageLoading shared
                ]
            ]
        ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = NoOp
    | CompletedSaleLoad (RemoteData (Graphql.Http.Error (Maybe Product)) (Maybe Product))
    | GotFormMsg FormMsg
    | SubmittedMainInformation MainInformationFormOutput
    | SubmittedImages ImagesFormOutput
    | SubmittedPriceAndInventory PriceAndInventoryFormOutput
    | GotSaveResponse (RemoteData (Graphql.Http.Error (Maybe Shop.Id)) (Maybe Shop.Id))
    | ClickedDecrementStockUnits
    | ClickedIncrementStockUnits


type FormMsg
    = MainInformationMsg (Form.Msg MainInformationFormInput)
    | ImagesMsg (Form.Msg ImagesFormInput)
    | PriceAndInventoryMsg (Form.Msg PriceAndInventoryFormInput)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    let
        t =
            loggedIn.shared.translators.t
    in
    case msg of
        NoOp ->
            UR.init model

        CompletedSaleLoad (RemoteData.Success maybeSale) ->
            case ( model, maybeSale ) of
                ( LoadingSaleUpdate step, Just sale ) ->
                    initEditingFormData sale step
                        |> EditingUpdate sale
                        |> UR.init

                ( _, _ ) ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Completed loading sale, but sale was unavailable or user wasn't editing sale"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Shop.Editor", function = "update" }
                            []

        CompletedSaleLoad (RemoteData.Failure error) ->
            LoadSaleFailed error
                |> UR.init
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when loading sale"
                    { moduleName = "Page.Shop.Editor", function = "update" }
                    []
                    error

        CompletedSaleLoad _ ->
            UR.init model

        GotSaveResponse (RemoteData.Success maybeId) ->
            let
                redirectUrl =
                    case maybeId of
                        Nothing ->
                            Route.Shop Shop.All

                        Just id ->
                            Route.ViewSale id
            in
            UR.init model
                |> UR.addCmd (Route.replaceUrl loggedIn.shared.navKey redirectUrl)
                |> UR.addExt (ShowFeedback Feedback.Success (t "shop.create_offer_success"))

        GotSaveResponse (RemoteData.Failure error) ->
            let
                internalError =
                    loggedIn.shared.translators.t "error.unknown"
            in
            case model of
                Creating form ->
                    EditingCreate form
                        |> UR.init
                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure internalError)
                        |> UR.logGraphqlError msg
                            (Just loggedIn.accountName)
                            "Got an error when creating a shop offer"
                            { moduleName = "Page.Shop.Editor", function = "update" }
                            []
                            error

                Saving sale form ->
                    EditingUpdate sale form
                        |> UR.init
                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure internalError)
                        |> UR.logGraphqlError msg
                            (Just loggedIn.accountName)
                            "Got an error when editing a shop offer"
                            { moduleName = "Page.Shop.Editor", function = "update" }
                            []
                            error

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Saved shop item, but wasn't creating or editing"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Shop.Editor", function = "update" }
                            []

        GotSaveResponse _ ->
            UR.init model

        GotFormMsg subMsg ->
            updateForm loggedIn.shared subMsg model

        SubmittedMainInformation formOutput ->
            let
                maybeCurrentStep =
                    getFormData model
                        |> Maybe.map .currentStep
            in
            case maybeCurrentStep of
                Just MainInformation ->
                    model
                        |> setCurrentStep (Images formOutput)
                        |> UR.init
                        |> UR.addCmd (setCurrentStepInUrl loggedIn.shared model Route.SaleImages)

                _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Submitted main information, but was in another step"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Shop.Editor", function = "update" }
                            []

        SubmittedImages formOutput ->
            let
                maybeCurrentStep =
                    getFormData model
                        |> Maybe.map .currentStep
            in
            case maybeCurrentStep of
                Just (Images mainInformation) ->
                    model
                        |> setCurrentStep (PriceAndInventory mainInformation formOutput)
                        |> UR.init
                        |> UR.addCmd (setCurrentStepInUrl loggedIn.shared model Route.SalePriceAndInventory)

                _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Submitted images, but was in another step"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Shop.Editor", function = "update" }
                            []

        SubmittedPriceAndInventory priceAndInventory ->
            let
                maybeCurrentStep =
                    getFormData model
                        |> Maybe.map .currentStep
            in
            case maybeCurrentStep of
                Just (PriceAndInventory mainInformation images) ->
                    case model of
                        EditingCreate formData ->
                            Creating formData
                                |> UR.init
                                |> UR.addExt
                                    (LoggedIn.mutation
                                        loggedIn
                                        (Shop.createProduct
                                            { symbol = priceAndInventory.price.symbol
                                            , title = mainInformation.title
                                            , description = mainInformation.description
                                            , images = images
                                            , price = priceAndInventory.price.amount
                                            , stockTracking = priceAndInventory.unitTracking
                                            }
                                            Shop.idSelectionSet
                                        )
                                        GotSaveResponse
                                    )
                                |> LoggedIn.withPrivateKey loggedIn
                                    [ Permission.Sell ]
                                    model
                                    { successMsg = msg, errorMsg = NoOp }

                        EditingUpdate sale formData ->
                            Saving sale formData
                                |> UR.init
                                |> UR.addExt
                                    (LoggedIn.mutation
                                        loggedIn
                                        (Shop.updateProduct
                                            { id = sale.id
                                            , symbol = priceAndInventory.price.symbol
                                            , title = mainInformation.title
                                            , description = mainInformation.description
                                            , images = images
                                            , price = priceAndInventory.price.amount
                                            , stockTracking = priceAndInventory.unitTracking
                                            }
                                            Shop.idSelectionSet
                                        )
                                        GotSaveResponse
                                    )
                                |> LoggedIn.withPrivateKey loggedIn
                                    [ Permission.Sell ]
                                    model
                                    { successMsg = msg, errorMsg = NoOp }

                        _ ->
                            model
                                |> UR.init
                                |> UR.logImpossible msg
                                    "Clicked save shop item, but wasn't editing or creating shop offer"
                                    (Just loggedIn.accountName)
                                    { moduleName = "Page.Shop.Editor", function = "update" }
                                    []

                _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Submitted price and inventory, but was in another step"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Shop.Editor", function = "update" }
                            []

        ClickedDecrementStockUnits ->
            updateFormStockUnits (\price -> price - 1) model

        ClickedIncrementStockUnits ->
            updateFormStockUnits (\price -> price + 1) model


updateFormStockUnits : (Int -> Int) -> Model -> UpdateResult
updateFormStockUnits updateFn model =
    let
        maybeFormInfo =
            case model of
                EditingCreate form ->
                    Just ( form, EditingCreate )

                Creating form ->
                    Just ( form, Creating )

                EditingUpdate product form ->
                    Just ( form, EditingUpdate product )

                Saving product form ->
                    Just ( form, Saving product )

                _ ->
                    Nothing
    in
    case maybeFormInfo of
        Nothing ->
            UR.init model

        Just ( formData, updateModel ) ->
            Form.updateValues
                (\values ->
                    case String.toInt values.unitsInStock of
                        Just unitsInStock ->
                            { values
                                | unitsInStock =
                                    updateFn unitsInStock
                                        |> max 0
                                        |> String.fromInt
                            }

                        Nothing ->
                            values
                )
                formData.priceAndInventory
                |> (\priceAndInventory -> { formData | priceAndInventory = priceAndInventory })
                |> updateModel
                |> UR.init


getFormData : Model -> Maybe FormData
getFormData model =
    case model of
        EditingCreate formData ->
            Just formData

        Creating formData ->
            Just formData

        EditingUpdate _ formData ->
            Just formData

        Saving _ formData ->
            Just formData

        _ ->
            Nothing


setCurrentStep : Step -> Model -> Model
setCurrentStep newStep model =
    case model of
        EditingCreate formData ->
            EditingCreate { formData | currentStep = newStep }

        Creating formData ->
            Creating { formData | currentStep = newStep }

        EditingUpdate product formData ->
            EditingUpdate product { formData | currentStep = newStep }

        Saving product formData ->
            Saving product { formData | currentStep = newStep }

        _ ->
            model


maybeGoBackOneStep : Route.EditSaleStep -> Model -> Model
maybeGoBackOneStep step model =
    let
        maybeNewStep =
            getFormData model
                |> Maybe.map
                    (\formData ->
                        case ( formData.currentStep, step ) of
                            ( Images _, Route.SaleMainInformation ) ->
                                MainInformation

                            ( PriceAndInventory mainInformationOutput _, Route.SaleImages ) ->
                                Images mainInformationOutput

                            _ ->
                                formData.currentStep
                    )
    in
    case maybeNewStep of
        Nothing ->
            model

        Just newStep ->
            setCurrentStep newStep model


setCurrentStepInUrl : Shared -> Model -> Route.EditSaleStep -> Cmd msg
setCurrentStepInUrl shared model step =
    let
        maybeRoute =
            case model of
                EditingCreate _ ->
                    Just (Route.NewSale step)

                Creating _ ->
                    Just (Route.NewSale step)

                LoadingSaleUpdate _ ->
                    Nothing

                EditingUpdate product _ ->
                    Just (Route.EditSale product.id step)

                Saving product _ ->
                    Just (Route.EditSale product.id step)

                LoadSaleFailed _ ->
                    Nothing
    in
    case maybeRoute of
        Nothing ->
            Cmd.none

        Just route ->
            Route.replaceUrl shared.navKey route


updateForm : Shared -> FormMsg -> Model -> UpdateResult
updateForm shared formMsg model =
    let
        maybeFormInfo =
            case model of
                EditingCreate form ->
                    Just ( form, EditingCreate )

                Creating form ->
                    Just ( form, Creating )

                EditingUpdate product form ->
                    Just ( form, EditingUpdate product )

                Saving product form ->
                    Just ( form, Saving product )

                _ ->
                    Nothing
    in
    case maybeFormInfo of
        Nothing ->
            UR.init model

        Just ( formData, updateModel ) ->
            case formMsg of
                MainInformationMsg subMsg ->
                    Form.update shared subMsg formData.mainInformation
                        |> UR.fromChild
                            (\newMainInformation -> updateModel { formData | mainInformation = newMainInformation })
                            (GotFormMsg << MainInformationMsg)
                            LoggedIn.addFeedback
                            model

                ImagesMsg subMsg ->
                    let
                        updatedForm =
                            Form.update shared subMsg formData.images

                        oldImages =
                            Form.getValue identity formData.images
                                |> List.filter (not << Form.File.isEmpty)

                        newImages =
                            Form.getValue identity updatedForm.model
                                |> List.filter (not << Form.File.isEmpty)

                        hasAddedNewImage =
                            List.length newImages > List.length oldImages

                        addNewImageField values =
                            values ++ [ Form.File.initModel Nothing ]
                    in
                    Form.update shared subMsg formData.images
                        |> UR.fromChild
                            (\newImagesForm ->
                                updateModel
                                    { formData
                                        | images =
                                            if hasAddedNewImage then
                                                Form.updateValues addNewImageField newImagesForm

                                            else
                                                newImagesForm
                                    }
                            )
                            (GotFormMsg << ImagesMsg)
                            LoggedIn.addFeedback
                            model

                PriceAndInventoryMsg subMsg ->
                    Form.update shared subMsg formData.priceAndInventory
                        |> UR.fromChild (\newPriceAndInventory -> updateModel { formData | priceAndInventory = newPriceAndInventory })
                            (GotFormMsg << PriceAndInventoryMsg)
                            LoggedIn.addFeedback
                            model


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        CompletedSaleLoad r ->
            [ "CompletedSaleLoad", UR.remoteDataToString r ]

        GotSaveResponse r ->
            [ "GotSaveResponse", UR.remoteDataToString r ]

        GotFormMsg subMsg ->
            "GotFormMsg" :: formMsgToString subMsg

        SubmittedMainInformation _ ->
            [ "SubmittedMainInformation" ]

        SubmittedImages _ ->
            [ "SubmittedImages" ]

        SubmittedPriceAndInventory _ ->
            [ "SubmittedPriceAndInventory" ]

        ClickedDecrementStockUnits ->
            [ "ClickedDecrementStockUnits" ]

        ClickedIncrementStockUnits ->
            [ "ClickedIncrementStockUnits" ]


formMsgToString : FormMsg -> List String
formMsgToString msg =
    case msg of
        MainInformationMsg subMsg ->
            "MainInformationMsg" :: Form.msgToString subMsg

        ImagesMsg subMsg ->
            "ImagesMsg" :: Form.msgToString subMsg

        PriceAndInventoryMsg subMsg ->
            "PriceAndInventoryMsg" :: Form.msgToString subMsg


getCurrentStep : Model -> Route.EditSaleStep
getCurrentStep model =
    case model of
        EditingCreate formData ->
            getCurrentStepFromFormData formData

        Creating formData ->
            getCurrentStepFromFormData formData

        LoadingSaleUpdate step ->
            step

        EditingUpdate _ formData ->
            getCurrentStepFromFormData formData

        Saving _ formData ->
            getCurrentStepFromFormData formData

        LoadSaleFailed _ ->
            Route.SaleMainInformation


getCurrentStepFromFormData : FormData -> Route.EditSaleStep
getCurrentStepFromFormData formData =
    case formData.currentStep of
        MainInformation ->
            Route.SaleMainInformation

        Images _ ->
            Route.SaleImages

        PriceAndInventory _ _ ->
            Route.SalePriceAndInventory
