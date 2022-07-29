module Page.Shop.Editor exposing
    ( Model
    , Msg(..)
    , getCurrentStep
    , initCreate
    , initUpdate
    , maybeSetStep
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
import Html.Attributes.Aria exposing (ariaHidden, ariaLabel)
import Html.Events exposing (onClick)
import Icons
import Markdown exposing (Markdown)
import Page
import Profile.EditKycForm exposing (Msg(..))
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import Shop exposing (Product)
import Translation
import UpdateResult as UR
import Utils
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
    , categories : Form.Model CategoriesFormInput
    , priceAndInventory : Form.Model PriceAndInventoryFormInput
    , currentStep : Step
    }


type Step
    = MainInformation
    | Images MainInformationFormOutput
    | Categories MainInformationFormOutput ImagesFormOutput
    | PriceAndInventory MainInformationFormOutput ImagesFormOutput CategoriesFormOutput


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
    Form.File.MultipleModel


type alias ImagesFormOutput =
    List String


imagesForm : Translation.Translators -> Form.Form Msg ImagesFormInput ImagesFormOutput
imagesForm translators =
    Form.succeed identity
        |> Form.withNoOutput
            (Form.arbitrary
                (p [ class "mb-4" ] [ text <| translators.t "shop.steps.images.guidance" ])
            )
        |> Form.with
            (Form.File.init { id = "product-images-input" }
                |> Form.File.withEntryContainerAttributes (\_ -> [ class "w-24 h-24 bg-gray-100 rounded grid place-items-center overflow-hidden" ])
                |> Form.File.withImageClass "max-w-24 max-h-24"
                |> Form.File.withImageCropperAttributes [ class "rounded" ]
                |> Form.File.withEditIconOverlay
                |> Form.fileMultiple
                    { parser = Ok
                    , translators = translators
                    , value = identity
                    , update = \new _ -> new
                    , externalError = always Nothing
                    }
            )


type alias CategoriesFormInput =
    {}


type alias CategoriesFormOutput =
    {}


categoriesForm : Form.Form msg CategoriesFormInput CategoriesFormOutput
categoriesForm =
    Form.succeed {}


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
        |> Form.withGroup [ class "grid lg:grid-cols-2 gap-8" ]
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
    , images = Form.init (Form.File.initMultiple { fileUrls = [], aspectRatio = Nothing })
    , categories = Form.init {}
    , priceAndInventory =
        Form.init
            { price = "0"
            , unitsInStock = "0"
            , trackUnits = False
            }
    , currentStep = MainInformation
    }


initEditingFormData : Translation.Translators -> Product -> Route.EditSaleStep -> FormData
initEditingFormData translators product step =
    { mainInformation =
        Form.init
            { title = product.title
            , description = Form.RichText.initModel "product-description-editor" (Just product.description)
            }
    , images =
        Form.File.initMultiple { fileUrls = product.images, aspectRatio = Nothing }
            |> Form.init
    , categories = Form.init {}
    , priceAndInventory =
        Form.init
            { price = Eos.formatSymbolAmount translators product.symbol product.price
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

            Route.SaleCategories ->
                Categories { title = product.title, description = product.description }
                    product.images

            Route.SalePriceAndInventory ->
                PriceAndInventory
                    { title = product.title, description = product.description }
                    product.images
                    {}
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
                    viewForm loggedIn { isEdit = False, isDisabled = False } model formData

                Creating formData ->
                    viewForm loggedIn { isEdit = False, isDisabled = True } model formData

                EditingUpdate _ formData ->
                    viewForm loggedIn { isEdit = True, isDisabled = False } model formData

                Saving _ formData ->
                    viewForm loggedIn { isEdit = True, isDisabled = True } model formData
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
    -> Model
    -> FormData
    -> Html Msg
viewForm ({ shared } as loggedIn) { isEdit, isDisabled } model formData =
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
            -> Form.Model input
            -> { submitText : String }
            -> (Form.Msg input -> FormMsg)
            -> (output -> Msg)
            -> Html Msg
        viewForm_ formFn formModel { submitText } toFormMsg onSubmitMsg =
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
                                    , disabled (isDisabled || Form.hasFieldsLoading formModel)
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

                Categories _ _ ->
                    ( 3, t "shop.steps.categories.title" )

                PriceAndInventory _ _ _ ->
                    ( 4, t "shop.steps.price_and_inventory.title" )

        isStepCompleted : Route.EditSaleStep -> Bool
        isStepCompleted step =
            case ( step, formData.currentStep ) of
                ( Route.SaleMainInformation, Images _ ) ->
                    True

                ( Route.SaleMainInformation, Categories _ _ ) ->
                    True

                ( Route.SaleMainInformation, PriceAndInventory _ _ _ ) ->
                    True

                ( Route.SaleImages, Categories _ _ ) ->
                    True

                ( Route.SaleImages, PriceAndInventory _ _ _ ) ->
                    True

                ( Route.SaleCategories, PriceAndInventory _ _ _ ) ->
                    True

                _ ->
                    False

        stepBall : Route.EditSaleStep -> Html msg
        stepBall step =
            let
                isCurrent =
                    case formData.currentStep of
                        MainInformation ->
                            step == Route.SaleMainInformation

                        Images _ ->
                            step == Route.SaleImages

                        Categories _ _ ->
                            step == Route.SaleCategories

                        PriceAndInventory _ _ _ ->
                            step == Route.SalePriceAndInventory

                maybeNewRoute =
                    setCurrentStepInRoute model step

                ( linkStepIndex, linkStepName ) =
                    case step of
                        Route.SaleMainInformation ->
                            ( 1, t "shop.steps.main_information.title" )

                        Route.SaleImages ->
                            ( 2, t "shop.steps.images.title" )

                        Route.SaleCategories ->
                            ( 3, t "shop.steps.categories.title" )

                        Route.SalePriceAndInventory ->
                            ( 4, t "shop.steps.price_and_inventory.title" )
            in
            a
                [ class "w-6 h-6 rounded-full flex-shrink-0 flex items-center justify-center transition-colors duration-300"
                , classList
                    [ ( "bg-orange-300 delay-300", isStepCompleted step || isCurrent )
                    , ( "bg-gray-900", not (isStepCompleted step || isCurrent) )
                    ]
                , case maybeNewRoute of
                    Just newRoute ->
                        Route.href newRoute

                    Nothing ->
                        class ""
                , ariaLabel <|
                    tr "shop.steps.step_link"
                        [ ( "index", String.fromInt linkStepIndex )
                        , ( "step_title", linkStepName )
                        ]
                ]
                [ div
                    [ class "transition-opacity duration-300"
                    , classList [ ( "opacity-0", not (isStepCompleted step) ) ]
                    ]
                    [ Icons.checkmark ""
                    ]
                ]

        stepLine step =
            div [ class "w-full h-px bg-gray-900 relative" ]
                [ div
                    [ class "bg-orange-300 w-full absolute left-0 top-0 bottom-0 transition-transform ease-out origin-left duration-300"
                    , classList
                        [ ( "scale-x-0 delay-300", not (isStepCompleted step) )
                        , ( "scale-x-100", isStepCompleted step )
                        ]
                    ]
                    []
                ]
    in
    div [ class "flex flex-col flex-grow" ]
        [ Page.viewHeader loggedIn pageTitle
        , div [ class "lg:container lg:mx-auto lg:px-4 lg:mt-6 lg:mb-20" ]
            [ div [ class "bg-white pt-4 pb-8 flex-grow flex flex-col min-h-150 lg:w-2/3 lg:mx-auto lg:rounded lg:shadow-lg lg:animate-fade-in-from-above-lg lg:fill-mode-none lg:motion-reduce:animate-none" ]
                [ div [ class "container mx-auto px-4 lg:max-w-none lg:mx-0 lg:px-6" ]
                    [ div [ class "mb-4 flex items-center" ]
                        [ stepBall Route.SaleMainInformation
                        , stepLine Route.SaleMainInformation
                        , stepBall Route.SaleImages
                        , stepLine Route.SaleImages
                        , stepBall Route.SaleCategories
                        , stepLine Route.SaleCategories
                        , stepBall Route.SalePriceAndInventory
                        ]
                    , h2 [ class "font-bold text-black mb-2" ]
                        [ text <|
                            tr "shop.steps.index"
                                [ ( "current", String.fromInt stepNumber )
                                , ( "total", String.fromInt 3 )
                                ]
                        ]
                    , text stepName
                    ]
                , hr [ class "mt-4 mb-6 border-gray-500 lg:mx-4 lg:mb-10", ariaHidden True ] []
                , case formData.currentStep of
                    MainInformation ->
                        viewForm_ (mainInformationForm shared.translators)
                            formData.mainInformation
                            { submitText = t "shop.steps.continue" }
                            MainInformationMsg
                            (SubmittedMainInformation ImagesMainInformationTarget)

                    Images _ ->
                        viewForm_ (imagesForm shared.translators)
                            formData.images
                            { submitText = t "shop.steps.continue" }
                            ImagesMsg
                            (SubmittedImages CategoriesImageTarget)

                    Categories _ _ ->
                        viewForm_ categoriesForm
                            formData.categories
                            { submitText = t "shop.steps.continue" }
                            CategoriesMsg
                            SubmittedCategories

                    PriceAndInventory _ _ _ ->
                        case loggedIn.selectedCommunity of
                            RemoteData.Success community ->
                                viewForm_ (priceAndInventoryForm shared.translators { isDisabled = isDisabled } community.symbol)
                                    formData.priceAndInventory
                                    { submitText = actionText }
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
    | SubmittedMainInformation MainInformationTarget MainInformationFormOutput
    | SubmittedImages ImagesTarget ImagesFormOutput
    | SubmittedCategories CategoriesFormOutput
    | SubmittedPriceAndInventory PriceAndInventoryFormOutput
    | GotSaveResponse (RemoteData (Graphql.Http.Error (Maybe Shop.Id)) (Maybe Shop.Id))
    | ClickedDecrementStockUnits
    | ClickedIncrementStockUnits


type MainInformationTarget
    = ImagesMainInformationTarget
    | CategoriesMainInformationTarget
    | PriceAndInventoryMainInformationTarget


type ImagesTarget
    = CategoriesImageTarget
    | PriceAndInventoryImageTarget


type FormMsg
    = MainInformationMsg (Form.Msg MainInformationFormInput)
    | ImagesMsg (Form.Msg ImagesFormInput)
    | CategoriesMsg (Form.Msg CategoriesFormInput)
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
                    initEditingFormData loggedIn.shared.translators sale step
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
                |> UR.addExt
                    (LoggedIn.DropFromRouteHistoryWhile
                        (\route ->
                            case route of
                                Route.NewSale _ ->
                                    True

                                Route.EditSale _ _ ->
                                    True

                                Route.ViewSale _ ->
                                    True

                                _ ->
                                    False
                        )
                    )
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

        SubmittedMainInformation target formOutput ->
            let
                maybeCurrentStep =
                    getFormData model
                        |> Maybe.map .currentStep

                targetCmd =
                    case target of
                        ImagesMainInformationTarget ->
                            Cmd.none

                        CategoriesMainInformationTarget ->
                            case getFormData model of
                                Nothing ->
                                    Cmd.none

                                Just formData ->
                                    Form.parse (imagesForm loggedIn.shared.translators)
                                        formData.images
                                        { onError = GotFormMsg << ImagesMsg
                                        , onSuccess = SubmittedImages CategoriesImageTarget
                                        }
                                        |> Utils.spawnMessage

                        PriceAndInventoryMainInformationTarget ->
                            case getFormData model of
                                Nothing ->
                                    Cmd.none

                                Just formData ->
                                    Form.parse (imagesForm loggedIn.shared.translators)
                                        formData.images
                                        { onError = GotFormMsg << ImagesMsg
                                        , onSuccess = SubmittedImages PriceAndInventoryImageTarget
                                        }
                                        |> Utils.spawnMessage
            in
            case maybeCurrentStep of
                Just MainInformation ->
                    model
                        |> setCurrentStep (Images formOutput)
                        |> UR.init
                        |> UR.addCmd (setCurrentStepInUrl loggedIn.shared model Route.SaleImages)
                        |> UR.addCmd targetCmd

                _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Submitted main information, but was in another step"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Shop.Editor", function = "update" }
                            []

        SubmittedImages target formOutput ->
            let
                maybeCurrentStep =
                    getFormData model
                        |> Maybe.map .currentStep

                targetCmd =
                    case target of
                        CategoriesImageTarget ->
                            Cmd.none

                        PriceAndInventoryImageTarget ->
                            case getFormData model of
                                Nothing ->
                                    Cmd.none

                                Just formData ->
                                    Form.parse categoriesForm
                                        formData.categories
                                        { onError = GotFormMsg << CategoriesMsg
                                        , onSuccess = SubmittedCategories
                                        }
                                        |> Utils.spawnMessage
            in
            case maybeCurrentStep of
                Just (Images mainInformation) ->
                    model
                        |> setCurrentStep (Categories mainInformation formOutput)
                        |> UR.init
                        |> UR.addCmd (setCurrentStepInUrl loggedIn.shared model Route.SalePriceAndInventory)
                        |> UR.addCmd targetCmd

                _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Submitted images, but was in another step"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Shop.Editor", function = "update" }
                            []

        SubmittedCategories _ ->
            Debug.todo ""

        SubmittedPriceAndInventory priceAndInventory ->
            let
                maybeCurrentStep =
                    getFormData model
                        |> Maybe.map .currentStep
            in
            case maybeCurrentStep of
                Just (PriceAndInventory mainInformation images categories) ->
                    case model of
                        EditingCreate formData ->
                            Creating formData
                                |> UR.init
                                |> UR.addExt
                                    (LoggedIn.mutation
                                        loggedIn
                                        (Shop.createProduct
                                            { title = mainInformation.title
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


maybeSetStep : Translation.Translators -> Route.EditSaleStep -> Model -> ( Model, Cmd Msg )
maybeSetStep translators step model =
    let
        maybeModelCmd =
            getFormData model
                |> Maybe.map
                    (\formData ->
                        case formData.currentStep of
                            MainInformation ->
                                case step of
                                    Route.SaleMainInformation ->
                                        ( model, Cmd.none )

                                    Route.SaleImages ->
                                        ( model
                                        , Form.parse (mainInformationForm translators)
                                            formData.mainInformation
                                            { onError = GotFormMsg << MainInformationMsg
                                            , onSuccess = SubmittedMainInformation ImagesMainInformationTarget
                                            }
                                            |> Utils.spawnMessage
                                        )

                                    Route.SaleCategories ->
                                        ( model
                                        , Form.parse (mainInformationForm translators)
                                            formData.mainInformation
                                            { onError = GotFormMsg << MainInformationMsg
                                            , onSuccess = SubmittedMainInformation CategoriesMainInformationTarget
                                            }
                                            |> Utils.spawnMessage
                                        )

                                    Route.SalePriceAndInventory ->
                                        ( model
                                        , Form.parse (mainInformationForm translators)
                                            formData.mainInformation
                                            { onError = GotFormMsg << MainInformationMsg
                                            , onSuccess = SubmittedMainInformation PriceAndInventoryMainInformationTarget
                                            }
                                            |> Utils.spawnMessage
                                        )

                            Images _ ->
                                case step of
                                    Route.SaleMainInformation ->
                                        ( setCurrentStep MainInformation model, Cmd.none )

                                    Route.SaleImages ->
                                        ( model, Cmd.none )

                                    Route.SaleCategories ->
                                        ( model
                                        , Form.parse (imagesForm translators)
                                            formData.images
                                            { onError = GotFormMsg << ImagesMsg
                                            , onSuccess = SubmittedImages CategoriesImageTarget
                                            }
                                            |> Utils.spawnMessage
                                        )

                                    Route.SalePriceAndInventory ->
                                        ( model
                                        , Form.parse (imagesForm translators)
                                            formData.images
                                            { onError = GotFormMsg << ImagesMsg
                                            , onSuccess = SubmittedImages PriceAndInventoryImageTarget
                                            }
                                            |> Utils.spawnMessage
                                        )

                            Categories mainInformationOutput _ ->
                                case step of
                                    Route.SaleMainInformation ->
                                        ( setCurrentStep MainInformation model, Cmd.none )

                                    Route.SaleImages ->
                                        ( setCurrentStep (Images mainInformationOutput) model, Cmd.none )

                                    Route.SaleCategories ->
                                        ( model, Cmd.none )

                                    Route.SalePriceAndInventory ->
                                        ( model
                                        , Form.parse categoriesForm
                                            formData.categories
                                            { onError = GotFormMsg << CategoriesMsg
                                            , onSuccess = SubmittedCategories
                                            }
                                            |> Utils.spawnMessage
                                        )

                            PriceAndInventory mainInformationOutput imagesOutput _ ->
                                case step of
                                    Route.SaleMainInformation ->
                                        ( setCurrentStep MainInformation model, Cmd.none )

                                    Route.SaleImages ->
                                        ( setCurrentStep (Images mainInformationOutput) model, Cmd.none )

                                    Route.SaleCategories ->
                                        ( setCurrentStep (Categories mainInformationOutput imagesOutput) model, Cmd.none )

                                    Route.SalePriceAndInventory ->
                                        ( model, Cmd.none )
                    )
    in
    case maybeModelCmd of
        Nothing ->
            ( model, Cmd.none )

        Just modelCmd ->
            modelCmd


setCurrentStepInRoute : Model -> Route.EditSaleStep -> Maybe Route.Route
setCurrentStepInRoute model step =
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


setCurrentStepInUrl : Shared -> Model -> Route.EditSaleStep -> Cmd msg
setCurrentStepInUrl shared model step =
    case setCurrentStepInRoute model step of
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
                    Form.update shared subMsg formData.images
                        |> UR.fromChild (\newImagesForm -> updateModel { formData | images = newImagesForm })
                            (GotFormMsg << ImagesMsg)
                            LoggedIn.addFeedback
                            model

                CategoriesMsg subMsg ->
                    Form.update shared subMsg formData.categories
                        |> UR.fromChild (\newCategoriesForm -> updateModel { formData | categories = newCategoriesForm })
                            (GotFormMsg << CategoriesMsg)
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

        SubmittedMainInformation _ _ ->
            [ "SubmittedMainInformation" ]

        SubmittedImages _ _ ->
            [ "SubmittedImages" ]

        SubmittedCategories _ ->
            [ "SubmittedCategories" ]

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

        CategoriesMsg subMsg ->
            "CategoriesMsg" :: Form.msgToString subMsg

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

        Categories _ _ ->
            Route.SaleCategories

        PriceAndInventory _ _ _ ->
            Route.SalePriceAndInventory
