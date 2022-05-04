module Page.Shop.Editor exposing
    ( Model
    , Msg(..)
    , initCreate
    , initUpdate
    , msgToString
    , update
    , view
    )

import Api
import Cambiatus.Enum.Permission as Permission
import Community exposing (Balance)
import Form
import Form.File
import Form.RichText
import Form.Select
import Form.Text
import Form.Validate
import Graphql.Http
import Graphql.SelectionSet
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, classList, disabled, maxlength, type_)
import Html.Attributes.Aria exposing (ariaLabel)
import Html.Events exposing (onClick)
import Http
import Icons
import Log
import Markdown exposing (Markdown)
import Page
import RemoteData exposing (RemoteData)
import Result exposing (Result)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import Shop exposing (Product)
import Translation
import UpdateResult as UR
import View.Feedback as Feedback
import View.Modal as Modal



-- INIT


initCreate : LoggedIn.Model -> ( Model, Cmd Msg )
initCreate loggedIn =
    ( LoadingBalancesCreate
    , Api.getBalances loggedIn.shared loggedIn.accountName CompletedBalancesLoad
    )


initUpdate : Shop.Id -> LoggedIn.Model -> ( Model, Cmd Msg )
initUpdate productId loggedIn =
    ( LoadingBalancesUpdate productId
    , Api.getBalances loggedIn.shared loggedIn.accountName CompletedBalancesLoad
    )



-- MODEL


type alias Model =
    Status


type
    Status
    -- Create
    = LoadingBalancesCreate
    | EditingCreate (List Balance) (Form.Model FormInput)
    | Creating (List Balance) (Form.Model FormInput)
      -- Update
    | LoadingBalancesUpdate Shop.Id
    | LoadingSaleUpdate (List Balance)
    | EditingUpdate (List Balance) Product DeleteModalStatus (Form.Model FormInput)
    | Saving (List Balance) Product (Form.Model FormInput)
    | Deleting (List Balance) Product (Form.Model FormInput)
      -- Errors
    | LoadBalancesFailed Http.Error
    | LoadSaleFailed (Graphql.Http.Error (Maybe Product))


type DeleteModalStatus
    = Open
    | Closed


type alias FormInput =
    { image : Form.File.Model
    , title : String
    , description : Form.RichText.Model
    , trackUnits : Bool
    , unitsInStock : String
    , price : String
    }


type alias FormOutput =
    { image : Maybe String
    , title : String
    , description : Markdown
    , unitTracking : Shop.StockTracking
    , price : Float
    }


type Step
    = MainInformation
    | Images
    | PriceAndInventory


type alias FormInput2 =
    { currentStep : Step
    , mainInformation : MainInformationFormInput
    , images : ImagesFormInput
    , priceAndInventory : PriceAndInventoryFormInput
    }


type alias FormOutput2 =
    { name : String
    , description : Markdown
    , images : List String
    , price : Float
    , unitTracking : Shop.StockTracking
    }


createForm2 : Translation.Translators -> Form.Form msg FormInput2 FormOutput2
createForm2 translators =
    Form.succeed
        (\mainInformation images priceAndInventory ->
            { name = mainInformation.name
            , description = mainInformation.description
            , images = images
            , price = priceAndInventory.price
            , unitTracking = priceAndInventory.unitTracking
            }
        )
        |> Form.withNesting
            { value = .mainInformation
            , update = \newMainInformation values -> { values | mainInformation = newMainInformation }
            }
            (mainInformationForm translators)
        |> Form.withNesting
            { value = .images
            , update = \newImages values -> { values | images = newImages }
            }
            imagesForm
        |> Form.withNesting
            { value = .priceAndInventory
            , update = \newPriceAndInventory values -> { values | priceAndInventory = newPriceAndInventory }
            }
            priceAndInventoryForm


type alias MainInformationFormInput =
    { name : String
    , description : Form.RichText.Model
    }


type alias MainInformationFormOutput =
    { name : String, description : Markdown }


mainInformationForm : Translation.Translators -> Form.Form msg MainInformationFormInput MainInformationFormOutput
mainInformationForm translators =
    Form.succeed MainInformationFormOutput
        |> Form.with
            (Form.Text.init
                { -- TODO - I18N
                  label = "Name"
                , id = "product-name-input"
                }
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.stringShorterThan 255
                            >> Form.Validate.stringLongerThan 3
                            >> Form.Validate.validate translators
                    , value = .name
                    , update = \newName values -> { values | name = newName }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.RichText.init
                { -- TODO - I18N
                  label = "Description"
                }
                |> Form.richText
                    { parser = Ok
                    , value = .description
                    , update = \newDescription values -> { values | description = newDescription }
                    , externalError = always Nothing
                    }
            )


type alias ImagesFormInput =
    -- TODO - Should this be a list?
    List Form.File.Model


type alias ImagesFormOutput =
    List String


imagesForm : Form.Form msg ImagesFormInput ImagesFormOutput
imagesForm =
    Debug.todo ""


type alias PriceAndInventoryFormInput =
    { price : String
    , unitsInStock : String
    , trackUnits : Bool
    }


type alias PriceAndInventoryFormOutput =
    { price : Float
    , unitTracking : Shop.StockTracking
    }


priceAndInventoryForm : Form.Form msg PriceAndInventoryFormInput PriceAndInventoryFormOutput
priceAndInventoryForm =
    Debug.todo ""


createForm : LoggedIn.Model -> Form.Form Msg FormInput FormOutput
createForm loggedIn =
    let
        ({ t } as translators) =
            loggedIn.shared.translators
    in
    Form.succeed
        (\maybeImage title description price trackUnits unitsInStock ->
            { image = maybeImage
            , title = title
            , description = description
            , unitTracking =
                if trackUnits then
                    unitsInStock

                else
                    Shop.NoTracking
            , price = price
            }
        )
        |> Form.with
            (Form.introspect
                (\values ->
                    Form.File.init { label = t "shop.photo_label", id = "image-uploader" }
                        |> Form.File.withVariant (Form.File.LargeRectangle Form.File.Gray)
                        |> Form.File.withContainerAttrs
                            [ class "mb-10 lg:place-self-center lg:w-2/3"
                            , classList
                                [ ( "lg:row-span-5", not values.trackUnits )
                                , ( "lg:row-span-6", values.trackUnits )
                                ]
                            ]
                        |> Form.File.withAttrs [ class "border border-dashed border-gray-900 rounded" ]
                        |> Form.file
                            { translators = translators
                            , value = .image
                            , update = \image input -> { input | image = image }
                            , externalError = always Nothing
                            }
                        |> Form.optional
                )
            )
        |> Form.with
            (Form.Text.init { label = t "shop.what_label", id = "title-input" }
                |> Form.Text.withExtraAttrs [ maxlength 255 ]
                |> Form.Text.withContainerAttrs [ class "lg:w-2/3" ]
                |> Form.Text.withPlaceholder (t "shop.what_label")
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.stringShorterThan 255
                            >> Form.Validate.stringLongerThan 3
                            >> Form.Validate.validate translators
                    , value = .title
                    , update = \title input -> { input | title = title }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.RichText.init { label = t "shop.description_label" }
                |> Form.RichText.withContainerAttrs [ class "mb-10 lg:w-2/3" ]
                |> Form.RichText.withPlaceholder (t "shop.description_placeholder")
                |> Form.richText
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.markdownLongerThan 10
                            >> Form.Validate.validate translators
                    , value = .description
                    , update = \description input -> { input | description = description }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Text.init { label = t "shop.price_label", id = "price-input" }
                |> (case loggedIn.selectedCommunity of
                        RemoteData.Success community ->
                            Form.Text.withCurrency community.symbol

                        _ ->
                            identity
                   )
                |> Form.Text.withExtraAttrs [ Html.Attributes.min "0" ]
                |> Form.Text.withContainerAttrs [ class "lg:w-2/3" ]
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.maskedFloat translators
                            >> Form.Validate.floatGreaterThan 0
                            >> Form.Validate.validate translators
                    , value = .price
                    , update = \price input -> { input | price = price }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Select.init
                { label = t "shop.track_stock_label"
                , id = "track-stock-select"
                , optionToString = boolToString
                }
                |> Form.Select.withOption False (t "shop.track_stock_no")
                |> Form.Select.withOption True (t "shop.track_stock_yes")
                |> Form.Select.withContainerAttrs [ class "mb-10 lg:w-2/3" ]
                |> Form.select (boolFromString >> Maybe.withDefault False)
                    { parser = Ok
                    , value = .trackUnits
                    , update = \trackUnits input -> { input | trackUnits = trackUnits }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.introspect
                (\values ->
                    if values.trackUnits then
                        Form.Text.init { label = t "shop.units_label", id = "units-in-stock-input" }
                            |> Form.Text.withPlaceholder "0"
                            |> Form.Text.asNumeric
                            |> Form.Text.withType Form.Text.Number
                            |> Form.Text.withExtraAttrs
                                [ Html.Attributes.min "0"
                                , class "text-center"
                                ]
                            |> Form.Text.withContainerAttrs [ class "lg:w-2/3" ]
                            |> Form.Text.withElements
                                [ button
                                    [ class "absolute top-1 bottom-1 left-1 px-4 rounded focus-ring bg-white text-orange-300 hover:text-orange-300/70"
                                    , type_ "button"
                                    , ariaLabel <| t "shop.subtract_unit"
                                    , onClick ClickedDecrementStockUnits
                                    ]
                                    [ Icons.minus "fill-current" ]
                                , button
                                    [ class "absolute top-1 bottom-1 right-1 px-4 rounded focus-ring bg-white text-orange-300 hover:text-orange-300/70"
                                    , type_ "button"
                                    , ariaLabel <| t "shop.add_unit"
                                    , onClick ClickedIncrementStockUnits
                                    ]
                                    [ Icons.plus "fill-current" ]
                                ]
                            |> Form.textField
                                { parser =
                                    Form.Validate.succeed
                                        >> Form.Validate.int
                                        >> Form.Validate.intGreaterThanOrEqualTo 0
                                        >> Form.Validate.map (\units -> Shop.UnitTracking { availableUnits = units })
                                        >> Form.Validate.validate translators
                                , value = .unitsInStock
                                , update = \unitsInStock input -> { input | unitsInStock = unitsInStock }
                                , externalError = always Nothing
                                }

                    else
                        Form.succeed Shop.NoTracking
                )
            )


boolToString : Bool -> String
boolToString bool =
    if bool then
        "True"

    else
        "False"


boolFromString : String -> Maybe Bool
boolFromString bool =
    case bool of
        "True" ->
            Just True

        "False" ->
            Just False

        _ ->
            Nothing


initForm : Form.Model FormInput
initForm =
    Form.init
        { image = Form.File.initModel Nothing
        , title = ""
        , description = Form.RichText.initModel "description-editor" Nothing
        , trackUnits = False
        , unitsInStock = "0"
        , price = "0"
        }


initEditingForm : Product -> Form.Model FormInput
initEditingForm product =
    Form.init
        { image = Form.File.initModel product.image
        , title = product.title
        , description = Form.RichText.initModel "description-editor" (Just product.description)
        , trackUnits = Shop.hasUnitTracking product
        , unitsInStock =
            case product.stockTracking of
                Shop.NoTracking ->
                    String.fromInt 0

                Shop.UnitTracking { availableUnits } ->
                    String.fromInt availableUnits
        , price = String.fromFloat product.price
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
                EditingUpdate _ _ _ _ ->
                    True

                Saving _ _ _ ->
                    True

                Deleting _ _ _ ->
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
                LoadingBalancesCreate ->
                    Page.fullPageLoading shared

                LoadingBalancesUpdate _ ->
                    Page.fullPageLoading shared

                LoadingSaleUpdate _ ->
                    Page.fullPageLoading shared

                LoadBalancesFailed error ->
                    Page.fullPageError (t "shop.title") error

                LoadSaleFailed error ->
                    Page.fullPageGraphQLError (t "shop.title") error

                EditingCreate _ form ->
                    viewForm loggedIn False False Closed form

                Creating _ form ->
                    viewForm loggedIn False True Closed form

                EditingUpdate _ _ confirmDelete form ->
                    viewForm loggedIn True False confirmDelete form

                Saving _ _ form ->
                    viewForm loggedIn True True Closed form

                Deleting _ _ form ->
                    viewForm loggedIn True True Closed form
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


viewForm : LoggedIn.Model -> Bool -> Bool -> DeleteModalStatus -> Form.Model FormInput -> Html Msg
viewForm ({ shared } as loggedIn) isEdit isDisabled deleteModal form =
    let
        { t } =
            shared.translators

        ( actionText, pageTitle ) =
            if isEdit then
                ( t "menu.save", t "shop.edit_offer" )

            else
                ( t "menu.create", t "shop.create_offer" )
    in
    div [ class "flex flex-col flex-grow mb-10 lg:mb-0" ]
        [ Page.viewHeader loggedIn pageTitle
        , div [ class "flex items-center flex-grow relative bg-white lg:bg-transparent" ]
            [ div [ class "bg-white top-0 bottom-0 left-0 right-1/2 absolute hidden lg:block" ] []
            , Form.view [ class "container mx-auto p-4 z-10 lg:py-16 grid lg:grid-cols-2 lg:justify-items-center" ]
                shared.translators
                (\submitButton ->
                    [ div [ class "lg:w-2/3 flex flex-col-reverse gap-4 lg:flex-row" ]
                        [ if isEdit then
                            button
                                [ class "button button-danger w-full"
                                , disabled isDisabled
                                , onClick ClickedDelete
                                , type_ "button"
                                ]
                                [ text (t "shop.delete") ]

                          else
                            text ""
                        , submitButton
                            [ class "button button-primary w-full"
                            , disabled isDisabled
                            ]
                            [ text actionText ]
                        ]
                    , if isEdit && deleteModal == Open then
                        viewConfirmDeleteModal t

                      else
                        text ""
                    ]
                )
                (createForm loggedIn)
                (Form.withDisabled isDisabled form)
                { toMsg = GotFormMsg
                , onSubmit = ClickedSave
                }
            ]
        ]


viewConfirmDeleteModal : (String -> String) -> Html Msg
viewConfirmDeleteModal t =
    Modal.initWith
        { closeMsg = ClickedDeleteCancel
        , isVisible = True
        }
        |> Modal.withHeader (t "shop.delete_modal.title")
        |> Modal.withBody
            [ text (t "shop.delete_modal.body") ]
        |> Modal.withFooter
            [ button
                [ class "modal-cancel"
                , onClick ClickedDeleteCancel
                , type_ "button"
                ]
                [ text (t "shop.delete_modal.cancel") ]
            , button
                [ class "modal-accept"
                , onClick ClickedDeleteConfirm
                , type_ "button"
                ]
                [ text (t "shop.delete_modal.confirm") ]
            ]
        |> Modal.toHtml



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = CompletedBalancesLoad (Result Http.Error (List Balance))
    | CompletedSaleLoad (RemoteData (Graphql.Http.Error (Maybe Product)) (Maybe Product))
    | GotFormMsg (Form.Msg FormInput)
    | ClickedSave FormOutput
    | ClickedDelete
    | ClickedDeleteConfirm
    | ClickedDeleteCancel
    | GotSaveResponse (RemoteData (Graphql.Http.Error (Maybe Shop.Id)) (Maybe Shop.Id))
    | GotDeleteResponse (RemoteData (Graphql.Http.Error (Maybe ())) (Maybe ()))
    | ClosedAuthModal
    | ClickedDecrementStockUnits
    | ClickedIncrementStockUnits


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    let
        t =
            loggedIn.shared.translators.t
    in
    case msg of
        CompletedBalancesLoad (Ok balances) ->
            case model of
                LoadingBalancesCreate ->
                    initForm
                        |> EditingCreate balances
                        |> UR.init

                LoadingBalancesUpdate saleId ->
                    let
                        addSaleFetch =
                            LoggedIn.query loggedIn
                                (Shop.productQuery saleId)
                                CompletedSaleLoad
                                |> UR.addExt
                    in
                    LoadingSaleUpdate balances
                        |> UR.init
                        |> addSaleFetch

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Completed loading balances, but user wasn't creating or updating sale"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Shop.Editor", function = "update" }
                            []

        CompletedBalancesLoad (Err error) ->
            LoadBalancesFailed error
                |> UR.init
                |> UR.logHttpError msg
                    (Just loggedIn.accountName)
                    "Got an error when loading balances for shop editor"
                    { moduleName = "Page.Shop.Editor", function = "update" }
                    [ Log.contextFromCommunity loggedIn.selectedCommunity ]
                    error

        CompletedSaleLoad (RemoteData.Success maybeSale) ->
            case ( model, maybeSale ) of
                ( LoadingSaleUpdate balances, Just sale ) ->
                    initEditingForm sale
                        |> EditingUpdate balances sale Closed
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

        ClickedSave formOutput ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    case model of
                        EditingCreate balances form ->
                            Creating balances form
                                |> UR.init
                                |> UR.addExt
                                    (LoggedIn.mutation
                                        loggedIn
                                        (Shop.createProduct
                                            { symbol = community.symbol
                                            , title = formOutput.title
                                            , description = formOutput.description
                                            , images =
                                                formOutput.image
                                                    |> Maybe.map List.singleton
                                                    |> Maybe.withDefault []
                                            , price = formOutput.price
                                            , stockTracking = formOutput.unitTracking
                                            }
                                            Shop.idSelectionSet
                                        )
                                        GotSaveResponse
                                    )
                                |> LoggedIn.withPrivateKey loggedIn
                                    [ Permission.Sell ]
                                    model
                                    { successMsg = msg, errorMsg = ClosedAuthModal }

                        EditingUpdate balances sale _ form ->
                            Saving balances sale form
                                |> UR.init
                                |> UR.addExt
                                    (LoggedIn.mutation
                                        loggedIn
                                        (Shop.updateProduct
                                            { id = sale.id
                                            , symbol = community.symbol
                                            , title = formOutput.title
                                            , description = formOutput.description
                                            , images =
                                                formOutput.image
                                                    |> Maybe.map List.singleton
                                                    |> Maybe.withDefault []
                                            , price = formOutput.price
                                            , stockTracking = formOutput.unitTracking
                                            }
                                            Shop.idSelectionSet
                                        )
                                        GotSaveResponse
                                    )
                                |> LoggedIn.withPrivateKey loggedIn
                                    [ Permission.Sell ]
                                    model
                                    { successMsg = msg, errorMsg = ClosedAuthModal }

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
                            "Clicked save shop item, but community wasn't loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Shop.Editor", function = "update" }
                            []

        ClickedDelete ->
            case model of
                EditingUpdate balances sale _ form ->
                    EditingUpdate balances sale Open form
                        |> UR.init

                _ ->
                    UR.init model

        ClickedDeleteCancel ->
            case model of
                EditingUpdate balances sale _ form ->
                    EditingUpdate balances sale Closed form
                        |> UR.init

                _ ->
                    UR.init model

        ClickedDeleteConfirm ->
            case model of
                EditingUpdate balances sale _ form ->
                    Deleting balances sale form
                        |> UR.init
                        |> UR.addExt
                            (LoggedIn.mutation loggedIn
                                (Shop.deleteProduct sale.id (Graphql.SelectionSet.succeed ()))
                                GotDeleteResponse
                            )
                        |> LoggedIn.withPrivateKey loggedIn
                            []
                            model
                            { successMsg = msg, errorMsg = ClosedAuthModal }

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Clicked delete shop item, but wasn't editing or creating shop offer"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Shop.Editor", function = "update" }
                            []

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
                |> UR.addCmd
                    (Route.replaceUrl loggedIn.shared.navKey redirectUrl)
                |> UR.addExt (ShowFeedback Feedback.Success (t "shop.create_offer_success"))

        GotSaveResponse (RemoteData.Failure error) ->
            let
                internalError =
                    loggedIn.shared.translators.t "error.unknown"
            in
            case model of
                Creating balances form ->
                    EditingCreate balances form
                        |> UR.init
                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure internalError)
                        |> UR.logGraphqlError msg
                            (Just loggedIn.accountName)
                            "Got an error when creating a shop offer"
                            { moduleName = "Page.Shop.Editor", function = "update" }
                            []
                            error

                Saving balances sale form ->
                    EditingUpdate balances sale Closed form
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

        GotDeleteResponse (RemoteData.Success _) ->
            model
                |> UR.init
                |> UR.addCmd
                    (Route.replaceUrl loggedIn.shared.navKey (Route.Shop Shop.All))
                |> UR.addExt (ShowFeedback Feedback.Success (t "shop.delete_offer_success"))

        GotDeleteResponse (RemoteData.Failure error) ->
            let
                internalError =
                    loggedIn.shared.translators.t "error.unknown"
            in
            case model of
                Deleting balances sale form ->
                    EditingUpdate balances sale Closed form
                        |> UR.init
                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure internalError)
                        |> UR.logGraphqlError msg
                            (Just loggedIn.accountName)
                            "Got an error when deleting a shop offer"
                            { moduleName = "Page.Shop.Editor", function = "update" }
                            []
                            error

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Deleted shop item, but wasn't in the state of Deleting"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Shop.Editor", function = "update" }
                            []

        GotDeleteResponse _ ->
            UR.init model

        ClosedAuthModal ->
            case model of
                EditingUpdate balances sale _ form ->
                    EditingUpdate balances sale Closed form
                        |> UR.init

                _ ->
                    UR.init model

        GotFormMsg subMsg ->
            updateForm loggedIn.shared subMsg model

        ClickedDecrementStockUnits ->
            updateFormStockUnits (\price -> price - 1) model

        ClickedIncrementStockUnits ->
            updateFormStockUnits (\price -> price + 1) model


updateFormStockUnits : (Int -> Int) -> Model -> UpdateResult
updateFormStockUnits updateFn model =
    let
        maybeFormInfo =
            case model of
                EditingCreate balances form ->
                    Just ( form, EditingCreate balances )

                Creating balances form ->
                    Just ( form, Creating balances )

                EditingUpdate balances product deleteModalStatus form ->
                    Just ( form, EditingUpdate balances product deleteModalStatus )

                Saving balances product form ->
                    Just ( form, Saving balances product )

                Deleting balances product form ->
                    Just ( form, Deleting balances product )

                _ ->
                    Nothing
    in
    case maybeFormInfo of
        Nothing ->
            UR.init model

        Just ( form, updateModel ) ->
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
                form
                |> updateModel
                |> UR.init


updateForm : Shared -> Form.Msg FormInput -> Model -> UpdateResult
updateForm shared subMsg model =
    let
        maybeFormInfo =
            case model of
                EditingCreate balances form ->
                    Just ( form, EditingCreate balances )

                Creating balances form ->
                    Just ( form, Creating balances )

                EditingUpdate balances product deleteModalStatus form ->
                    Just ( form, EditingUpdate balances product deleteModalStatus )

                Saving balances product form ->
                    Just ( form, Saving balances product )

                Deleting balances product form ->
                    Just ( form, Deleting balances product )

                _ ->
                    Nothing
    in
    case maybeFormInfo of
        Nothing ->
            UR.init model

        Just ( form, updateModel ) ->
            Form.update shared
                subMsg
                form
                |> UR.fromChild updateModel
                    GotFormMsg
                    LoggedIn.addFeedback
                    model


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedBalancesLoad r ->
            [ "CompletedBalancesLoad", UR.resultToString r ]

        CompletedSaleLoad r ->
            [ "CompletedSaleLoad", UR.remoteDataToString r ]

        ClickedSave _ ->
            [ "ClickedSave" ]

        ClickedDelete ->
            [ "ClickedDelete" ]

        ClickedDeleteConfirm ->
            [ "ClickedDeleteConfirm" ]

        ClickedDeleteCancel ->
            [ "ClickedDeleteCancel" ]

        GotSaveResponse r ->
            [ "GotSaveResponse", UR.remoteDataToString r ]

        GotDeleteResponse r ->
            [ "GotDeleteResponse", UR.remoteDataToString r ]

        ClosedAuthModal ->
            [ "ClosedAuthModal" ]

        GotFormMsg subMsg ->
            "GotFormMsg" :: Form.msgToString subMsg

        ClickedDecrementStockUnits ->
            [ "ClickedDecrementStockUnits" ]

        ClickedIncrementStockUnits ->
            [ "ClickedIncrementStockUnits" ]
