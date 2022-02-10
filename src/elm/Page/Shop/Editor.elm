module Page.Shop.Editor exposing
    ( Model
    , Msg(..)
    , initCreate
    , initUpdate
    , jsAddressToMsg
    , msgToString
    , update
    , view
    )

import Api
import Community exposing (Balance)
import Eos exposing (Symbol)
import Eos.Account as Eos
import Form
import Form.File
import Form.RichText
import Form.Select
import Form.Text
import Form.Validate
import Graphql.Http
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, disabled, id, maxlength, type_)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Log
import Markdown exposing (Markdown)
import Page
import RemoteData exposing (RemoteData)
import Result exposing (Result)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import Shop exposing (Product, ProductId)
import UpdateResult as UR
import View.Feedback as Feedback
import View.Modal as Modal



-- INIT


initCreate : LoggedIn.Model -> ( Model, Cmd Msg )
initCreate loggedIn =
    ( LoadingBalancesCreate
    , Api.getBalances loggedIn.shared loggedIn.accountName CompletedBalancesLoad
    )


initUpdate : ProductId -> LoggedIn.Model -> ( Model, Cmd Msg )
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
    | LoadingBalancesUpdate ProductId
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
    , unitTracking : UnitTracking
    , price : Float
    }


type UnitTracking
    = TrackUnits Int
    | DontTrackUnits


createForm : LoggedIn.Model -> Form.Form msg FormInput FormOutput
createForm loggedIn =
    let
        ({ t } as translators) =
            loggedIn.shared.translators
    in
    Form.succeed
        (\maybeImage title description trackUnits unitsInStock price ->
            { image = maybeImage
            , title = title
            , description = description
            , unitTracking =
                if trackUnits then
                    TrackUnits unitsInStock

                else
                    DontTrackUnits
            , price = price
            }
        )
        |> Form.with
            (Form.File.init { label = "", id = "image-uploader" }
                |> Form.File.withVariant (Form.File.LargeRectangle Form.File.Gray)
                |> Form.File.withContainerAttrs [ class "mb-10" ]
                |> Form.file
                    { translators = translators
                    , value = .image
                    , update = \image input -> { input | image = image }
                    , externalError = always Nothing
                    }
                |> Form.optional
            )
        |> Form.with
            (Form.Text.init { label = t "shop.what_label", id = "title-input" }
                |> Form.Text.withExtraAttrs [ maxlength 255 ]
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
                |> Form.RichText.withContainerAttrs [ class "mb-10" ]
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
            (Form.Select.init
                { label = t "shop.track_stock_label"
                , id = "track-stock-select"
                , optionToString = boolToString
                }
                |> Form.Select.withOption False (t "shop.track_stock_no")
                |> Form.Select.withOption True (t "shop.track_stock_yes")
                |> Form.Select.withContainerAttrs [ class "mb-10" ]
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
                            |> Form.Text.asNumeric
                            |> Form.Text.withType Form.Text.Number
                            |> Form.Text.withExtraAttrs [ Html.Attributes.min "0" ]
                            |> Form.textField
                                { parser =
                                    Form.Validate.succeed
                                        >> Form.Validate.int
                                        >> Form.Validate.intGreaterThanOrEqualTo 0
                                        >> Form.Validate.intLowerThanOrEqualTo 2000
                                        >> Form.Validate.validate translators
                                , value = .unitsInStock
                                , update = \unitsInStock input -> { input | unitsInStock = unitsInStock }
                                , externalError = always Nothing
                                }

                    else
                        Form.succeed 0
                )
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
        , trackUnits = product.trackStock
        , unitsInStock = String.fromInt product.units
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
    div [ class "bg-white" ]
        [ Page.viewHeader loggedIn pageTitle
        , Form.view [ class "container mx-auto px-4" ]
            shared.translators
            (\submitButton ->
                [ div [ class "flex flex-col-reverse sm:flex-row align-center justify-center mb-10" ]
                    [ if isEdit then
                        button
                            [ class "button button-danger w-full mt-4 sm:w-40 sm:mt-0 sm:mr-4"
                            , disabled isDisabled
                            , onClick ClickedDelete
                            , type_ "button"
                            ]
                            [ text (t "shop.delete") ]

                      else
                        text ""
                    , submitButton
                        [ class "button button-primary w-full sm:w-40"
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
                ]
                [ text (t "shop.delete_modal.cancel") ]
            , button
                [ class "modal-accept"
                , onClick ClickedDeleteConfirm
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
    | GotSaveResponse (Result Value String)
    | GotDeleteResponse (Result Value String)
    | ClosedAuthModal


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
                            case String.toInt saleId of
                                Nothing ->
                                    identity

                                Just id ->
                                    LoggedIn.query loggedIn
                                        (Shop.productQuery id)
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
            case model of
                EditingCreate balances form ->
                    performRequest
                        (ClickedSave formOutput)
                        (Creating balances form)
                        loggedIn
                        "createsale"
                        (encodeCreateForm loggedIn formOutput)
                        |> LoggedIn.withPrivateKey loggedIn
                            model
                            { successMsg = msg, errorMsg = ClosedAuthModal }

                EditingUpdate balances sale _ form ->
                    case loggedIn.selectedCommunity of
                        RemoteData.Success community ->
                            performRequest
                                (ClickedSave formOutput)
                                (Saving balances sale form)
                                loggedIn
                                "updatesale"
                                (encodeUpdateForm sale formOutput community.symbol)
                                |> LoggedIn.withPrivateKey loggedIn
                                    model
                                    { successMsg = msg, errorMsg = ClosedAuthModal }

                        _ ->
                            UR.init model

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Clicked save shop item, but wasn't editing or creating shop offer"
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
                    performRequest
                        ClickedDeleteConfirm
                        (Deleting balances sale form)
                        loggedIn
                        "deletesale"
                        (encodeDeleteForm sale)
                        |> LoggedIn.withPrivateKey loggedIn
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

        GotSaveResponse (Ok _) ->
            UR.init model
                |> UR.addCmd
                    (Route.replaceUrl loggedIn.shared.navKey (Route.Shop Shop.All))
                |> UR.addExt (ShowFeedback Feedback.Success (t "shop.create_offer_success"))

        GotSaveResponse (Err error) ->
            let
                internalError =
                    loggedIn.shared.translators.t "error.unknown"
            in
            case model of
                Creating balances form ->
                    EditingCreate balances form
                        |> UR.init
                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure internalError)
                        |> UR.logJsonValue msg
                            (Just loggedIn.accountName)
                            "Got an error when creating a shop offer"
                            { moduleName = "Page.Shop.Editor", function = "update" }
                            []
                            error

                Saving balances sale form ->
                    EditingUpdate balances sale Closed form
                        |> UR.init
                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure internalError)
                        |> UR.logJsonValue msg
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

        GotDeleteResponse (Ok _) ->
            model
                |> UR.init
                |> UR.addCmd
                    (Route.replaceUrl loggedIn.shared.navKey (Route.Shop Shop.All))
                |> UR.addExt (ShowFeedback Feedback.Success (t "shop.delete_offer_success"))

        GotDeleteResponse (Err error) ->
            let
                internalError =
                    loggedIn.shared.translators.t "error.unknown"
            in
            case model of
                Deleting balances sale form ->
                    EditingUpdate balances sale Closed form
                        |> UR.init
                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure internalError)
                        |> UR.logJsonValue msg
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

        ClosedAuthModal ->
            case model of
                EditingUpdate balances sale _ form ->
                    EditingUpdate balances sale Closed form
                        |> UR.init

                _ ->
                    UR.init model

        GotFormMsg subMsg ->
            updateForm loggedIn.shared subMsg model


performRequest : Msg -> Status -> LoggedIn.Model -> String -> Value -> UpdateResult
performRequest msg status { shared, accountName } action data =
    status
        |> UR.init
        |> UR.addPort
            { responseAddress = msg
            , responseData = Encode.null
            , data =
                Eos.encodeTransaction
                    [ { accountName = shared.contracts.community
                      , name = action
                      , authorization =
                            { actor = accountName
                            , permissionName = Eos.samplePermission
                            }
                      , data = data
                      }
                    ]
            }


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


encodeCreateForm : LoggedIn.Model -> FormOutput -> Value
encodeCreateForm loggedIn form =
    let
        creator =
            Eos.encodeName loggedIn.accountName

        image =
            form.image
                |> Maybe.withDefault ""
                |> Encode.string

        title =
            form.title
                |> Encode.string

        description =
            form.description
                |> Markdown.encode

        price =
            form.price

        quantity =
            case
                Maybe.map (\c -> Eos.Asset price c.symbol)
                    (RemoteData.toMaybe loggedIn.selectedCommunity)
            of
                Nothing ->
                    Encode.string ""

                Just asset ->
                    Eos.encodeAsset asset

        trackStock =
            (case form.unitTracking of
                DontTrackUnits ->
                    False

                TrackUnits _ ->
                    True
            )
                |> Eos.boolToEosBool
                |> Eos.encodeEosBool

        units =
            (case form.unitTracking of
                DontTrackUnits ->
                    0

                TrackUnits units_ ->
                    units_
            )
                |> Encode.int
    in
    Encode.object
        [ ( "from", creator )
        , ( "title", title )
        , ( "description", description )
        , ( "quantity", quantity )
        , ( "image", image )
        , ( "track_stock", trackStock )
        , ( "units", units )
        ]


encodeUpdateForm : Product -> FormOutput -> Symbol -> Value
encodeUpdateForm product form selectedCommunity =
    let
        saleId =
            Encode.int product.id

        image =
            Maybe.withDefault "" form.image
                |> Encode.string

        title =
            form.title
                |> Encode.string

        description =
            form.description
                |> Markdown.encode

        price =
            form.price

        quantity =
            Eos.encodeAsset { amount = price, symbol = selectedCommunity }

        trackStock =
            (case form.unitTracking of
                DontTrackUnits ->
                    False

                TrackUnits _ ->
                    True
            )
                |> Eos.boolToEosBool
                |> Eos.encodeEosBool

        units =
            (case form.unitTracking of
                DontTrackUnits ->
                    0

                TrackUnits units_ ->
                    units_
            )
                |> Encode.int
    in
    Encode.object
        [ ( "sale_id", saleId )
        , ( "title", title )
        , ( "description", description )
        , ( "quantity", quantity )
        , ( "image", image )
        , ( "track_stock", trackStock )
        , ( "units", units )
        ]


encodeDeleteForm : Product -> Value
encodeDeleteForm product =
    Encode.object
        [ ( "sale_id", Encode.int product.id )
        ]


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "ClickedSave" :: [] ->
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.field "transactionId" Decode.string
                        |> Decode.map Ok
                    , Decode.succeed (Err Encode.null)
                    ]
                )
                val
                |> Result.map (Just << GotSaveResponse)
                |> Result.withDefault Nothing

        "ClickedDeleteConfirm" :: [] ->
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.field "transactionId" Decode.string
                        |> Decode.map Ok
                    , Decode.succeed (Err Encode.null)
                    ]
                )
                val
                |> Result.map (Just << GotDeleteResponse)
                |> Result.withDefault Nothing

        _ ->
            Nothing


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
            [ "GotSaveResponse", UR.resultToString r ]

        GotDeleteResponse r ->
            [ "GotDeleteResponse", UR.resultToString r ]

        ClosedAuthModal ->
            [ "ClosedAuthModal" ]

        GotFormMsg subMsg ->
            "GotFormMsg" :: Form.msgToString subMsg
