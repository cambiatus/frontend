module Page.Shop.Editor exposing (Model, Msg(..), initCreate, initUpdate, jsAddressToMsg, msgToString, subscriptions, update, view)

import Api
import Api.Graphql
import Asset.Icon as Icon
import Browser.Events as Events
import Community exposing (Balance)
import DataValidator exposing (Validator, getInput, greaterThanOrEqual, hasErrors, listErrors, longerThan, lowerThanOrEqual, newValidator, oneOf, updateInput, validate)
import Eos exposing (Symbol)
import Eos.Account as Eos
import File exposing (File)
import Graphql.Http
import Html exposing (Html, button, div, input, label, option, p, select, span, text, textarea)
import Html.Attributes exposing (accept, class, classList, disabled, for, id, maxlength, multiple, required, selected, style, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Http
import I18Next
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Page
import Result exposing (Result)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..), FeedbackStatus(..))
import Shop exposing (Sale, SaleId)
import Task
import UpdateResult as UR
import Utils exposing (decodeEnterKeyDown)
import View.Modal as Modal



-- INIT


initCreate : LoggedIn.Model -> ( Model, Cmd Msg )
initCreate loggedIn =
    ( LoadingBalancesCreate
    , Api.getBalances loggedIn.shared loggedIn.accountName CompletedBalancesLoad
    )


initUpdate : SaleId -> LoggedIn.Model -> ( Model, Cmd Msg )
initUpdate saleId loggedIn =
    ( LoadingBalancesUpdate saleId
    , Api.getBalances loggedIn.shared loggedIn.accountName CompletedBalancesLoad
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map PressedEnter (Events.onKeyDown decodeEnterKeyDown)



-- MODEL


type alias Model =
    Status


type
    Status
    -- Create
    = LoadingBalancesCreate
    | EditingCreate (List Balance) ImageStatus Form
    | Creating (List Balance) ImageStatus Form
      -- Update
    | LoadingBalancesUpdate SaleId
    | LoadingSaleUpdate (List Balance) SaleId
    | EditingUpdate (List Balance) Sale ImageStatus DeleteModalStatus Form
    | Saving (List Balance) Sale ImageStatus Form
    | Deleting (List Balance) Sale ImageStatus Form
      -- Errors
    | LoadBalancesFailed Http.Error
    | LoadSaleFailed (Graphql.Http.Error (Maybe Sale))


type DeleteModalStatus
    = Open
    | Closed


type alias FormError =
    Maybe String


type ImageStatus
    = NoImage
    | Uploading
    | UploadFailed Http.Error
    | Uploaded String


type alias Form =
    { image : Validator (Maybe String)
    , title : Validator String
    , description : Validator String
    , trackStock : Validator (Maybe String)
    , units : Validator String
    , price : Validator String
    , error : FormError
    }


trackYes : String
trackYes =
    "yes"


trackNo : String
trackNo =
    "no"


initForm : List String -> Form
initForm balanceOptions =
    let
        image =
            newValidator Nothing identity False []

        title =
            newValidator "" (\v -> Just v) True []

        description =
            []
                |> longerThan 10
                |> newValidator "" (\v -> Just v) True

        trackStock =
            []
                |> oneOf [ trackYes, trackNo ]
                |> newValidator (Just trackNo) identity True

        units =
            []
                |> greaterThanOrEqual 0
                |> lowerThanOrEqual 2000
                |> newValidator "0" (\v -> Just v) False

        price =
            []
                |> greaterThanOrEqual 0
                |> newValidator "" (\v -> Just v) True
    in
    { image = image
    , title = title
    , description = description
    , trackStock = trackStock
    , units = units
    , price = price
    , error = Nothing
    }



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        shared =
            loggedIn.shared

        t =
            I18Next.t shared.translations

        isEdit =
            case model of
                EditingUpdate _ _ _ _ _ ->
                    True

                Saving _ _ _ _ ->
                    True

                Deleting _ _ _ _ ->
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
                    Page.fullPageLoading

                LoadingBalancesUpdate _ ->
                    Page.fullPageLoading

                LoadingSaleUpdate _ _ ->
                    Page.fullPageLoading

                LoadBalancesFailed error ->
                    Page.fullPageError (t "shop.title") error

                LoadSaleFailed error ->
                    Page.fullPageGraphQLError (t "shop.title") error

                EditingCreate balances imageStatus form ->
                    viewForm loggedIn balances imageStatus False False Closed form

                Creating balances imageStatus form ->
                    viewForm loggedIn balances imageStatus False True Closed form

                EditingUpdate balances _ imageStatus confirmDelete form ->
                    viewForm loggedIn balances imageStatus True False confirmDelete form

                Saving balances _ imageStatus form ->
                    viewForm loggedIn balances imageStatus True True Closed form

                Deleting balances _ imageStatus form ->
                    viewForm loggedIn balances imageStatus True True Closed form
    in
    { title = title
    , content =
        case loggedIn.hasShop of
            LoggedIn.FeatureLoaded True ->
                content

            LoggedIn.FeatureLoaded False ->
                Page.fullPageNotFound
                    (t "error.pageNotFound")
                    (t "shop.disabled.description")

            LoggedIn.FeatureLoading ->
                Page.fullPageLoading
    }


viewForm : LoggedIn.Model -> List Balance -> ImageStatus -> Bool -> Bool -> DeleteModalStatus -> Form -> Html Msg
viewForm ({ shared } as loggedIn) balances imageStatus isEdit isDisabled deleteModal form =
    let
        t =
            I18Next.t shared.translations

        fieldId s =
            "shop-editor-" ++ s

        imageStyle =
            case getInput form.image of
                Just url ->
                    style "background-image" ("url(" ++ url ++ ")")

                Nothing ->
                    style "" ""

        imageView =
            case imageStatus of
                Uploading ->
                    [ div
                        [ class "spinner" ]
                        []
                    ]

                _ ->
                    [ Icon.addPhoto ""
                    , span
                        []
                        [ text (t "shop.photo_label") ]
                    ]

        trackStock =
            getInput form.trackStock

        ( actionText, pageTitle ) =
            if isEdit then
                ( t "menu.save", t "shop.edit_offer" )

            else
                ( t "menu.create", t "shop.create_offer" )
    in
    div []
        [ Page.viewHeader loggedIn pageTitle (Route.Shop Shop.All)
        , div
            [ class "container mx-auto px-4 py-2 max-w-screen-md" ]
            [ div
                [ class "bg-white rounded-lg" ]
                [ div [ class "px-4 py-6" ]
                    [ div [ class "text-heading font-medium" ] [ text pageTitle ]
                    , if isEdit then
                        button
                            [ class "btn delete-button"
                            , disabled isDisabled
                            , onClick ClickedDelete
                            ]
                            [ text (t "shop.delete") ]

                      else
                        text ""
                    , if isEdit && deleteModal == Open then
                        viewConfirmDeleteModal t

                      else
                        text ""
                    ]
                , div
                    [ class "shop-editor__image-upload w-full px-4 mb-10" ]
                    [ input
                        [ id (fieldId "image")
                        , class "hidden-img-input"
                        , type_ "file"
                        , accept "image/*"
                        , Page.onFileChange EnteredImage
                        , multiple False
                        , disabled isDisabled
                        ]
                        []
                    , label
                        [ for (fieldId "image")
                        , imageStyle
                        ]
                        imageView
                    ]
                , div [ class "px-4 flex flex-col" ]
                    [ formField
                        [ div
                            [ class "input-label" ]
                            [ text (t "shop.what_label") ]
                        , input
                            [ class "input w-full"
                            , classList [ ( "field-with-error", hasErrors form.title ) ]
                            , type_ "text"
                            , id (fieldId "title")
                            , value (getInput form.title)
                            , maxlength 255
                            , onInput EnteredTitle
                            , required True
                            , disabled isDisabled
                            ]
                            []
                        , viewFieldErrors (listErrors shared.translations form.title)
                        ]
                    , formField
                        [ div
                            [ class "input-label" ]
                            [ text (t "shop.description_label") ]
                        , textarea
                            [ class "input textarea-input w-full"
                            , classList [ ( "field-with-error", hasErrors form.description ) ]
                            , id (fieldId "description")
                            , value (getInput form.description)
                            , maxlength 255
                            , onInput EnteredDescription
                            , required True
                            , disabled isDisabled
                            ]
                            []
                        , viewFieldErrors (listErrors shared.translations form.description)
                        ]
                    , formField
                        [ div
                            [ class "input-label" ]
                            [ text (t "shop.track_stock_label") ]
                        , select
                            [ class "form-select select w-full"
                            , id (fieldId "trackStock")
                            , required True
                            , disabled isDisabled
                            , on "change"
                                (Decode.map EnteredTrackStock Html.Events.targetValue)
                            ]
                            [ option
                                [ value trackYes
                                , selected (trackStock == Just trackYes)
                                ]
                                [ text (t "shop.track_stock_yes") ]
                            , option
                                [ value trackNo
                                , selected (trackStock == Just trackNo)
                                ]
                                [ text (t "shop.track_stock_no") ]
                            ]
                        , viewFieldErrors (listErrors shared.translations form.trackStock)
                        ]
                    , if trackStock == Just trackYes then
                        formField
                            [ div
                                [ class "input-label" ]
                                [ text (t "shop.units_label") ]
                            , input
                                [ class "input w-full"
                                , classList [ ( "field-with-error", hasErrors form.units ) ]
                                , type_ "number"
                                , id (fieldId "units")
                                , value (getInput form.units)
                                , onInput EnteredUnits
                                , required True
                                , disabled isDisabled
                                , Html.Attributes.min "0"
                                ]
                                []
                            , viewFieldErrors (listErrors shared.translations form.units)
                            ]

                      else
                        text ""
                    , formField
                        [ div
                            [ class "input-label" ]
                            [ text (t "shop.price_label") ]
                        , input
                            [ class "input w-full"
                            , classList [ ( "field-with-error", hasErrors form.price ) ]
                            , id (fieldId "price")
                            , value (getInput form.price)
                            , onInput EnteredPrice
                            , required True
                            , disabled isDisabled
                            , Html.Attributes.min "0"
                            ]
                            []
                        , viewFieldErrors (listErrors shared.translations form.price)
                        ]
                    , case form.error of
                        Nothing ->
                            text ""

                        Just err ->
                            viewFieldErrors [ err ]
                    , div
                        [ class "flex align-center justify-center mb-10"
                        , disabled (isDisabled || imageStatus == Uploading)
                        ]
                        [ button
                            [ class "button button-primary w-full sm:w-40"
                            , onClick ClickedSave
                            ]
                            [ text actionText ]
                        ]
                    ]
                ]
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
                ]
                [ text (t "shop.delete_modal.cancel") ]
            , button
                [ class "modal-accept"
                , onClick ClickedDeleteConfirm
                ]
                [ text (t "shop.delete_modal.confirm") ]
            ]
        |> Modal.toHtml


formField : List (Html msg) -> Html msg
formField =
    div [ class "mb-10" ]


viewFieldErrors : List String -> Html msg
viewFieldErrors errors =
    let
        viewErrors =
            List.map
                (\error ->
                    span
                        [ class "form-error" ]
                        [ text error ]
                )
                errors
    in
    div
        [ class "form-error" ]
        viewErrors



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = CompletedBalancesLoad (Result Http.Error (List Balance))
    | CompletedSaleLoad (Result (Graphql.Http.Error (Maybe Sale)) (Maybe Sale))
    | CompletedImageUpload (Result Http.Error String)
    | EnteredImage (List File)
    | EnteredTitle String
    | EnteredDescription String
    | EnteredTrackStock String
    | EnteredUnits String
    | EnteredPrice String
    | ClickedSave
    | ClickedDelete
    | ClickedDeleteConfirm
    | ClickedDeleteCancel
    | GotSaveResponse (Result Value String)
    | GotDeleteResponse (Result Value String)
    | PressedEnter Bool


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    let
        t =
            I18Next.t loggedIn.shared.translations
    in
    case msg of
        CompletedBalancesLoad (Ok balances) ->
            case model of
                LoadingBalancesCreate ->
                    let
                        balanceOptions =
                            List.map
                                (\balance ->
                                    Eos.symbolToString balance.asset.symbol
                                )
                                balances
                    in
                    EditingCreate balances NoImage (initForm balanceOptions)
                        |> UR.init

                LoadingBalancesUpdate saleId ->
                    let
                        saleFetch =
                            case String.toInt saleId of
                                Nothing ->
                                    Cmd.none

                                Just id ->
                                    Api.Graphql.query loggedIn.shared (Shop.saleQuery id) CompletedSaleLoad
                    in
                    LoadingSaleUpdate balances saleId
                        |> UR.init
                        |> UR.addCmd saleFetch

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg []

        CompletedBalancesLoad (Err error) ->
            LoadBalancesFailed error
                |> UR.init
                |> UR.logHttpError msg error

        CompletedSaleLoad (Ok maybeSale) ->
            case ( model, maybeSale ) of
                ( LoadingSaleUpdate balances _, Just sale ) ->
                    let
                        balanceOptions =
                            List.map
                                (\balance ->
                                    Eos.symbolToString balance.asset.symbol
                                )
                                balances

                        trackStock =
                            if sale.trackStock then
                                trackYes

                            else
                                trackNo
                    in
                    EditingUpdate balances sale NoImage Closed (initForm balanceOptions)
                        |> updateForm
                            (\form ->
                                { form
                                    | image = updateInput sale.image form.image
                                    , title = updateInput sale.title form.title
                                    , description = updateInput sale.description form.description
                                    , trackStock = updateInput (Just trackStock) form.trackStock
                                    , units = updateInput (String.fromInt sale.units) form.units
                                    , price = updateInput (String.fromFloat sale.price) form.price
                                }
                            )
                        |> UR.init

                ( _, _ ) ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg []

        CompletedSaleLoad (Err error) ->
            LoadSaleFailed error
                |> UR.init
                |> UR.logGraphqlError msg error

        CompletedImageUpload (Ok url) ->
            case model of
                EditingCreate balances _ form ->
                    EditingCreate balances (Uploaded url) form
                        |> updateForm
                            (\form_ ->
                                { form_ | image = updateInput (Just url) form_.image }
                            )
                        |> UR.init

                EditingUpdate balances sale _ _ form ->
                    EditingUpdate balances sale (Uploaded url) Closed form
                        |> updateForm
                            (\form_ ->
                                { form_ | image = updateInput (Just url) form_.image }
                            )
                        |> UR.init

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg []

        CompletedImageUpload (Err error) ->
            case model of
                EditingCreate balances _ form ->
                    EditingCreate balances (UploadFailed error) form
                        |> UR.init
                        |> UR.logHttpError msg error

                EditingUpdate balances sale _ _ form ->
                    EditingUpdate balances sale (UploadFailed error) Closed form
                        |> UR.init
                        |> UR.logHttpError msg error

                _ ->
                    model
                        |> UR.init
                        |> UR.logHttpError msg error

        EnteredImage (file :: _) ->
            let
                uploadImage =
                    Api.uploadImage loggedIn.shared file CompletedImageUpload
            in
            case model of
                EditingCreate balances _ form ->
                    EditingCreate balances Uploading form
                        |> UR.init
                        |> UR.addCmd uploadImage

                EditingUpdate balances sale _ _ form ->
                    EditingUpdate balances sale Uploading Closed form
                        |> UR.init
                        |> UR.addCmd uploadImage

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg []

        EnteredImage _ ->
            model
                |> UR.init

        EnteredTitle title ->
            model
                |> updateForm
                    (\form ->
                        { form | title = updateInput title form.title }
                    )
                |> UR.init

        EnteredDescription description ->
            if String.length description > 255 then
                UR.init model

            else
                model
                    |> updateForm
                        (\form ->
                            { form | description = updateInput description form.description }
                        )
                    |> UR.init

        EnteredTrackStock trackStock ->
            model
                |> updateForm
                    (\form ->
                        { form | trackStock = updateInput (Just trackStock) form.trackStock }
                    )
                |> UR.init

        EnteredUnits units ->
            model
                |> updateForm
                    (\form ->
                        { form | units = updateInput units form.units }
                    )
                |> UR.init

        EnteredPrice value ->
            model
                |> updateForm
                    (\form ->
                        { form | price = updateInput (getNumericValues value) form.price }
                    )
                |> UR.init

        ClickedSave ->
            if LoggedIn.isAuth loggedIn then
                let
                    validatedModel =
                        model
                            |> updateForm validateForm
                in
                case validatedModel of
                    EditingCreate balances (Uploaded url) form ->
                        if isValidForm form then
                            performRequest
                                ClickedSave
                                (Creating balances (Uploaded url) form)
                                loggedIn
                                "createsale"
                                (encodeCreateForm loggedIn form)

                        else
                            validatedModel
                                |> UR.init

                    EditingCreate balances (UploadFailed error) form ->
                        if isValidForm form then
                            performRequest
                                ClickedSave
                                (Creating balances (UploadFailed error) form)
                                loggedIn
                                "createsale"
                                (encodeCreateForm loggedIn form)

                        else
                            validatedModel
                                |> UR.init

                    EditingCreate balances NoImage form ->
                        if isValidForm form then
                            performRequest
                                ClickedSave
                                (Creating balances NoImage form)
                                loggedIn
                                "createsale"
                                (encodeCreateForm loggedIn form)

                        else
                            validatedModel
                                |> UR.init

                    EditingUpdate balances sale (Uploaded url) _ form ->
                        if isValidForm form then
                            performRequest
                                ClickedSave
                                (Saving balances sale (Uploaded url) form)
                                loggedIn
                                "updatesale"
                                (encodeUpdateForm sale form loggedIn.selectedCommunity)

                        else
                            validatedModel
                                |> UR.init

                    EditingUpdate balances sale (UploadFailed error) _ form ->
                        if isValidForm form then
                            performRequest
                                ClickedSave
                                (Saving balances sale (UploadFailed error) form)
                                loggedIn
                                "updatesale"
                                (encodeUpdateForm sale form loggedIn.selectedCommunity)

                        else
                            validatedModel
                                |> UR.init

                    EditingUpdate balances sale NoImage _ form ->
                        if isValidForm form then
                            performRequest
                                ClickedSave
                                (Saving balances sale NoImage form)
                                loggedIn
                                "updatesale"
                                (encodeUpdateForm sale form loggedIn.selectedCommunity)

                        else
                            validatedModel
                                |> UR.init

                    _ ->
                        validatedModel
                            |> UR.init
                            |> UR.logImpossible msg []

            else
                model
                    |> UR.init
                    |> UR.addExt (RequiredAuthentication (Just ClickedSave))

        ClickedDelete ->
            case model of
                EditingUpdate balances sale imageStatus _ form ->
                    EditingUpdate balances sale imageStatus Open form
                        |> UR.init

                _ ->
                    UR.init model

        ClickedDeleteCancel ->
            case model of
                EditingUpdate balances sale imageStatus _ form ->
                    EditingUpdate balances sale imageStatus Closed form
                        |> UR.init

                _ ->
                    UR.init model

        ClickedDeleteConfirm ->
            if LoggedIn.isAuth loggedIn then
                case model of
                    EditingUpdate balances sale (Uploaded url) _ form ->
                        performRequest
                            ClickedDeleteConfirm
                            (Deleting balances sale (Uploaded url) form)
                            loggedIn
                            "deletesale"
                            (encodeDeleteForm sale)

                    EditingUpdate balances sale (UploadFailed error) _ form ->
                        performRequest
                            ClickedDeleteConfirm
                            (Deleting balances sale (UploadFailed error) form)
                            loggedIn
                            "deletesale"
                            (encodeDeleteForm sale)

                    EditingUpdate balances sale NoImage _ form ->
                        performRequest
                            ClickedDeleteConfirm
                            (Deleting balances sale NoImage form)
                            loggedIn
                            "deletesale"
                            (encodeDeleteForm sale)

                    _ ->
                        model
                            |> UR.init
                            |> UR.logImpossible msg []

            else
                model
                    |> UR.init
                    |> UR.addExt (RequiredAuthentication (Just ClickedDeleteConfirm))

        GotSaveResponse (Ok _) ->
            UR.init model
                |> UR.addCmd
                    (Route.replaceUrl loggedIn.shared.navKey (Route.Shop Shop.All))
                |> UR.addExt (ShowFeedback Success (t "shop.create_offer_success"))

        GotSaveResponse (Err error) ->
            let
                internalError =
                    I18Next.t loggedIn.shared.translations "error.unknown"
            in
            case model of
                Creating balances imageStatus form ->
                    EditingCreate balances imageStatus form
                        |> updateForm
                            (\form_ ->
                                { form_
                                    | error = Just internalError
                                }
                            )
                        |> UR.init
                        |> UR.logDebugValue msg error

                Saving balances sale imageStatus form ->
                    EditingUpdate balances sale imageStatus Closed form
                        |> updateForm
                            (\form_ ->
                                { form_
                                    | error = Just internalError
                                }
                            )
                        |> UR.init
                        |> UR.logDebugValue msg error

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg []

        GotDeleteResponse (Ok _) ->
            model
                |> UR.init
                |> UR.addCmd
                    (Route.replaceUrl loggedIn.shared.navKey (Route.Shop Shop.All))
                |> UR.addExt (ShowFeedback Success (t "shop.delete_offer_success"))

        GotDeleteResponse (Err error) ->
            let
                internalError =
                    I18Next.t loggedIn.shared.translations "error.unknown"
            in
            case model of
                Deleting balances sale imageStatus form ->
                    EditingUpdate balances sale imageStatus Closed form
                        |> updateForm
                            (\form_ ->
                                { form_
                                    | error = Just internalError
                                }
                            )
                        |> UR.init
                        |> UR.logDebugValue msg error

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg []

        PressedEnter val ->
            if val then
                UR.init model
                    |> UR.addCmd
                        (Task.succeed ClickedSave
                            |> Task.perform identity
                        )

            else
                UR.init model


performRequest : Msg -> Status -> LoggedIn.Model -> String -> Value -> UpdateResult
performRequest msg status { shared, accountName, selectedCommunity } action data =
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


updateForm : (Form -> Form) -> Model -> Model
updateForm transform model =
    case model of
        EditingCreate balances imageStatus form ->
            EditingCreate balances imageStatus (transform form)

        Creating balances imageStatus form ->
            Creating balances imageStatus (transform form)

        EditingUpdate balances sale imageStatus confirmDelete form ->
            EditingUpdate balances sale imageStatus confirmDelete (transform form)

        Saving balances sale imageStatus form ->
            Saving balances sale imageStatus (transform form)

        Deleting balances sale imageStatus form ->
            Deleting balances sale imageStatus (transform form)

        _ ->
            model


validateForm : Form -> Form
validateForm form =
    { form
        | image = validate form.image
        , title = validate form.title
        , description = validate form.description
        , trackStock = validate form.trackStock
        , units = validate form.units
        , price = validate form.price
    }


isValidForm : Form -> Bool
isValidForm form =
    hasErrors form.image
        || hasErrors form.title
        || hasErrors form.description
        || hasErrors form.trackStock
        || hasErrors form.units
        || hasErrors form.price
        |> not


encodeCreateForm : LoggedIn.Model -> Form -> Value
encodeCreateForm loggedIn form =
    let
        creator =
            Eos.encodeName loggedIn.accountName

        image =
            Maybe.withDefault "" (getInput form.image)
                |> Encode.string

        title =
            form.title
                |> getInput
                |> Encode.string

        description =
            form.description
                |> getInput
                |> Encode.string

        price =
            String.toFloat (getInput form.price)

        quantity =
            case Maybe.map2 (\p s -> Eos.Asset p s) price (Just loggedIn.selectedCommunity) of
                Nothing ->
                    Encode.string ""

                Just asset ->
                    Eos.encodeAsset asset

        trackStock =
            if getInput form.trackStock == Just trackYes then
                True
                    |> Eos.boolToEosBool
                    |> Eos.encodeEosBool

            else
                False
                    |> Eos.boolToEosBool
                    |> Eos.encodeEosBool

        units =
            case String.toInt (getInput form.units) of
                Nothing ->
                    Encode.int 0

                Just units_ ->
                    Encode.int units_
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


encodeUpdateForm : Sale -> Form -> Symbol -> Value
encodeUpdateForm sale form selectedCommunity =
    let
        saleId =
            Encode.int sale.id

        image =
            Maybe.withDefault "" (getInput form.image)
                |> Encode.string

        title =
            form.title
                |> getInput
                |> Encode.string

        description =
            form.description
                |> getInput
                |> Encode.string

        price =
            String.toFloat (getInput form.price)

        quantity =
            case Maybe.map2 Eos.Asset price (Just selectedCommunity) of
                Nothing ->
                    Encode.string ""

                Just asset ->
                    Eos.encodeAsset asset

        trackStock =
            if getInput form.trackStock == Just trackYes then
                True
                    |> Eos.boolToEosBool
                    |> Eos.encodeEosBool

            else
                False
                    |> Eos.boolToEosBool
                    |> Eos.encodeEosBool

        units =
            case String.toInt (getInput form.units) of
                Nothing ->
                    Encode.int 0

                Just units_ ->
                    Encode.int units_
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


getNumericValues : String -> String
getNumericValues value =
    value
        |> String.toList
        |> List.filter Char.isDigit
        |> List.map String.fromChar
        |> String.join ""


encodeDeleteForm : Sale -> Value
encodeDeleteForm sale =
    Encode.object
        [ ( "sale_id", Encode.int sale.id )
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
            [ "CompletedSaleLoad", UR.resultToString r ]

        CompletedImageUpload r ->
            [ "CompletedImageUpload", UR.resultToString r ]

        EnteredImage _ ->
            [ "EnteredImage" ]

        EnteredTitle _ ->
            [ "EnteredTitle" ]

        EnteredDescription _ ->
            [ "EnteredDescription" ]

        EnteredTrackStock _ ->
            [ "EnteredTrackStock" ]

        EnteredUnits _ ->
            [ "EnteredUnit" ]

        EnteredPrice _ ->
            [ "EnteredPrice" ]

        ClickedSave ->
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

        PressedEnter _ ->
            [ "PressedEnter" ]
