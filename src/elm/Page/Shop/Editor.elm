module Page.Shop.Editor exposing
    ( Model
    , Msg(..)
    , initCreate
    , initUpdate
    , jsAddressToMsg
    , msgToString
    , subscriptions
    , update
    , view
    )

import Api
import Api.Graphql
import Browser.Events as Events
import Community exposing (Balance)
import DataValidator exposing (Validator, getInput, greaterThanOrEqual, hasErrors, listErrors, longerThan, lowerThanOrEqual, newValidator, oneOf, updateInput, validate)
import Eos exposing (Symbol)
import Eos.Account as Eos
import File exposing (File)
import Graphql.Http
import Html exposing (Html, button, div, p, span, text)
import Html.Attributes exposing (class, disabled, id, maxlength, required, rows, value)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Page
import RemoteData exposing (RemoteData)
import Result exposing (Result)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Shop exposing (Product, ProductId)
import Task
import UpdateResult as UR
import Utils exposing (decodeEnterKeyDown)
import View.Feedback as Feedback
import View.Form.FileUploader as FileUploader
import View.Form.Input as Input
import View.Form.Select as Select
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
    | LoadingBalancesUpdate ProductId
    | LoadingSaleUpdate (List Balance)
    | EditingUpdate (List Balance) Product ImageStatus DeleteModalStatus Form
    | Saving (List Balance) Product ImageStatus Form
    | Deleting (List Balance) Product ImageStatus Form
      -- Errors
    | LoadBalancesFailed Http.Error
    | LoadSaleFailed (Graphql.Http.Error (Maybe Product))


type DeleteModalStatus
    = Open
    | Closed


type alias FormError =
    Maybe String


type alias ImageStatus =
    RemoteData Http.Error String


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


initForm : Form
initForm =
    let
        image =
            newValidator Nothing identity False []

        title =
            []
                |> longerThan 3
                |> newValidator "" (\v -> Just v) True

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
            shared.translators.t

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
                    Page.fullPageLoading shared

                LoadingBalancesUpdate _ ->
                    Page.fullPageLoading shared

                LoadingSaleUpdate _ ->
                    Page.fullPageLoading shared

                LoadBalancesFailed error ->
                    Page.fullPageError (t "shop.title") error

                LoadSaleFailed error ->
                    Page.fullPageGraphQLError (t "shop.title") error

                EditingCreate _ imageStatus form ->
                    viewForm loggedIn imageStatus False False Closed form

                Creating _ imageStatus form ->
                    viewForm loggedIn imageStatus False True Closed form

                EditingUpdate _ _ imageStatus confirmDelete form ->
                    viewForm loggedIn imageStatus True False confirmDelete form

                Saving _ _ imageStatus form ->
                    viewForm loggedIn imageStatus True True Closed form

                Deleting _ _ imageStatus form ->
                    viewForm loggedIn imageStatus True True Closed form
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


viewForm : LoggedIn.Model -> ImageStatus -> Bool -> Bool -> DeleteModalStatus -> Form -> Html Msg
viewForm ({ shared } as loggedIn) imageStatus isEdit isDisabled deleteModal form =
    let
        t =
            shared.translators.t

        fieldId s =
            "shop-editor-" ++ s

        trackStock =
            getInput form.trackStock

        ( actionText, pageTitle ) =
            if isEdit then
                ( t "menu.save", t "shop.edit_offer" )

            else
                ( t "menu.create", t "shop.create_offer" )
    in
    div [ class "bg-white" ]
        [ Page.viewHeader loggedIn pageTitle
        , div
            [ class "container mx-auto" ]
            [ div [ class "px-4 py-6" ]
                [ if isEdit then
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
            , FileUploader.init
                { label = ""
                , id = fieldId "image"
                , onFileInput = EnteredImage
                , status = imageStatus
                }
                |> FileUploader.withBackground FileUploader.Gray
                |> FileUploader.withAttrs [ class "px-4 mb-10" ]
                |> FileUploader.toHtml shared.translators
            , div [ class "px-4 flex flex-col" ]
                [ Input.init
                    { label = t "shop.what_label"
                    , id = fieldId "title"
                    , onInput = EnteredTitle
                    , disabled = isDisabled
                    , value = getInput form.title
                    , placeholder = Nothing
                    , problems = listErrors shared.translations form.title |> Just
                    , translators = shared.translators
                    }
                    |> Input.withAttrs [ maxlength 255, required True ]
                    |> Input.toHtml
                , Input.init
                    { label = t "shop.description_label"
                    , id = fieldId "description"
                    , onInput = EnteredDescription
                    , disabled = isDisabled
                    , value = getInput form.description
                    , placeholder = Nothing
                    , problems = listErrors shared.translations form.description |> Just
                    , translators = shared.translators
                    }
                    |> Input.withAttrs [ maxlength 255, required True, rows 5 ]
                    |> Input.withCounter 255
                    |> Input.withInputType Input.TextArea
                    |> Input.toHtml
                , Select.init
                    { id = fieldId "trackStock"
                    , label = t "shop.track_stock_label"
                    , onInput = EnteredTrackStock
                    , firstOption = { value = trackNo, label = t "shop.track_stock_no" }
                    , value = trackStock |> Maybe.withDefault trackNo
                    , valueToString = identity
                    , disabled = isDisabled
                    , problems = listErrors shared.translations form.trackStock |> Just
                    }
                    |> Select.withOption { value = trackYes, label = t "shop.track_stock_yes" }
                    |> Select.withAttrs [ required True ]
                    |> Select.toHtml
                , if trackStock == Just trackYes then
                    Input.init
                        { label = t "shop.units_label"
                        , id = fieldId "units"
                        , onInput = EnteredUnits
                        , disabled = isDisabled
                        , value = getInput form.units
                        , placeholder = Nothing
                        , problems = listErrors shared.translations form.units |> Just
                        , translators = shared.translators
                        }
                        |> Input.asNumeric
                        |> Input.withType Input.Number
                        |> Input.withAttrs [ required True, Html.Attributes.min "0", Html.Attributes.max "2000" ]
                        |> Input.toHtml

                  else
                    text ""
                , Input.init
                    { label = t "shop.price_label"
                    , id = fieldId "price"
                    , onInput = EnteredPrice
                    , disabled = isDisabled
                    , value = getInput form.price
                    , placeholder = Nothing
                    , problems = listErrors shared.translations form.price |> Just
                    , translators = shared.translators
                    }
                    |> (case loggedIn.selectedCommunity of
                            RemoteData.Success community ->
                                Input.withCurrency community.symbol

                            _ ->
                                identity
                       )
                    |> Input.withAttrs [ required True, Html.Attributes.min "0" ]
                    |> Input.toHtml
                , case form.error of
                    Nothing ->
                        text ""

                    Just err ->
                        viewFieldErrors [ err ]
                , div
                    [ class "flex align-center justify-center mb-10"
                    , disabled (isDisabled || RemoteData.isLoading imageStatus)
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


viewFieldErrors : List String -> Html msg
viewFieldErrors errors =
    let
        viewErrors =
            List.map
                (\error -> span [ class "form-error" ] [ text error ])
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
    | CompletedSaleLoad (RemoteData (Graphql.Http.Error (Maybe Product)) (Maybe Product))
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
                    EditingCreate balances RemoteData.NotAsked initForm
                        |> UR.init

                LoadingBalancesUpdate saleId ->
                    let
                        saleFetch =
                            case String.toInt saleId of
                                Nothing ->
                                    Cmd.none

                                Just id ->
                                    Api.Graphql.query loggedIn.shared
                                        (Just loggedIn.authToken)
                                        (Shop.productQuery id)
                                        CompletedSaleLoad
                    in
                    LoadingSaleUpdate balances
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

        CompletedSaleLoad (RemoteData.Success maybeSale) ->
            case ( model, maybeSale ) of
                ( LoadingSaleUpdate balances, Just sale ) ->
                    let
                        trackStock =
                            if sale.trackStock then
                                trackYes

                            else
                                trackNo
                    in
                    EditingUpdate balances sale RemoteData.NotAsked Closed initForm
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

        CompletedSaleLoad (RemoteData.Failure error) ->
            LoadSaleFailed error
                |> UR.init
                |> UR.logGraphqlError msg error

        CompletedSaleLoad _ ->
            UR.init model

        CompletedImageUpload (Ok url) ->
            case model of
                EditingCreate balances _ form ->
                    EditingCreate balances (RemoteData.Success url) form
                        |> updateForm
                            (\form_ ->
                                { form_ | image = updateInput (Just url) form_.image }
                            )
                        |> UR.init

                EditingUpdate balances sale _ _ form ->
                    EditingUpdate balances sale (RemoteData.Success url) Closed form
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
                    EditingCreate balances (RemoteData.Failure error) form
                        |> UR.init
                        |> UR.logHttpError msg error
                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure (t "error.invalid_image_file"))

                EditingUpdate balances sale _ _ form ->
                    EditingUpdate balances sale (RemoteData.Failure error) Closed form
                        |> UR.init
                        |> UR.logHttpError msg error
                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure (t "error.invalid_image_file"))

                _ ->
                    model
                        |> UR.init
                        |> UR.logHttpError msg error
                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure (t "error.invalid_image_file"))

        EnteredImage (file :: _) ->
            let
                uploadImage =
                    Api.uploadImage loggedIn.shared file CompletedImageUpload
            in
            case model of
                EditingCreate balances _ form ->
                    EditingCreate balances RemoteData.Loading form
                        |> UR.init
                        |> UR.addCmd uploadImage

                EditingUpdate balances sale _ _ form ->
                    EditingUpdate balances sale RemoteData.Loading Closed form
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
            let
                trimmedUnits =
                    if String.length units > 4 then
                        String.left 4 units

                    else
                        units
            in
            model
                |> updateForm
                    (\form ->
                        { form | units = updateInput trimmedUnits form.units }
                    )
                |> UR.init

        EnteredPrice value ->
            let
                trimmedPrice =
                    if String.length value > 12 then
                        String.left 12 value

                    else
                        value
            in
            model
                |> updateForm
                    (\form ->
                        { form | price = updateInput (getNumericValues trimmedPrice) form.price }
                    )
                |> UR.init

        ClickedSave ->
            let
                validatedModel =
                    model
                        |> updateForm validateForm
            in
            case validatedModel of
                EditingCreate balances (RemoteData.Success url) form ->
                    if isValidForm form then
                        performRequest
                            ClickedSave
                            (Creating balances (RemoteData.Success url) form)
                            loggedIn
                            "createsale"
                            (encodeCreateForm loggedIn form)
                            |> LoggedIn.withAuthentication loggedIn
                                model
                                { successMsg = msg, errorMsg = ClosedAuthModal }

                    else
                        validatedModel
                            |> UR.init

                EditingCreate balances (RemoteData.Failure error) form ->
                    if isValidForm form then
                        performRequest
                            ClickedSave
                            (Creating balances (RemoteData.Failure error) form)
                            loggedIn
                            "createsale"
                            (encodeCreateForm loggedIn form)
                            |> LoggedIn.withAuthentication loggedIn
                                model
                                { successMsg = msg, errorMsg = ClosedAuthModal }

                    else
                        validatedModel
                            |> UR.init

                EditingCreate balances RemoteData.NotAsked form ->
                    if isValidForm form then
                        performRequest
                            ClickedSave
                            (Creating balances RemoteData.NotAsked form)
                            loggedIn
                            "createsale"
                            (encodeCreateForm loggedIn form)
                            |> LoggedIn.withAuthentication loggedIn
                                model
                                { successMsg = msg, errorMsg = ClosedAuthModal }

                    else
                        validatedModel
                            |> UR.init

                EditingUpdate balances sale (RemoteData.Success url) _ form ->
                    case loggedIn.selectedCommunity of
                        RemoteData.Success community ->
                            if isValidForm form then
                                performRequest
                                    ClickedSave
                                    (Saving balances sale (RemoteData.Success url) form)
                                    loggedIn
                                    "updatesale"
                                    (encodeUpdateForm sale form community.symbol)
                                    |> LoggedIn.withAuthentication loggedIn
                                        model
                                        { successMsg = msg, errorMsg = ClosedAuthModal }

                            else
                                validatedModel
                                    |> UR.init

                        _ ->
                            validatedModel |> UR.init

                EditingUpdate balances sale (RemoteData.Failure error) _ form ->
                    case loggedIn.selectedCommunity of
                        RemoteData.Success community ->
                            if isValidForm form then
                                performRequest
                                    ClickedSave
                                    (Saving balances sale (RemoteData.Failure error) form)
                                    loggedIn
                                    "updatesale"
                                    (encodeUpdateForm sale form community.symbol)
                                    |> LoggedIn.withAuthentication loggedIn
                                        model
                                        { successMsg = msg, errorMsg = ClosedAuthModal }

                            else
                                validatedModel
                                    |> UR.init

                        _ ->
                            validatedModel |> UR.init

                EditingUpdate balances sale RemoteData.NotAsked _ form ->
                    case loggedIn.selectedCommunity of
                        RemoteData.Success community ->
                            if isValidForm form then
                                performRequest
                                    ClickedSave
                                    (Saving balances sale RemoteData.NotAsked form)
                                    loggedIn
                                    "updatesale"
                                    (encodeUpdateForm sale form community.symbol)
                                    |> LoggedIn.withAuthentication loggedIn
                                        model
                                        { successMsg = msg, errorMsg = ClosedAuthModal }

                            else
                                validatedModel
                                    |> UR.init

                        _ ->
                            validatedModel |> UR.init

                _ ->
                    validatedModel
                        |> UR.init
                        |> UR.logImpossible msg []

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
            case model of
                EditingUpdate balances sale (RemoteData.Success url) _ form ->
                    performRequest
                        ClickedDeleteConfirm
                        (Deleting balances sale (RemoteData.Success url) form)
                        loggedIn
                        "deletesale"
                        (encodeDeleteForm sale)
                        |> LoggedIn.withAuthentication loggedIn
                            model
                            { successMsg = msg, errorMsg = ClosedAuthModal }

                EditingUpdate balances sale (RemoteData.Failure error) _ form ->
                    performRequest
                        ClickedDeleteConfirm
                        (Deleting balances sale (RemoteData.Failure error) form)
                        loggedIn
                        "deletesale"
                        (encodeDeleteForm sale)
                        |> LoggedIn.withAuthentication loggedIn
                            model
                            { successMsg = msg, errorMsg = ClosedAuthModal }

                EditingUpdate balances sale RemoteData.NotAsked _ form ->
                    performRequest
                        ClickedDeleteConfirm
                        (Deleting balances sale RemoteData.NotAsked form)
                        loggedIn
                        "deletesale"
                        (encodeDeleteForm sale)
                        |> LoggedIn.withAuthentication loggedIn
                            model
                            { successMsg = msg, errorMsg = ClosedAuthModal }

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg []

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
                |> UR.addExt (ShowFeedback Feedback.Success (t "shop.delete_offer_success"))

        GotDeleteResponse (Err error) ->
            let
                internalError =
                    loggedIn.shared.translators.t "error.unknown"
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

        ClosedAuthModal ->
            case model of
                EditingUpdate balances sale imageStatus _ form ->
                    EditingUpdate balances sale imageStatus Closed form
                        |> UR.init

                _ ->
                    UR.init model


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
            case Maybe.map2 (\p c -> Eos.Asset p c.symbol) price (RemoteData.toMaybe loggedIn.selectedCommunity) of
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
            if getInput form.trackStock == Just trackYes then
                case String.toInt (getInput form.units) of
                    Nothing ->
                        Encode.int 0

                    Just units_ ->
                        Encode.int units_

            else
                Encode.int 0
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


encodeUpdateForm : Product -> Form -> Symbol -> Value
encodeUpdateForm product form selectedCommunity =
    let
        saleId =
            Encode.int product.id

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
    String.filter Char.isDigit value


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
            [ "EnteredUnits" ]

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

        ClosedAuthModal ->
            [ "ClosedAuthModal" ]
