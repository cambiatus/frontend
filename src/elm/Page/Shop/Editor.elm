module Page.Shop.Editor exposing (Model, Msg(..), initCreate, initUpdate, jsAddressToMsg, msgToString, subscriptions, update, view)

import Api
import Api.Graphql
import Asset.Icon as Icon
import Community exposing (Balance)
import DataValidator exposing (Validator, getInput, greaterThanOrEqual, hasErrors, listErrors, longerThan, newValidator, oneOf, updateInput, validate)
import Eos exposing (Symbol)
import Eos.Account as Eos
import File exposing (File)
import Graphql.Http
import Html exposing (Attribute, Html, button, div, h3, input, label, option, select, span, text, textarea)
import Html.Attributes exposing (accept, attribute, class, disabled, for, hidden, id, maxlength, multiple, required, selected, style, type_, value)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Http
import I18Next
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Page
import Result exposing (Result)
import Route
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import Shop exposing (Sale, SaleId)
import UpdateResult as UR



-- INIT


initCreate : LoggedIn.Model -> ( Model, Cmd Msg )
initCreate loggedIn =
    ( { status = LoadingBalancesCreate
      }
    , Api.getBalances loggedIn.shared loggedIn.accountName CompletedBalancesLoad
    )


initUpdate : SaleId -> LoggedIn.Model -> ( Model, Cmd Msg )
initUpdate saleId loggedIn =
    ( { status = LoadingBalancesUpdate saleId
      }
    , Api.getBalances loggedIn.shared loggedIn.accountName CompletedBalancesLoad
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MODEL


type alias Model =
    { status : Status
    }


type
    Status
    -- Create
    = LoadingBalancesCreate
    | EditingCreate (List Balance) ImageStatus Form
    | Creating (List Balance) ImageStatus Form
      -- Update
    | LoadingBalancesUpdate SaleId
    | LoadingSaleUpdate (List Balance) SaleId
    | EditingUpdate (List Balance) Sale ImageStatus Form
    | Saving (List Balance) Sale ImageStatus Form
    | Deleting (List Balance) Sale ImageStatus Form
      -- Errors
    | LoadBalancesFailed Http.Error
    | LoadSaleFailed (Graphql.Http.Error (Maybe Sale))


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
    , symbol : Validator (Maybe Symbol)
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

        symbol =
            []
                |> oneOf balanceOptions
                |> newValidator Nothing (Maybe.map Eos.symbolToString) True

        trackStock =
            []
                |> oneOf [ trackYes, trackNo ]
                |> newValidator (Just trackNo) identity True

        units =
            []
                |> greaterThanOrEqual 0
                |> newValidator "0" (\v -> Just v) False

        price =
            []
                |> greaterThanOrEqual 0
                |> newValidator "" (\v -> Just v) True
    in
    { image = image
    , title = title
    , description = description
    , symbol = symbol
    , trackStock = trackStock
    , units = units
    , price = price
    , error = Nothing
    }



-- VIEW


view : LoggedIn.Model -> Model -> Html Msg
view loggedIn model =
    let
        shared =
            loggedIn.shared

        t =
            I18Next.t shared.translations
    in
    case model.status of
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
            viewForm shared balances imageStatus False False form

        Creating balances imageStatus form ->
            viewForm shared balances imageStatus False True form

        EditingUpdate balances _ imageStatus form ->
            viewForm shared balances imageStatus True False form

        Saving balances _ imageStatus form ->
            viewForm shared balances imageStatus True True form

        Deleting balances _ imageStatus form ->
            viewForm shared balances imageStatus True True form


viewForm : Shared -> List Balance -> ImageStatus -> Bool -> Bool -> Form -> Html Msg
viewForm shared balances imageStatus isEdit isDisabled form =
    let
        ipfsUrl =
            shared.endpoints.ipfs

        t =
            I18Next.t shared.translations

        fieldId s =
            "shop-editor-" ++ s

        imageStyle =
            case getInput form.image of
                Just hash ->
                    style "background-image" ("url(" ++ ipfsUrl ++ "/" ++ hash ++ ")")

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

        symbol =
            getInput form.symbol

        trackStock =
            getInput form.trackStock

        ( actionText, pageTitle ) =
            if isEdit then
                ( t "menu.save", t "shop.edit_offer" )

            else
                ( t "menu.create", t "shop.create_offer" )
    in
    div
        [ class "container mx-auto px-4 py-2" ]
        [ div
            [ class "bg-white rounded-lg" ]
            [ div [ class "px-4 py-6 border-b border-gray-500" ]
                [ div [ class "text-heading font-medium" ] [ text pageTitle ]
                , case isEdit of
                    True ->
                        button
                            [ class "btn delete-button"
                            , disabled isDisabled
                            , onClick ClickedDelete
                            ]
                            [ text (t "DELETE") ]

                    False ->
                        text ""
                ]
            , div
                [ class "shop-editor__image-upload w-full " ]
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
            , div [ class "px-4 py-6 flex flex-col" ]
                [ formField
                    [ div
                        [ class "input-label" ]
                        [ text (t "shop.what_label") ]
                    , viewFieldErrors (listErrors shared.translations form.title)
                    , input
                        [ class "input w-full"
                        , type_ "text"
                        , id (fieldId "title")
                        , value (getInput form.title)
                        , maxlength 255
                        , onInput EnteredTitle
                        , required True
                        , disabled isDisabled
                        ]
                        []
                    ]
                , formField
                    [ div
                        [ class "input-label" ]
                        [ text (t "shop.description_label") ]
                    , viewFieldErrors (listErrors shared.translations form.description)
                    , textarea
                        [ class "input w-full"
                        , id (fieldId "description")
                        , value (getInput form.description)
                        , maxlength 255
                        , onInput EnteredDescription
                        , required True
                        , disabled isDisabled
                        ]
                        []
                    ]
                , div [ class "mt-2" ]
                    [ formField
                        [ label
                            [ class "input-label" ]
                            [ text (t "shop.which_community_label") ]
                        , viewFieldErrors (listErrors shared.translations form.symbol)
                        , select
                            [ class "input w-full mb-2 form-select select"
                            , id (fieldId "symbol")
                            , required True
                            , disabled (isEdit || isDisabled)
                            , Html.Events.on "change"
                                (Decode.map
                                    (\symbolStr ->
                                        if String.isEmpty symbolStr then
                                            EnteredSymbol Nothing

                                        else
                                            EnteredSymbol (Just symbolStr)
                                    )
                                    Html.Events.targetValue
                                )
                            ]
                            (option
                                [ hidden True
                                , attribute "value" ""
                                , selected (symbol == Nothing)
                                ]
                                [ text (t "shop.choose_community_label") ]
                                :: List.map
                                    (\b ->
                                        option
                                            [ value (Eos.symbolToString b.asset.symbol)
                                            , selected (symbol == Just b.asset.symbol)
                                            ]
                                            [ text (Eos.symbolToString b.asset.symbol) ]
                                    )
                                    balances
                            )
                        ]
                    ]
                , div [ class "mt-2" ]
                    [ formField
                        [ div
                            [ class "input-label" ]
                            [ text (t "shop.track_stock_label") ]
                        , viewFieldErrors (listErrors shared.translations form.trackStock)
                        , select
                            [ class "form-select select w-1/2"
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
                        ]
                    , if trackStock == Just trackYes then
                        formField
                            [ div
                                [ class "input-label" ]
                                [ text (t "shop.units_label") ]
                            , viewFieldErrors (listErrors shared.translations form.units)
                            , input
                                [ class "input w-full"
                                , type_ "number"
                                , id (fieldId "units")
                                , value (getInput form.units)
                                , onInput EnteredUnits
                                , required True
                                , disabled isDisabled
                                , Html.Attributes.min "0"
                                ]
                                []
                            ]

                      else
                        text ""
                    ]
                , div
                    [ class "mt-2" ]
                    [ formField
                        [ span
                            [ class "input-label" ]
                            [ text (t "shop.price_label") ]
                        , viewFieldErrors (listErrors shared.translations form.price)
                        , input
                            [ class "input w-full"
                            , id (fieldId "price")
                            , value (getInput form.price)
                            , onInput EnteredPrice
                            , required True
                            , disabled isDisabled
                            , Html.Attributes.min "0"
                            ]
                            []
                        ]
                    ]
                , case form.error of
                    Nothing ->
                        text ""

                    Just err ->
                        viewFieldErrors [ err ]
                , div
                    [ class "flex align-center justify-center mt-6"
                    , disabled (isDisabled || imageStatus == Uploading)
                    ]
                    [ button
                        [ class "button button-primary"
                        , onClick ClickedSave
                        ]
                        [ text actionText ]
                    ]
                ]
            ]
        ]


formField : List (Html msg) -> Html msg
formField =
    div [ class "mb-2" ]


viewFieldErrors : List String -> Html msg
viewFieldErrors errors =
    let
        viewErrors =
            List.map
                (\error ->
                    span
                        [ class "field-error" ]
                        [ text error ]
                )
                errors
    in
    div
        [ class "form-field-error" ]
        viewErrors


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown toMsg =
    on "keydown" (Decode.map toMsg keyCode)



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
    | EnteredSymbol (Maybe String)
    | EnteredTrackStock String
    | EnteredUnits String
    | EnteredPrice String
    | ClickedSave
    | ClickedDelete
    | GotSaveResponse (Result Value String)
    | GotDeleteResponse (Result Value String)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        CompletedBalancesLoad (Ok balances) ->
            case model.status of
                LoadingBalancesCreate ->
                    let
                        balanceOptions =
                            List.map
                                (\balance ->
                                    Eos.symbolToString balance.asset.symbol
                                )
                                balances
                    in
                    model
                        |> updateStatus (EditingCreate balances NoImage (initForm balanceOptions))
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
                    model
                        |> updateStatus (LoadingSaleUpdate balances saleId)
                        |> UR.init
                        |> UR.addCmd saleFetch

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg []

        CompletedBalancesLoad (Err error) ->
            model
                |> updateStatus (LoadBalancesFailed error)
                |> UR.init
                |> UR.logHttpError msg error

        CompletedSaleLoad (Ok maybeSale) ->
            case ( model.status, maybeSale ) of
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
                    model
                        |> updateStatus (EditingUpdate balances sale NoImage (initForm balanceOptions))
                        |> updateForm
                            (\form ->
                                { form
                                    | image = updateInput sale.image form.image
                                    , title = updateInput sale.title form.title
                                    , description = updateInput sale.description form.description
                                    , symbol = updateInput (Just sale.symbol) form.symbol
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
            model
                |> updateStatus (LoadSaleFailed error)
                |> UR.init
                |> UR.logGraphqlError msg error

        CompletedImageUpload (Ok hash) ->
            case model.status of
                EditingCreate balances _ form ->
                    model
                        |> updateStatus (EditingCreate balances (Uploaded hash) form)
                        |> updateForm
                            (\form_ ->
                                { form_ | image = updateInput (Just hash) form_.image }
                            )
                        |> UR.init

                EditingUpdate balances sale _ form ->
                    model
                        |> updateStatus (EditingUpdate balances sale (Uploaded hash) form)
                        |> updateForm
                            (\form_ ->
                                { form_ | image = updateInput (Just hash) form_.image }
                            )
                        |> UR.init

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg []

        CompletedImageUpload (Err error) ->
            case model.status of
                EditingCreate balances _ form ->
                    model
                        |> updateStatus (EditingCreate balances (UploadFailed error) form)
                        |> UR.init
                        |> UR.logHttpError msg error

                EditingUpdate balances sale _ form ->
                    model
                        |> updateStatus (EditingUpdate balances sale (UploadFailed error) form)
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
            case model.status of
                EditingCreate balances _ form ->
                    model
                        |> updateStatus (EditingCreate balances Uploading form)
                        |> UR.init
                        |> UR.addCmd uploadImage

                EditingUpdate balances sale _ form ->
                    model
                        |> updateStatus (EditingUpdate balances sale Uploading form)
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
            model
                |> updateForm
                    (\form ->
                        { form | description = updateInput description form.description }
                    )
                |> UR.init

        EnteredSymbol symbolStr ->
            let
                maybeSymbol =
                    Maybe.andThen
                        Eos.symbolFromString
                        symbolStr
            in
            model
                |> updateForm
                    (\form ->
                        { form | symbol = updateInput maybeSymbol form.symbol }
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
                case validatedModel.status of
                    EditingCreate balances (Uploaded hash) form ->
                        if isValidForm form then
                            performRequest
                                ClickedSave
                                validatedModel
                                (Creating balances (Uploaded hash) form)
                                loggedIn.accountName
                                "createsale"
                                (encodeCreateForm loggedIn form)

                        else
                            validatedModel
                                |> UR.init

                    EditingCreate balances (UploadFailed error) form ->
                        if isValidForm form then
                            performRequest
                                ClickedSave
                                validatedModel
                                (Creating balances (UploadFailed error) form)
                                loggedIn.accountName
                                "createsale"
                                (encodeCreateForm loggedIn form)

                        else
                            validatedModel
                                |> UR.init

                    EditingCreate balances NoImage form ->
                        if isValidForm form then
                            performRequest
                                ClickedSave
                                validatedModel
                                (Creating balances NoImage form)
                                loggedIn.accountName
                                "createsale"
                                (encodeCreateForm loggedIn form)

                        else
                            validatedModel
                                |> UR.init

                    EditingUpdate balances sale (Uploaded hash) form ->
                        if isValidForm form then
                            performRequest
                                ClickedSave
                                validatedModel
                                (Saving balances sale (Uploaded hash) form)
                                loggedIn.accountName
                                "updatesale"
                                (encodeUpdateForm sale form)

                        else
                            validatedModel
                                |> UR.init

                    EditingUpdate balances sale (UploadFailed error) form ->
                        if isValidForm form then
                            performRequest
                                ClickedSave
                                validatedModel
                                (Saving balances sale (UploadFailed error) form)
                                loggedIn.accountName
                                "updatesale"
                                (encodeUpdateForm sale form)

                        else
                            validatedModel
                                |> UR.init

                    EditingUpdate balances sale NoImage form ->
                        if isValidForm form then
                            performRequest
                                ClickedSave
                                validatedModel
                                (Saving balances sale NoImage form)
                                loggedIn.accountName
                                "updatesale"
                                (encodeUpdateForm sale form)

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
            if LoggedIn.isAuth loggedIn then
                case model.status of
                    EditingUpdate balances sale (Uploaded hash) form ->
                        performRequest
                            ClickedDelete
                            model
                            (Deleting balances sale (Uploaded hash) form)
                            loggedIn.accountName
                            "deletesale"
                            (encodeDeleteForm sale)

                    EditingUpdate balances sale (UploadFailed error) form ->
                        performRequest
                            ClickedDelete
                            model
                            (Deleting balances sale (UploadFailed error) form)
                            loggedIn.accountName
                            "deletesale"
                            (encodeDeleteForm sale)

                    EditingUpdate balances sale NoImage form ->
                        performRequest
                            ClickedDelete
                            model
                            (Deleting balances sale NoImage form)
                            loggedIn.accountName
                            "deletesale"
                            (encodeDeleteForm sale)

                    _ ->
                        model
                            |> UR.init
                            |> UR.logImpossible msg []

            else
                model
                    |> UR.init
                    |> UR.addExt (RequiredAuthentication (Just ClickedDelete))

        GotSaveResponse (Ok _) ->
            UR.init model
                |> UR.addCmd
                    (Route.replaceUrl loggedIn.shared.navKey (Route.Shop Shop.All))

        GotSaveResponse (Err error) ->
            let
                internalError =
                    I18Next.t loggedIn.shared.translations "error.unknown"
            in
            case model.status of
                Creating balances imageStatus form ->
                    model
                        |> updateStatus (EditingCreate balances imageStatus form)
                        |> updateForm
                            (\form_ ->
                                { form_
                                    | error = Just internalError
                                }
                            )
                        |> UR.init
                        |> UR.logDebugValue msg error

                Saving balances sale imageStatus form ->
                    model
                        |> updateStatus (EditingUpdate balances sale imageStatus form)
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

        GotDeleteResponse (Err error) ->
            let
                internalError =
                    I18Next.t loggedIn.shared.translations "error.unknown"
            in
            case model.status of
                Deleting balances sale imageStatus form ->
                    model
                        |> updateStatus (EditingUpdate balances sale imageStatus form)
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


performRequest : Msg -> Model -> Status -> Eos.Name -> String -> Value -> UpdateResult
performRequest msg model status account action data =
    model
        |> updateStatus status
        |> UR.init
        |> UR.addPort
            { responseAddress = msg
            , responseData = Encode.null
            , data =
                Eos.encodeTransaction
                    { actions =
                        [ { accountName = "bes.cmm"
                          , name = action
                          , authorization =
                                { actor = account
                                , permissionName = Eos.samplePermission
                                }
                          , data = data
                          }
                        ]
                    }
            }


updateForm : (Form -> Form) -> Model -> Model
updateForm transform model =
    let
        status =
            case model.status of
                LoadingBalancesCreate ->
                    model.status

                LoadingBalancesUpdate _ ->
                    model.status

                LoadingSaleUpdate _ _ ->
                    model.status

                LoadBalancesFailed _ ->
                    model.status

                LoadSaleFailed _ ->
                    model.status

                EditingCreate balances imageStatus form ->
                    EditingCreate balances imageStatus (transform form)

                Creating balances imageStatus form ->
                    Creating balances imageStatus (transform form)

                EditingUpdate balances sale imageStatus form ->
                    EditingUpdate balances sale imageStatus (transform form)

                Saving balances sale imageStatus form ->
                    Saving balances sale imageStatus (transform form)

                Deleting balances sale imageStatus form ->
                    Deleting balances sale imageStatus (transform form)
    in
    { model | status = status }


updateStatus : Status -> Model -> Model
updateStatus status model =
    { model | status = status }


validateForm : Form -> Form
validateForm form =
    { form
        | image = validate form.image
        , title = validate form.title
        , description = validate form.description
        , symbol = validate form.symbol
        , trackStock = validate form.trackStock
        , units = validate form.units
        , price = validate form.price
    }


isValidForm : Form -> Bool
isValidForm form =
    hasErrors form.image
        || hasErrors form.title
        || hasErrors form.description
        || hasErrors form.symbol
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

        symbol =
            getInput form.symbol

        price =
            String.toFloat (getInput form.price)

        quantity =
            case Maybe.map2 (\p s -> Eos.Asset p s) price symbol of
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


encodeUpdateForm : Sale -> Form -> Value
encodeUpdateForm sale form =
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

        symbol =
            getInput form.symbol

        price =
            String.toFloat (getInput form.price)

        quantity =
            case Maybe.map2 Eos.Asset price symbol of
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

        "ClickedDelete" :: [] ->
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

        EnteredSymbol _ ->
            [ "EnteredSymbol" ]

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

        GotSaveResponse r ->
            [ "GotSaveResponse", UR.resultToString r ]

        GotDeleteResponse r ->
            [ "GotDeleteResponse", UR.resultToString r ]
