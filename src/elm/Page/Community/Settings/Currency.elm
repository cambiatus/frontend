module Page.Community.Settings.Currency exposing
    ( Model
    , Msg
    , init
    , jsAddressToMsg
    , msgToString
    , receiveBroadcast
    , update
    , view
    )

import Api
import Community
import Eos
import Eos.Account as Eos
import Html exposing (Html, br, button, div, form, span, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Page
import Ports
import RemoteData
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Translators)
import Token
import UpdateResult as UR
import View.Feedback as Feedback
import View.Form.Input as Input
import View.Form.Radio as Radio



-- MODEL


type alias Model =
    { minimumBalance : String
    , maximumSupply : String
    , tokenType : Token.TokenType
    , naturalExpirationPeriod : String
    , juridicalExpirationPeriod : String
    , renovationAmount : String
    , isLoading : Bool
    , errors : List ( Field, String )
    }


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( { minimumBalance = ""
      , maximumSupply = ""
      , tokenType = Token.Mcc
      , naturalExpirationPeriod = ""
      , juridicalExpirationPeriod = ""
      , renovationAmount = ""
      , isLoading = True
      , errors = []
      }
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )



-- UPDATE


type Msg
    = Ignored
    | EnteredMinimumBalance String
    | EnteredMaximumSupply String
    | EnteredNaturalExpirationPeriod String
    | EnteredJuridicalExpirationPeriod String
    | EnteredRenovationAmount String
    | ClickedSubmit
    | GotSubmitResponse (Result Encode.Value Token.UpdateTokenData)
    | CompletedLoadCommunity Community.Model
    | CompletedLoadExpiryOpts (Result Http.Error (Maybe Token.ExpiryOptsData))


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model ({ shared } as loggedIn) =
    case msg of
        Ignored ->
            UR.init model

        EnteredMinimumBalance minimumBalance ->
            { model | minimumBalance = minimumBalance }
                |> withSymbolValidation validateMinimumBalance MinimumBalance loggedIn
                |> UR.init

        EnteredMaximumSupply maximumSupply ->
            { model | maximumSupply = maximumSupply }
                |> withSymbolValidation validateMaximumSupply MaximumSupply loggedIn
                |> UR.init

        EnteredNaturalExpirationPeriod naturalExpirationPeriod ->
            { model | naturalExpirationPeriod = naturalExpirationPeriod }
                |> setErrors NaturalExpirationPeriod validateNaturalExpirationPeriod
                |> UR.init

        EnteredJuridicalExpirationPeriod juridicalExpirationPeriod ->
            { model | juridicalExpirationPeriod = juridicalExpirationPeriod }
                |> setErrors JuridicalExpirationPeriod validateJuridicalExpirationPeriod
                |> UR.init

        EnteredRenovationAmount renovationAmount ->
            { model | renovationAmount = renovationAmount }
                |> withSymbolValidation validateRenovationAmount RenovationAmount loggedIn
                |> UR.init

        ClickedSubmit ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    case validateModel community.symbol model of
                        Ok ( validUpdateTokenData, validExpiryOptsData ) ->
                            if LoggedIn.hasPrivateKey loggedIn then
                                { model | isLoading = True }
                                    |> UR.init
                                    |> UR.addPort (savePort validUpdateTokenData validExpiryOptsData loggedIn)

                            else
                                UR.init model
                                    |> UR.addExt
                                        (Just ClickedSubmit
                                            |> LoggedIn.RequiredAuthentication
                                        )

                        Err withError ->
                            UR.init withError

                _ ->
                    UR.init model
                        |> UR.logImpossible msg [ "CommunityNotLoaded" ]

        GotSubmitResponse (Ok updateTokenData) ->
            { model | isLoading = False }
                |> UR.init
                |> (case loggedIn.selectedCommunity of
                        RemoteData.Success community ->
                            UR.addExt
                                ({ community | minBalance = Just updateTokenData.minBalance.amount }
                                    |> LoggedIn.CommunityLoaded
                                    |> LoggedIn.ExternalBroadcast
                                )
                                >> UR.addExt
                                    (LoggedIn.ShowFeedback Feedback.Success
                                        (shared.translators.t "community.create.success")
                                    )
                                >> UR.addCmd (Route.pushUrl shared.navKey Route.CommunitySettings)

                        _ ->
                            UR.logImpossible msg [ "WithoutCommunity" ]
                   )

        GotSubmitResponse (Err val) ->
            { model | isLoading = False }
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure (shared.translators.t "community.error_saving"))
                |> UR.logDebugValue msg val

        CompletedLoadCommunity community ->
            let
                tokenType =
                    community.tokenType |> Maybe.withDefault Token.Mcc

                fetchExpiryOptsData =
                    case tokenType of
                        Token.Mcc ->
                            identity

                        Token.Expiry ->
                            UR.addCmd (Api.getExpiryOpts shared community.symbol CompletedLoadExpiryOpts)
            in
            { model
                | minimumBalance =
                    Maybe.map String.fromFloat community.minBalance
                        |> Maybe.withDefault "0"
                , maximumSupply =
                    Maybe.map String.fromFloat community.maxSupply
                        |> Maybe.withDefault "21000000"
                , tokenType = tokenType
                , isLoading =
                    case tokenType of
                        Token.Mcc ->
                            False

                        Token.Expiry ->
                            True
            }
                |> UR.init
                |> fetchExpiryOptsData

        CompletedLoadExpiryOpts (Ok (Just expiryOptsData)) ->
            { model
                | naturalExpirationPeriod =
                    expiryOptsData.naturalExpirationPeriod
                        |> String.fromInt
                , juridicalExpirationPeriod =
                    expiryOptsData.juridicalExpirationPeriod
                        |> String.fromInt
                , renovationAmount =
                    expiryOptsData.renovationAmount.amount
                        |> String.fromFloat
                , isLoading = False
            }
                |> UR.init

        CompletedLoadExpiryOpts (Ok Nothing) ->
            { model | isLoading = False }
                |> UR.init
                |> UR.logImpossible msg [ "NoExpiryOpts" ]
                |> UR.addExt
                    (LoggedIn.ShowFeedback Feedback.Failure
                        (shared.translators.t "settings.community_currency.expiryopts_not_found")
                    )

        CompletedLoadExpiryOpts (Err err) ->
            { model | isLoading = False }
                |> UR.init
                |> UR.logHttpError msg err
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure (shared.translators.t "error.unknown"))


withSymbolValidation : (Eos.Symbol -> Model -> Result String a) -> Field -> LoggedIn.Model -> Model -> Model
withSymbolValidation fn field loggedIn_ model_ =
    case loggedIn_.selectedCommunity of
        RemoteData.Success community ->
            model_
                |> setErrors field (fn community.symbol)

        _ ->
            model_


savePort : Token.UpdateTokenData -> Maybe Token.ExpiryOptsData -> LoggedIn.Model -> Ports.JavascriptOutModel Msg
savePort updateTokenData maybeExpiryOpts loggedIn =
    let
        authorization =
            { actor = loggedIn.accountName
            , permissionName = Eos.samplePermission
            }
    in
    { responseAddress = ClickedSubmit
    , responseData = Token.encodeUpdateTokenData updateTokenData
    , data =
        Eos.encodeTransaction
            ({ accountName = loggedIn.shared.contracts.token
             , name = "update"
             , authorization = authorization
             , data = Token.encodeUpdateTokenData updateTokenData
             }
                :: (case maybeExpiryOpts of
                        Just expiryOpts ->
                            [ { accountName = loggedIn.shared.contracts.token
                              , name = "setexpiry"
                              , authorization = authorization
                              , data = Token.encodeExpiryOpts expiryOpts
                              }
                            ]

                        Nothing ->
                            []
                   )
            )
    }



-- VALIDATION


type Field
    = MinimumBalance
    | MaximumSupply
    | NaturalExpirationPeriod
    | JuridicalExpirationPeriod
    | RenovationAmount


isFieldError : Field -> ( Field, String ) -> Bool
isFieldError field ( errorField, _ ) =
    field == errorField


validateIntInput : String -> Result String Int
validateIntInput numberInput =
    String.toInt numberInput
        |> Result.fromMaybe "error.validator.text.only_numbers"


validateSymbolInput : Eos.Symbol -> String -> Result String Eos.Asset
validateSymbolInput symbol numberInput =
    let
        validateParsing =
            String.toFloat numberInput
                |> Maybe.map (\amount -> { symbol = symbol, amount = amount })
                |> Result.fromMaybe "error.validator.text.only_numbers"
    in
    case String.split "." numberInput of
        [] ->
            Err "error.required"

        [ "" ] ->
            Err "error.required"

        [ _ ] ->
            validateParsing

        _ :: decimalDigits :: _ ->
            if String.length decimalDigits > Eos.getSymbolPrecision symbol then
                Err "error.contracts.transfer.symbol precision mismatch"

            else
                validateParsing


setErrors : Field -> (Model -> Result String a) -> Model -> Model
setErrors field modelValidation model =
    let
        errorsWithoutField =
            List.filter (not << isFieldError field) model.errors
    in
    { model
        | errors =
            case modelValidation model of
                Err err ->
                    ( field, err ) :: errorsWithoutField

                Ok _ ->
                    errorsWithoutField
    }


validateMinimumBalance : Eos.Symbol -> Model -> Result String Eos.Asset
validateMinimumBalance symbol model =
    validateSymbolInput symbol model.minimumBalance


validateMaximumSupply : Eos.Symbol -> Model -> Result String Eos.Asset
validateMaximumSupply symbol model =
    validateSymbolInput symbol model.maximumSupply


validateNaturalExpirationPeriod : Model -> Result String Int
validateNaturalExpirationPeriod model =
    validateIntInput model.naturalExpirationPeriod


validateJuridicalExpirationPeriod : Model -> Result String Int
validateJuridicalExpirationPeriod model =
    validateIntInput model.juridicalExpirationPeriod


validateRenovationAmount : Eos.Symbol -> Model -> Result String Eos.Asset
validateRenovationAmount symbol model =
    validateSymbolInput symbol model.renovationAmount


validateModel : Eos.Symbol -> Model -> Result Model ( Token.UpdateTokenData, Maybe Token.ExpiryOptsData )
validateModel symbol model =
    let
        tokenValidation =
            Result.map2 Token.UpdateTokenData
                (validateMaximumSupply symbol model)
                (validateMinimumBalance symbol model)

        expiryOptsValidation =
            Result.map3 (Token.ExpiryOptsData symbol)
                (validateNaturalExpirationPeriod model)
                (validateJuridicalExpirationPeriod model)
                (validateRenovationAmount symbol model)

        modelWithErrors =
            model
                |> setErrors MinimumBalance (validateMinimumBalance symbol)
                |> setErrors MaximumSupply (validateMaximumSupply symbol)
                |> setErrors NaturalExpirationPeriod validateNaturalExpirationPeriod
                |> setErrors JuridicalExpirationPeriod validateJuridicalExpirationPeriod
                |> setErrors RenovationAmount (validateRenovationAmount symbol)
    in
    case model.tokenType of
        Token.Mcc ->
            case tokenValidation of
                Ok validToken ->
                    Ok ( validToken, Nothing )

                Err _ ->
                    Err modelWithErrors

        Token.Expiry ->
            case Result.map2 Tuple.pair tokenValidation expiryOptsValidation of
                Ok ( validToken, validOpts ) ->
                    Ok ( validToken, Just validOpts )

                Err _ ->
                    Err modelWithErrors



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view ({ shared } as loggedIn) model =
    let
        { t } =
            shared.translators

        title =
            t "settings.community_currency.title"

        content =
            case loggedIn.selectedCommunity of
                RemoteData.Failure e ->
                    Page.fullPageGraphQLError title e

                RemoteData.Loading ->
                    Page.fullPageLoading shared

                RemoteData.NotAsked ->
                    Page.fullPageLoading shared

                RemoteData.Success community ->
                    div [ class "bg-white" ]
                        [ Page.viewHeader loggedIn title
                        , view_ loggedIn community model
                        ]
    in
    { title = title
    , content = content
    }


view_ : LoggedIn.Model -> Community.Model -> Model -> Html Msg
view_ { shared } community model =
    let
        { t } =
            shared.translators
    in
    form
        [ class "w-full px-4 pb-10"
        , onSubmit ClickedSubmit
        ]
        [ div [ class "container mx-auto pt-4" ]
            ([ viewInformativeFields shared.translators community
             , viewGeneralFields shared.translators community model
             , case model.tokenType of
                Token.Mcc ->
                    []

                Token.Expiry ->
                    viewExpiryFields shared.translators community model
             , [ button
                    [ class "button button-primary w-full mt-12"
                    , disabled model.isLoading
                    ]
                    [ text (t "menu.save") ]
               ]
             ]
                |> List.concat
            )
        ]


viewInformativeFields : Translators -> Community.Model -> List (Html Msg)
viewInformativeFields ({ t, tr } as translators) community =
    let
        precision =
            Eos.getSymbolPrecision community.symbol

        symbolExample =
            case Eos.formatSymbolAmount community.symbol 100 |> String.split "." of
                [] ->
                    []

                [ withoutDecimalPlaces ] ->
                    [ span [ class "text-xl" ] [ text withoutDecimalPlaces ] ]

                beforeSeparator :: afterSeparator :: _ ->
                    [ span [ class "text-xl" ] [ text beforeSeparator ]
                    , span [ class "text-sm" ] [ text ("," ++ afterSeparator) ]
                    ]
    in
    [ Input.init
        { label = t "community.create.labels.currency_name"
        , id = "currency_name_field"
        , onInput = \_ -> Ignored
        , disabled = True
        , value = community.name
        , placeholder = Nothing
        , problems = Nothing
        , translators = translators
        }
        |> Input.toHtml
    , div [ class "flex w-full space-x-8" ]
        [ div [ class "w-full" ]
            [ Input.init
                { label = t "community.create.labels.currency_symbol"
                , id = "currency_symbol_field"
                , onInput = \_ -> Ignored
                , disabled = True
                , value = Eos.symbolToSymbolCodeString community.symbol
                , placeholder = Nothing
                , problems = Nothing
                , translators = translators
                }
                |> Input.toHtml
            ]
        , div [ class "w-full" ]
            [ Input.init
                { label = t "settings.community_currency.decimal_places"
                , id = "currency_precision_field"
                , onInput = \_ -> Ignored
                , disabled = True
                , value = String.fromInt precision
                , placeholder = Nothing
                , problems = Nothing
                , translators = translators
                }
                |> Input.withAttrs [ class "w-full" ]
                |> Input.withElements
                    [ span [ class "absolute right-0 inset-y-0 flex items-center pr-3 text-gray-900" ]
                        [ text (Eos.formatSymbolAmount community.symbol 100) ]
                    ]
                |> Input.toHtml
            ]
        ]
    , div [ class "bg-gray-100 py-4 text-center mb-10" ]
        [ div [ class "text-xl font-medium mb-4" ]
            (symbolExample
                ++ [ span [ class "ml-4 text-green" ] [ text (Eos.symbolToSymbolCodeString community.symbol) ] ]
            )
        , span [ class "uppercase text-black text-xs tracking-widest" ]
            [ text (t "settings.community_currency.format")
            , br [] []
            , text (tr "settings.community_currency.supports_decimal_places" [ ( "amount", String.fromInt precision ) ])
            ]
        ]
    ]


viewGeneralFields : Translators -> Community.Model -> Model -> List (Html Msg)
viewGeneralFields ({ t } as translators) community model =
    [ Input.init
        { label = t "community.create.labels.min_balance"
        , id = "minimum_balance_field"
        , onInput = EnteredMinimumBalance
        , disabled = False
        , value = model.minimumBalance
        , placeholder = Just (Eos.formatSymbolAmount community.symbol 0)
        , problems = errorsForField translators MinimumBalance model
        , translators = translators
        }
        |> Input.withCurrency community.symbol
        |> Input.toHtml
    , Input.init
        { label = t "community.create.labels.max_supply"
        , id = "maximum_supply_field"
        , onInput = EnteredMaximumSupply
        , disabled = False
        , value = model.maximumSupply
        , placeholder = Just (Eos.formatSymbolAmount community.symbol 21000000)
        , problems = errorsForField translators MaximumSupply model
        , translators = translators
        }
        |> Input.withCurrency community.symbol
        |> Input.toHtml
    , Radio.init
        { label = "settings.community_currency.token_type"
        , name = "token_type_radio"
        , optionToString = Token.tokenTypeToString
        , activeOption = model.tokenType
        , onSelect = \_ -> Ignored
        , areOptionsEqual = (==)
        }
        |> Radio.withOption Token.Mcc (\_ -> text "MCC")
        |> Radio.withOption Token.Expiry (\_ -> text (t "settings.community_currency.expiry"))
        |> Radio.withAttrs [ class "mb-8" ]
        |> Radio.withDisabled True
        |> Radio.toHtml translators
    ]


viewExpiryFields : Translators -> Community.Model -> Model -> List (Html Msg)
viewExpiryFields ({ t } as translators) community model =
    let
        withSeconds input =
            input
                |> Input.withAttrs [ class "pr-20" ]
                |> Input.withElements
                    [ span [ class "absolute inset-y-0 right-1 flex items-center bg-white pl-1 my-2" ]
                        [ text (t "settings.community_currency.seconds") ]
                    ]
    in
    [ Input.init
        { label = t "settings.community_currency.natural_expiration_period"
        , id = "natural_expiration_period_field"
        , onInput = EnteredNaturalExpirationPeriod
        , disabled = False
        , value = model.naturalExpirationPeriod
        , placeholder = Just "10"
        , problems = errorsForField translators NaturalExpirationPeriod model
        , translators = translators
        }
        |> withSeconds
        |> Input.toHtml
    , Input.init
        { label = t "settings.community_currency.juridical_expiration_period"
        , id = "juridical_expiration_period_field"
        , onInput = EnteredJuridicalExpirationPeriod
        , disabled = False
        , value = model.juridicalExpirationPeriod
        , placeholder = Just "15"
        , problems = errorsForField translators JuridicalExpirationPeriod model
        , translators = translators
        }
        |> withSeconds
        |> Input.toHtml
    , Input.init
        { label = t "settings.community_currency.renovation_amount"
        , id = "renovation_amount_field"
        , onInput = EnteredRenovationAmount
        , disabled = False
        , value = model.renovationAmount
        , placeholder = Just (Eos.formatSymbolAmount community.symbol 100)
        , problems = errorsForField translators RenovationAmount model
        , translators = translators
        }
        |> Input.withCurrency community.symbol
        |> Input.toHtml
    ]


errorsForField : Translators -> Field -> Model -> Maybe (List String)
errorsForField translators field model =
    List.filter (isFieldError field) model.errors
        |> List.map (Tuple.second >> translators.t)
        |> Just



-- UTILS


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityLoaded community ->
            Just (CompletedLoadCommunity community)

        _ ->
            Nothing


jsAddressToMsg : List String -> Encode.Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "ClickedSubmit" :: [] ->
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.map2 (\_ updateTokenData -> Ok updateTokenData)
                        (Decode.field "transactionId" Decode.string)
                        (Decode.field "addressData" Token.updateTokenDataDecoder)
                    , Decode.succeed (Err val)
                    ]
                )
                val
                |> Result.map GotSubmitResponse
                |> Result.toMaybe

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    case msg of
        Ignored ->
            [ "Ignored" ]

        EnteredMinimumBalance _ ->
            [ "EnteredMinimumBalance" ]

        EnteredMaximumSupply _ ->
            [ "EnteredMaximumSupply" ]

        EnteredNaturalExpirationPeriod _ ->
            [ "EnteredNaturalExpirationPeriod" ]

        EnteredJuridicalExpirationPeriod _ ->
            [ "EnteredJuridicalExpirationPeriod" ]

        EnteredRenovationAmount _ ->
            [ "EnteredRenovationAmount" ]

        ClickedSubmit ->
            [ "ClickedSubmit" ]

        GotSubmitResponse r ->
            [ "GotSubmitResponse", UR.resultToString r ]

        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        CompletedLoadExpiryOpts r ->
            [ "CompletedLoadExpiryOpts", UR.resultToString r ]
