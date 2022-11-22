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

import Community
import Dict
import Eos
import Eos.Account as Eos
import Form
import Form.Radio
import Form.Text
import Form.Validate
import Html exposing (Html, br, div, span, text)
import Html.Attributes exposing (class, disabled)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Log
import Page
import RemoteData
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Translators)
import Token
import UpdateResult as UR
import View.Feedback as Feedback



-- MODEL


type alias Model =
    { isLoading : Bool
    , form : FormStatus
    }


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( { isLoading = True
      , form = Loading
      }
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )


type FormStatus
    = Loading
    | Loaded (Form.Model FormInput)


type alias FormInput =
    { minimumBalance : String
    , maximumSupply : String
    , tokenType : Token.TokenType
    , naturalExpirationPeriod : String
    , juridicalExpirationPeriod : String
    , renovationAmount : String
    }


type alias FormOutput =
    { minimumBalance : Float
    , maximumSupply : Float
    , tokenInfo : TokenInfo
    }


type TokenInfo
    = NoExpiryOpts
    | WithExpiryOpts Token.ExpiryOptsData



-- UPDATE


type Msg
    = ClosedAuthModal
    | ClickedSubmit FormOutput
    | GotSubmitResponse (Result Encode.Value Token.UpdateTokenData)
    | CompletedLoadCommunity Community.Model
    | CompletedLoadExpiryOpts (Result Http.Error (Maybe Token.ExpiryOptsData))
    | CompletedLoadToken (Result Http.Error Token.Model)
    | GotFormMsg (Form.Msg FormInput)


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model ({ shared } as loggedIn) =
    case msg of
        ClosedAuthModal ->
            { model | isLoading = False }
                |> UR.init

        ClickedSubmit formOutput ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    let
                        asset amount =
                            { amount = amount, symbol = community.symbol }

                        updateTokenData =
                            { maxSupply = asset formOutput.maximumSupply
                            , minBalance = asset formOutput.minimumBalance
                            }

                        authorization =
                            { actor = loggedIn.accountName, permissionName = Eos.samplePermission }
                    in
                    { model | isLoading = True }
                        |> UR.init
                        |> UR.addPort
                            { responseAddress = ClickedSubmit formOutput
                            , responseData = Token.encodeUpdateTokenData updateTokenData
                            , data =
                                Eos.encodeTransaction
                                    ({ accountName = loggedIn.shared.contracts.token
                                     , name = "update"
                                     , authorization = authorization
                                     , data = Token.encodeUpdateTokenData updateTokenData
                                     }
                                        :: (case formOutput.tokenInfo of
                                                NoExpiryOpts ->
                                                    []

                                                WithExpiryOpts expiryOpts ->
                                                    [ { accountName = loggedIn.shared.contracts.token
                                                      , name = "setexpiry"
                                                      , authorization = authorization
                                                      , data = Token.encodeExpiryOpts expiryOpts
                                                      }
                                                    ]
                                           )
                                    )
                            }
                        |> LoggedIn.withPrivateKey loggedIn
                            []
                            model
                            { successMsg = msg, errorMsg = ClosedAuthModal }

                _ ->
                    UR.init model
                        |> UR.logImpossible msg
                            "Tried submitting token edit, but community wasn't loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.Currency", function = "update" }
                            []

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
                                >> UR.addBreadcrumb
                                    { type_ = Log.DebugBreadcrumb
                                    , category = msg
                                    , message = "Submitted Currency page"
                                    , data = Dict.empty
                                    , level = Log.DebugLevel
                                    }

                        _ ->
                            UR.logImpossible msg
                                "Completed updating token, but community wasn't loaded"
                                (Just loggedIn.accountName)
                                { moduleName = "Page.Community.Settings.Currency", function = "update" }
                                []
                   )

        GotSubmitResponse (Err val) ->
            { model | isLoading = False }
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure (shared.translators.t "community.error_saving"))
                |> UR.logJsonValue msg
                    (Just loggedIn.accountName)
                    "Got an error when submitting currency form"
                    { moduleName = "Page.Community.Settings.Currency", function = "update" }
                    [ Log.contextFromCommunity loggedIn.selectedCommunity ]
                    val

        CompletedLoadCommunity community ->
            { model | isLoading = True }
                |> UR.init
                |> UR.addCmd (Token.getToken shared community.symbol CompletedLoadToken)

        CompletedLoadToken (Ok token) ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    let
                        fetchExpiryOptsData =
                            case token.type_ of
                                Token.Mcc ->
                                    identity

                                Token.Expiry ->
                                    UR.addCmd (Token.getExpiryOpts shared community.symbol CompletedLoadExpiryOpts)
                    in
                    { model
                        | isLoading =
                            case token.type_ of
                                Token.Mcc ->
                                    False

                                Token.Expiry ->
                                    True
                        , form =
                            case token.type_ of
                                Token.Mcc ->
                                    { minimumBalance =
                                        community.minBalance
                                            |> Maybe.withDefault 0
                                            |> String.fromFloat
                                    , maximumSupply =
                                        community.maxSupply
                                            |> Maybe.withDefault 21000000
                                            |> String.fromFloat
                                    , tokenType = Token.Mcc
                                    , naturalExpirationPeriod = "10"
                                    , juridicalExpirationPeriod = "15"
                                    , renovationAmount = "100"
                                    }
                                        |> Form.init
                                        |> Loaded

                                Token.Expiry ->
                                    Loading
                    }
                        |> UR.init
                        |> fetchExpiryOptsData

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Completed loading token information, but community wasn't loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.Currency", function = "update" }
                            []

        CompletedLoadToken (Err err) ->
            { model | isLoading = False }
                |> UR.init
                |> UR.addExt
                    (LoggedIn.ShowFeedback Feedback.Failure
                        (shared.translators.t "settings.community_currency.token_error")
                    )
                |> UR.logHttpError msg
                    (Just loggedIn.accountName)
                    "Got an error when trying to load token information"
                    { moduleName = "Page.Community.Settings.Currency", function = "update" }
                    [ Log.contextFromCommunity loggedIn.selectedCommunity ]
                    err

        CompletedLoadExpiryOpts (Ok (Just expiryOptsData)) ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    { model
                        | isLoading = False
                        , form =
                            { minimumBalance =
                                community.minBalance
                                    |> Maybe.withDefault 0
                                    |> String.fromFloat
                            , maximumSupply =
                                community.maxSupply
                                    |> Maybe.withDefault 21000000
                                    |> String.fromFloat
                            , tokenType = Token.Expiry
                            , naturalExpirationPeriod = String.fromInt expiryOptsData.naturalExpirationPeriod
                            , juridicalExpirationPeriod = String.fromInt expiryOptsData.juridicalExpirationPeriod
                            , renovationAmount = String.fromFloat expiryOptsData.renovationAmount.amount
                            }
                                |> Form.init
                                |> Loaded
                    }
                        |> UR.init

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Completed loading token expiry opts, but community wasn't loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Settings.Currency", function = "update" }
                            []

        CompletedLoadExpiryOpts (Ok Nothing) ->
            { model | isLoading = False }
                |> UR.init
                |> UR.addExt
                    (LoggedIn.ShowFeedback Feedback.Failure
                        (shared.translators.t "settings.community_currency.expiryopts_not_found")
                    )
                |> UR.logImpossible msg
                    "Completed loading expiry_opts, but got Nothing"
                    (Just loggedIn.accountName)
                    { moduleName = "Page.Community.Settings.Currency", function = "update" }
                    [ Log.contextFromCommunity loggedIn.selectedCommunity ]

        CompletedLoadExpiryOpts (Err err) ->
            { model | isLoading = False }
                |> UR.init
                |> UR.logHttpError msg
                    (Just loggedIn.accountName)
                    "Got an error when loading expiry_opts for a community"
                    { moduleName = "Page.Community.Settings.Currency", function = "update" }
                    [ Log.contextFromCommunity loggedIn.selectedCommunity ]
                    err
                |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure (shared.translators.t "error.unknown"))

        GotFormMsg subMsg ->
            case model.form of
                Loading ->
                    UR.init model

                Loaded form ->
                    Form.update shared subMsg form
                        |> UR.fromChild (\newForm -> { model | form = Loaded newForm })
                            GotFormMsg
                            LoggedIn.addFeedback
                            model



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
                    case model.form of
                        Loading ->
                            Page.fullPageLoading shared

                        Loaded form ->
                            div [ class "bg-white" ]
                                [ Page.viewHeader loggedIn title
                                , Form.view [ class "container mx-auto px-4 pt-4 pb-10" ]
                                    shared.translators
                                    (\submitButton ->
                                        [ submitButton
                                            [ class "button button-primary w-full mt-12"
                                            , disabled (not loggedIn.hasAcceptedCodeOfConduct)
                                            ]
                                            [ text <| t "menu.save" ]
                                        ]
                                    )
                                    (createForm shared.translators community)
                                    (Form.withDisabled model.isLoading form)
                                    { toMsg = GotFormMsg
                                    , onSubmit = ClickedSubmit
                                    }
                                ]
    in
    { title = title
    , content = content
    }


createForm : Translators -> Community.Model -> Form.Form msg FormInput FormOutput
createForm ({ t } as translators) community =
    Form.succeed FormOutput
        |> Form.withNoOutput (communityInfoForm translators community)
        |> Form.with
            (Form.Text.init { label = t "community.create.labels.min_balance", id = "minimum-balance-input" }
                |> Form.Text.withCurrency community.symbol
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.maskedFloat translators
                            >> Form.Validate.validate translators
                    , value = .minimumBalance
                    , update = \minimumBalance input -> { input | minimumBalance = minimumBalance }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Text.init { label = t "community.create.labels.max_supply", id = "maximum-supply-input" }
                |> Form.Text.withCurrency community.symbol
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.maskedFloat translators
                            >> Form.Validate.validate translators
                    , value = .maximumSupply
                    , update = \maximumSupply input -> { input | maximumSupply = maximumSupply }
                    , externalError = always Nothing
                    }
            )
        |> Form.withNoOutput
            (Form.Radio.init
                { label = t "settings.community_currency.token_type"
                , id = "token-type-input"
                , optionToString = Token.tokenTypeToString
                }
                |> Form.Radio.withOption Token.Mcc (text "MCC")
                |> Form.Radio.withOption Token.Expiry (text <| t "settings.community_currency.expiry")
                |> Form.Radio.withDisabled True
                |> Form.Radio.withContainerAttrs [ class "mb-8" ]
                |> Form.radio (Token.tokenTypeFromString >> Maybe.withDefault Token.Mcc)
                    { parser = Ok
                    , value = .tokenType
                    , update = \tokenType input -> { input | tokenType = tokenType }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.introspect
                (\values ->
                    case values.tokenType of
                        Token.Mcc ->
                            Form.succeed NoExpiryOpts

                        Token.Expiry ->
                            Form.mapOutput WithExpiryOpts (expiryOptsForm translators community)
                )
            )


communityInfoForm : Translators -> Community.Model -> Form.Form msg input ()
communityInfoForm ({ t, tr } as translators) community =
    let
        symbolExample =
            case Eos.formatSymbolAmount translators community.symbol 100 |> String.split "." of
                [] ->
                    []

                [ withoutDecimalPlaces ] ->
                    [ span [ class "text-xl" ] [ text withoutDecimalPlaces ] ]

                beforeSeparator :: afterSeparator :: _ ->
                    [ span [ class "text-xl" ] [ text beforeSeparator ]
                    , span [ class "text-sm" ] [ text ("," ++ afterSeparator) ]
                    ]
    in
    Form.succeed ()
        |> Form.withNoOutput
            (Form.Text.init
                { label = t "community.create.labels.currency_name"
                , id = "currency-name-input"
                }
                |> Form.Text.withDisabled True
                |> Form.textField
                    { parser = Ok
                    , value = \_ -> community.name
                    , update = \_ input -> input
                    , externalError = always Nothing
                    }
            )
        |> Form.withNoOutput
            (Form.succeed (\_ _ -> ())
                |> Form.withGroup [ class "grid grid-cols-2 gap-8" ]
                    (Form.Text.init
                        { label = t "community.create.labels.currency_symbol"
                        , id = "currency-symbol-input"
                        }
                        |> Form.Text.withDisabled True
                        |> Form.textField
                            { parser = Ok
                            , value = \_ -> Eos.symbolToSymbolCodeString community.symbol
                            , update = \_ input -> input
                            , externalError = always Nothing
                            }
                    )
                    (Form.Text.init
                        { label = t "settings.community_currency.decimal_places"
                        , id = "currency-precision-input"
                        }
                        |> Form.Text.withDisabled True
                        |> Form.Text.withElements
                            [ span [ class "absolute right-0 inset-y-0 flex items-center pr-3 text-gray-900" ]
                                [ text <| Eos.formatSymbolAmount translators community.symbol 100 ]
                            ]
                        |> Form.textField
                            { parser = Ok
                            , value = \_ -> Eos.getSymbolPrecision community.symbol |> String.fromInt
                            , update = \_ input -> input
                            , externalError = always Nothing
                            }
                    )
            )
        |> Form.withDecoration
            (div [ class "bg-gray-100 py-4 text-center mb-10" ]
                [ div [ class "text-xl font-semibold mb-4" ]
                    (symbolExample
                        ++ [ span [ class "ml-4 text-green" ] [ text <| Eos.symbolToSymbolCodeString community.symbol ] ]
                    )
                , span [ class "uppercase text-black text-sm tracking-widest" ]
                    [ text <| t "settings.community_currency.format"
                    , br [] []
                    , text <|
                        tr "settings.community_currency.supports_decimal_places"
                            [ ( "amount", Eos.getSymbolPrecision community.symbol |> String.fromInt ) ]
                    ]
                ]
            )


expiryOptsForm : Translators -> Community.Model -> Form.Form msg FormInput Token.ExpiryOptsData
expiryOptsForm ({ t } as translators) community =
    let
        withSeconds input =
            input
                |> Form.Text.withExtraAttrs [ class "pr-20" ]
                |> Form.Text.withElements
                    [ span [ class "absolute inset-y-0 right-4 flex items-center bg-white pl-1 my-2" ]
                        [ text (t "settings.community_currency.seconds") ]
                    ]
    in
    Form.succeed Token.ExpiryOptsData
        |> Form.with (Form.succeed community.symbol)
        |> Form.with
            (Form.Text.init { label = t "settings.community_currency.natural_expiration_period", id = "natural-expiration-period-input" }
                |> withSeconds
                |> Form.Text.withPlaceholder "10"
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.int
                            >> Form.Validate.validate translators
                    , value = .naturalExpirationPeriod
                    , update = \period input -> { input | naturalExpirationPeriod = period }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Text.init { label = t "settings.community_currency.juridical_expiration_period", id = "juridical-expiration-period-input" }
                |> withSeconds
                |> Form.Text.withPlaceholder "15"
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.int
                            >> Form.Validate.validate translators
                    , value = .juridicalExpirationPeriod
                    , update = \period input -> { input | juridicalExpirationPeriod = period }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Text.init { label = t "settings.community_currency.renovation_amount", id = "renovation-amount-input" }
                |> Form.Text.withPlaceholder (Eos.formatSymbolAmount translators community.symbol 100)
                |> Form.Text.withCurrency community.symbol
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.maskedFloat translators
                            >> Form.Validate.custom (\amount -> Ok { amount = amount, symbol = community.symbol })
                            >> Form.Validate.validate translators
                    , value = .renovationAmount
                    , update = \renovationAmount input -> { input | renovationAmount = renovationAmount }
                    , externalError = always Nothing
                    }
            )



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
        ClosedAuthModal ->
            [ "ClosedAuthModal" ]

        ClickedSubmit _ ->
            [ "ClickedSubmit" ]

        GotSubmitResponse r ->
            [ "GotSubmitResponse", UR.resultToString r ]

        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        CompletedLoadExpiryOpts r ->
            [ "CompletedLoadExpiryOpts", UR.resultToString r ]

        CompletedLoadToken r ->
            [ "CompletedLoadToken", UR.resultToString r ]

        GotFormMsg subMsg ->
            "GotFormMsg" :: Form.msgToString subMsg
