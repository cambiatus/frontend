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
import Eos
import Eos.Account as Eos
import Html exposing (Html, button, div, form, span, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onSubmit)
import Json.Decode as Decode
import Json.Encode as Encode
import Page
import Ports
import RemoteData
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Translators)
import UpdateResult as UR
import View.Feedback as Feedback
import View.Form.Input as Input



-- MODEL


type alias Model =
    { inviterReward : String
    , invitedReward : String
    , minimumBalance : String
    , isLoading : Bool
    , errors : List ( Field, String )
    }


type alias ValidModel =
    { inviterReward : Float
    , invitedReward : Float
    , minimumBalance : Float
    }


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( { inviterReward = ""
      , invitedReward = ""
      , minimumBalance = ""
      , isLoading = True
      , errors = []
      }
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )



-- UPDATE


type Msg
    = Ignored
    | EnteredInviterReward String
    | EnteredInvitedReward String
    | EnteredMinimumBalance String
    | ClickedSubmit
    | GotSubmitResponse (Result Encode.Value ValidModel)
    | CompletedLoadCommunity Community.Model


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model ({ shared } as loggedIn) =
    case msg of
        Ignored ->
            UR.init model

        EnteredInviterReward inviterReward ->
            { model | inviterReward = inviterReward }
                |> withSymbolValidation validateInviterReward InviterReward loggedIn
                |> UR.init

        EnteredInvitedReward invitedReward ->
            { model | invitedReward = invitedReward }
                |> withSymbolValidation validateInvitedReward InvitedReward loggedIn
                |> UR.init

        EnteredMinimumBalance minimumBalance ->
            { model | minimumBalance = minimumBalance }
                |> withSymbolValidation validateMinimumBalance MinimumBalance loggedIn
                |> UR.init

        ClickedSubmit ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    case validateModel community.symbol model of
                        Ok validatedModel ->
                            if LoggedIn.hasPrivateKey loggedIn then
                                { model | isLoading = True }
                                    |> UR.init
                                    |> UR.addPort (savePort validatedModel loggedIn community)

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

        GotSubmitResponse (Ok validModel) ->
            { model | isLoading = False }
                |> UR.init
                |> (case loggedIn.selectedCommunity of
                        RemoteData.Success community ->
                            UR.addExt
                                ({ community
                                    | inviterReward = validModel.inviterReward
                                    , invitedReward = validModel.invitedReward
                                    , minBalance = Just validModel.minimumBalance
                                 }
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
            { model
                | inviterReward = String.fromFloat community.inviterReward
                , invitedReward = String.fromFloat community.invitedReward
                , minimumBalance =
                    Maybe.map String.fromFloat community.minBalance
                        |> Maybe.withDefault "0"
                , isLoading = False
            }
                |> UR.init


withSymbolValidation : (Eos.Symbol -> Model -> Result String a) -> Field -> LoggedIn.Model -> Model -> Model
withSymbolValidation fn field loggedIn_ model_ =
    case loggedIn_.selectedCommunity of
        RemoteData.Success community ->
            fn community.symbol model_
                |> (\result -> setErrors field result model_)

        _ ->
            model_


savePort : ValidModel -> LoggedIn.Model -> Community.Model -> Ports.JavascriptOutModel Msg
savePort validModel loggedIn community =
    let
        authorization =
            { actor = loggedIn.accountName
            , permissionName = Eos.samplePermission
            }

        asset amount =
            { amount = amount
            , symbol = community.symbol
            }
    in
    { responseAddress = ClickedSubmit
    , responseData = encodeValidModel validModel
    , data =
        Eos.encodeTransaction
            [ { accountName = loggedIn.shared.contracts.community
              , name = "update"
              , authorization = authorization
              , data =
                    { asset = asset 0
                    , logo = community.logo
                    , name = community.name
                    , description = community.description
                    , inviterReward = asset validModel.inviterReward
                    , invitedReward = asset validModel.invitedReward
                    , hasObjectives = Eos.boolToEosBool community.hasObjectives
                    , hasShop = Eos.boolToEosBool community.hasShop
                    }
                        |> Community.encodeUpdateData
              }
            , { accountName = loggedIn.shared.contracts.token
              , name = "update"
              , authorization = authorization
              , data =
                    { maxSupply = asset 21000000.0
                    , minBalance = asset validModel.minimumBalance
                    }
                        |> Community.encodeUpdateTokenData
              }
            ]
    }



-- VALIDATION


type Field
    = InviterReward
    | InvitedReward
    | MinimumBalance


isFieldError : Field -> ( Field, String ) -> Bool
isFieldError field ( errorField, _ ) =
    field == errorField


validateNumberInput : Eos.Symbol -> String -> Result String Float
validateNumberInput symbol numberInput =
    let
        validateParsing =
            String.toFloat numberInput
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


setErrors : Field -> Result String a -> Model -> Model
setErrors field validationResult model =
    let
        errorsWithoutField =
            List.filter (not << isFieldError field) model.errors
    in
    { model
        | errors =
            case validationResult of
                Err err ->
                    ( field, err ) :: errorsWithoutField

                Ok _ ->
                    errorsWithoutField
    }


validateInviterReward : Eos.Symbol -> Model -> Result String Float
validateInviterReward symbol model =
    validateNumberInput symbol model.inviterReward


validateInvitedReward : Eos.Symbol -> Model -> Result String Float
validateInvitedReward symbol model =
    validateNumberInput symbol model.invitedReward


validateMinimumBalance : Eos.Symbol -> Model -> Result String Float
validateMinimumBalance symbol model =
    validateNumberInput symbol model.minimumBalance


validateModel : Eos.Symbol -> Model -> Result Model ValidModel
validateModel symbol model =
    let
        inviterValidation =
            validateInviterReward symbol model

        invitedValidation =
            validateInvitedReward symbol model

        minimumBalanceValidation =
            validateMinimumBalance symbol model
    in
    case Result.map3 ValidModel inviterValidation invitedValidation minimumBalanceValidation of
        Ok valid ->
            Ok valid

        Err _ ->
            model
                |> setErrors InviterReward inviterValidation
                |> setErrors InvitedReward invitedValidation
                |> setErrors MinimumBalance minimumBalanceValidation
                |> Err



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
                        [ Page.viewHeader loggedIn title Route.CommunitySettings
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

        precision =
            Eos.getSymbolPrecision community.symbol

        fillWithPrecision amount =
            if precision == 0 then
                String.fromInt amount

            else
                String.fromInt amount ++ "." ++ String.join "" (List.repeat precision "0")
    in
    form
        [ class "w-full px-4 pb-10"
        , onSubmit ClickedSubmit
        ]
        [ div [ class "container mx-auto pt-4" ]
            [ Input.init
                { label = t "community.create.labels.currency_name"
                , id = "currency_name_field"
                , onInput = \_ -> Ignored
                , disabled = True
                , value = community.name
                , placeholder = Nothing
                , problems = Nothing
                , translators = shared.translators
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
                        , translators = shared.translators
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
                        , translators = shared.translators
                        }
                        |> Input.withAttrs [ class "w-full" ]
                        |> Input.toHtml
                    ]
                ]
            , div [ class "bg-gray-100 py-4 text-center mb-10" ]
                [ div [ class "text-xl font-medium space-x-4 mb-4" ]
                    [ span [ class "text-black" ]
                        [ text
                            (String.fromFloat pi
                                |> String.left
                                    (if precision == 0 then
                                        1

                                     else
                                        2 + precision
                                    )
                            )
                        ]
                    , span [ class "text-green" ] [ text (Eos.symbolToSymbolCodeString community.symbol) ]
                    ]
                , span [ class "uppercase text-black text-xs tracking-widest" ]
                    [ text (t "settings.community_currency.format") ]
                ]
            , Input.init
                { label = t "community.create.labels.inviter_reward"
                , id = "inviter_reward_field"
                , onInput = EnteredInviterReward
                , disabled = False
                , value = model.inviterReward
                , placeholder = Just (fillWithPrecision 10)
                , problems = errorsForField shared.translators InviterReward model
                , translators = shared.translators
                }
                |> Input.withCurrency community.symbol
                |> Input.toHtml
            , Input.init
                { label = t "community.create.labels.invited_reward"
                , id = "invited_reward_field"
                , onInput = EnteredInvitedReward
                , disabled = False
                , value = model.invitedReward
                , placeholder = Just (fillWithPrecision 5)
                , problems = errorsForField shared.translators InvitedReward model
                , translators = shared.translators
                }
                |> Input.withCurrency community.symbol
                |> Input.toHtml
            , Input.init
                { label = t "community.create.labels.min_balance"
                , id = "minimum_balance_field"
                , onInput = EnteredMinimumBalance
                , disabled = False
                , value = model.minimumBalance
                , placeholder = Just (fillWithPrecision 0)
                , problems = errorsForField shared.translators MinimumBalance model
                , translators = shared.translators
                }
                |> Input.withCurrency community.symbol
                |> Input.toHtml
            , button
                [ class "button button-primary w-full mt-12"
                , disabled model.isLoading
                ]
                [ text (t "menu.save") ]
            ]
        ]


errorsForField : Translators -> Field -> Model -> Maybe (List String)
errorsForField translators field model =
    List.filter (isFieldError field) model.errors
        |> List.map (Tuple.second >> translators.t)
        |> Just



-- UTILS


encodeValidModel : ValidModel -> Encode.Value
encodeValidModel validModel =
    Encode.object
        [ ( "inviterReward", Encode.float validModel.inviterReward )
        , ( "invitedReward", Encode.float validModel.invitedReward )
        , ( "minimumBalance", Encode.float validModel.minimumBalance )
        ]


validModelDecoder : Decode.Decoder ValidModel
validModelDecoder =
    Decode.map3 ValidModel
        (Decode.field "inviterReward" Decode.float)
        (Decode.field "invitedReward" Decode.float)
        (Decode.field "minimumBalance" Decode.float)


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
                    [ Decode.map2 (\_ validModel -> Ok validModel)
                        (Decode.field "transactionId" Decode.string)
                        (Decode.field "addressData" validModelDecoder)
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

        EnteredInviterReward _ ->
            [ "EnteredInviterReward" ]

        EnteredInvitedReward _ ->
            [ "EnteredInvitedReward" ]

        EnteredMinimumBalance _ ->
            [ "EnteredMinimumBalance" ]

        ClickedSubmit ->
            [ "ClickedSubmit" ]

        GotSubmitResponse r ->
            [ "GotSubmitResponse", UR.resultToString r ]

        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]
