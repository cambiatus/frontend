module Page.Community.Settings.Currency exposing
    ( Model
    , Msg
    , init
    , msgToString
    , receiveBroadcast
    , update
    , view
    )

import Community
import Eos
import Html exposing (Html, button, div, form, span, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onSubmit)
import Page
import RemoteData
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Translators)
import UpdateResult as UR
import View.Form.Input as Input



-- MODEL


type alias Model =
    { inviterReward : String
    , invitedReward : String
    , minimumBalance : String
    , isLoading : Bool
    , errors : List ( Field, String )
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
    | CompletedLoadCommunity Community.Model


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        Ignored ->
            UR.init model

        EnteredInviterReward inviterReward ->
            { model | inviterReward = inviterReward }
                |> withSymbol validateInviterReward loggedIn
                |> UR.init

        EnteredInvitedReward invitedReward ->
            { model | invitedReward = invitedReward }
                |> withSymbol validateInvitedReward loggedIn
                |> UR.init

        EnteredMinimumBalance minimumBalance ->
            { model | minimumBalance = minimumBalance }
                |> withSymbol validateMinimumBalance loggedIn
                |> UR.init

        ClickedSubmit ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    case validateModel community.symbol model of
                        Ok validatedModel ->
                            if LoggedIn.hasPrivateKey loggedIn then
                                -- TODO - Persist data
                                { validatedModel | isLoading = True }
                                    |> UR.init

                            else
                                UR.init validatedModel
                                    |> UR.addExt
                                        (Just ClickedSubmit
                                            |> LoggedIn.RequiredAuthentication
                                        )

                        Err withError ->
                            UR.init withError

                _ ->
                    UR.init model
                        |> UR.logImpossible msg [ "CommunityNotLoaded" ]

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


withSymbol : (Eos.Symbol -> Model -> Model) -> LoggedIn.Model -> Model -> Model
withSymbol fn loggedIn model =
    case loggedIn.selectedCommunity of
        RemoteData.Success community ->
            fn community.symbol model

        _ ->
            model



-- VALIDATION


type Field
    = InviterReward
    | InvitedReward
    | MinimumBalance


isFieldError : Field -> ( Field, String ) -> Bool
isFieldError field ( errorField, _ ) =
    field == errorField


validateNumberInput : Eos.Symbol -> String -> Result String String
validateNumberInput symbol numberInput =
    let
        validateParsing =
            case String.toFloat numberInput of
                Nothing ->
                    Err "error.validator.text.only_numbers"

                Just _ ->
                    Ok numberInput
    in
    case String.split "." numberInput of
        [] ->
            Err "error.required"

        [ "" ] ->
            Err "error.required"

        [ _ ] ->
            validateParsing

        _ :: decimalDigits :: _ ->
            if
                (String.isEmpty decimalDigits && Eos.getSymbolPrecision symbol == 0)
                    || (String.length decimalDigits > Eos.getSymbolPrecision symbol)
            then
                Err "error.contracts.transfer.symbol precision mismatch"

            else
                validateParsing


setErrors : Field -> Model -> Result String String -> Model
setErrors field model validationResult =
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


validateInviterReward : Eos.Symbol -> Model -> Model
validateInviterReward symbol model =
    validateNumberInput symbol model.inviterReward
        |> setErrors InviterReward model


validateInvitedReward : Eos.Symbol -> Model -> Model
validateInvitedReward symbol model =
    validateNumberInput symbol model.invitedReward
        |> setErrors InvitedReward model


validateMinimumBalance : Eos.Symbol -> Model -> Model
validateMinimumBalance symbol model =
    validateNumberInput symbol model.minimumBalance
        |> setErrors MinimumBalance model


validateModel : Eos.Symbol -> Model -> Result Model Model
validateModel symbol model =
    let
        validatedModel =
            model
                |> validateInviterReward symbol
                |> validateInvitedReward symbol
                |> validateMinimumBalance symbol
    in
    if List.isEmpty validatedModel.errors then
        Ok validatedModel

    else
        Err validatedModel



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


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityLoaded community ->
            Just (CompletedLoadCommunity community)

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

        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]
