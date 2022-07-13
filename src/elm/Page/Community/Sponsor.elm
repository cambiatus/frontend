module Page.Community.Sponsor exposing (Model, Msg, init, msgToString, receiveBroadcast, subscriptions, update, view)

import Browser.Dom
import Cambiatus.Enum.CurrencyType
import Cambiatus.Mutation
import Cambiatus.Object.Community
import Cambiatus.Object.Contribution
import Community
import Dict
import Environment
import Form exposing (Form)
import Form.Select
import Form.Text
import Form.Validate
import Graphql.Http
import Graphql.Operation exposing (RootMutation)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html, div, img, p, text)
import Html.Attributes exposing (class, src)
import Log
import Page
import Ports
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import Task
import Translation exposing (Translators)
import UpdateResult as UR
import Utils
import View.Feedback
import View.PaypalButtons as PaypalButtons



-- MODEL


type alias Model =
    { isCreatingOrder : Bool
    , externalAmountProblem : Maybe AmountProblem
    , form : Form.Model FormInput
    }


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( { isCreatingOrder = False
      , externalAmountProblem = Nothing
      , form =
            Form.init
                { amount = ""
                , selectedCurrency = defaultPaypalCurrency loggedIn.shared
                }
      }
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )



-- TYPES


type alias Contribution =
    { amount : Float
    , communityName : String
    , currency : Cambiatus.Enum.CurrencyType.CurrencyType
    , id : String
    }


type AmountProblem
    = InvalidAmount
    | AmountTooSmall
    | AmountTooBig



-- UPDATE


type Msg
    = NoOp
    | CompletedLoadCommunity Community.Model
    | RequestedPaypalInfoFromJs String
    | StartedCreatingContribution String FormOutput
    | CreatedContribution String (RemoteData (Graphql.Http.Error (Maybe Contribution)) (Maybe Contribution))
    | PaypalApproved
    | PaypalCanceled
    | PaypalErrored PaypalButtons.Error
    | GotFormMsg (Form.Msg FormInput)


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        NoOp ->
            UR.init model

        CompletedLoadCommunity community ->
            { model
                | form =
                    Form.updateValues
                        (\values ->
                            { values
                                | selectedCurrency =
                                    getPaypalCurrency community.contributionConfiguration loggedIn.shared
                            }
                        )
                        model.form
            }
                |> UR.init
                |> UR.addCmd focusAmountField

        RequestedPaypalInfoFromJs id ->
            case RemoteData.map .contributionConfiguration loggedIn.selectedCommunity of
                RemoteData.Success (Just configuration) ->
                    UR.init model
                        |> UR.addMsg
                            (Form.parse (createForm loggedIn.shared configuration model)
                                model.form
                                { onError = GotFormMsg
                                , onSuccess = StartedCreatingContribution id
                                }
                            )

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Tried creating a contribution, but community wasn't loaded, so couldn't get contribution configuration"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Sponsor", function = "update" }
                            []

        StartedCreatingContribution id formOutput ->
            { model | isCreatingOrder = True }
                |> UR.init
                |> UR.addExt
                    (LoggedIn.mutation loggedIn
                        (createContributionSelectionSet
                            { amount = formOutput.amount
                            , currency = toCurrencyType formOutput.currency
                            }
                        )
                        (CreatedContribution id)
                    )

        CreatedContribution id (RemoteData.Success (Just contribution)) ->
            model
                |> UR.init
                |> UR.addCmd
                    (Ports.sendPaypalInfo
                        (Ports.SuccessfulContribution
                            { amount = contribution.amount
                            , communityName = contribution.communityName
                            , targetId = id
                            , invoiceId = contribution.id
                            , currency = contribution.currency
                            }
                        )
                    )

        CreatedContribution _ (RemoteData.Failure err) ->
            model
                |> UR.init
                |> UR.addCmd (Ports.sendPaypalInfo Ports.ContributionWithError)
                |> UR.addExt (LoggedIn.ShowFeedback View.Feedback.Failure (loggedIn.shared.translators.t "sponsorship.contribution_error"))
                |> UR.logGraphqlError msg
                    (Just loggedIn.accountName)
                    "Got an error when creating a contribution"
                    { moduleName = "Page.Community.Sponsor", function = "update" }
                    [ Log.contextFromCommunity loggedIn.selectedCommunity ]
                    err

        CreatedContribution _ _ ->
            model
                |> UR.init

        PaypalApproved ->
            { model | isCreatingOrder = False }
                |> UR.init
                |> UR.addCmd (Route.pushUrl loggedIn.shared.navKey Route.CommunityThankYou)

        PaypalCanceled ->
            { model | isCreatingOrder = False }
                |> UR.init
                |> UR.addCmd focusAmountField
                |> UR.addBreadcrumb
                    { type_ = Log.InfoBreadcrumb
                    , category = msg
                    , message = "Canceled PayPal contribution"
                    , data = Dict.empty
                    , level = Log.Info
                    }

        PaypalErrored PaypalButtons.AmountTooSmall ->
            { model
                | externalAmountProblem = Just AmountTooSmall
                , isCreatingOrder = False
            }
                |> UR.init
                |> UR.addCmd focusAmountField

        PaypalErrored PaypalButtons.AmountTooBig ->
            { model
                | externalAmountProblem = Just AmountTooBig
                , isCreatingOrder = False
            }
                |> UR.init
                |> UR.addCmd focusAmountField

        PaypalErrored PaypalButtons.InvalidAmount ->
            { model
                | externalAmountProblem = Just InvalidAmount
                , isCreatingOrder = False
            }
                |> UR.init
                |> UR.addCmd focusAmountField

        PaypalErrored (PaypalButtons.LoadError loadError) ->
            { model | isCreatingOrder = False }
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback View.Feedback.Failure (loggedIn.shared.translators.t "sponsorship.load_error"))
                |> UR.logEvent
                    { username = Just loggedIn.accountName
                    , message = "Error loading paypal buttons"
                    , tags = [ Log.TypeTag Log.PaypalError ]
                    , location = { moduleName = "Page.Community.Sponsor", function = "update" }
                    , contexts =
                        [ Log.contextFromCommunity loggedIn.selectedCommunity
                        , { name = "Paypal Error", extras = Dict.fromList [ ( "JSON error", loadError ) ] }
                        ]
                    , transaction = msg
                    , level = Log.Error
                    }

        PaypalErrored (PaypalButtons.UnknownError unknownError) ->
            { model | isCreatingOrder = False }
                |> UR.init
                |> UR.addExt (LoggedIn.ShowFeedback View.Feedback.Failure (loggedIn.shared.translators.t "sponsorship.unknown_error"))
                |> UR.logEvent
                    { username = Just loggedIn.accountName
                    , message = "Something went wrong with PayPal buttons"
                    , tags = [ Log.TypeTag Log.PaypalError ]
                    , location = { moduleName = "Page.Community.Sponsor", function = "update" }
                    , contexts =
                        [ Log.contextFromCommunity loggedIn.selectedCommunity
                        , { name = "Paypal Error", extras = Dict.fromList [ ( "JSON error", unknownError ) ] }
                        ]
                    , transaction = msg
                    , level = Log.Error
                    }

        GotFormMsg subMsg ->
            Form.update loggedIn.shared subMsg model.form
                |> UR.fromChild
                    (\newForm -> { model | form = newForm })
                    GotFormMsg
                    LoggedIn.addFeedback
                    model


amountFieldId : String
amountFieldId =
    "sponsor-amount-input"


defaultPaypalCurrency : Shared -> PaypalButtons.Currency
defaultPaypalCurrency shared =
    case shared.environment of
        Environment.Development ->
            PaypalButtons.BRL

        Environment.Staging ->
            PaypalButtons.BRL

        Environment.Demo ->
            PaypalButtons.USD

        Environment.Production ->
            PaypalButtons.USD


getPaypalCurrency : Maybe Community.ContributionConfiguration -> Shared -> PaypalButtons.Currency
getPaypalCurrency maybeConfig shared =
    maybeConfig
        |> Maybe.map (.acceptedCurrencies >> List.filterMap toPaypalCurrency)
        |> Maybe.andThen List.head
        |> Maybe.withDefault (defaultPaypalCurrency shared)


toPaypalCurrency : Cambiatus.Enum.CurrencyType.CurrencyType -> Maybe PaypalButtons.Currency
toPaypalCurrency currencyType =
    case currencyType of
        Cambiatus.Enum.CurrencyType.Brl ->
            Just PaypalButtons.BRL

        Cambiatus.Enum.CurrencyType.Btc ->
            Nothing

        Cambiatus.Enum.CurrencyType.Crc ->
            Nothing

        Cambiatus.Enum.CurrencyType.Eos ->
            Nothing

        Cambiatus.Enum.CurrencyType.Eth ->
            Nothing

        Cambiatus.Enum.CurrencyType.Usd ->
            Just PaypalButtons.USD


toCurrencyType : PaypalButtons.Currency -> Cambiatus.Enum.CurrencyType.CurrencyType
toCurrencyType currency =
    case currency of
        PaypalButtons.BRL ->
            Cambiatus.Enum.CurrencyType.Brl

        PaypalButtons.USD ->
            Cambiatus.Enum.CurrencyType.Usd


toHumanString : Translators -> PaypalButtons.Currency -> String
toHumanString { t } currency =
    case currency of
        PaypalButtons.BRL ->
            t "currency.brl_plural"

        PaypalButtons.USD ->
            t "currency.usd_plural"



-- FORMS


type alias FormInput =
    { amount : String
    , selectedCurrency : PaypalButtons.Currency
    }


type alias FormOutput =
    { amount : Float
    , currency : PaypalButtons.Currency
    }


createForm : Shared -> Community.ContributionConfiguration -> Model -> Form msg FormInput FormOutput
createForm ({ translators } as shared) contributionConfiguration model =
    let
        defaultCurrency =
            defaultPaypalCurrency shared
    in
    Form.succeed FormOutput
        |> Form.with
            (Form.introspect
                (\{ selectedCurrency } ->
                    Form.Text.init
                        { label = translators.t "sponsorship.enter_amount"
                        , id = amountFieldId
                        }
                        |> Form.Text.withCurrency (PaypalButtons.currencyToSymbol selectedCurrency)
                        |> Form.Text.withLabelAttrs [ class "text-purple-500 text-lg text-center normal-case" ]
                        |> Form.textField
                            { parser =
                                Form.Validate.succeed
                                    >> Form.Validate.maskedFloat translators
                                    >> Form.Validate.custom
                                        (\x ->
                                            if x < PaypalButtons.minimumAmount then
                                                Err
                                                    (\translators_ ->
                                                        AmountTooSmall
                                                            |> amountProblemToString translators_
                                                                selectedCurrency
                                                    )

                                            else if x > PaypalButtons.maximumAmount then
                                                Err
                                                    (\translators_ ->
                                                        AmountTooBig
                                                            |> amountProblemToString translators_
                                                                selectedCurrency
                                                    )

                                            else
                                                Ok x
                                        )
                                    >> Form.Validate.validate translators
                            , value = .amount
                            , update = \amount input -> { input | amount = amount }
                            , externalError =
                                \_ ->
                                    model.externalAmountProblem
                                        |> Maybe.map (amountProblemToString translators selectedCurrency)
                            }
                )
            )
        |> Form.with
            (case
                contributionConfiguration.acceptedCurrencies
                    |> List.filterMap toPaypalCurrency
             of
                [] ->
                    Form.succeed defaultCurrency

                [ currency ] ->
                    Form.succeed currency

                currencies ->
                    let
                        toOption currency =
                            { option = currency
                            , label =
                                toHumanString translators currency
                                    ++ " ("
                                    ++ PaypalButtons.currencyToString currency
                                    ++ ")"
                            }
                    in
                    Form.Select.init
                        { id = "currency-select"
                        , label = translators.t "sponsorship.select_currency"
                        , optionToString = PaypalButtons.currencyToString
                        }
                        |> Form.Select.withOptions (List.map toOption currencies)
                        |> Form.Select.withLabelAttrs [ class "text-purple-500 text-lg text-center normal-case" ]
                        |> Form.Select.withContainerAttrs [ class "mb-10" ]
                        |> Form.select
                            (PaypalButtons.currencyFromString >> Maybe.withDefault defaultCurrency)
                            { parser = Ok
                            , value = .selectedCurrency
                            , update = \currency input -> { input | selectedCurrency = currency }
                            , externalError = always Nothing
                            }
            )



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn model =
    let
        title =
            loggedIn.shared.translators.t "sponsorship.sponsor"

        content =
            div [ class "flex flex-col md:flex-grow" ]
                [ Page.viewHeader loggedIn title
                , case loggedIn.selectedCommunity of
                    RemoteData.Success community ->
                        case community.contributionConfiguration of
                            Nothing ->
                                Page.fullPageNotFound
                                    (loggedIn.shared.translators.t "sponsorship.no_support")
                                    (loggedIn.shared.translators.t "sponsorship.no_support_subtitle")

                            Just contributionConfiguration ->
                                view_ loggedIn.shared community contributionConfiguration model

                    RemoteData.Loading ->
                        Page.fullPageLoading loggedIn.shared

                    RemoteData.NotAsked ->
                        Page.fullPageLoading loggedIn.shared

                    RemoteData.Failure err ->
                        Page.fullPageGraphQLError title err
                ]
    in
    { title = title
    , content = content
    }


view_ : Shared -> Community.Model -> Community.ContributionConfiguration -> Model -> Html Msg
view_ ({ translators } as shared) community contributionConfiguration model =
    div [ class "m-4 bg-white rounded md:m-0 md:bg-white md:flex-grow" ]
        [ div [ class "container mx-auto" ]
            [ div [ class "flex flex-col items-center w-full px-4 py-7 bg-white md:w-1/2 md:mx-auto" ]
                [ img [ class "h-10", src community.logo ] []
                , p [ class "font-bold text-black text-3xl mb-8" ] [ text community.name ]
                , Form.viewWithoutSubmit [ class "w-full lg:w-3/4" ]
                    translators
                    (\{ amount, selectedCurrency } ->
                        [ PaypalButtons.view [ class "w-full lg:w-3/4" ]
                            { id = "paypal-buttons-sponsorship"
                            , value =
                                amount
                                    |> Translation.floatStringFromSeparatedString translators
                                    |> String.toFloat
                                    |> Maybe.andThen
                                        (\floatAmount ->
                                            if floatAmount < PaypalButtons.minimumAmount then
                                                Nothing

                                            else if floatAmount > PaypalButtons.maximumAmount then
                                                Nothing

                                            else
                                                Just floatAmount
                                        )
                            , currency = selectedCurrency
                            , onApprove = PaypalApproved
                            , onCancel = PaypalCanceled
                            , onError = PaypalErrored
                            }
                        ]
                    )
                    (createForm shared contributionConfiguration model)
                    model.form
                    { toMsg = GotFormMsg }
                ]
            ]
        ]


amountProblemToString : Translators -> PaypalButtons.Currency -> AmountProblem -> String
amountProblemToString translators currency amountProblem =
    case amountProblem of
        InvalidAmount ->
            translators.t "sponsorship.invalid_amount"

        AmountTooSmall ->
            translators.tr "sponsorship.amount_too_small"
                [ ( "minimum", Utils.formatFloat (Just translators) 2 PaypalButtons.minimumAmount )
                , ( "currency", PaypalButtons.currencyToString currency )
                ]

        AmountTooBig ->
            translators.tr "sponsorship.amount_too_big"
                [ ( "maximum", Utils.formatFloat (Just translators) 2 PaypalButtons.maximumAmount )
                , ( "currency", PaypalButtons.currencyToString currency )
                ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.requestPaypalInfoFromJs RequestedPaypalInfoFromJs



-- GRAPHQL


createContributionSelectionSet : { amount : Float, currency : Cambiatus.Enum.CurrencyType.CurrencyType } -> SelectionSet (Maybe Contribution) RootMutation
createContributionSelectionSet { amount, currency } =
    Cambiatus.Mutation.contribution
        { amount = amount, currency = currency }
        (SelectionSet.succeed Contribution
            |> SelectionSet.with Cambiatus.Object.Contribution.amount
            |> SelectionSet.with (Cambiatus.Object.Contribution.community Cambiatus.Object.Community.name)
            |> SelectionSet.with Cambiatus.Object.Contribution.currency
            |> SelectionSet.with Cambiatus.Object.Contribution.id
        )



-- UTILS


receiveBroadcast : LoggedIn.BroadcastMsg -> Maybe Msg
receiveBroadcast broadcastMsg =
    case broadcastMsg of
        LoggedIn.CommunityLoaded community ->
            Just (CompletedLoadCommunity community)

        _ ->
            Nothing


focusAmountField : Cmd Msg
focusAmountField =
    Browser.Dom.focus amountFieldId
        |> Task.attempt (\_ -> NoOp)


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        RequestedPaypalInfoFromJs _ ->
            [ "RequestedPaypalInfoFromJs" ]

        StartedCreatingContribution _ _ ->
            [ "StartedCreatingContribution" ]

        CreatedContribution _ _ ->
            [ "CreatedContribution" ]

        PaypalApproved ->
            [ "PaypalApproved" ]

        PaypalCanceled ->
            [ "PaypalCanceled" ]

        PaypalErrored _ ->
            [ "PaypalErrored" ]

        GotFormMsg subMsg ->
            "GotFormMsg" :: Form.msgToString subMsg
