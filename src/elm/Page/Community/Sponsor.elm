module Page.Community.Sponsor exposing (Model, Msg, init, msgToString, receiveBroadcast, subscriptions, update, view)

import Api.Graphql
import Browser.Dom
import Cambiatus.Enum.CurrencyType
import Cambiatus.Mutation
import Cambiatus.Object.Community
import Cambiatus.Object.Contribution
import Community
import Dict
import Eos
import Graphql.Http
import Graphql.Operation exposing (RootMutation)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (Html, div, h1, img, label, text)
import Html.Attributes exposing (class, for, src)
import Log
import Mask
import Page
import Ports
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared as Shared exposing (Shared, Translators)
import Task
import UpdateResult as UR
import Utils
import View.Feedback
import View.Form.Input as Input
import View.Form.Select as Select
import View.PaypalButtons as PaypalButtons



-- MODEL


type alias Model =
    { amount : String
    , amountProblem : Maybe AmountProblem
    , isCreatingOrder : Bool
    , selectedCurrency : PaypalButtons.Currency
    }


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( { amount = ""
      , amountProblem = Nothing
      , isCreatingOrder = False
      , selectedCurrency = defaultPaypalCurrency loggedIn.shared
      }
    , Cmd.batch
        [ focusAmountField
        , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
        ]
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
    | SelectedCurrency PaypalButtons.Currency
    | EnteredAmount String
    | RequestedPaypalInfoFromJs String
    | CreatedContribution String (RemoteData (Graphql.Http.Error (Maybe Contribution)) (Maybe Contribution))
    | PaypalApproved
    | PaypalCanceled
    | PaypalErrored PaypalButtons.Error


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        NoOp ->
            UR.init model

        CompletedLoadCommunity community ->
            { model | selectedCurrency = getPaypalCurrency community.contributionConfiguration loggedIn.shared }
                |> UR.init

        SelectedCurrency currency ->
            { model | selectedCurrency = currency }
                |> UR.init

        EnteredAmount amount ->
            { model
                | amount = amount
                , amountProblem =
                    case
                        amount
                            |> Mask.removeFloat (Shared.decimalSeparators loggedIn.shared.translators)
                            |> String.toFloat
                    of
                        Nothing ->
                            Just InvalidAmount

                        Just value ->
                            if value < PaypalButtons.minimumAmount then
                                Just AmountTooSmall

                            else if value > PaypalButtons.maximumAmount then
                                Just AmountTooBig

                            else
                                Nothing
            }
                |> UR.init

        RequestedPaypalInfoFromJs id ->
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    case
                        model.amount
                            |> Mask.removeFloat (Shared.decimalSeparators loggedIn.shared.translators)
                            |> String.toFloat
                    of
                        Nothing ->
                            -- We handle this case on JS. If the amount is not a
                            -- valid `Float`, we get an `InvalidAmount` error
                            model
                                |> UR.init

                        Just amount ->
                            { model | isCreatingOrder = True }
                                |> UR.init
                                |> UR.addCmd
                                    (Api.Graphql.mutation loggedIn.shared
                                        (Just loggedIn.authToken)
                                        (createContributionSelectionSet
                                            { amount = amount
                                            , communityId = community.symbol
                                            , currency = toCurrencyType model.selectedCurrency
                                            }
                                        )
                                        (CreatedContribution id)
                                    )

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Tried creating a contribution, but community wasn't loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Sponsor", function = "update" }
                            []

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
                | amountProblem = Just AmountTooSmall
                , isCreatingOrder = False
            }
                |> UR.init
                |> UR.addCmd focusAmountField

        PaypalErrored PaypalButtons.AmountTooBig ->
            { model
                | amountProblem = Just AmountTooBig
                , isCreatingOrder = False
            }
                |> UR.init
                |> UR.addCmd focusAmountField

        PaypalErrored PaypalButtons.InvalidAmount ->
            { model
                | amountProblem = Just InvalidAmount
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


amountFieldId : String
amountFieldId =
    "sponsor-amount-input"


defaultPaypalCurrency : Shared -> PaypalButtons.Currency
defaultPaypalCurrency shared =
    case shared.environment of
        Shared.Development ->
            PaypalButtons.BRL

        Shared.Staging ->
            PaypalButtons.BRL

        Shared.Demo ->
            PaypalButtons.USD

        Shared.Production ->
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
                (img [ class "h-10", src community.logo ] []
                    -- TODO - Use new typography text-size class (#622)
                    :: h1 [ class "font-bold text-black text-[46px] mb-8" ] [ text community.name ]
                    :: label
                        [ for amountFieldId

                        -- TODO - Move this to the input's label, use new typography text-size class (#622)
                        , class "text-center text-purple-500 text-[22px] mb-2 font-bold"
                        ]
                        [ text <| translators.t "sponsorship.enter_amount" ]
                    :: (Input.init
                            { label = ""
                            , id = amountFieldId
                            , onInput = EnteredAmount
                            , disabled = model.isCreatingOrder
                            , value = model.amount
                            , placeholder = Nothing
                            , problems =
                                model.amountProblem
                                    |> Maybe.map
                                        (amountProblemToString translators
                                            model.selectedCurrency
                                            >> List.singleton
                                        )
                            , translators = translators
                            }
                            |> Input.withContainerAttrs [ class "w-full lg:w-2/3" ]
                            |> Input.withCurrency (PaypalButtons.currencyToSymbol model.selectedCurrency)
                            |> Input.toHtml
                       )
                    :: (case
                            contributionConfiguration.acceptedCurrencies
                                |> List.filterMap toPaypalCurrency
                        of
                            [] ->
                                []

                            [ _ ] ->
                                []

                            firstCurrency :: otherCurrencies ->
                                let
                                    currencyOption currency =
                                        { value = currency
                                        , label =
                                            toHumanString translators currency
                                                ++ " ("
                                                ++ PaypalButtons.currencyToString currency
                                                ++ ")"
                                        }
                                in
                                -- TODO - Move this to the input's label, use new typography text-size class (#622)
                                [ label
                                    [ for "currency-selector"
                                    , class "text-center text-purple-500 text-[22px] mb-2 font-bold"
                                    ]
                                    [ text <| translators.t "sponsorship.select_currency" ]
                                , Select.init
                                    { id = "currency-selector"
                                    , label = ""
                                    , onInput = SelectedCurrency
                                    , firstOption = currencyOption firstCurrency
                                    , value = model.selectedCurrency
                                    , valueToString =
                                        PaypalButtons.currencyToSymbol
                                            >> Eos.symbolToSymbolCodeString
                                    , disabled = model.isCreatingOrder
                                    , problems = Nothing
                                    }
                                    |> Select.withOptions (List.map currencyOption otherCurrencies)
                                    |> Select.withContainerAttrs [ class "w-full lg:w-2/3" ]
                                    |> Select.toHtml
                                ]
                       )
                    ++ [ PaypalButtons.view [ class "w-full lg:w-2/3" ]
                            { id = "sponsorship-paypal-buttons"
                            , value =
                                model.amount
                                    |> Mask.removeFloat (Shared.decimalSeparators translators)
                                    |> String.toFloat
                                    |> Maybe.andThen
                                        (\amount ->
                                            if amount < PaypalButtons.minimumAmount then
                                                Nothing

                                            else if amount > PaypalButtons.maximumAmount then
                                                Nothing

                                            else
                                                Just amount
                                        )
                            , currency = model.selectedCurrency
                            , onApprove = PaypalApproved
                            , onCancel = PaypalCanceled
                            , onError = PaypalErrored
                            }
                       ]
                )
            ]
        ]


amountProblemToString : Translators -> PaypalButtons.Currency -> AmountProblem -> String
amountProblemToString translators currency amountProblem =
    case amountProblem of
        InvalidAmount ->
            translators.t "sponsorship.invalid_amount"

        AmountTooSmall ->
            translators.tr "sponsorship.amount_too_small"
                [ ( "minimum", Utils.formatFloat PaypalButtons.minimumAmount 2 False )
                , ( "currency", PaypalButtons.currencyToString currency )
                ]

        AmountTooBig ->
            translators.tr "sponsorship.amount_too_big"
                [ ( "maximum", Utils.formatFloat PaypalButtons.maximumAmount 2 False )
                , ( "currency", PaypalButtons.currencyToString currency )
                ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.requestPaypalInfoFromJs RequestedPaypalInfoFromJs



-- GRAPHQL


createContributionSelectionSet : { amount : Float, communityId : Eos.Symbol, currency : Cambiatus.Enum.CurrencyType.CurrencyType } -> SelectionSet (Maybe Contribution) RootMutation
createContributionSelectionSet { amount, communityId, currency } =
    Cambiatus.Mutation.contribution
        { amount = amount, communityId = Eos.symbolToString communityId, currency = currency }
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

        SelectedCurrency _ ->
            [ "SelectedCurrency" ]

        EnteredAmount _ ->
            [ "EnteredAmount" ]

        RequestedPaypalInfoFromJs _ ->
            [ "RequestedPaypalInfoFromJs" ]

        CreatedContribution _ _ ->
            [ "CreatedContribution" ]

        PaypalApproved ->
            [ "PaypalApproved" ]

        PaypalCanceled ->
            [ "PaypalCanceled" ]

        PaypalErrored _ ->
            [ "PaypalErrored" ]
