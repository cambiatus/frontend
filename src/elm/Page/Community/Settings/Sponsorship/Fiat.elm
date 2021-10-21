module Page.Community.Settings.Sponsorship.Fiat exposing (Model, Msg, init, msgToString, receiveBroadcast, update, view)

import Cambiatus.Enum.CurrencyType
import Community
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Icons
import Maybe.Extra
import Page
import RemoteData
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared exposing (Shared)
import UpdateResult as UR
import View.Form.Checkbox
import View.Form.Input
import View.Form.Toggle



-- MODEL


type alias Model =
    {}


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( {}
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )



-- TYPES


type Msg
    = NoOp
    | CompletedLoadCommunity Community.Model


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)



-- UPDATE


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        NoOp ->
            UR.init model

        CompletedLoadCommunity community ->
            let
                maybeRedirect =
                    if community.creator /= loggedIn.accountName then
                        UR.addCmd (Route.pushUrl loggedIn.shared.navKey Route.Dashboard)

                    else
                        identity
            in
            model
                |> UR.init
                |> maybeRedirect



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view loggedIn _ =
    let
        title =
            loggedIn.shared.translators.t "sponsorship.fiat.title"

        content =
            case loggedIn.selectedCommunity of
                RemoteData.Success community ->
                    div []
                        [ Page.viewHeader loggedIn title
                        , view_ loggedIn.shared community
                        ]

                RemoteData.Loading ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.NotAsked ->
                    Page.fullPageLoading loggedIn.shared

                RemoteData.Failure err ->
                    Page.fullPageGraphQLError title err
    in
    { title = title, content = content }


view_ : Shared -> Community.Model -> Html Msg
view_ { translators } community =
    div [ class "container mx-auto px-4 my-4" ]
        [ div [ class "p-4 bg-white rounded" ]
            [ View.Form.Toggle.init
                { label =
                    div [ class "flex" ]
                        [ Icons.paypal "mr-2"
                        , text "Paypal"
                        ]
                , id = "paypal-toggle"
                , onToggle = \_ -> NoOp
                , disabled = True
                , value =
                    community.contributionConfiguration
                        |> Maybe.andThen .paypalAccount
                        |> Maybe.Extra.isJust
                }
                |> View.Form.Toggle.toHtml translators
            , View.Form.Input.init
                { label = translators.t "sponsorship.fiat.paypal_account"
                , id = "paypal-account-input"
                , onInput = \_ -> NoOp
                , disabled = True
                , value =
                    community.contributionConfiguration
                        |> Maybe.andThen .paypalAccount
                        |> Maybe.withDefault ""
                , placeholder = Just (translators.t "sponsorship.fiat.paypal_example")
                , problems = Nothing
                , translators = translators
                }
                |> View.Form.Input.withContainerAttrs [ class "mt-5 mb-2" ]
                |> View.Form.Input.withAttrs []
                |> View.Form.Input.toHtml
            , p [ class "mb-10 text-gray-900" ]
                [ text (translators.t "sponsorship.fiat.how_to_change") ]

            -- TODO - Use new typography classes (#622)
            , p [ class "text-green tracking-wide uppercase text-caption block mb-2" ]
                [ text (translators.t "sponsorship.cards.fiat.title") ]
            , div [ class "grid xs-max:grid-cols-1 grid-cols-2 sm:grid-cols-3 lg:grid-cols-6 gap-y-6" ]
                (Cambiatus.Enum.CurrencyType.list
                    |> List.filter isPaypalCurrency
                    |> List.map (currencyTypeToRadioButton community)
                )
            ]
        ]


isPaypalCurrency : Cambiatus.Enum.CurrencyType.CurrencyType -> Bool
isPaypalCurrency currencyType =
    case currencyType of
        Cambiatus.Enum.CurrencyType.Brl ->
            True

        Cambiatus.Enum.CurrencyType.Btc ->
            False

        Cambiatus.Enum.CurrencyType.Crc ->
            False

        Cambiatus.Enum.CurrencyType.Eos ->
            False

        Cambiatus.Enum.CurrencyType.Eth ->
            False

        Cambiatus.Enum.CurrencyType.Usd ->
            True


currencyTypeToRadioButton : Community.Model -> Cambiatus.Enum.CurrencyType.CurrencyType -> Html Msg
currencyTypeToRadioButton community currencyType =
    let
        asString =
            Cambiatus.Enum.CurrencyType.toString currencyType
    in
    View.Form.Checkbox.init
        { description = text asString
        , id = "currencies_" ++ asString
        , value =
            community.contributionConfiguration
                |> Maybe.map (\config -> List.member currencyType config.acceptedCurrencies)
                |> Maybe.withDefault False
        , disabled = True
        , onCheck = \_ -> NoOp
        }
        |> View.Form.Checkbox.withContainerAttrs [ class "flex items-center" ]
        |> View.Form.Checkbox.toHtml



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
        NoOp ->
            [ "NoOp" ]

        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]
