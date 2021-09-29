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
import View.Form.Input
import View.Form.Select
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
    let
        firstCurrencyOption =
            community.contributionConfiguration
                |> Maybe.andThen (List.head << .acceptedCurrencies)
                |> Maybe.withDefault Cambiatus.Enum.CurrencyType.Brl
    in
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
            , View.Form.Select.init
                { id = "currency-select"
                , label = translators.t "sponsorship.fiat.accepted_currencies"
                , onInput = \_ -> NoOp
                , firstOption = currencyTypeToSelectOption firstCurrencyOption
                , value = Cambiatus.Enum.CurrencyType.Brl
                , valueToString = Cambiatus.Enum.CurrencyType.toString
                , disabled = True
                , problems = Nothing
                }
                |> View.Form.Select.withOptions
                    (Cambiatus.Enum.CurrencyType.list
                        |> List.filter ((/=) firstCurrencyOption)
                        |> List.map currencyTypeToSelectOption
                    )
                |> View.Form.Select.toHtml
            ]
        ]


currencyTypeToSelectOption : Cambiatus.Enum.CurrencyType.CurrencyType -> { value : Cambiatus.Enum.CurrencyType.CurrencyType, label : String }
currencyTypeToSelectOption currencyType =
    { value = currencyType, label = Cambiatus.Enum.CurrencyType.toString currencyType }



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
