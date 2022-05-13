module Page.Community.Settings.Sponsorship.Fiat exposing (Model, Msg, init, msgToString, receiveBroadcast, update, view)

import Cambiatus.Enum.CurrencyType
import Community
import Form
import Form.Checkbox
import Form.Text
import Form.Toggle
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Icons
import Maybe.Extra
import Page
import RemoteData
import Route
import Session.LoggedIn as LoggedIn
import Session.Shared as Shared exposing (Shared)
import UpdateResult as UR



-- MODEL


type alias Model =
    {}


init : LoggedIn.Model -> ( Model, Cmd Msg )
init loggedIn =
    ( {}
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )


type alias FormInput =
    { isPaypalEnabled : Bool
    , paypalAccount : String
    , acceptedCurrencies : List Cambiatus.Enum.CurrencyType.CurrencyType
    }


type alias FormOutput =
    ()


createForm : Shared.Translators -> Form.Form msg FormInput FormOutput
createForm { t } =
    let
        checkboxes =
            Cambiatus.Enum.CurrencyType.list
                |> List.filter isPaypalCurrency
                |> List.map
                    (\currency ->
                        let
                            currencyString =
                                Cambiatus.Enum.CurrencyType.toString currency
                        in
                        Form.Checkbox.init
                            { label = text currencyString
                            , id = "currencies-" ++ currencyString ++ "-checkbox"
                            }
                            |> Form.checkbox
                                { parser = Ok
                                , value = \input -> List.member currency input.acceptedCurrencies
                                , update = \_ input -> input
                                , externalError = always Nothing
                                }
                    )

        addCheckboxes form =
            List.foldl Form.withNoOutput form checkboxes
    in
    Form.succeed ()
        |> Form.withNoOutput
            (Form.Toggle.init
                { label =
                    div [ class "flex" ]
                        [ Icons.paypal "mr-2"
                        , text "Paypal"
                        ]
                , id = "accept-paypal"
                }
                |> Form.toggle
                    { parser = Ok
                    , value = .isPaypalEnabled
                    , update = \_ input -> input
                    , externalError = always Nothing
                    }
            )
        |> Form.withNoOutput
            (Form.Text.init
                { label = t "sponsorship.fiat.paypal_account"
                , id = "paypal-account-input"
                }
                |> Form.Text.withPlaceholder (t "sponsorship.fiat.paypal_example")
                |> Form.Text.withContainerAttrs [ class "mt-5 mb-2" ]
                |> Form.textField
                    { parser = Ok
                    , value = .paypalAccount
                    , update = \_ input -> input
                    , externalError = always Nothing
                    }
            )
        |> Form.withDecoration
            (p [ class "mb-10 text-gray-900" ]
                [ text <| t "sponsorship.fiat.how_to_change" ]
            )
        |> Form.withDecoration
            (p [ class "label" ]
                [ text <| t "sponsorship.cards.fiat.title" ]
            )
        |> Form.withNoOutput
            (Form.succeed (\_ _ -> ())
                |> Form.withGroup [ class "grid xs-max:grid-cols-1 grid-cols-2 sm:grid-cols-3 lg:grid-cols-6 gap-y-6" ]
                    (Form.succeed ()
                        |> addCheckboxes
                    )
                    (Form.succeed ())
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
        [ Form.viewWithoutSubmit [ class "p-4 bg-white rounded" ]
            translators
            (\_ -> [])
            (createForm translators)
            (Form.init
                { isPaypalEnabled =
                    community.contributionConfiguration
                        |> Maybe.andThen .paypalAccount
                        |> Maybe.Extra.isJust
                , paypalAccount =
                    community.contributionConfiguration
                        |> Maybe.andThen .paypalAccount
                        |> Maybe.withDefault ""
                , acceptedCurrencies =
                    community.contributionConfiguration
                        |> Maybe.map .acceptedCurrencies
                        |> Maybe.withDefault []
                }
                |> Form.withDisabled True
            )
            { toMsg = \_ -> NoOp }
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
