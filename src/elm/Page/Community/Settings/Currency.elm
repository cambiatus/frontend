module Page.Community.Settings.Currency exposing
    ( Model
    , Msg
    , init
    , msgToString
    , update
    , view
    )

import Community
import Eos
import Html exposing (Html, button, div, form, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Page
import RemoteData
import Route
import Session.LoggedIn as LoggedIn
import UpdateResult as UR
import View.Form.Input as Input



-- MODEL


type alias Model =
    { inviterReward : String
    , invitedReward : String
    , minimumBalance : String
    }


init : LoggedIn.Model -> ( Model, Cmd Msg )
init _ =
    ( { inviterReward = ""
      , invitedReward = ""
      , minimumBalance = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Ignored
    | EnteredInviterReward String
    | EnteredInvitedReward String
    | EnteredMinimumBalance String
    | ClickedSubmit


type alias UpdateResult =
    UR.UpdateResult Model Msg (LoggedIn.External Msg)


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model loggedIn =
    case msg of
        Ignored ->
            UR.init model

        EnteredInviterReward inviterReward ->
            { model | inviterReward = inviterReward }
                |> UR.init

        EnteredInvitedReward invitedReward ->
            { model | invitedReward = invitedReward }
                |> UR.init

        EnteredMinimumBalance minimumBalance ->
            { model | minimumBalance = minimumBalance }
                |> UR.init

        ClickedSubmit ->
            -- TODO
            UR.init model



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
            String.join "," (String.fromInt amount :: List.repeat precision "0")
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
                , value = community.name -- TODO - This should be currency name
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
                , problems = Nothing
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
                , problems = Nothing
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
                , problems = Nothing
                , translators = shared.translators
                }
                |> Input.withCurrency community.symbol
                |> Input.toHtml
            , button
                [ class "button button-primary w-full mt-8"

                -- , disabled model.isLoading
                ]
                [ text (t "menu.save") ]
            ]
        ]



-- UTILS


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
