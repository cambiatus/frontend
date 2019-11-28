module Page.Dashboard.Community exposing (Model, Msg, UpdateResult, init, jsAddressToMsg, msgToString, update, viewCard, viewTableRow)

import Account exposing (Profile)
import Api
import Api.Graphql
import Asset.Icon as Icon
import Avatar exposing (Avatar)
import Community exposing (Action, Balance, Objective, Transaction)
import Eos
import Eos.Account as Eos
import Graphql.Http
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (class, classList, colspan, disabled, for, href, id, maxlength, minlength, placeholder, required, selected, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit, targetValue)
import Http
import I18Next exposing (Delims(..), t, tr)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode exposing (Value)
import List.Extra as List
import Log
import Ports
import Route
import Select
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared as Shared exposing (Shared)
import Simple.Fuzzy
import Transfer exposing (Transfer)
import UpdateResult as UR


init : LoggedIn.Model -> Balance -> ( Model, Cmd Msg )
init { shared } balance =
    let
        cmd =
            Api.Graphql.query shared
                (Community.logoTitleQuery balance.asset.symbol)
                CompletedDashboardInfoLoad
    in
    ( { balance = balance
      , status = ViewingBalance
      , dashboardInfo = Nothing
      , autoCompleteState = Select.newState ""
      }
    , cmd
    )



-- MODEL


type alias Model =
    { balance : Balance
    , status : Status
    , dashboardInfo : Maybe Community.DashboardInfo
    , autoCompleteState : Select.State
    }


type Status
    = ViewingBalance
    | Transferring TransferStatus


type TransferStatus
    = EditingTransfer TransferForm
    | SendingTransfer TransferForm
    | SendingTransferFailed TransferForm


type alias TransferForm =
    { selectedProfile : Maybe Profile
    , amount : String
    , memo : String
    }


emptyTransferForm : TransferForm
emptyTransferForm =
    { selectedProfile = Nothing
    , amount = ""
    , memo = ""
    }



-- VIEW


viewCard : LoggedIn.Model -> Int -> Model -> Html Msg
viewCard loggedIn index model =
    case model.status of
        ViewingBalance ->
            viewCardBalance loggedIn model

        Transferring (EditingTransfer f) ->
            viewCardTransfer loggedIn model index f False

        Transferring (SendingTransfer f) ->
            viewCardTransfer loggedIn model index f True

        Transferring (SendingTransferFailed f) ->
            viewCardFailure loggedIn


viewCardLoading : Html msg
viewCardLoading =
    div [ class "card card--full-spinner" ]
        [ div [ class "spinner spinner--delay" ] [] ]


viewCardFailure : LoggedIn.Model -> Html Msg
viewCardFailure loggedIn =
    let
        text_ loc =
            text (t loggedIn.shared.translations loc)
    in
    div
        [ class "card card--failure" ]
        [ div []
            [ p [ class "card__title--failure" ]
                [ text_ "dashboard.ops"
                , br [] []
                , text "dashboard.something_wrong"
                ]
            , p [ class "card__sub-title", style "text-align" "center" ] [ text_ "dashboard.please_try_again" ]
            ]
        , Icon.dead "card__big-icon"
        , div [ class "card__button-row" ]
            [ button
                [ class "btn btn--primary btn--big"
                , onClick ClickedBackFromFailure
                ]
                [ text_ "dashboard.back"
                ]
            ]
        , button
            [ class "card__close-btn"
            , onClick ClickedBackFromFailure
            , type_ "button"
            ]
            [ Icon.close "" ]
        ]


viewCardBalance : LoggedIn.Model -> Model -> Html Msg
viewCardBalance loggedIn ({ balance } as model) =
    let
        ipfsUrl =
            loggedIn.shared.endpoints.ipfs

        isBespiralSymbol : Bool
        isBespiralSymbol =
            balance.asset.symbol == Shared.bespiralSymbol loggedIn.shared

        text_ s =
            text (t loggedIn.shared.translations s)

        symbolText =
            Eos.symbolToString balance.asset.symbol

        balanceText =
            String.fromFloat balance.asset.amount ++ " "

        logo =
            case model.dashboardInfo of
                Just info ->
                    info.logo

                Nothing ->
                    ""

        title =
            case model.dashboardInfo of
                Just info ->
                    info.title

                Nothing ->
                    ""
    in
    div [ class "w-full sm:w-1/2 lg:w-1/3 xl:w-1/4 px-2 mb-6" ]
        [ div [ class "flex flex-wrap px-3 pt-5 pb-2 rounded-lg hover:shadow-lg bg-white" ]
            [ div [ class "w-1/3" ] [ img [ class "object-none object-scale-down h-20", src (ipfsUrl ++ "/" ++ logo) ] [] ]
            , div [ class "w-2/3 pl-4 overflow-x-hidden overflow-y-hidden" ]
                [ p [ class "font-medium leading-none font-sans" ] [ text title ]
                , div [ class "flex" ]
                    [ div [ class "text-3xl text-orange-100" ] [ text balanceText ]
                    , div [ class "uppercase text-sm font-thin mt-3 ml-2 text-orange-100 font-sans" ] [ text symbolText ]
                    ]
                , p [ class "text-xs text-gray-600 leading-none font-sans tracking-wide" ] [ text_ "account.my_wallet.balances.current" ]
                ]
            , div [ class "w-full flex-shrink" ]
                [ a
                    [ class "float-right button button-secondary button-small w-20 px-1 py-2 text-xs"
                    , Route.href (Route.Community balance.asset.symbol)
                    ]
                    [ text_ "menu.explore" ]
                ]
            , div [ class "w-full flex bg-white border-t border-gray-300 mt-2 py-3" ]
                [ a
                    [ class "button button-primary button-small w-1/2 md:px-0 lg:mx-1"
                    , Route.href (Route.Community balance.asset.symbol)
                    ]
                    [ text (t loggedIn.shared.translations "community.actions.title") ]
                , button
                    [ class "button button-primary button-small w-1/2 md:px-0 lg:mx-1"
                    , onClick ClickedTransfer
                    ]
                    [ text (t loggedIn.shared.translations "account.my_wallet.balances.button") ]
                ]
            ]
        ]


viewCardTransfer : LoggedIn.Model -> Model -> Int -> TransferForm -> Bool -> Html Msg
viewCardTransfer loggedIn ({ balance } as model) index f isDisabled =
    let
        b =
            balance

        text_ s =
            text (t loggedIn.shared.translations s)
    in
    form
        [ class "card card--transfer"
        , onSubmit ClickedSendTransfer
        ]
        [ div [ class "card__balance" ]
            [ span [ class "card__sub-title" ]
                [ text_ "account.my_wallet.balances.current" ]
            , span [ class "card__balance-text" ]
                [ text (String.fromFloat b.asset.amount ++ " " ++ Eos.symbolToString b.asset.symbol) ]
            ]
        , div [ class "card__form" ]
            [ label [] [ text_ "account.my_wallet.transfer.send_to" ]
            , div [] [ autoCompleteAccount loggedIn.shared model f isDisabled ]
            , label []
                [ text (I18Next.tr loggedIn.shared.translations Curly "account.my_wallet.transfer.amount" [ ( "symbol", Eos.symbolToString b.asset.symbol ) ]) ]
            , div [] [ viewInputAmount loggedIn.shared f isDisabled ]
            , label []
                [ text_ "account.my_wallet.transfer.memo" ]
            , div [] [ viewInputMemo loggedIn.shared f isDisabled ]
            ]
        , div [ class "card__button-row" ]
            [ button
                [ class "btn btn--primary"
                , disabled isDisabled
                ]
                [ text_ "account.my_wallet.transfer.submit" ]
            ]
        , button
            [ class "card__close-btn"
            , onClick ClickedCancelTransfer
            , type_ "button"
            , disabled isDisabled
            ]
            [ Icon.close "" ]
        ]



-- VIEW TABLE ROW


viewTableRow : LoggedIn.Model -> Model -> List (Html Msg)
viewTableRow loggedIn model =
    let
        logo =
            case model.dashboardInfo of
                Just info ->
                    info.logo

                Nothing ->
                    ""

        title =
            case model.dashboardInfo of
                Just info ->
                    info.title

                Nothing ->
                    ""
    in
    case model.status of
        ViewingBalance ->
            [ viewTableRowHelper loggedIn model.balance logo title False
            ]

        Transferring (EditingTransfer f) ->
            [ viewTableRowHelper loggedIn model.balance logo title True
            , viewTableRowTransfer loggedIn model f False
            ]

        Transferring (SendingTransfer f) ->
            [ viewTableRowHelper loggedIn model.balance logo title True
            , viewTableRowTransfer loggedIn model f True
            ]

        Transferring (SendingTransferFailed f) ->
            [ viewTableRowHelper loggedIn model.balance logo title True
            , viewTableRowTransfer loggedIn model f False
            ]


viewTableRowHelper : LoggedIn.Model -> Balance -> String -> String -> Bool -> Html Msg
viewTableRowHelper loggedIn balance logo title showTransfer =
    let
        ipfsUrl =
            loggedIn.shared.endpoints.ipfs

        isBespiralSymbol : Bool
        isBespiralSymbol =
            balance.asset.symbol == Shared.bespiralSymbol loggedIn.shared
    in
    div
        [ classList
            [ ( "card-table__row", True )
            , ( "card-table__row--highlight", isBespiralSymbol )
            , ( "card-table__row--transfer", showTransfer )
            ]
        ]
        [ div [ class "card-table__community" ]
            [ div
                [ class "card__image-background card-table__image"
                , Community.logoBackground ipfsUrl (Just logo)
                ]
                []
            , span [] [ text title ]
            ]
        , span [ class "card-table__balance" ]
            [ text (String.fromFloat balance.asset.amount ++ " " ++ Eos.symbolToString balance.asset.symbol) ]
        , div [ class "card-table__action" ]
            [ button
                [ classList
                    [ ( "btn", True )
                    , ( "btn--primary", True )
                    , ( "btn--active", showTransfer )
                    ]
                , disabled showTransfer
                , onClick ClickedTransfer
                ]
                [ text (t loggedIn.shared.translations "account.my_wallet.balances.button") ]
            , a
                [ class "btn btn--primary"
                , Route.href (Route.Community balance.asset.symbol)
                ]
                [ text (t loggedIn.shared.translations "menu.explore") ]
            ]
        ]


viewTableRowTransfer : LoggedIn.Model -> Model -> TransferForm -> Bool -> Html Msg
viewTableRowTransfer loggedIn ({ balance } as model) f isDisabled =
    let
        text_ s =
            text (t loggedIn.shared.translations s)
    in
    form
        [ class "card-table__row-transfer"
        , onSubmit ClickedSendTransfer
        ]
        [ div [ class "card-table__row-transfer__cell" ]
            [ label []
                [ text (I18Next.tr loggedIn.shared.translations Curly "account.my_wallet.transfer.amount" [ ( "symbol", Eos.symbolToString balance.asset.symbol ) ]) ]
            , viewInputAmount loggedIn.shared f isDisabled
            ]
        , div [ class "card-table__row-transfer__cell" ]
            [ label [] [ text_ "account.my_wallet.transfer.memo" ]
            , viewInputMemo loggedIn.shared f isDisabled
            ]
        , div [ class "card-table__row-transfer__cell card-table__row-transfer__cell--button" ]
            [ button
                [ class "btn btn--primary"
                , disabled isDisabled
                ]
                [ text_ "account.my_wallet.transfer.submit" ]
            ]
        , button
            [ class "row__close-btn circle-background"
            , onClick ClickedCancelTransfer
            , type_ "button"
            , disabled isDisabled
            ]
            [ Icon.close "" ]
        ]



-- VIEW HELPERS


viewInputAmount : Shared -> TransferForm -> Bool -> Html Msg
viewInputAmount shared f isDisabled =
    input
        [ class "input input--amount"
        , type_ "number"
        , placeholder (t shared.translations "account.my_wallet.transfer.amount_placeholder")
        , value f.amount
        , onInput EnteredTransferAmount
        , HtmlAttr.min "0"
        , required True
        , disabled isDisabled
        ]
        []


viewInputMemo : Shared -> TransferForm -> Bool -> Html Msg
viewInputMemo shared f isDisabled =
    input
        [ class "input"
        , placeholder (t shared.translations "account.my_wallet.transfer.memo_placeholder")
        , type_ "text"
        , value f.memo
        , onInput EnteredTransferMemo
        , disabled isDisabled
        , maxlength 255
        ]
        []


viewAutoCompleteItem : Shared -> Profile -> Html Never
viewAutoCompleteItem shared profile =
    let
        ipfsUrl =
            shared.endpoints.ipfs
    in
    div [ class "autocomplete-item" ]
        [ div [ class "flex-grow-3" ] [ Avatar.view ipfsUrl profile.avatar "profile-img-avatar-select" ]
        , div [ class "flex-grow-7" ]
            [ text (Eos.nameToString profile.accountName)
            , text " "
            , case profile.userName of
                Just name ->
                    text ("(" ++ name ++ ")")

                Nothing ->
                    text ""
            ]
        ]


autoCompleteAccount : Shared -> Model -> TransferForm -> Bool -> Html Msg
autoCompleteAccount shared model form isDisabled =
    let
        users =
            Maybe.map .members model.dashboardInfo
                |> Maybe.withDefault []

        selectedUsers =
            Maybe.map (\v -> [ v ]) form.selectedProfile
                |> Maybe.withDefault []
    in
    Html.map SelectMsg (Select.view (selectConfig shared isDisabled) model.autoCompleteState users selectedUsers)


filter : Int -> (a -> String) -> String -> List a -> Maybe (List a)
filter minChars toLabel query items =
    if String.length query < minChars then
        Nothing

    else
        items
            |> Simple.Fuzzy.filter toLabel query
            |> Just


selectConfig : Shared -> Bool -> Select.Config Msg Profile
selectConfig shared isDisabled =
    Select.newConfig
        { onSelect = OnSelect
        , toLabel = \p -> Eos.nameToString p.accountName
        , filter = filter 2 (\p -> Eos.nameToString p.accountName)
        }
        |> Select.withInputClass "input"
        |> Select.withInputId "input-id"
        |> Select.withMenuClass "autocomplete-items"
        |> Select.withNotFound "No matches"
        |> Select.withNotFoundClass "text-red"
        |> Select.withNotFoundStyles [ ( "padding", "0 2rem" ) ]
        |> Select.withDisabled isDisabled
        |> Select.withHighlightedItemClass "autocomplete-item-highlight"
        |> Select.withPrompt (t shared.translations "account.my_wallet.transfer.send_to_placeholder")
        |> Select.withItemHtml (viewAutoCompleteItem shared)



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = Ignored
    | ClickedTransfer
    | EnteredTransferAmount String
    | EnteredTransferMemo String
    | ClickedSendTransfer
    | ClickedCancelTransfer
    | GotTransferResult (Result Value String)
    | ClickedBackFromFailure
    | ClickedBackToStart
    | CompletedDashboardInfoLoad (Result (Graphql.Http.Error (Maybe Community.DashboardInfo)) (Maybe Community.DashboardInfo))
    | OnSelect (Maybe Profile)
    | SelectMsg (Select.Msg Profile)


update : LoggedIn.Model -> Msg -> Model -> UpdateResult
update loggedIn msg model =
    let
        onlyLogImpossible desc =
            UR.init model
                |> UR.logImpossible msg desc
    in
    case msg of
        Ignored ->
            UR.init model

        ClickedTransfer ->
            case model.status of
                ViewingBalance ->
                    EditingTransfer emptyTransferForm
                        |> Transferring
                        |> updateStatus model
                        |> UR.init

                _ ->
                    onlyLogImpossible []

        EnteredTransferAmount str ->
            case model.status of
                Transferring (EditingTransfer form) ->
                    EditingTransfer { form | amount = str }
                        |> Transferring
                        |> updateStatus model
                        |> UR.init

                _ ->
                    onlyLogImpossible []

        EnteredTransferMemo str ->
            case model.status of
                Transferring (EditingTransfer form) ->
                    EditingTransfer { form | memo = str }
                        |> Transferring
                        |> updateStatus model
                        |> UR.init

                _ ->
                    onlyLogImpossible []

        ClickedSendTransfer ->
            case ( model.status, LoggedIn.isAuth loggedIn ) of
                ( Transferring (EditingTransfer form), True ) ->
                    let
                        account =
                            Maybe.map .accountName form.selectedProfile
                                |> Maybe.withDefault (Eos.stringToName "")
                    in
                    SendingTransfer form
                        |> Transferring
                        |> updateStatus model
                        |> UR.init
                        |> UR.addPort
                            { responseAddress = ClickedSendTransfer
                            , responseData = Encode.null
                            , data =
                                Eos.encodeTransaction
                                    { actions =
                                        [ { accountName = "bes.token"
                                          , name = "transfer"
                                          , authorization =
                                                { actor = loggedIn.accountName
                                                , permissionName = Eos.samplePermission
                                                }
                                          , data =
                                                { from = loggedIn.accountName
                                                , to = Eos.nameQueryUrlParser (Eos.nameToString account)
                                                , value =
                                                    { amount =
                                                        String.toFloat form.amount
                                                            |> Maybe.withDefault 0.0
                                                    , symbol = model.balance.asset.symbol
                                                    }
                                                , memo = form.memo
                                                }
                                                    |> Transfer.encodeEosActionData
                                          }
                                        ]
                                    }
                            }

                ( Transferring (EditingTransfer form), False ) ->
                    UR.init model
                        |> UR.addExt
                            (Just ClickedSendTransfer
                                |> RequiredAuthentication
                            )

                _ ->
                    onlyLogImpossible []

        ClickedCancelTransfer ->
            case model.status of
                Transferring (EditingTransfer form) ->
                    ViewingBalance
                        |> updateStatus model
                        |> UR.init

                Transferring (SendingTransferFailed form) ->
                    ViewingBalance
                        |> updateStatus model
                        |> UR.init

                _ ->
                    onlyLogImpossible []

        GotTransferResult (Ok txId) ->
            case model.status of
                Transferring (SendingTransfer form) ->
                    let
                        balance =
                            model.balance

                        asset =
                            balance.asset
                    in
                    ViewingBalance
                        |> updateStatus
                            { model
                                | balance =
                                    { balance
                                        | asset =
                                            { asset
                                                | amount =
                                                    String.toFloat form.amount
                                                        |> Maybe.withDefault 0
                                                        |> (\i -> balance.asset.amount - i)
                                            }
                                    }
                            }
                        |> UR.init

                _ ->
                    onlyLogImpossible []

        GotTransferResult (Err _) ->
            case model.status of
                Transferring (SendingTransfer form) ->
                    SendingTransferFailed form
                        |> Transferring
                        |> updateStatus model
                        |> UR.init

                _ ->
                    onlyLogImpossible []

        ClickedBackFromFailure ->
            case model.status of
                Transferring (SendingTransferFailed f) ->
                    EditingTransfer f
                        |> Transferring
                        |> updateStatus model
                        |> UR.init

                _ ->
                    onlyLogImpossible []

        ClickedBackToStart ->
            ViewingBalance
                |> updateStatus model
                |> UR.init

        CompletedDashboardInfoLoad (Err _) ->
            model |> UR.init

        CompletedDashboardInfoLoad (Ok res) ->
            case res of
                Just result ->
                    result
                        |> updateDashboardInfo model
                        |> UR.init

                Nothing ->
                    model
                        |> UR.init

        OnSelect maybeProfile ->
            case model.status of
                Transferring (EditingTransfer form) ->
                    EditingTransfer { form | selectedProfile = maybeProfile }
                        |> Transferring
                        |> updateStatus model
                        |> UR.init

                _ ->
                    model |> UR.init

        SelectMsg subMsg ->
            let
                ( updated, cmd ) =
                    Select.update (selectConfig loggedIn.shared False) subMsg model.autoCompleteState
            in
            UR.init { model | autoCompleteState = updated }
                |> UR.addCmd cmd


updateDashboardInfo : Model -> Community.DashboardInfo -> Model
updateDashboardInfo model result =
    { model | dashboardInfo = Just result }


updateStatus : Model -> Status -> Model
updateStatus model status =
    { model | status = status }


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "ClickedSendTransfer" :: [] ->
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.field "transactionId" Decode.string
                        |> Decode.map Ok
                    , Decode.succeed (Err Encode.null)
                    ]
                )
                val
                |> Result.map (Just << GotTransferResult)
                |> Result.withDefault Nothing

        _ ->
            Nothing


msgToString : Msg -> List String
msgToString msg =
    let
        resultToString ss r =
            case r of
                Ok _ ->
                    ss ++ [ "Ok" ]

                Err _ ->
                    ss ++ [ "Err" ]
    in
    case msg of
        Ignored ->
            [ "Ignored" ]

        ClickedTransfer ->
            [ "ClickedTransfer" ]

        EnteredTransferAmount _ ->
            [ "EnteredTransferAmount" ]

        EnteredTransferMemo _ ->
            [ "EnteredTransferMemo" ]

        ClickedSendTransfer ->
            [ "ClickedSendTransfer" ]

        ClickedCancelTransfer ->
            [ "ClickedCancelTransfer" ]

        GotTransferResult result ->
            resultToString [ "GotTransferResult" ] result

        ClickedBackFromFailure ->
            [ "ClickedBackFromFailure" ]

        ClickedBackToStart ->
            [ "ClickedBackToStart" ]

        CompletedDashboardInfoLoad result ->
            resultToString [ "CompletedDashboardInfoLoad" ] result

        OnSelect _ ->
            [ "OnSelect" ]

        SelectMsg subMsg ->
            "SelectMsg" :: "sub" :: []
