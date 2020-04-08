module Page.Dashboard.Balance exposing
    ( Model
    , Msg
    , UpdateResult
    , init
    , jsAddressToMsg
    , msgToString
    , subscription
    , update
    , viewCard
    , viewInvitationModal
    )

import Api
import Api.Graphql
import Asset.Icon as Icon
import Avatar
import Browser.Events
import Community exposing (Balance)
import Eos
import Eos.Account as Eos
import Graphql.Http
import Html exposing (Html, a, br, button, div, form, img, input, p, span, text)
import Html.Attributes as HtmlAttr exposing (class, classList, disabled, href, id, maxlength, placeholder, required, src, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import I18Next exposing (Delims(..), t, tr)
import Icons
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode exposing (Value)
import Profile exposing (Profile)
import Route
import Select
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared exposing (Shared)
import Simple.Fuzzy
import Task
import Transfer
import UpdateResult as UR
import Url
import Utils



-- INIT


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
      , inviteModal = Closed
      , copied = False
      }
    , cmd
    )



-- SUBSCRIPTION


subscription : Model -> Sub Msg
subscription _ =
    Sub.map PressedEnter (Browser.Events.onKeyDown Utils.decodeEnterKeyDown)



-- MODEL


type alias Model =
    { balance : Balance
    , status : Status
    , dashboardInfo : Maybe Community.DashboardInfo
    , autoCompleteState : Select.State
    , inviteModal : ModalStatus
    , copied : Bool
    }


type Status
    = ViewingBalance
    | Transferring TransferStatus


type TransferStatus
    = EditingTransfer TransferForm
    | SendingTransfer TransferForm
    | SendingTransferFailed TransferForm


type ModalStatus
    = Closed
    | Loading
    | Failed String
    | Loaded String


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
        [ div [ class "flex flex-col items-center justify-center px-3 pt-5 pb-2 rounded-lg hover:shadow-lg bg-white" ]
            [ div [ class "" ] [ img [ class "object-none object-scale-down h-20 mb-2", src (ipfsUrl ++ "/" ++ logo) ] [] ]
            , div [ class "" ] [ p [ class "leading-none text-menu" ] [ text title ] ]
            , div [ class "flex items-center justify-center" ]
                [ div [ class "text-indigo-500 font-bold text-3xl" ]
                    [ text balanceText ]
                , div [ class "text-indigo-500 ml-2" ]
                    [ text symbolText ]
                ]
            , div [ class "input-label mb-4" ]
                [ text_ "account.my_wallet.balances.current" ]
            , div [ class "flex flex-col py-3 w-full" ]
                [ a
                    [ class "button button-secondary button-sm text-xs font-medium w-full mb-4"
                    , Route.href (Route.Community balance.asset.symbol)
                    ]
                    [ text_ "menu.explore" ]
                , button
                    [ class "button button-secondary button-sm text-xs w-full mb-4"
                    , onClick CreateInvite
                    ]
                    [ text_ "community.invite.title" ]
                , button
                    [ class "button button-primary button-sm text-xs w-full"
                    , onClick ClickedTransfer
                    ]
                    [ text_ "account.my_wallet.balances.button" ]
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
        [ class "w-full sm:w-1/2 lg:w-1/3 xl:w-1/4 px-2 mb-6 px-4 py-2 bg-white rounded-lg shadow-lg relative"
        , onSubmit ClickedSendTransfer
        ]
        [ div [ class "flex flex-col justify-center items-center" ]
            [ div [ class "flex items-center" ]
                [ div [ class "text-indigo-500 font-bold text-3xl" ]
                    [ text (String.fromFloat b.asset.amount) ]
                , div [ class "text-indigo-500 ml-2" ]
                    [ text (Eos.symbolToString b.asset.symbol) ]
                ]
            , div [ class "input-label" ]
                [ text_ "account.my_wallet.balances.current" ]
            ]
        , div [ class "" ]
            [ div [ class "mb-2" ]
                [ span [ class "input-label" ]
                    [ text_ "account.my_wallet.transfer.send_to" ]
                , div [ class "flex flex-row border rounded" ]
                    [ autoCompleteAccount loggedIn.shared model f isDisabled ]
                ]
            , div [ class "mb-2" ]
                [ span [ class "input-label" ]
                    [ text (I18Next.tr loggedIn.shared.translations Curly "account.my_wallet.transfer.amount" [ ( "symbol", Eos.symbolToString b.asset.symbol ) ]) ]
                , div [ class "flex flex-row border rounded" ]
                    [ viewInputAmount loggedIn.shared f isDisabled ]
                ]
            , div [ class "mb-2" ]
                [ span [ class "input-label" ]
                    [ text_ "account.my_wallet.transfer.memo" ]
                , div [ class "flex flex-row border rounded" ]
                    [ viewInputMemo loggedIn.shared f isDisabled ]
                ]
            ]
        , div [ class "card__button-row" ]
            [ button
                [ class "btn btn--primary"
                , disabled isDisabled
                ]
                [ text_ "account.my_wallet.transfer.submit" ]
            ]
        , button
            [ onClick ClickedCancelTransfer
            , disabled isDisabled
            , class "absolute flex top-0 right-0 rounded-full h-10 w-10 justify-center"
            ]
            [ Icons.close "fill-current text-gray-400"
            ]
        ]


viewInvitationModal : LoggedIn.Model -> Model -> Html Msg
viewInvitationModal { shared } model =
    let
        t s =
            I18Next.t shared.translations s

        text_ s =
            text (t s)

        protocol =
            case shared.url.protocol of
                Url.Http ->
                    "http://"

                Url.Https ->
                    "https://"

        url invitationId =
            protocol ++ shared.url.host ++ "/invite/" ++ invitationId
    in
    case model.inviteModal of
        Closed ->
            text ""

        _ ->
            div [ class "modal container" ]
                [ div [ class "modal-bg", onClick CloseInviteModal ] []
                , div [ class "modal-content" ]
                    [ div [ class "w-full" ]
                        [ p [ class "text-2xl font-medium mb-4" ]
                            [ text_ "community.invite.title" ]
                        , button [ onClick CloseInviteModal ]
                            [ Icons.close "absolute fill-current text-gray-400 top-0 right-0 mx-8 my-4" ]
                        , case model.inviteModal of
                            Closed ->
                                text ""

                            Loading ->
                                div [ class "flex items-center justify-center" ]
                                    [ div [ class "spinner spinner--delay" ] [] ]

                            Failed err ->
                                div []
                                    [ div [ class "flex items-center justify-center text-heading text-red" ]
                                        [ p [ class "text-sm text-red" ] [ text err ] ]
                                    , div [ class "w-full md:bg-gray-100 flex md:absolute rounded-b-lg md:inset-x-0 md:bottom-0 md:p-4 justify-center items-center" ]
                                        [ button
                                            [ class "button button-primary"
                                            , onClick CloseInviteModal
                                            ]
                                            [ text "OK" ]
                                        ]
                                    ]

                            Loaded invitationId ->
                                div [ class "flex flex-wrap items-center mt-24 md:mt-0" ]
                                    [ div [ class "flex flex-col items-left w-full mb-4" ]
                                        [ span [ class "input-label" ]
                                            [ text_ "community.invite.label" ]
                                        , input
                                            [ class "text-menu border p-2 md:border-none md:text-heading outline-none text-black"
                                            , id "invitation-id"
                                            , value (url invitationId)
                                            , disabled True
                                            ]
                                            []
                                        ]
                                    , div [ class "w-full md:bg-gray-100 flex md:absolute rounded-b-lg md:inset-x-0 md:bottom-0 md:p-4 justify-center items-center" ]
                                        [ button
                                            [ classList
                                                [ ( "button-primary", not model.copied )
                                                , ( "button-success", model.copied )
                                                ]
                                            , class "button w-full md:w-48"
                                            , onClick (CopyToClipboard "invitation-id")
                                            ]
                                            [ if model.copied then
                                                text_ "community.invite.copied"

                                              else
                                                text_ "community.invite.copy"
                                            ]
                                        ]
                                    ]
                        ]
                    ]
                ]



-- VIEW HELPERS


viewInputAmount : Shared -> TransferForm -> Bool -> Html Msg
viewInputAmount shared f isDisabled =
    input
        [ class "input block border-none"
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
        [ class "input block border-none"
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
    div [ class "pt-3 pl-3 flex flex-row items-center w-select z-30" ]
        [ div [ class "pr-3" ]
            [ Avatar.view ipfsUrl profile.avatar "h-7 w-7" ]
        , div [ class "flex flex-col font-sans border-b border-gray-500 pb-3 w-full" ]
            [ span [ class "text-black text-body leading-loose" ]
                [ text (Eos.nameToString profile.account) ]
            , span [ class "leading-caption uppercase text-green text-caption" ]
                [ case profile.userName of
                    Just name ->
                        text name

                    Nothing ->
                        text ""
                ]
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
        , toLabel = \p -> Eos.nameToString p.account
        , filter = filter 2 (\p -> Eos.nameToString p.account)
        }
        |> Select.withInputClass "input h-12 w-full border-none placeholder-gray-900"
        |> Select.withClear False
        |> Select.withMenuClass "border-t-none border-solid border-gray-100 border rounded-b z-30 bg-white"
        |> Select.withNotFound "No matches"
        |> Select.withNotFoundClass "text-red border-solid border-gray-100 border rounded z-30 bg-white w-select"
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
    | CompletedDashboardInfoLoad (Result (Graphql.Http.Error (Maybe Community.DashboardInfo)) (Maybe Community.DashboardInfo))
    | OnSelect (Maybe Profile)
    | SelectMsg (Select.Msg Profile)
    | CreateInvite
    | CloseInviteModal
    | CompletedInviteCreation (Result Http.Error String)
    | CopyToClipboard String
    | CopiedToClipboard
    | PressedEnter Bool


update : LoggedIn.Model -> Msg -> Model -> UpdateResult
update ({ shared } as loggedIn) msg model =
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
                            Maybe.map .account form.selectedProfile
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
                    Select.update (selectConfig shared False) subMsg model.autoCompleteState
            in
            UR.init { model | autoCompleteState = updated }
                |> UR.addCmd cmd

        CreateInvite ->
            UR.init
                { model | inviteModal = Loading }
                |> UR.addCmd
                    (CompletedInviteCreation
                        |> Api.communityInvite shared model.balance.asset.symbol loggedIn.accountName
                    )

        CloseInviteModal ->
            UR.init
                { model | inviteModal = Closed }

        CompletedInviteCreation (Ok invitationId) ->
            { model | inviteModal = Loaded invitationId }
                |> UR.init

        CompletedInviteCreation (Err httpError) ->
            UR.init
                { model | inviteModal = Failed (I18Next.t shared.translations "community.invite.failed") }
                |> UR.logHttpError msg httpError

        CopyToClipboard elementId ->
            model
                |> UR.init
                |> UR.addPort
                    { responseAddress = CopiedToClipboard
                    , responseData = Encode.null
                    , data =
                        Encode.object
                            [ ( "id", Encode.string elementId )
                            , ( "name", Encode.string "copyToClipboard" )
                            ]
                    }

        CopiedToClipboard ->
            { model | copied = True }
                |> UR.init

        PressedEnter val ->
            if val then
                UR.init model
                    |> UR.addCmd
                        (Task.succeed ClickedSendTransfer
                            |> Task.perform identity
                        )

            else
                UR.init model


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

        "CopiedToClipboard" :: _ ->
            Just CopiedToClipboard

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

        CompletedDashboardInfoLoad result ->
            resultToString [ "CompletedDashboardInfoLoad" ] result

        OnSelect _ ->
            [ "OnSelect" ]

        SelectMsg _ ->
            [ "SelectMsg", "sub" ]

        CreateInvite ->
            [ "CreateInvite" ]

        CloseInviteModal ->
            [ "CloseInviteModal" ]

        CompletedInviteCreation _ ->
            [ "CompletedInviteCreation" ]

        CopyToClipboard _ ->
            [ "CopyToClipboard" ]

        CopiedToClipboard ->
            [ "CopiedToClipboard" ]

        PressedEnter _ ->
            [ "PressedEnter" ]
