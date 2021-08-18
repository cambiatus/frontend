module Page.Community.Transfer exposing
    ( Model
    , Msg
    , init
    , jsAddressToMsg
    , msgToString
    , receiveBroadcast
    , update
    , view
    )

import Api
import Community
import Dict
import Eos exposing (Symbol)
import Eos.Account as Eos
import Eos.EosError as EosError
import Graphql.Document
import Html exposing (Html, button, div, form, span, text)
import Html.Attributes exposing (class, classList, disabled, maxlength, rows, type_, value)
import Html.Events exposing (onSubmit)
import Http
import I18Next
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode exposing (Value)
import List.Extra as LE
import Log
import Page
import Profile
import RemoteData exposing (RemoteData)
import Route
import Select
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared as Shared exposing (Shared)
import Token
import Transfer
import UpdateResult as UR
import View.Feedback as Feedback
import View.Form.Input as Input


init : LoggedIn.Model -> Maybe String -> ( Model, Cmd Msg )
init loggedIn maybeTo =
    ( initModel maybeTo
    , LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn
    )



-- MODEL


type alias Model =
    { maybeTo : Maybe String
    , transferStatus : TransferStatus
    , autoCompleteState : Select.State
    , balance : RemoteData BalanceError Community.Balance
    , token : RemoteData Http.Error Token.Model
    }


type BalanceError
    = BalanceNotFound
    | WithHttpError Http.Error


type MaxAmountError
    = TokenError Http.Error
    | BalanceError BalanceError


initModel : Maybe String -> Model
initModel maybeTo =
    { maybeTo = maybeTo
    , transferStatus = EditingTransfer emptyForm
    , autoCompleteState = Select.newState ""
    , balance = RemoteData.Loading
    , token = RemoteData.Loading
    }


type TransferStatus
    = EditingTransfer Form
    | CreatingSubscription Form
    | SendingTransfer Form


type Validation
    = Valid
    | Invalid String (Maybe I18Next.Replacements)


type alias Form =
    { selectedProfile : Maybe Profile.Minimal
    , selectedProfileValidation : Validation
    , amount : String
    , amountValidation : Validation
    , memo : String
    , memoValidation : Validation
    }


emptyForm : Form
emptyForm =
    { selectedProfile = Nothing
    , selectedProfileValidation = Valid
    , amount = ""
    , amountValidation = Valid
    , memo = ""
    , memoValidation = Valid
    }


validAmountCharacter : Symbol -> Char -> Bool
validAmountCharacter symbol c =
    let
        separator =
            if Eos.getSymbolPrecision symbol > 0 then
                c == '.'

            else
                False
    in
    Char.isDigit c || separator


validateSelectedProfile : Eos.Name -> Form -> Form
validateSelectedProfile currentAccount form =
    { form
        | selectedProfileValidation =
            case form.selectedProfile of
                Just profile ->
                    if profile.account == currentAccount then
                        Invalid "transfer.to_self" Nothing

                    else
                        Valid

                Nothing ->
                    Invalid "transfer.no_profile" Nothing
    }


validateAmount : Symbol -> RemoteData MaxAmountError Float -> Form -> Form
validateAmount symbol maxAmountStatus form =
    let
        symbolPrecision =
            Eos.getSymbolPrecision symbol

        amountPrecision =
            String.toList form.amount
                |> LE.dropWhile (\c -> c /= '.')
                |> List.drop 1
                |> List.length

        maxAmount =
            RemoteData.withDefault 0 maxAmountStatus
    in
    { form
        | amountValidation =
            if amountPrecision > symbolPrecision then
                Invalid "error.contracts.transfer.symbol precision mismatch" Nothing

            else if
                String.all (validAmountCharacter symbol) form.amount
                    && (String.length form.amount > 0)
            then
                case String.toFloat form.amount of
                    Nothing ->
                        Invalid "transfer.no_amount" Nothing

                    Just amount ->
                        if amount > maxAmount then
                            Invalid "transfer.too_much"
                                (Just
                                    [ ( "token", Eos.symbolToSymbolCodeString symbol )
                                    , ( "max_asset", Eos.assetToString { amount = maxAmount, symbol = symbol } )
                                    ]
                                )

                        else
                            Valid

            else
                Invalid "transfer.no_amount" Nothing
    }


validateMemo : Form -> Form
validateMemo form =
    { form
        | memoValidation =
            if String.length form.memo <= memoMaxLength then
                Valid

            else
                Invalid "transfer.memo_too_long" (Just [ ( "maxlength", String.fromInt memoMaxLength ) ])
    }


validateForm : Eos.Name -> RemoteData MaxAmountError Float -> Symbol -> Form -> Form
validateForm currentAccount maxAmount symbol form =
    form
        |> validateSelectedProfile currentAccount
        |> validateAmount symbol maxAmount
        |> validateMemo


isFormValid : Form -> Bool
isFormValid form =
    (form.selectedProfileValidation == Valid)
        && (form.amountValidation == Valid)
        && (form.memoValidation == Valid)


memoMaxLength : Int
memoMaxLength =
    255



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view ({ shared } as loggedIn) model =
    let
        content =
            case ( loggedIn.selectedCommunity, model.transferStatus, maxTransferAmount model ) of
                ( RemoteData.NotAsked, _, _ ) ->
                    Page.fullPageLoading shared

                ( RemoteData.Loading, _, _ ) ->
                    Page.fullPageLoading shared

                ( _, _, RemoteData.NotAsked ) ->
                    Page.fullPageLoading shared

                ( _, _, RemoteData.Loading ) ->
                    Page.fullPageLoading shared

                ( RemoteData.Failure e, _, _ ) ->
                    Page.fullPageGraphQLError (shared.translators.t "community.objectives.title_plural") e

                ( _, _, RemoteData.Failure e ) ->
                    let
                        httpError =
                            case e of
                                TokenError error ->
                                    error

                                BalanceError (WithHttpError error) ->
                                    error

                                BalanceError BalanceNotFound ->
                                    Http.Timeout
                    in
                    Page.fullPageError (shared.translators.t "community.objectives.title_plural") httpError

                ( RemoteData.Success community, EditingTransfer f, RemoteData.Success _ ) ->
                    viewForm loggedIn model f community False

                ( RemoteData.Success community, CreatingSubscription f, RemoteData.Success _ ) ->
                    viewForm loggedIn model f community True

                ( RemoteData.Success community, SendingTransfer f, RemoteData.Success _ ) ->
                    viewForm loggedIn model f community True
    in
    { title = shared.translators.t "transfer.title"
    , content = content
    }


viewForm : LoggedIn.Model -> Model -> Form -> Community.Model -> Bool -> Html Msg
viewForm ({ shared } as loggedIn) model f community isDisabled =
    let
        text_ s =
            text (loggedIn.shared.translators.t s)

        currBalance =
            model.balance
                |> RemoteData.map .asset
                |> RemoteData.withDefault { amount = 0, symbol = community.symbol }
    in
    div [ class "bg-white" ]
        [ Page.viewHeader loggedIn (shared.translators.t "transfer.title")
        , form [ class "container mx-auto p-4", onSubmit SubmitForm ]
            [ div [ class "mb-10" ]
                [ span [ class "label" ]
                    [ text_ "account.my_wallet.transfer.send_to" ]
                , div []
                    [ viewAutoCompleteAccount shared model f isDisabled community ]
                , viewError shared.translators f.selectedProfileValidation
                ]
            , Input.init
                { label =
                    shared.translators.tr "account.my_wallet.transfer.amount"
                        [ ( "symbol", Eos.symbolToSymbolCodeString community.symbol ) ]
                , id = "transfer-amount-field"
                , onInput = EnteredAmount
                , disabled = isDisabled
                , value = f.amount
                , placeholder = Just "0"
                , problems =
                    validationToString shared.translators f.amountValidation
                        |> Maybe.map List.singleton
                , translators = shared.translators
                }
                |> Input.withContainerAttrs [ class "mb-4" ]
                |> Input.withCurrency community.symbol
                |> Input.toHtml
            , div [ class "bg-gray-100 uppercase text-sm px-2 inline-block mb-10" ]
                [ text
                    (shared.translators.tr "account.my_wallet.your_current_balance"
                        [ ( "balance", Eos.assetToString currBalance ) ]
                    )
                ]
            , Input.init
                { label = shared.translators.t "account.my_wallet.transfer.memo"
                , id = "transfer-memo-field"
                , onInput = EnteredMemo
                , disabled = isDisabled
                , value = f.memo
                , placeholder = Nothing
                , problems =
                    validationToString shared.translators f.memoValidation
                        |> Maybe.map List.singleton
                , translators = shared.translators
                }
                |> Input.withInputType Input.TextArea
                |> Input.withAttrs [ rows 5, maxlength memoMaxLength ]
                |> Input.withCounter 255
                |> Input.toHtml
            , div [ class "mt-6" ]
                [ button
                    [ class "button button-primary w-full"
                    , classList [ ( "button-disabled", isDisabled ) ]
                    , disabled isDisabled
                    , type_ "submit"
                    ]
                    [ text_ "account.my_wallet.transfer.submit" ]
                ]
            ]
        ]


validationToString : Shared.Translators -> Validation -> Maybe String
validationToString { t, tr } validation =
    case validation of
        Valid ->
            Nothing

        Invalid e (Just replacements) ->
            Just <| tr e replacements

        Invalid e Nothing ->
            Just <| t e


viewError : Shared.Translators -> Validation -> Html msg
viewError translators validation =
    case validationToString translators validation of
        Nothing ->
            text ""

        Just translatedError ->
            span [ class "form-error" ] [ text translatedError ]


viewAutoCompleteAccount : Shared -> Model -> Form -> Bool -> Community.Model -> Html Msg
viewAutoCompleteAccount shared model form isDisabled community =
    let
        selectedUsers =
            Maybe.map (\v -> [ v ]) form.selectedProfile
                |> Maybe.withDefault []
    in
    div []
        [ Html.map SelectMsg
            (Select.view
                (selectConfiguration shared isDisabled)
                model.autoCompleteState
                community.members
                selectedUsers
            )
        ]


selectConfiguration : Shared -> Bool -> Select.Config Msg Profile.Minimal
selectConfiguration shared isDisabled =
    Profile.selectConfig
        (Select.newConfig
            { onSelect = OnSelect
            , toLabel = \p -> Eos.nameToString p.account
            , filter = Profile.selectFilter 2 (\p -> Eos.nameToString p.account)
            }
        )
        shared
        isDisabled



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = CompletedLoadCommunity Community.Model
    | CompletedLoadBalance (Result Http.Error (Maybe Community.Balance))
    | CompletedLoadToken (Result Http.Error Token.Model)
    | ClosedAuthModal
    | OnSelect (Maybe Profile.Minimal)
    | SelectMsg (Select.Msg Profile.Minimal)
    | EnteredAmount String
    | EnteredMemo String
    | SubmitForm
    | PushTransaction
    | GotTransferResult (Result (Maybe Value) String)
    | Redirect Value


getProfile : Maybe String -> Community.Model -> TransferStatus
getProfile maybeTo community =
    case maybeTo of
        Just name ->
            let
                member =
                    List.head (List.filter (\m -> Eos.nameToString m.account == name) community.members)
            in
            case member of
                Just profile ->
                    EditingTransfer { emptyForm | selectedProfile = Just profile }

                Nothing ->
                    EditingTransfer emptyForm

        Nothing ->
            EditingTransfer emptyForm


update : Msg -> Model -> LoggedIn.Model -> UpdateResult
update msg model ({ shared } as loggedIn) =
    let
        onlyLogImpossible : String -> UpdateResult
        onlyLogImpossible desc =
            UR.init model
                |> UR.logImpossible msg
                    desc
                    (Just loggedIn.accountName)
                    { moduleName = "Page.Community.Transfer", function = "update" }
                    []
    in
    case msg of
        CompletedLoadCommunity community ->
            UR.init { model | transferStatus = getProfile model.maybeTo community }
                |> UR.addCmd (fetchBalance shared loggedIn.accountName community)
                |> UR.addCmd (Token.getToken shared community.symbol CompletedLoadToken)

        CompletedLoadBalance (Ok (Just balance)) ->
            { model | balance = RemoteData.Success balance }
                |> UR.init

        CompletedLoadBalance (Ok Nothing) ->
            { model | balance = RemoteData.Failure BalanceNotFound }
                |> UR.init
                |> UR.logImpossible msg
                    "Couldn't find balance related to community on transfer page"
                    (Just loggedIn.accountName)
                    { moduleName = "Page.Community.Transfer", function = "update" }
                    [ Log.contextFromCommunity loggedIn.selectedCommunity ]

        CompletedLoadBalance (Err httpError) ->
            { model | balance = RemoteData.Failure (WithHttpError httpError) }
                |> UR.init
                |> UR.logHttpError msg
                    (Just loggedIn.accountName)
                    "Got an error when loading balance on the transfer page"
                    { moduleName = "Page.Community.Transfer", function = "update" }
                    [ Log.contextFromCommunity loggedIn.selectedCommunity ]
                    httpError

        CompletedLoadToken (Ok token) ->
            { model | token = RemoteData.Success token }
                |> UR.init
                |> UR.addBreadcrumb
                    { type_ = Log.InfoBreadcrumb
                    , category = msg
                    , message = "Completed loading token on transfer page"
                    , data = Dict.empty
                    , level = Log.Info
                    }

        CompletedLoadToken (Err httpError) ->
            { model | token = RemoteData.Failure httpError }
                |> UR.init
                |> UR.logHttpError msg
                    (Just loggedIn.accountName)
                    "Got an error when loading token on the transfer page"
                    { moduleName = "Page.Community.Transfer", function = "update" }
                    [ Log.contextFromCommunity loggedIn.selectedCommunity ]
                    httpError

        ClosedAuthModal ->
            let
                form =
                    case model.transferStatus of
                        EditingTransfer form_ ->
                            form_

                        CreatingSubscription form_ ->
                            form_

                        SendingTransfer form_ ->
                            form_
            in
            UR.init { model | transferStatus = EditingTransfer form }

        OnSelect maybeProfile ->
            case model.transferStatus of
                EditingTransfer form ->
                    { model
                        | transferStatus =
                            EditingTransfer
                                ({ form | selectedProfile = maybeProfile }
                                    |> validateSelectedProfile loggedIn.accountName
                                )
                    }
                        |> UR.init

                _ ->
                    model |> UR.init

        SelectMsg subMsg ->
            let
                ( updated, cmd ) =
                    Select.update (selectConfiguration shared False) subMsg model.autoCompleteState
            in
            UR.init { model | autoCompleteState = updated }
                |> UR.addCmd cmd

        EnteredAmount value ->
            case ( loggedIn.selectedCommunity, model.transferStatus ) of
                ( RemoteData.Success selectedCommunity, EditingTransfer form ) ->
                    let
                        getNumericValues : String -> String
                        getNumericValues =
                            String.filter (validAmountCharacter selectedCommunity.symbol)
                    in
                    { model
                        | transferStatus =
                            EditingTransfer
                                ({ form | amount = getNumericValues value }
                                    |> validateAmount selectedCommunity.symbol
                                        (maxTransferAmount model)
                                )
                    }
                        |> UR.init

                _ ->
                    model |> UR.init

        EnteredMemo value ->
            case model.transferStatus of
                EditingTransfer form ->
                    { model
                        | transferStatus =
                            EditingTransfer
                                ({ form | memo = value }
                                    |> validateMemo
                                )
                    }
                        |> UR.init

                _ ->
                    model |> UR.init

        SubmitForm ->
            case ( model.transferStatus, loggedIn.selectedCommunity ) of
                ( EditingTransfer form, RemoteData.Success community ) ->
                    case form.selectedProfile of
                        Just to ->
                            let
                                newForm =
                                    validateForm loggedIn.accountName
                                        (maxTransferAmount model)
                                        community.symbol
                                        form

                                subscriptionDoc =
                                    Transfer.transferSucceedSubscription community.symbol (Eos.nameToString loggedIn.accountName) (Eos.nameToString to.account)
                                        |> Graphql.Document.serializeSubscription
                            in
                            if isFormValid newForm then
                                { model | transferStatus = CreatingSubscription newForm }
                                    |> UR.init
                                    |> UR.addPort
                                        { responseAddress = SubmitForm
                                        , responseData = Encode.null
                                        , data =
                                            Encode.object
                                                [ ( "name", Encode.string "subscribeToTransfer" )
                                                , ( "subscription", Encode.string subscriptionDoc )
                                                ]
                                        }
                                    |> UR.addExt LoggedIn.HideFeedback

                            else
                                { model | transferStatus = EditingTransfer newForm }
                                    |> UR.init

                        Nothing ->
                            { model
                                | transferStatus =
                                    EditingTransfer
                                        (validateForm loggedIn.accountName
                                            (maxTransferAmount model)
                                            community.symbol
                                            form
                                        )
                            }
                                |> UR.init

                _ ->
                    model |> UR.init

        PushTransaction ->
            case ( model.transferStatus, loggedIn.selectedCommunity ) of
                ( CreatingSubscription form, RemoteData.Success community ) ->
                    let
                        account =
                            Maybe.map .account form.selectedProfile
                                |> Maybe.withDefault (Eos.stringToName "")
                    in
                    { model | transferStatus = SendingTransfer form }
                        |> UR.init
                        |> UR.addPort
                            { responseAddress = PushTransaction
                            , responseData = Encode.null
                            , data =
                                Eos.encodeTransaction
                                    [ { accountName = loggedIn.shared.contracts.token
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
                                                , symbol = community.symbol
                                                }
                                            , memo = form.memo
                                            }
                                                |> Transfer.encodeEosActionData
                                      }
                                    ]
                            }
                        |> LoggedIn.withAuthentication loggedIn
                            model
                            { successMsg = msg, errorMsg = ClosedAuthModal }

                _ ->
                    onlyLogImpossible "Pushed transaction, but wasn't creating subscription or community wasn't loaded"

        GotTransferResult (Ok _) ->
            case model.transferStatus of
                SendingTransfer form ->
                    model
                        |> UR.init
                        |> UR.addBreadcrumb
                            { type_ = Log.DebugBreadcrumb
                            , category = msg
                            , message = "Transferred to another user"
                            , data =
                                Dict.fromList
                                    [ ( "to"
                                      , Maybe.map .account form.selectedProfile
                                            |> Maybe.withDefault (Eos.stringToName "")
                                            |> Eos.encodeName
                                      )
                                    , ( "from", Eos.encodeName loggedIn.accountName )
                                    , ( "amount", Encode.string form.amount )
                                    ]
                            , level = Log.DebugLevel
                            }

                _ ->
                    onlyLogImpossible "Got successful transfer result, but wasn't sending transfer"

        GotTransferResult (Err eosErrorString) ->
            let
                errorMessage =
                    EosError.parseTransferError loggedIn.shared.translators eosErrorString
            in
            case model.transferStatus of
                SendingTransfer form ->
                    { model | transferStatus = EditingTransfer form }
                        |> UR.init
                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure errorMessage)

                _ ->
                    onlyLogImpossible "Got transfer result with error, but wasn't sending transfer"

        Redirect value ->
            case ( model.transferStatus, loggedIn.selectedCommunity ) of
                ( SendingTransfer form, RemoteData.Success community ) ->
                    case form.selectedProfile of
                        Just to ->
                            let
                                sub =
                                    Transfer.transferSucceedSubscription
                                        community.symbol
                                        (Eos.nameToString loggedIn.accountName)
                                        (Eos.nameToString to.account)
                                        |> Graphql.Document.decoder
                            in
                            case Decode.decodeValue sub value of
                                Ok res ->
                                    model
                                        |> UR.init
                                        |> UR.addCmd
                                            (Route.ViewTransfer res.id
                                                |> Route.replaceUrl shared.navKey
                                            )

                                Err err ->
                                    model
                                        |> UR.init
                                        |> UR.logDecodingError msg
                                            (Just loggedIn.accountName)
                                            "Got an error when decoding transfer subscription"
                                            { moduleName = "Page.Community.Transfer", function = "update" }
                                            []
                                            err

                        Nothing ->
                            model
                                |> UR.init
                                |> UR.logImpossible msg
                                    "After transferring there wasn't a selected profile"
                                    (Just loggedIn.accountName)
                                    { moduleName = "Page.Community.Transfer", function = "update" }
                                    []

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg
                            "Tried transfering, but community is not loaded"
                            (Just loggedIn.accountName)
                            { moduleName = "Page.Community.Transfer", function = "update" }
                            []


fetchBalance : Shared -> Eos.Name -> Community.Model -> Cmd Msg
fetchBalance shared accountName community =
    Api.getBalances shared
        accountName
        (Result.map
            (\balances ->
                LE.find (.asset >> .symbol >> (==) community.symbol) balances
            )
            >> CompletedLoadBalance
        )


maxTransferAmount : Model -> RemoteData MaxAmountError Float
maxTransferAmount model =
    RemoteData.map2 (\balance token -> balance.asset.amount - token.minBalance.amount)
        (RemoteData.mapError BalanceError model.balance)
        (RemoteData.mapError TokenError model.token)


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        [ "PushTransaction" ] ->
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.field "transactionId" Decode.string
                        |> Decode.map Ok
                    , Decode.field "error" (Decode.nullable Decode.value)
                        |> Decode.map Err
                    ]
                )
                val
                |> Result.map (Just << GotTransferResult)
                |> Result.withDefault Nothing

        [ "SubmitForm" ] ->
            let
                result =
                    Decode.decodeValue (Decode.field "state" Decode.string) val
                        |> Result.withDefault ""
            in
            case result of
                "starting" ->
                    Just PushTransaction

                "responded" ->
                    Decode.decodeValue (Decode.field "data" Decode.value) val
                        |> Result.map Redirect
                        |> Result.toMaybe

                _ ->
                    Nothing

        _ ->
            Nothing


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
        CompletedLoadCommunity _ ->
            [ "CompletedLoadCommunity" ]

        CompletedLoadBalance r ->
            [ "CompletedLoadBalance", UR.resultToString r ]

        CompletedLoadToken r ->
            [ "CompletedLoadToken", UR.resultToString r ]

        ClosedAuthModal ->
            [ "ClosedAuthModal" ]

        OnSelect _ ->
            [ "OnSelect" ]

        SelectMsg _ ->
            [ "SelectMsg", "sub" ]

        EnteredAmount _ ->
            [ "EnteredAmount" ]

        EnteredMemo _ ->
            [ "EnteredMemo" ]

        SubmitForm ->
            [ "SubmitForm" ]

        PushTransaction ->
            [ "PushTransaction" ]

        GotTransferResult result ->
            [ "GotTransferResult", UR.resultToString result ]

        Redirect _ ->
            [ "Redirect" ]
