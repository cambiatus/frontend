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
import Eos
import Eos.Account as Eos
import Eos.EosError as EosError
import Form
import Form.RichText
import Form.Text
import Form.UserPicker
import Form.Validate
import Graphql.Document
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, disabled)
import Http
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import List.Extra as LE
import Log
import Markdown exposing (Markdown)
import Page
import Profile
import RemoteData exposing (RemoteData)
import Route
import Session.LoggedIn as LoggedIn exposing (External)
import Session.Shared exposing (Shared)
import Token
import Transfer
import UpdateResult as UR
import View.Feedback as Feedback


init : LoggedIn.Model -> Maybe Eos.Name -> UpdateResult
init loggedIn maybeTo =
    initModel maybeTo
        |> UR.init
        |> UR.addCmd (LoggedIn.maybeInitWith CompletedLoadCommunity .selectedCommunity loggedIn)
        |> UR.addExt (LoggedIn.RequestedReloadCommunityField Community.MembersField)



-- MODEL


type alias Model =
    { maybeTo : Maybe Eos.Name
    , balance : RemoteData BalanceError Community.Balance
    , token : RemoteData Http.Error Token.Model
    , form : Form.Model FormInput
    , transferStatus : TransferStatus
    }


type BalanceError
    = BalanceNotFound
    | WithHttpError Http.Error


type MaxAmountError
    = TokenError Http.Error
    | BalanceError BalanceError


initModel : Maybe Eos.Name -> Model
initModel maybeTo =
    { maybeTo = maybeTo
    , balance = RemoteData.Loading
    , token = RemoteData.Loading
    , form =
        Form.init
            { selectedProfile = Form.UserPicker.initSingle { id = "transfer-profile-select" }
            , amount = ""
            , memo = Form.RichText.initModel "memo-editor" Nothing
            }
    , transferStatus = EditingTransfer
    }


type TransferStatus
    = EditingTransfer
    | CreatingSubscription FormOutput
    | SendingTransfer FormOutput


createForm : LoggedIn.Model -> Community.Model -> Eos.Asset -> Float -> Form.Form msg FormInput FormOutput
createForm loggedIn community balance maxTransferAmount =
    let
        { translators } =
            loggedIn.shared
    in
    Form.succeed FormOutput
        |> Form.with
            (Form.UserPicker.init
                { label = translators.t "account.my_wallet.transfer.send_to"
                , currentUser = loggedIn.accountName
                , profiles = community.members
                }
                |> Form.userPicker
                    { parser =
                        \maybeUser ->
                            case maybeUser of
                                Nothing ->
                                    Err <| translators.t "transfer.no_profile"

                                Just user ->
                                    if user.account == loggedIn.accountName then
                                        Err <| translators.t "transfer.to_self"

                                    else
                                        Ok user
                    , value = .selectedProfile
                    , update = \selectedProfile input -> { input | selectedProfile = selectedProfile }
                    , externalError = always Nothing
                    }
            )
        |> Form.with
            (Form.Text.init
                { label =
                    translators.tr "account.my_wallet.transfer.amount"
                        [ ( "symbol", Eos.symbolToSymbolCodeString community.symbol ) ]
                , id = "transfer-amount-field"
                }
                |> Form.Text.withContainerAttrs [ class "mb-4" ]
                |> Form.Text.withCurrency balance.symbol
                |> Form.textField
                    { parser =
                        Form.Validate.succeed
                            >> Form.Validate.maskedFloat translators
                            >> Form.Validate.floatLowerThanOrEqualTo maxTransferAmount
                            >> Form.Validate.withCustomError
                                (\translators_ ->
                                    translators_.tr "transfer.too_much"
                                        [ ( "token", Eos.symbolToSymbolCodeString balance.symbol )
                                        , ( "max_asset"
                                          , Eos.assetToString translators_
                                                { amount = maxTransferAmount
                                                , symbol = balance.symbol
                                                }
                                          )
                                        ]
                                )
                            >> Form.Validate.map (\amount -> { amount = amount, symbol = community.symbol })
                            >> Form.Validate.validate translators
                    , value = .amount
                    , update = \amount input -> { input | amount = amount }
                    , externalError = always Nothing
                    }
            )
        |> Form.withNoOutput
            (span [ class "bg-gray-100 uppercase text-sm px-2 inline-block mb-10" ]
                [ text <|
                    translators.tr "account.my_wallet.your_current_balance"
                        [ ( "balance", Eos.assetToString translators balance ) ]
                ]
                |> Form.arbitrary
            )
        |> Form.with
            (Form.RichText.init
                { label = translators.t "account.my_wallet.transfer.memo" }
                |> Form.richText
                    { parser = Ok
                    , value = .memo
                    , update = \memo input -> { input | memo = memo }
                    , externalError = always Nothing
                    }
            )


type alias FormInput =
    { selectedProfile : Form.UserPicker.SinglePickerModel
    , amount : String
    , memo : Form.RichText.Model
    }


type alias FormOutput =
    { selectedProfile : Profile.Minimal
    , amount : Eos.Asset
    , memo : Markdown
    }



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view ({ shared } as loggedIn) model =
    let
        content =
            case ( loggedIn.selectedCommunity, getMaxTransferAmount model ) of
                ( RemoteData.NotAsked, _ ) ->
                    Page.fullPageLoading shared

                ( RemoteData.Loading, _ ) ->
                    Page.fullPageLoading shared

                ( _, RemoteData.NotAsked ) ->
                    Page.fullPageLoading shared

                ( _, RemoteData.Loading ) ->
                    Page.fullPageLoading shared

                ( RemoteData.Failure e, _ ) ->
                    Page.fullPageGraphQLError (shared.translators.t "community.objectives.title_plural") e

                ( _, RemoteData.Failure e ) ->
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

                ( RemoteData.Success community, RemoteData.Success maxTransferAmount ) ->
                    viewForm loggedIn model community maxTransferAmount
    in
    { title = shared.translators.t "transfer.title"
    , content = content
    }


viewForm : LoggedIn.Model -> Model -> Community.Model -> Float -> Html Msg
viewForm ({ shared } as loggedIn) model community maxTransferAmount =
    let
        text_ s =
            text (loggedIn.shared.translators.t s)

        currBalance =
            model.balance
                |> RemoteData.map .asset
                |> RemoteData.withDefault { amount = 0, symbol = community.symbol }

        isDisabled =
            case model.transferStatus of
                EditingTransfer ->
                    False

                CreatingSubscription _ ->
                    True

                SendingTransfer _ ->
                    True
    in
    div [ class "bg-white" ]
        [ Page.viewHeader loggedIn (shared.translators.t "transfer.title")
        , Form.view [ class "container mx-auto p-4" ]
            shared.translators
            (\submitButton ->
                [ submitButton
                    [ class "w-full mt-6 button button-primary"
                    , disabled (not loggedIn.hasAcceptedCodeOfConduct)
                    ]
                    [ text_ "account.my_wallet.transfer.submit" ]
                ]
            )
            (createForm loggedIn community currBalance maxTransferAmount)
            (Form.withDisabled isDisabled model.form)
            { toMsg = GotFormMsg, onSubmit = SubmittedForm }
        ]



-- UPDATE


type alias UpdateResult =
    UR.UpdateResult Model Msg (External Msg)


type Msg
    = CompletedLoadCommunity Community.Model
    | CompletedLoadBalance (Result Http.Error (Maybe Community.Balance))
    | CompletedLoadToken (Result Http.Error Token.Model)
    | ClosedAuthModal
    | GotFormMsg (Form.Msg FormInput)
    | SubmittedForm FormOutput
    | CreatedTransferSubscription
    | GotTransferTransactionResult (Result (Maybe Value) String)
    | GotTransferResultFromWebSocket Value


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
            let
                newModel =
                    case model.maybeTo of
                        Nothing ->
                            model

                        Just toAccount ->
                            { model
                                | form =
                                    Form.updateValues
                                        (\values ->
                                            { values
                                                | selectedProfile =
                                                    Form.UserPicker.setSingle
                                                        (LE.find (.account >> (==) toAccount)
                                                            community.members
                                                        )
                                                        values.selectedProfile
                                            }
                                        )
                                        model.form
                            }
            in
            newModel
                |> UR.init
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
            UR.init { model | transferStatus = EditingTransfer }

        GotFormMsg subMsg ->
            Form.update shared subMsg model.form
                |> UR.fromChild
                    (\formModel -> { model | form = formModel })
                    GotFormMsg
                    LoggedIn.addFeedback
                    model

        SubmittedForm formOutput ->
            if model.transferStatus /= EditingTransfer then
                UR.init model

            else
                let
                    subscriptionDoc =
                        Transfer.transferSucceedSubscription
                            formOutput.amount.symbol
                            { from = loggedIn.accountName
                            , to = formOutput.selectedProfile.account
                            }
                            |> Graphql.Document.serializeSubscription
                in
                { model | transferStatus = CreatingSubscription formOutput }
                    |> UR.init
                    |> UR.addPort
                        { responseAddress = SubmittedForm formOutput
                        , responseData = Encode.null
                        , data =
                            Encode.object
                                [ ( "name", Encode.string "subscribeToTransfer" )
                                , ( "subscription", Encode.string subscriptionDoc )
                                ]
                        }

        CreatedTransferSubscription ->
            case model.transferStatus of
                CreatingSubscription formOutput ->
                    { model | transferStatus = SendingTransfer formOutput }
                        |> UR.init
                        |> UR.addPort
                            { responseAddress = CreatedTransferSubscription
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
                                            , to = formOutput.selectedProfile.account
                                            , value = formOutput.amount
                                            , memo = formOutput.memo
                                            }
                                                |> Transfer.encodeEosActionWithMarkdown
                                      }
                                    ]
                            }
                        |> LoggedIn.withPrivateKey loggedIn
                            []
                            model
                            { successMsg = msg, errorMsg = ClosedAuthModal }

                _ ->
                    UR.init model

        GotTransferTransactionResult (Ok _) ->
            UR.init model

        GotTransferTransactionResult (Err eosErrorString) ->
            case model.transferStatus of
                SendingTransfer _ ->
                    let
                        errorMessage =
                            EosError.parseTransferError loggedIn.shared.translators eosErrorString
                    in
                    { model | transferStatus = EditingTransfer }
                        |> UR.init
                        |> UR.addExt (LoggedIn.ShowFeedback Feedback.Failure errorMessage)

                _ ->
                    onlyLogImpossible "Got transfer result with error, but wasn't sending transfer"

        GotTransferResultFromWebSocket value ->
            case model.transferStatus of
                SendingTransfer formOutput ->
                    let
                        subscriptionDecoder =
                            Transfer.transferSucceedSubscription formOutput.amount.symbol
                                { from = loggedIn.accountName
                                , to = formOutput.selectedProfile.account
                                }
                                |> Graphql.Document.decoder
                    in
                    case Decode.decodeValue subscriptionDecoder value of
                        Ok res ->
                            model
                                |> UR.init
                                |> UR.addCmd
                                    (Route.replaceUrl shared.navKey
                                        (Route.ViewTransfer res.id)
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

                _ ->
                    UR.init model


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


getMaxTransferAmount : Model -> RemoteData MaxAmountError Float
getMaxTransferAmount model =
    RemoteData.map2 (\balance token -> balance.asset.amount - token.minBalance.amount)
        (RemoteData.mapError BalanceError model.balance)
        (RemoteData.mapError TokenError model.token)


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        [ "SubmittedForm" ] ->
            let
                result =
                    Decode.decodeValue (Decode.field "state" Decode.string) val
            in
            case result of
                Ok "starting" ->
                    Just CreatedTransferSubscription

                Ok "responded" ->
                    Decode.decodeValue (Decode.field "data" Decode.value) val
                        |> Result.map GotTransferResultFromWebSocket
                        |> Result.toMaybe

                _ ->
                    Nothing

        [ "CreatedTransferSubscription" ] ->
            Decode.decodeValue
                (Decode.oneOf
                    [ Decode.field "transactionId" Decode.string
                        |> Decode.map Ok
                    , Decode.field "error" (Decode.nullable Decode.value)
                        |> Decode.map Err
                    ]
                )
                val
                |> Result.map (Just << GotTransferTransactionResult)
                |> Result.withDefault Nothing

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

        GotFormMsg subMsg ->
            "GotFormMsg" :: Form.msgToString subMsg

        SubmittedForm _ ->
            [ "SubmittedForm" ]

        CreatedTransferSubscription ->
            [ "CreatedTransferSubscription" ]

        GotTransferTransactionResult r ->
            [ "GotTransferTransactionResult", UR.resultToString r ]

        GotTransferResultFromWebSocket _ ->
            [ "GotTransferResultFromWebSocket" ]
