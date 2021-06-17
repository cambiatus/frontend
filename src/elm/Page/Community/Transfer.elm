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

import Community
import Eos exposing (Symbol)
import Eos.Account as Eos
import Eos.EosError as EosError
import Graphql.Document
import Html exposing (Html, button, div, form, span, text)
import Html.Attributes exposing (class, classList, disabled, maxlength, rows, type_, value)
import Html.Events exposing (onSubmit)
import I18Next
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode exposing (Value)
import List.Extra as LE
import Page
import Profile
import RemoteData
import Route
import Select
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared as Shared
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
    }


initModel : Maybe String -> Model
initModel maybeTo =
    { maybeTo = maybeTo
    , transferStatus = EditingTransfer emptyForm
    , autoCompleteState = Select.newState ""
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


validateAmount : Symbol -> Form -> Form
validateAmount symbol form =
    let
        symbolPrecision =
            Eos.getSymbolPrecision symbol

        amountPrecision =
            String.toList form.amount
                |> LE.dropWhile (\c -> c /= '.')
                |> List.drop 1
                |> List.length
    in
    { form
        | amountValidation =
            if amountPrecision > symbolPrecision then
                Invalid "error.contracts.transfer.symbol precision mismatch" Nothing

            else if
                String.all (validAmountCharacter symbol) form.amount
                    && (String.length form.amount > 0)
            then
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


validateForm : Eos.Name -> Symbol -> Form -> Form
validateForm currentAccount symbol form =
    form
        |> validateSelectedProfile currentAccount
        |> validateAmount symbol
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
            case ( loggedIn.selectedCommunity, model.transferStatus ) of
                ( RemoteData.NotAsked, _ ) ->
                    Page.fullPageLoading shared

                ( RemoteData.Loading, _ ) ->
                    Page.fullPageLoading shared

                ( RemoteData.Failure e, _ ) ->
                    Page.fullPageGraphQLError (shared.translators.t "community.objectives.title_plural") e

                ( RemoteData.Success community, EditingTransfer f ) ->
                    viewForm loggedIn model f community False

                ( RemoteData.Success community, CreatingSubscription f ) ->
                    viewForm loggedIn model f community True

                ( RemoteData.Success community, SendingTransfer f ) ->
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
    in
    div [ class "bg-white" ]
        [ Page.viewHeader loggedIn (shared.translators.t "transfer.title")
        , form [ class "container mx-auto p-4", onSubmit SubmitForm ]
            [ div [ class "mb-10" ]
                [ span [ class "input-label" ]
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
                |> Input.withCurrency community.symbol
                |> Input.toHtml
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


viewAutoCompleteAccount : Shared.Shared -> Model -> Form -> Bool -> Community.Model -> Html Msg
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


selectConfiguration : Shared.Shared -> Bool -> Select.Config Msg Profile.Minimal
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
        onlyLogImpossible desc =
            UR.init model
                |> UR.logImpossible msg desc
    in
    case msg of
        CompletedLoadCommunity community ->
            UR.init { model | transferStatus = getProfile model.maybeTo community }

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
                    onlyLogImpossible []

        GotTransferResult (Ok _) ->
            case model.transferStatus of
                SendingTransfer _ ->
                    model
                        |> UR.init

                _ ->
                    onlyLogImpossible []

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
                    onlyLogImpossible []

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

                                Err _ ->
                                    model
                                        |> UR.init
                                        |> UR.logImpossible msg []

                        Nothing ->
                            model
                                |> UR.init
                                |> UR.logImpossible msg []

                _ ->
                    model
                        |> UR.init
                        |> UR.logImpossible msg []


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
