module Page.Community.Transfer exposing
    ( Model
    , Msg
    , init
    , jsAddressToMsg
    , msgToString
    , subscription
    , update
    , view
    )

import Api.Graphql
import Browser.Events
import Community exposing (Model)
import Eos exposing (Symbol)
import Eos.Account as Eos
import Eos.EosError as EosError
import Graphql.Document
import Graphql.Http
import Html exposing (Html, button, div, form, input, span, text, textarea)
import Html.Attributes exposing (class, disabled, placeholder, required, rows, type_, value)
import Html.Events exposing (onInput, onSubmit)
import I18Next exposing (Delims(..), t)
import Json.Decode as Decode exposing (Value)
import Json.Encode as Encode exposing (Value)
import Page
import Profile exposing (Profile)
import Route
import Select
import Session.LoggedIn as LoggedIn exposing (External(..))
import Session.Shared as Shared
import Task
import Transfer
import UpdateResult as UR
import Utils


init : LoggedIn.Model -> Symbol -> Maybe String -> ( Model, Cmd Msg )
init { shared } symbol maybeTo =
    ( initModel symbol maybeTo
    , Api.Graphql.query shared (Community.communityQuery symbol) CompletedLoad
    )


subscription : Model -> Sub Msg
subscription _ =
    Sub.map PressedEnter (Browser.Events.onKeyDown Utils.decodeEnterKeyDown)



-- MODEL


type alias Model =
    { communityId : Symbol
    , maybeTo : Maybe String
    , status : Status
    , autoCompleteState : Select.State
    }


initModel : Symbol -> Maybe String -> Model
initModel symbol maybeTo =
    { communityId = symbol
    , maybeTo = maybeTo
    , status = Loading
    , autoCompleteState = Select.newState ""
    }


type Status
    = Loading
    | Loaded Community.Model TransferStatus
    | NotFound
    | Failed (Graphql.Http.Error (Maybe Community.Model))


type TransferStatus
    = EditingTransfer Form
    | CreatingSubscription Form
    | SendingTransfer Form
    | SendingTransferFailed Form


type Validation
    = Valid
    | Invalid String


type alias Form =
    { selectedProfile : Maybe Profile
    , selectedProfileValidation : Validation
    , amount : String
    , amountValidation : Validation
    , memo : String
    }


emptyForm : Form
emptyForm =
    { selectedProfile = Nothing
    , selectedProfileValidation = Valid
    , amount = ""
    , amountValidation = Valid
    , memo = ""
    }


validateForm : Form -> Form
validateForm form =
    let
        isAllowedChar : Char -> Bool
        isAllowedChar c =
            Char.isDigit c || c == ','
    in
    { form
        | selectedProfileValidation =
            case form.selectedProfile of
                Just _ ->
                    Valid

                Nothing ->
                    Invalid ""
        , amountValidation =
            if (String.toList form.amount |> List.any isAllowedChar) || String.length form.amount > 0 then
                Valid

            else
                Invalid ""
    }


isFormValid : Form -> Bool
isFormValid form =
    form.selectedProfileValidation == Valid && form.amountValidation == Valid



-- VIEW


view : LoggedIn.Model -> Model -> { title : String, content : Html Msg }
view ({ shared } as loggedIn) model =
    let
        content =
            case model.status of
                Loading ->
                    Page.fullPageLoading

                NotFound ->
                    Page.viewCardEmpty [ text "Community not found" ]

                Failed e ->
                    Page.fullPageGraphQLError (t shared.translations "community.objectives.title_plural") e

                Loaded community (EditingTransfer f) ->
                    viewForm loggedIn model f community False

                Loaded community (CreatingSubscription f) ->
                    viewForm loggedIn model f community True

                Loaded community (SendingTransfer f) ->
                    viewForm loggedIn model f community True

                Loaded community (SendingTransferFailed f) ->
                    viewForm loggedIn model f community False
    in
    { title = t shared.translations "transfer.title"
    , content = content
    }


viewForm : LoggedIn.Model -> Model -> Form -> Community.Model -> Bool -> Html Msg
viewForm ({ shared } as loggedIn) model f community isDisabled =
    let
        text_ s =
            text (t loggedIn.shared.translations s)
    in
    div [ class "bg-white" ]
        [ Page.viewHeader loggedIn (t shared.translations "transfer.title") Route.Dashboard
        , form [ class "container mx-auto p-4", onSubmit SubmitForm ]
            [ div [ class "mb-10" ]
                [ span [ class "input-label" ]
                    [ text_ "account.my_wallet.transfer.send_to" ]
                , div [ class "" ]
                    [ viewAutoCompleteAccount shared model f isDisabled community ]
                ]
            , div [ class "mb-10" ]
                [ span [ class "input-label" ]
                    [ text
                        (I18Next.tr shared.translations
                            Curly
                            "account.my_wallet.transfer.amount"
                            [ ( "symbol", Eos.symbolToSymbolCodeString community.symbol ) ]
                        )
                    ]
                , div [ class "flex h-12 rounded-sm border border-gray-500" ]
                    [ input
                        [ class "block w-4/5 border-none px-4 py-3 outline-none"
                        , placeholder "0"
                        , disabled isDisabled
                        , required True
                        , onInput EnteredAmount
                        , value f.amount
                        ]
                        []
                    , span
                        [ class "w-1/5 flex text-white items-center justify-center bg-indigo-500 text-body uppercase rounded-r-sm" ]
                        [ text (Eos.symbolToSymbolCodeString community.symbol) ]
                    ]
                ]
            , div [ class "mb-10" ]
                [ span [ class "input-label" ]
                    [ text_ "account.my_wallet.transfer.memo" ]
                , textarea
                    [ class "w-full input rounded-sm"
                    , rows 5
                    , disabled isDisabled
                    , onInput EnteredMemo
                    ]
                    []
                ]
            , div [ class "mt-6" ]
                [ button
                    [ class "button button-primary w-full"
                    , disabled isDisabled
                    , type_ "submit"
                    ]
                    [ text_ "account.my_wallet.transfer.submit" ]
                ]
            ]
        ]


viewAutoCompleteAccount : Shared.Shared -> Model -> Form -> Bool -> Community.Model -> Html Msg
viewAutoCompleteAccount shared model form isDisabled community =
    let
        users =
            community.members

        selectedUsers =
            Maybe.map (\v -> [ v ]) form.selectedProfile
                |> Maybe.withDefault []
    in
    div []
        [ Html.map SelectMsg
            (Select.view
                (selectConfiguration shared isDisabled)
                model.autoCompleteState
                users
                selectedUsers
            )
        ]


selectConfiguration : Shared.Shared -> Bool -> Select.Config Msg Profile
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
    = CompletedLoad (Result (Graphql.Http.Error (Maybe Community.Model)) (Maybe Community.Model))
    | OnSelect (Maybe Profile)
    | SelectMsg (Select.Msg Profile)
    | EnteredAmount String
    | EnteredMemo String
    | SubmitForm
    | PressedEnter Bool
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
        CompletedLoad (Ok community) ->
            case community of
                Just cmm ->
                    UR.init { model | status = Loaded cmm (getProfile model.maybeTo cmm) }

                Nothing ->
                    UR.init { model | status = NotFound }

        CompletedLoad (Err error) ->
            { model | status = Failed error }
                |> UR.init
                |> UR.logGraphqlError msg error

        OnSelect maybeProfile ->
            case model.status of
                Loaded community (EditingTransfer form) ->
                    { model
                        | status =
                            EditingTransfer { form | selectedProfile = maybeProfile }
                                |> Loaded community
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
            let
                getNumericValues : String -> String
                getNumericValues v =
                    v
                        |> String.toList
                        |> List.filter (\d -> Char.isDigit d || d == '.')
                        |> List.map String.fromChar
                        |> String.join ""
            in
            case model.status of
                Loaded community (EditingTransfer form) ->
                    { model
                        | status =
                            EditingTransfer { form | amount = getNumericValues value }
                                |> Loaded community
                    }
                        |> UR.init

                _ ->
                    model |> UR.init

        EnteredMemo value ->
            case model.status of
                Loaded community (EditingTransfer form) ->
                    { model
                        | status =
                            EditingTransfer { form | memo = value }
                                |> Loaded community
                    }
                        |> UR.init

                _ ->
                    model |> UR.init

        SubmitForm ->
            case model.status of
                Loaded c (EditingTransfer form) ->
                    case form.selectedProfile of
                        Just to ->
                            let
                                newForm =
                                    validateForm form

                                subscriptionDoc =
                                    Transfer.transferSucceedSubscription model.communityId (Eos.nameToString loggedIn.accountName) (Eos.nameToString to.account)
                                        |> Graphql.Document.serializeSubscription
                            in
                            if isFormValid newForm then
                                { model | status = Loaded c (CreatingSubscription newForm) }
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

                            else
                                { model | status = Loaded c (EditingTransfer newForm) }
                                    |> UR.init

                        Nothing ->
                            { model | status = Loaded c (EditingTransfer (validateForm form)) } |> UR.init

                _ ->
                    model |> UR.init

        PushTransaction ->
            case ( model.status, LoggedIn.isAuth loggedIn ) of
                ( Loaded c (CreatingSubscription form), True ) ->
                    let
                        account =
                            Maybe.map .account form.selectedProfile
                                |> Maybe.withDefault (Eos.stringToName "")
                    in
                    { model | status = Loaded c (SendingTransfer form) }
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
                                                , symbol = model.communityId
                                                }
                                            , memo = form.memo
                                            }
                                                |> Transfer.encodeEosActionData
                                      }
                                    ]
                            }

                ( Loaded _ (CreatingSubscription _), False ) ->
                    UR.init model
                        |> UR.addExt
                            (Just PushTransaction
                                |> RequiredAuthentication
                            )

                _ ->
                    onlyLogImpossible []

        PressedEnter val ->
            if val then
                UR.init model
                    |> UR.addCmd
                        (Task.succeed SubmitForm
                            |> Task.perform identity
                        )

            else
                UR.init model

        GotTransferResult (Ok _) ->
            case model.status of
                Loaded _ (SendingTransfer _) ->
                    model
                        |> UR.init

                _ ->
                    onlyLogImpossible []

        GotTransferResult (Err eosErrorString) ->
            let
                errorMessage =
                    EosError.parseTransferError loggedIn.shared.translators eosErrorString
            in
            case model.status of
                Loaded c (SendingTransfer form) ->
                    { model | status = Loaded c (SendingTransferFailed form) }
                        |> UR.init
                        |> UR.addExt
                            (LoggedIn.ShowFeedback
                                LoggedIn.Failure
                                errorMessage
                            )

                _ ->
                    onlyLogImpossible []

        Redirect value ->
            case model.status of
                Loaded _ (SendingTransfer form) ->
                    case form.selectedProfile of
                        Just to ->
                            let
                                sub =
                                    Transfer.transferSucceedSubscription
                                        model.communityId
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


msgToString : Msg -> List String
msgToString msg =
    case msg of
        CompletedLoad r ->
            [ "CompletedLoad", UR.resultToString r ]

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

        PressedEnter _ ->
            [ "PressedEnter" ]

        PushTransaction ->
            [ "PushTransaction" ]

        GotTransferResult result ->
            [ "GotTransferResult", UR.resultToString result ]

        Redirect _ ->
            [ "Redirect" ]
