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
import Community exposing (Community, communityQuery)
import Eos exposing (Symbol)
import Eos.Account as Eos
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
    | Loaded Community TransferStatus
    | NotFound
    | Failed (Graphql.Http.Error (Maybe Community))


type TransferStatus
    = EditingTransfer Form
    | SendingTransfer Form
    | SendingTransferFailed Form


type alias Form =
    { selectedProfile : Maybe Profile
    , amount : String
    , memo : String
    }


emptyForm : Form
emptyForm =
    { selectedProfile = Nothing
    , amount = ""
    , memo = ""
    }



-- VIEW


view : LoggedIn.Model -> Model -> Html Msg
view ({ shared } as loggedIn) model =
    case model.status of
        Loading ->
            Page.fullPageLoading

        NotFound ->
            Page.viewCardEmpty [ text "Community not found" ]

        Failed e ->
            Page.fullPageGraphQLError (t shared.translations "community.objectives.title_plural") e

        Loaded community (EditingTransfer f) ->
            viewForm loggedIn model f community False

        Loaded community (SendingTransfer f) ->
            viewForm loggedIn model f community True

        Loaded _ (SendingTransferFailed _) ->
            text "Failed transfer"


viewForm : LoggedIn.Model -> Model -> Form -> Community -> Bool -> Html Msg
viewForm ({ shared } as loggedIn) model f community isDisabled =
    let
        text_ s =
            text (t loggedIn.shared.translations s)
    in
    div [ class "bg-white" ]
        [ Page.viewHeader loggedIn (t shared.translations "transfer.title") (Route.Community model.communityId)
        , form [ class "container mx-auto py-4", onSubmit SubmitForm ]
            [ div [ class "mb-10" ]
                [ span [ class "input-label" ]
                    [ text_ "account.my_wallet.transfer.send_to" ]
                , div [ class "" ]
                    [ autoCompleteAccount shared model f isDisabled community ]
                ]
            , div [ class "mb-10" ]
                [ span [ class "input-label" ]
                    [ text
                        (I18Next.tr shared.translations
                            Curly
                            "account.my_wallet.transfer.amount"
                            [ ( "symbol", Eos.symbolToString community.symbol ) ]
                        )
                    ]
                , div [ class "flex h-12 rounded-sm border border-gray-500" ]
                    [ input
                        [ class "block w-4/5 border-none px-4 py-3 outline-none"
                        , placeholder "0.00"
                        , disabled isDisabled
                        , required True
                        , onInput EnteredAmount
                        , value f.amount
                        ]
                        []
                    , span
                        [ class "w-1/5 flex text-white items-center justify-center bg-indigo-500 text-body uppercase rounded-r-sm" ]
                        [ text (Eos.symbolToString community.symbol) ]
                    ]
                ]
            , div [ class "mb-10" ]
                [ span [ class "input-label" ]
                    [ text_ "community.actions.form.description_label" ]
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


autoCompleteAccount : Shared.Shared -> Model -> Form -> Bool -> Community -> Html Msg
autoCompleteAccount shared model form isDisabled community =
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
    = CompletedLoad (Result (Graphql.Http.Error (Maybe Community)) (Maybe Community))
    | OnSelect (Maybe Profile)
    | SelectMsg (Select.Msg Profile)
    | EnteredAmount String
    | EnteredMemo String
    | SubmitForm
    | PressedEnter Bool
    | GotTransferResult (Result Value String)


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
                    UR.init { model | status = Loaded cmm (EditingTransfer emptyForm) }

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
                        |> List.filter Char.isDigit
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
            case ( model.status, LoggedIn.isAuth loggedIn ) of
                ( Loaded c (EditingTransfer form), True ) ->
                    let
                        account =
                            Maybe.map .account form.selectedProfile
                                |> Maybe.withDefault (Eos.stringToName "")
                    in
                    { model | status = Loaded c (SendingTransfer form) }
                        |> UR.init
                        |> UR.addPort
                            { responseAddress = SubmitForm
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
                                                    , symbol = model.communityId
                                                    }
                                                , memo = form.memo
                                                }
                                                    |> Transfer.encodeEosActionData
                                          }
                                        ]
                                    }
                            }

                ( Loaded _ (EditingTransfer _), False ) ->
                    UR.init model
                        |> UR.addExt
                            (Just SubmitForm
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
                        -- TODO redirect
                        -- TODO add global msg
                        |> UR.init

                _ ->
                    onlyLogImpossible []

        GotTransferResult (Err _) ->
            case model.status of
                Loaded c (SendingTransfer form) ->
                    { model | status = Loaded c (SendingTransferFailed form) }
                        -- TODO add global failure msg
                        |> UR.init

                _ ->
                    onlyLogImpossible []


jsAddressToMsg : List String -> Value -> Maybe Msg
jsAddressToMsg addr val =
    case addr of
        "SubmitForm" :: [] ->
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

        GotTransferResult result ->
            resultToString [ "GotTransferResult" ] result
