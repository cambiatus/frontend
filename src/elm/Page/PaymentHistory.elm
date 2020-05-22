module Page.PaymentHistory exposing (Model, Msg, init, msgToString, update, view)

import Api.Graphql
import Avatar
import Date exposing (Date)
import DatePicker exposing (DateEvent(..), defaultSettings, off)
import Eos
import Eos.Account
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Html exposing (Html, button, div, h1, h2, label, p, span, text, ul)
import Html.Attributes as Attrs exposing (class, style)
import List.Extra as LE
import Page
import Profile exposing (Profile)
import Select
import Session.Guest as Guest exposing (External(..))
import Session.Shared as Shared exposing (Shared)
import Strftime
import Time exposing (Month(..), Weekday(..))
import Transfer exposing (QueryTransfers, Transfer, userFilter)
import UpdateResult as UR
import Utils



-- MSG


type Msg
    = NoOp
    | CompletedLoadUserTransfers (Result (Graphql.Http.Error (Maybe QueryTransfers)) (Maybe QueryTransfers))
    | OnSelect (Maybe Profile)
    | SelectMsg (Select.Msg Profile)
    | ToDatePicker DatePicker.Msg


fetchTransfers : Shared -> Eos.Account.Name -> Cmd Msg
fetchTransfers shared accountName =
    -- TODO: Query only `to` transfers if possible. Now query returns all the transfers (`from` ang `to`)
    Api.Graphql.query shared
        (Transfer.transfersQuery
            (userFilter accountName)
            (\args ->
                { args | first = Present 10 }
            )
        )
        CompletedLoadUserTransfers


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
        NoOp ->
            [ "NoOp" ]

        CompletedLoadUserTransfers result ->
            resultToString [ "CompletedLoadUserTransfers" ] result

        OnSelect _ ->
            [ "OnSelect" ]

        SelectMsg _ ->
            [ "SelectMsg" ]

        ToDatePicker _ ->
            [ "ToDatePicker" ]



-- MODEL


type alias Model =
    { autocompleteState : Select.State
    , selectedProfile : Maybe Profile
    , currentAccount : Eos.Account.Name
    , userFullName : String
    , date : Maybe Date
    , datePicker : DatePicker.DatePicker
    , fetchingTransfersStatus : GraphqlStatus (Maybe QueryTransfers) (List Transfer)
    , transfersToCurrentUser : List Transfer
    }


type GraphqlStatus err a
    = LoadingGraphql
    | LoadedGraphql a
    | FailedGraphql (Graphql.Http.Error err)


filterTransfers : List Transfer -> Eos.Account.Name -> List Transfer
filterTransfers transfers currentUser =
    let
        isCurrentUser : Transfer -> Bool
        isCurrentUser t =
            t.to.account == currentUser
    in
    List.filter isCurrentUser transfers


settings : DatePicker.Settings
settings =
    { defaultSettings
        | changeYear = off
        , inputClassList = [ ( "input", True ), ( "w-full", True ) ]
        , dateFormatter = Date.format "E, d MMM y"
        , firstDayOfWeek = Mon
        , inputAttributes =
            [ Attrs.required False
            , Attrs.readonly True
            ]
    }


init : Guest.Model -> ( Model, Cmd Msg )
init guest =
    let
        ( datePicker, datePickerFx ) =
            DatePicker.init

        accountName =
            let
                uriLastPart =
                    String.split "/" guest.shared.url.path
                        |> LE.last
            in
            Eos.Account.stringToName <|
                Maybe.withDefault "" uriLastPart
    in
    ( { autocompleteState = Select.newState ""
      , selectedProfile = Nothing
      , date = Nothing
      , currentAccount = accountName
      , userFullName = ""
      , fetchingTransfersStatus = LoadingGraphql
      , transfersToCurrentUser = []
      , datePicker = datePicker
      }
    , Cmd.batch
        [ Cmd.map ToDatePicker datePickerFx
        , fetchTransfers guest.shared accountName
        ]
    )



-- UPDATE


update : Msg -> Model -> Guest.Model -> UpdateResult
update msg model guest =
    case msg of
        CompletedLoadUserTransfers (Ok maybeTransfers) ->
            let
                transfersToCurrentUser =
                    filterTransfers (Transfer.getTransfers maybeTransfers) model.currentAccount

                currentUserFullName =
                    -- Get current user details from the first transfer
                    -- TODO: User couldn't have any transfers! Get Profile details via GraphQl query.
                    transfersToCurrentUser
                        |> List.head
                        |> Maybe.andThen (\f -> f.to.userName)
                        |> Maybe.withDefault (Eos.Account.nameToString model.currentAccount)
            in
            { model
                | fetchingTransfersStatus = LoadedGraphql (Transfer.getTransfers maybeTransfers)
                , transfersToCurrentUser = transfersToCurrentUser
                , userFullName = currentUserFullName
            }
                |> UR.init

        CompletedLoadUserTransfers (Err err) ->
            { model | fetchingTransfersStatus = FailedGraphql err }
                |> UR.init
                |> UR.logGraphqlError msg err

        OnSelect maybeProfile ->
            { model
                | selectedProfile = maybeProfile
            }
                |> UR.init

        SelectMsg subMsg ->
            let
                ( updated, cmd ) =
                    Select.update (selectConfiguration guest.shared False) subMsg model.autocompleteState
            in
            UR.init { model | autocompleteState = updated }
                |> UR.addCmd cmd

        ToDatePicker subMsg ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update settings subMsg model.datePicker

                newDate =
                    case dateEvent of
                        Picked changedDate ->
                            Just changedDate

                        _ ->
                            model.date
            in
            { model
                | date = newDate
                , datePicker = newDatePicker
            }
                |> UR.init

        _ ->
            model |> UR.init


type alias UpdateResult =
    UR.UpdateResult Model Msg External



-- VIEW


view : Guest.Model -> Model -> Html Msg
view guest model =
    case model.fetchingTransfersStatus of
        LoadedGraphql _ ->
            div [ class "bg-white" ]
                [ viewSplash model.userFullName
                , div [ class "mx-4 max-w-md md:m-auto" ]
                    [ h2 [ class "text-center text-black text-2xl" ] [ text "Payment History" ]
                    , viewUserAutocomplete guest model
                    , viewPeriodSelector model
                    , viewPayersList guest model
                    , viewPagination
                    ]
                ]

        LoadingGraphql ->
            Page.fullPageLoading

        FailedGraphql err ->
            div [] [ Page.fullPageGraphQLError "Sorry, no user found" err ]


viewSplash name =
    div
        [ class "bg-black bg-cover h-56 mb-6 flex justify-center items-center"
        , style "background-image" "url(/images/bg_cafe.png)"
        ]
        [ h1 [ class "text-white text-center text-5xl mx-3" ] [ text name ]
        ]


viewUserAutocomplete guest model =
    div [ class "my-4" ]
        [ label
            [ class "block" ]
            [ span [ class "text-green tracking-wide uppercase text-caption block mb-1" ]
                [ text "Payer" ]
            ]
        , viewAutoCompleteAccount guest.shared model False
        ]


viewAutoCompleteAccount : Shared.Shared -> Model -> Bool -> Html Msg
viewAutoCompleteAccount shared model isDisabled =
    let
        users =
            []

        selectedUsers =
            Maybe.map (\v -> [ v ]) model.selectedProfile
                |> Maybe.withDefault []
    in
    div []
        [ Html.map SelectMsg
            (Select.view
                (selectConfiguration shared isDisabled)
                model.autocompleteState
                users
                selectedUsers
            )
        ]


selectConfiguration : Shared.Shared -> Bool -> Select.Config Msg Profile
selectConfiguration shared isDisabled =
    Profile.selectConfig
        (Select.newConfig
            { onSelect = OnSelect
            , toLabel = \p -> Eos.Account.nameToString p.account
            , filter = Profile.selectFilter 2 (\p -> Eos.Account.nameToString p.account)
            }
            |> Select.withInputClass "form-input"
        )
        shared
        isDisabled


viewPeriodSelector model =
    div [ class "my-4" ]
        [ label
            [ class "block" ]
            [ span [ class "text-green tracking-wide uppercase text-caption block mb-1" ]
                [ text "Date" ]
            , DatePicker.view model.date settings model.datePicker
                |> Html.map ToDatePicker
            ]
        ]


viewPayment guest payment =
    let
        payer =
            payment.from

        userName =
            Eos.Account.nameToString payer.account

        time =
            Utils.posixDateTime (Just payment.blockTime)
                |> Strftime.format "%d %b %Y" Time.utc

        ipfsUrl =
            guest.shared.endpoints.ipfs

        avatarImg =
            Avatar.view ipfsUrl payer.avatar "max-w-full max-h-full"

        amount =
            String.concat
                [ String.fromFloat payment.value
                , " "
                , Eos.symbolToString payment.symbol
                ]
    in
    div
        [ class "bg-gray-100 text-center py-6 my-6 rounded-lg" ]
        [ div [ class "rounded-full m-auto overflow-hidden border-white border-2 bg-grey w-14 h-14" ]
            [ avatarImg ]
        , p [ class "text-black mt-2" ]
            [ text userName ]
        , p [ class "uppercase text-gray-900 text-xs my-1" ]
            [ text time ]
        , p [ class "text-green text-4xl my-3" ]
            [ text amount ]
        , p [ class "tracking-widest text-2xl" ]
            [ text "\u{1F916} 🇨🇷 💜 😙 😎 💻 😇 🎃" ]
        ]


viewPayersList : Guest.Model -> Model -> Html msg
viewPayersList guest model =
    ul [ class "" ]
        (List.map (viewPayment guest) model.transfersToCurrentUser)


viewPagination =
    div [ class "pb-8" ]
        [ button [ class "button m-auto button-primary w-full sm:w-40" ] [ text "Show More" ]
        ]
