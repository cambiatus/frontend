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
    | CompletedProfileLoad (Result (Graphql.Http.Error (Maybe Profile)) (Maybe Profile))
    | OnSelect (Maybe Profile)
    | SelectMsg (Select.Msg Profile)
    | ToDatePicker DatePicker.Msg


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

        CompletedProfileLoad r ->
            [ "CompletedProfileLoad", UR.resultToString r ]

        OnSelect _ ->
            [ "OnSelect" ]

        SelectMsg _ ->
            [ "SelectMsg" ]

        ToDatePicker _ ->
            [ "ToDatePicker" ]



-- MODEL


type alias Model =
    { payerAutocompleteState : Select.State
    , selectedPayerProfile : Maybe Profile
    , currentProfile : Maybe Profile
    , profileLoadingStatus : QueryStatus (Maybe Profile) Profile
    , transfersLoadingStatus : QueryStatus (Maybe QueryTransfers) (List Transfer)
    , selectedDate : Maybe Date
    , datePicker : DatePicker.DatePicker
    , transfersToCurrentUser : List Transfer
    }


type QueryStatus err resp
    = Loading
    | Loaded resp
    | Failed (Graphql.Http.Error err)


fetchProfile : Shared -> Eos.Account.Name -> Cmd Msg
fetchProfile shared accountName =
    Api.Graphql.query shared
        (Profile.query accountName)
        CompletedProfileLoad


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
    ( { payerAutocompleteState = Select.newState ""
      , selectedPayerProfile = Nothing
      , selectedDate = Nothing
      , currentProfile = Nothing
      , profileLoadingStatus = Loading
      , transfersLoadingStatus = Loading
      , transfersToCurrentUser = []
      , datePicker = datePicker
      }
    , Cmd.batch
        [ Cmd.map ToDatePicker datePickerFx
        , fetchProfile guest.shared accountName
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
                    case model.currentProfile of
                        Just p ->
                            filterTransfers (Transfer.getTransfers maybeTransfers) p.account

                        Nothing ->
                            []
            in
            { model
                | transfersLoadingStatus = Loaded (Transfer.getTransfers maybeTransfers)
                , transfersToCurrentUser = transfersToCurrentUser
            }
                |> UR.init

        CompletedLoadUserTransfers (Err err) ->
            { model | transfersLoadingStatus = Failed err }
                |> UR.init
                |> UR.logGraphqlError msg err

        CompletedProfileLoad (Ok Nothing) ->
            -- TODO: not found account
            UR.init model

        CompletedProfileLoad (Ok (Just profile)) ->
            UR.init
                { model
                    | profileLoadingStatus = Loaded profile
                    , currentProfile = Just profile
                }

        CompletedProfileLoad (Err err) ->
            UR.init { model | profileLoadingStatus = Failed err }
                |> UR.logGraphqlError msg err

        OnSelect maybeProfile ->
            { model
                | selectedPayerProfile = maybeProfile
            }
                |> UR.init

        SelectMsg subMsg ->
            let
                ( updated, cmd ) =
                    Select.update (selectConfiguration guest.shared False) subMsg model.payerAutocompleteState
            in
            UR.init { model | payerAutocompleteState = updated }
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
                            model.selectedDate
            in
            { model
                | selectedDate = newDate
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
    div [ class "bg-white" ]
        [ case model.currentProfile of
            Just p ->
                viewSplash model

            Nothing ->
                div [] [ text "profile not found" ]
        , div [ class "mx-4 max-w-md md:m-auto" ]
            [ h2 [ class "text-center text-black text-2xl" ] [ text "Payment History" ]
            , case model.transfersLoadingStatus of
                Loaded _ ->
                    if List.isEmpty model.transfersToCurrentUser then
                        div [] [ text "No payment for this user" ]

                    else
                        div []
                            [ viewUserAutocomplete guest model
                            , viewPeriodSelector model
                            , viewPayersList guest model
                            , viewPagination
                            ]

                Loading ->
                    div [] [ text "loading" ]

                Failed err ->
                    div [] [ Page.fullPageGraphQLError "Sorry, no user found" err ]
            ]
        ]


viewSplash model =
    let
        name =
            case model.currentProfile of
                Just p ->
                    Maybe.withDefault (Eos.Account.nameToString p.account) p.userName

                Nothing ->
                    ""
    in
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
            Maybe.map (\v -> [ v ]) model.selectedPayerProfile
                |> Maybe.withDefault []
    in
    div []
        [ Html.map SelectMsg
            (Select.view
                (selectConfiguration shared isDisabled)
                model.payerAutocompleteState
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
            , DatePicker.view model.selectedDate settings model.datePicker
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
