module Page.PaymentHistory exposing (Model, Msg, init, msgToString, update, view)

import Date exposing (Date, day, fromPosix, month, weekday, year)
import DatePicker exposing (DateEvent(..), defaultSettings, off)
import Eos.Account as Eos
import Html exposing (Html, button, div, h1, h2, img, input, label, p, span, text, ul)
import Html.Attributes exposing (class, placeholder, src, style)
import Profile exposing (Profile)
import Select
import Session.Guest as Guest exposing (External(..))
import Session.Shared as Shared
import Task
import Time exposing (Month(..), Weekday(..))
import UpdateResult as UR



-- MSG


type Msg
    = NoOp
    | OnSelect (Maybe Profile)
    | SelectMsg (Select.Msg Profile)
    | ToDatePicker DatePicker.Msg


msgToString : Msg -> List String
msgToString msg =
    case msg of
        NoOp ->
            [ "NoOp" ]

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
    , users : List Profile
    , date : Maybe Date
    , datePicker : DatePicker.DatePicker
    }


settings : DatePicker.Settings
settings =
    { defaultSettings
        | changeYear = off
        , inputClassList = [ ( "input", True ), ( "w-full", True ) ]
        , dateFormatter = Date.format "E, d MMM y"
    }


init : Guest.Model -> ( Model, Cmd Msg )
init guest =
    let
        ( datePicker, datePickerFx ) =
            DatePicker.init
    in
    ( { autocompleteState = Select.newState ""
      , selectedProfile = Nothing
      , users = []
      , date = Nothing
      , datePicker = datePicker
      }
    , Cmd.map ToDatePicker datePickerFx
    )



-- UPDATE


update : Msg -> Model -> Guest.Model -> UpdateResult
update msg model guest =
    case msg of
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
    div [ class "bg-white" ]
        [ viewSplash
        , div [ class "mx-4 max-w-md md:m-auto" ]
            [ h2 [ class "text-center text-black text-2xl" ] [ text "Payment History" ]
            , viewUserAutocomplete guest model
            , viewPeriodSelector model
            , viewPayersList
            , viewPagination
            ]
        ]


viewSplash =
    div
        [ class "bg-black bg-cover h-56 mb-6 flex justify-center items-center"
        , style "background-image" "url(/images/bg_cafe.png)"
        ]
        [ h1 [ class "text-white text-center text-5xl mx-3" ] [ text "Pura Vida Cafe" ]
        ]


viewUserAutocomplete guest model =
    div [ class "my-4" ]
        [ label
            [ class "block" ]
            [ span [ class "text-green tracking-wide uppercase text-caption block mb-1" ]
                [ text "User" ]
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
            , toLabel = \p -> Eos.nameToString p.account
            , filter = Profile.selectFilter 2 (\p -> Eos.nameToString p.account)
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
                [ text "Period" ]
            , DatePicker.view model.date settings model.datePicker
                |> Html.map ToDatePicker
            ]
        ]


viewPayment payment =
    div
        [ class "bg-gray-100 text-center py-6 my-6 rounded-lg"
        ]
        [ div [ class "rounded-full m-auto overflow-hidden border-white border-2 bg-grey w-14 h-14" ]
            [ img
                [ class "max-w-full max-h-full"
                , src payment.userpic
                ]
                []
            ]
        , p [ class "text-black mt-2" ]
            [ text payment.username ]
        , p [ class "uppercase text-gray-900 text-xs my-1" ]
            [ text payment.paymentDate ]
        , p [ class "text-green text-4xl my-3" ]
            [ text payment.paymentAmount ]
        , p [ class "tracking-widest text-2xl" ]
            [ text payment.emojiHash ]
        ]


viewPayersList =
    ul [ class "" ]
        (List.map viewPayment
            (List.repeat
                10
                { userpic = "/images/woman.png"
                , username = "vasya222"
                , paymentDate = "today, 14:03"
                , paymentAmount = "1234 COS"
                , emojiHash = "\u{1F916}🇨🇷💜😙😎💻😇🎃"
                }
            )
        )


viewPagination =
    div [ class "pb-8" ]
        [ button [ class "button m-auto button-primary w-full sm:w-40" ] [ text "Show More" ] ]
